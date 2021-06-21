module PUSPacket
    ( PUSPktType(..)
    , PUSPktSeqFlags(..)
    , PUSPacketHdr(..)
    , PUSSecStdHdr(..)
    , PUSSecHdr(..)
    , PUSType(..)
    , PUSSubType(..)
    , pusPktParser
    , idleApid
    ) where

import           RIO

import           CRC
import           CUCTime
import           Data.Attoparsec.Binary
import           Data.Attoparsec.ByteString    as A
import           Data.Bits

data PUSPktType = PusTM | PusTC
  deriving (Show)

getType :: Word16 -> PUSPktType
getType val = if val .&. 0b0001_0000_0000_0000 == 0 then PusTM else PusTC


getDfhFlag :: Word16 -> Bool
getDfhFlag val = (val .&. 0b0000_1000_0000_0000) /= 0


data PUSPktSeqFlags =
  SegFirst
  | SegCont
  | SegLast
  | SegStandalone
  deriving (Eq, Ord, Enum, Show)



getSeqFlags :: Word16 -> PUSPktSeqFlags
getSeqFlags val = case val .&. 0b1100_0000_0000_0000 of
    0b0100_0000_0000_0000 -> SegFirst
    0b0000_0000_0000_0000 -> SegCont
    0b1000_0000_0000_0000 -> SegLast
    0b1100_0000_0000_0000 -> SegStandalone
    _                     -> absurd undefined 


idleApid :: Word16 
idleApid = 0b11111111111


data PUSPacketHdr = PUSPacketHdr
    { pHdrType     :: !PUSPktType
    , pHdrDfhFlag  :: !Bool
    , pHdrAPID     :: !Word16
    , pHdrSeqFlags :: !PUSPktSeqFlags
    , pHdrSSC      :: !Word16
    , pHdrLength   :: !Word16
    }
    deriving Show

pusPktHdrParser :: Parser PUSPacketHdr
pusPktHdrParser = do
    w1  <- anyWord16be
    w2  <- anyWord16be
    len <- anyWord16be

    pure PUSPacketHdr { pHdrType     = getType w1
                      , pHdrDfhFlag  = getDfhFlag w1
                      , pHdrAPID     = w1 .&. 0b0000_0111_1111_1111
                      , pHdrSeqFlags = getSeqFlags w2
                      , pHdrSSC      = w2 .&. 0b0011_1111_1111_1111
                      , pHdrLength   = len
                      }



data PUSSecStdHdr = PUSSecStdHdr
    { pDfhVersion   :: !Word8
    , pDfhType     :: PUSType
    , pDfhSubType  :: PUSSubType
    , pDfhSourceID :: !Word8
    , pDfhTime     :: !CUCTime
    }
    deriving Show

pusSecStdHdrParser :: Parser PUSSecStdHdr
pusSecStdHdrParser =
    PUSSecStdHdr
        <$> anyWord8
        <*> pusTypeParser
        <*> pusSubTypeParser
        <*> anyWord8
        <*> cucTimeParser


newtype PUSType = PUSType Word8
  deriving (Show)

pusTypeParser :: Parser PUSType
pusTypeParser = PUSType <$> anyWord8


newtype PUSSubType = PUSSubType Word8
  deriving (Show)

pusSubTypeParser :: Parser PUSSubType
pusSubTypeParser = PUSSubType <$> anyWord8


data PUSSecHdr =
    PUSEmptyHeader
    | PUSStdHeader PUSSecStdHdr
  deriving (Show)


pusSecHdrParser :: Bool -> Parser PUSSecHdr
pusSecHdrParser False = pure PUSEmptyHeader
pusSecHdrParser True  = PUSStdHeader <$> pusSecStdHdrParser

dfhLength :: PUSSecHdr -> Int
dfhLength PUSEmptyHeader = 0
dfhLength PUSStdHeader{} = 10


data PUSPacket = PUSPacket
    { pusHdr    :: !PUSPacketHdr
    , pusSecHdr :: !PUSSecHdr
    , pusData   :: !ByteString
    , pusCRC    :: !Word16
    }
    deriving Show


pusPktParser :: Parser PUSPacket
pusPktParser = do
    hdr <- pusPktHdrParser
    dfh <- pusSecHdrParser (pHdrDfhFlag hdr)

    let len = fromIntegral (pHdrLength hdr) + 1 - dfhLength dfh - crcLength
    dat <- A.take len
    crc <- anyWord16be

    pure PUSPacket { pusHdr    = hdr
                   , pusSecHdr = dfh
                   , pusData   = dat
                   , pusCRC    = crc
                   }
