module TMFrame
    ( TMFrame(..)
    , TMFrameHeader(..)
    , tmFrameParser
    , idleFrameFHP
    ) where


import           Data.Attoparsec.Binary
import           Data.Attoparsec.ByteString    as A
import           RIO

import           Data.Bits

import           CRC
import           Config

data TMFrameHeader = TMFrameHeader
    { frHdrVersion :: !Word8
    , frHdrSCID    :: !Word16
    , frHdrVCID    :: !Word8
    , frHdrMCFC    :: !Word8
    , frHdrVCFC    :: !Word8
    , frHdrFHP     :: !Word16
    }
    deriving Show


idleFrameFHP :: Word16 
idleFrameFHP = 0b11111111110


data TMFrame = TMFrame
    { frameHdr  :: !TMFrameHeader
    , frameData :: !ByteString
    , frameOcf  :: Maybe Word32
    }
    deriving Show



tmFrameHeaderParser :: Parser (Bool, TMFrameHeader)
tmFrameHeaderParser = do
    w1   <- anyWord16be
    mcfc <- anyWord8
    vcfc <- anyWord8
    w2   <- anyWord16be

    let version = fromIntegral $ (w1 .&. 0b1100_0000_0000_0000) `shiftR` 14
        scid    = (w1 .&. 0b0011_1111_1111_0000) `shiftR` 4
        vcid    = fromIntegral $ (w1 .&. 0b0000_0000_0000_1110) `shiftR` 1
        ocfFlag = (w1 .&. 0b0000_0000_0000_0001) /= 0

        fhp     = w2 .&. 0b0000_0111_1111_1111

    pure
        ( ocfFlag
        , TMFrameHeader { frHdrVersion = version
                        , frHdrSCID    = scid
                        , frHdrVCID    = vcid
                        , frHdrMCFC    = mcfc
                        , frHdrVCFC    = vcfc
                        , frHdrFHP     = fhp
                        }
        )


hdrLength :: Int
hdrLength = 6

calcDataLen :: Config -> Bool -> Int
calcDataLen cfg ocfFlag =
    fromIntegral (cfgTmFrameLen cfg) - hdrLength - ocfLen - crcLength
    where ocfLen = if ocfFlag then 4 else 0


tmFrameParser :: Config -> Parser TMFrame
tmFrameParser cfg = do
    (ocfFlag, hdr) <- tmFrameHeaderParser
    dat            <- A.take (calcDataLen cfg ocfFlag)
    ocf            <- if ocfFlag then Just <$> anyWord32be else pure Nothing

    pure TMFrame { frameHdr = hdr, frameData = dat, frameOcf = ocf }
