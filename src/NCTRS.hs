module NCTRS
    ( NcduTM(..)
    , QualityFlag(..)
    , NcduStreamType(..)
    , StreamType(..)
    , ncduTmC
    ) where


import           RIO

import           Data.Attoparsec.Binary
import           Data.Attoparsec.ByteString    as A

import           Conduit
import           Data.Conduit.Attoparsec

import           CDSTime


data QualityFlag = Good | Bad
  deriving (Show, Generic)


data StreamType = ONLT | ONLC | OFFL
  deriving (Show, Generic)

data NcduStreamType =
  SLC StreamType
  | MC StreamType
  | VC StreamType
  | BadFrame StreamType
  deriving (Show, Generic)



data NcduTM = NcduTM
    { ncduSize           :: !Word32
    , ncduSCID           :: !Word16
    , ncduDataStreamType :: NcduStreamType
    , ncduVCID           :: !Word8
    , ncduRouteID        :: !Word16
    , ncduERT            :: !CDSTime 
    , ncduSequence       :: !Word8
    , ncduQuality        :: !QualityFlag
    , ncduData           :: !ByteString
    }
    deriving (Show, Generic)

ncduStreamTypeParser :: Parser NcduStreamType
ncduStreamTypeParser = do
    b <- anyWord8
    case b of
        0  -> pure $ SLC ONLT
        1  -> pure $ MC ONLT
        2  -> pure $ VC ONLT
        3  -> pure $ BadFrame ONLT
        4  -> pure $ SLC ONLC
        5  -> pure $ MC ONLC
        6  -> pure $ VC ONLC
        7  -> pure $ BadFrame ONLC
        8  -> pure $ SLC OFFL
        9  -> pure $ MC OFFL
        10 -> pure $ VC OFFL
        11 -> pure $ BadFrame OFFL
        x  -> fail $ "Illegal NCDU TM Stream Type: " <> show x



qualityFlagParser :: Parser QualityFlag
qualityFlagParser = do
    b <- anyWord8
    case b of
        0 -> pure Good
        1 -> pure Bad
        x -> fail $ "Illegal NCDU qualitiy flag value: " <> show x



ncduTmParser :: Parser NcduTM
ncduTmParser = do
    size       <- anyWord32be
    scid       <- anyWord16be
    streamType <- ncduStreamTypeParser
    vcid       <- anyWord8
    route      <- anyWord16be
    ert        <- cdsTimeParser 
    sequ       <- anyWord8
    quality    <- qualityFlagParser

    let len = fromIntegral size - 20

    when (len < 0) $ fail $ "Illegal size in NCDU TM message: " <> show size

    dat <- A.take len

    pure NcduTM { ncduSize           = size
                , ncduSCID           = scid
                , ncduDataStreamType = streamType
                , ncduVCID           = vcid
                , ncduRouteID        = route
                , ncduERT            = ert
                , ncduSequence       = sequ
                , ncduQuality        = quality
                , ncduData           = dat
                }







ncduTmC :: (MonadIO m, MonadReader env m, HasLogFunc env) => ConduitT ByteString NcduTM m ()
ncduTmC = conduitParserEither ncduTmParser .| sink
  where
    sink = do
        x <- await
        case x of
            Just (Left err) -> do
                logError $ "Error on parsing NCDU TM: " <> displayShow err
            Just (Right (_, ncdu)) -> do
                yield ncdu
                sink
            Nothing -> pure ()
