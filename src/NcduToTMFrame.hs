module NcduToTMFrame
    ( TMFrameMeta(..)
    , ncduToTMFrameC
    ) where

import           RIO
import qualified RIO.Text                      as T


import           Conduit
import           Data.Attoparsec.ByteString

import           Data.Time.Clock

import           CDSTime
import           CRC
import           Config
import           NCTRS
import           TMFrame


data TMFrameMeta = TMFrameMeta
    { metaERT     :: UTCTime
    , metaQuality :: QualityFlag
    , metaFrame   :: TMFrame
    }
    deriving Show


ncduToTMFrameC :: (MonadIO m, MonadReader env m, HasLogFunc env) => Config -> ConduitT NcduTM TMFrameMeta m ()
ncduToTMFrameC cfg = awaitForever $ \ncdu -> do
    let dat = ncduData ncdu
    if checkCRC dat
        then do
            case parseOnly (tmFrameParser cfg) dat of
                Left err -> do
                    logError $  "Error parsing TM Frame: " <> display (T.pack err)
                Right frame -> do
                    let meta = TMFrameMeta { metaERT = toUTCTime (ncduERT ncdu)
                                           , metaQuality = ncduQuality ncdu
                                           , metaFrame = frame
                                           }
                    yield meta
        else do
            logError $ "CRC error on frame: " <> display (T.pack (show dat))
