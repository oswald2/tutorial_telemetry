module NcduToTMFrame
    ( TMFrameMeta(..)
    , ncduToTMFrameC
    ) where

import           RIO
import qualified RIO.Text                      as T


import           Conduit
import           Data.Attoparsec.ByteString

import           Data.Time.Clock

import           AppState
import           CDSTime
import           CRC
import           NCTRS
import           TMFrame


data TMFrameMeta = TMFrameMeta
    { metaERT     :: !UTCTime
    , metaQuality :: !QualityFlag
    , metaGap     :: Maybe (Word8, Word8)
    , metaFrame   :: !TMFrame
    }
    deriving (Show, Generic, NFData)


ncduToTMFrameC
    :: ConduitT NcduTM TMFrameMeta (ResourceT (RIO AppState)) ()
ncduToTMFrameC = do
    cfg <- getConfig <$> ask
    awaitForever $ \ncdu -> do
        let dat = ncduData ncdu
        if checkCRC dat
            then do
                case parseOnly (tmFrameParser cfg) dat of
                    Left err -> do
                        logError $ "Error parsing TM Frame: " <> display
                            (T.pack err)
                    Right frame -> do
                        let meta = force $ TMFrameMeta
                                { metaERT     = toUTCTime (ncduERT ncdu)
                                , metaQuality = ncduQuality ncdu
                                , metaGap     = Nothing 
                                , metaFrame   = frame
                                }
                        yield meta
            else do
                logError $ "CRC error on frame: " <> display (T.pack (show dat))
