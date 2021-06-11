module NcduToTMFrame
    ( TMFrameMeta(..)
    , ncduToTMFrameC
    ) where

import qualified Data.Text.IO                  as T
import           RIO
import qualified RIO.Text                      as T


import           Conduit
import           Data.Attoparsec.ByteString

import           Data.Time.Clock

import           CDSTime
import           Config
import           NCTRS
import           TMFrame


data TMFrameMeta = TMFrameMeta
    { metaERT     :: UTCTime
    , metaQuality :: QualityFlag
    , metaFrame   :: TMFrame
    } deriving (Show)


ncduToTMFrameC :: (MonadIO m) => Config -> ConduitT NcduTM TMFrameMeta m ()
ncduToTMFrameC cfg = awaitForever $ \ncdu -> do
    case parseOnly (tmFrameParser cfg) (ncduData ncdu) of
        Left err -> do
            liftIO $ T.putStrLn $ "Error parsing TM Frame: " <> T.pack err
        Right frame -> do
            let meta = TMFrameMeta { metaERT     = toUTCTime (ncduERT ncdu)
                                   , metaQuality = ncduQuality ncdu
                                   , metaFrame   = frame
                                   }
            yield meta

