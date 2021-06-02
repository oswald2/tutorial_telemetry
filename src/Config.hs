module Config
    ( Config(..)
    , defaultConfig
    , writeConfig
    , readConfig
    ) where

import           Data.Aeson
import           Data.Aeson.Encode.Pretty       ( encodePretty )
import           RIO
import qualified RIO.ByteString.Lazy           as BL
import qualified RIO.Text                      as T

data Config = Config
    { cfgHostname :: !Text
    , cfgPort     :: !Word16
    }
    deriving (Show, Generic, ToJSON, FromJSON)


defaultConfig :: Config
defaultConfig = Config { cfgHostname = "localhost", cfgPort = 2502 }


writeConfig :: Config -> FilePath -> IO ()
writeConfig cfg file = BL.writeFile file (encodePretty cfg)


readConfig :: FilePath -> IO (Either Text Config)
readConfig file = do
    res <- try $ BL.readFile file
    case res of
        Left (e :: IOException) -> do
            pure
                $  Left
                $  T.pack
                $  "Could not open file: "
                <> file
                <> ", reason: "
                <> show e
        Right content -> case eitherDecode content of
            Left  err -> pure $ Left (T.pack err)
            Right cfg -> pure (Right cfg)
