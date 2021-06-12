{-# LANGUAGE 
    TypeOperators 
    , DataKinds
    , StandaloneDeriving
    , FlexibleInstances
#-}
module Main where

import           RIO
import qualified RIO.Text                      as T

import           AppState
import           Conduit
import           Config
import           Data.Conduit.Network
import qualified Data.Text.IO                  as T
import           NCTRS
import           NcduToTMFrame

import           Options.Generic
import           Text.Show.Pretty


prettyShowC :: (MonadIO m, MonadReader env m, HasLogFunc env, Show a) => ConduitT a Void m ()
prettyShowC = awaitForever $ \x -> logDebug $ display (T.pack (ppShow x))



connectClient :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env, HasConfig env) => m ()
connectClient = do
    cfg <- getConfig <$> ask 

    let settings = clientSettings (fromIntegral (cfgPort cfg))
                                  (encodeUtf8 (cfgHostname cfg))

    res <- try $ runGeneralTCPClient settings $ \appData -> do
        runConduitRes
            $  appSource appData
            .| ncduTmC
            .| ncduToTMFrameC cfg
            .| prettyShowC
    case res of
        Left (_ :: SomeException) -> do
            logWarn "Could not connect, reconnecting..."
            threadDelay 2_000_000
            connectClient
        Right _ -> connectClient 


data Options w = Options
    { version            :: w ::: Bool <?> "Display version information"
    , config             :: w ::: Maybe String <?> "Specify a config file"
    , writeDefaultConfig :: w ::: Bool <?> "Write default config to a file"
    }
    deriving Generic

instance ParseRecord (Options Wrapped)
deriving instance Show (Options Wrapped)



main :: IO ()
main = do
    opts <- unwrapRecord "tutorial-telemetry"
    when (version opts) $ do
        T.putStrLn "tutorial-telemeter version: 1.0.0"
        exitSuccess
    when (writeDefaultConfig opts) $ do
        writeConfig defaultConfig "DefaultConfig.json"
        T.putStrLn "Wrote default config into 'DefaultConfig.json'."
        exitSuccess
    cfg <- case config opts of
        Nothing   -> pure defaultConfig
        Just file -> do
            res <- readConfig file
            case res of
                Left err -> do
                    T.putStrLn err
                    exitFailure
                Right c -> pure c

    logOptions <- logOptionsHandle stderr True

    withLogFunc logOptions $ \logF -> do
        let app = AppState 
                { appLogFunc = logF
                , appConfig = cfg
                }
                
        runRIO app $ do
            logInfo "Starting app"
            connectClient 
