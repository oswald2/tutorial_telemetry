{-# LANGUAGE 
    TypeOperators 
    , DataKinds
    , StandaloneDeriving
    , FlexibleInstances
#-}
module Main where

-- import qualified RIO.Text                      as T
import           RIO

import           AppState
import           Config
import qualified Data.Text.IO                  as T

import           Options.Generic

import           Chains
import           GHC.Conc
import           TMDefinitions



data Options w = Options
    { version            :: w ::: Bool <?> "Display version information"
    , config             :: w ::: Maybe String <?> "Specify a config file"
    , writeDefaultConfig :: w ::: Bool <?> "Write default config to a file"
    , writeDefaultTMDefs
          :: w ::: Bool <?> "Write default TM Packet Definitions to a file"
    , tmPacketDefs :: w ::: Maybe String <?> "Specify a TM Packet Definitions file"
    }
    deriving Generic

instance ParseRecord (Options Wrapped)
deriving instance Show (Options Wrapped)



main :: IO ()
main = do
    np <- getNumProcessors
    setNumCapabilities np

    opts <- unwrapRecord "tutorial-telemetry"
    when (version opts) $ do
        T.putStrLn "tutorial-telemeter version: 1.0.0"
        exitSuccess
    when (writeDefaultConfig opts) $ do
        writeConfig defaultConfig "DefaultConfig.json"
        T.putStrLn "Wrote default config into 'DefaultConfig.json'."
        exitSuccess
    when (writeDefaultTMDefs opts) $ do
        writeTMDefinitions defaultDefs "DefaultTMDefinitions.json"
        T.putStrLn "Wrote default config into 'DefaultTMDefinitions.json'."
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
    defs <- generatePktIdx <$> case tmPacketDefs opts of
        Nothing   -> pure defaultDefs
        Just file -> do
            res <- readTMDefinitions file
            case res of
                Left err -> do
                    T.putStrLn err
                    exitFailure
                Right c -> pure c

    logOptions <- logOptionsHandle stderr True
    --let logOptions = setLogMinLevel LevelWarn logOptions'

    withLogFunc logOptions $ \logF -> do
        let app = AppState { appLogFunc = logF, appConfig = cfg }

        runRIO app $ do
            logInfo "Starting app"
            runChains defs 
