{-# LANGUAGE 
    TypeOperators 
    , DataKinds
    , StandaloneDeriving
    , FlexibleInstances
#-}
module Main where

import           RIO
-- import qualified RIO.Text                      as T

import           AppState
import           Config
import qualified Data.Text.IO                  as T

import           Options.Generic

import           Chains


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
        let app = AppState { appLogFunc = logF, appConfig = cfg }

        runRIO app $ do
            logInfo "Starting app"
            runChains
