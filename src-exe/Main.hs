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
import           Classes
import           GHC.Conc                       ( getNumProcessors
                                                , setNumCapabilities
                                                )
import           Statistics
import           TMDefinitions



data Options w = Options
    { version            :: w ::: Bool <?> "Display version information"
    , config             :: w ::: Maybe String <?> "Specify a config file"
    , writeDefaultConfig :: w ::: Bool <?> "Write default config to a file"
    , writeDefaultTMDefs
          :: w ::: Bool <?> "Write default TM Packet Definitions to a file"
    , tmPacketDefs
          :: w ::: Maybe String <?> "Specify a TM Packet Definitions file"
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
        frameVar <- newTVarIO initialStatistics
        pktVar   <- newTVarIO initialStatistics
        let app = AppState { appLogFunc          = logF
                           , appConfig           = cfg
                           , appFrameStatistics  = frameVar
                           , appPacketStatistics = pktVar
                           }

        runRIO app $ do
            logInfo "Starting app"

            void $ async statThread 

            void $ async $ runChains defs
            let loop = do 
                    l <- liftIO $ T.getLine 
                    if l == "quit" then return() else loop
            loop




statThread
    :: (MonadIO m, MonadReader env m, HasStats env, HasLogFunc env) => m ()
statThread = do
    env <- ask
    go env Nothing Nothing 
  where
    go env oldFrameStats oldPktStats = do
        threadDelay 2000000
        let frameVar  = getFrameStats env
            packetVar = getPacketStats env
        frameStats <- readTVarIO frameVar
        case oldFrameStats of
            Nothing       -> pure ()
            Just oldStats -> do
                let (totalRate, totalDataRate) = statTotal frameStats
                    (rate     , dataRate     ) = statCalc frameStats oldStats
                logDebug
                    $  "Frame Rate: total: "
                    <> display totalRate
                    <> " Frames/sec "
                    <> formatBytes totalDataRate
                    <> "   Current rate: "
                    <> display rate
                    <> " Frames/sec "
                    <> formatBytes dataRate
                    <> " Frames received: " 
                    <> display (frameStats ^. statN)

        pktStats <- readTVarIO packetVar
        case oldPktStats of
            Nothing       -> pure ()
            Just oldStats -> do
                let (totalRate, totalDataRate) = statTotal pktStats
                    (rate     , dataRate     ) = statCalc pktStats oldStats
                logDebug
                    $  "Packet Rate: total: "
                    <> display totalRate
                    <> " Pkts/sec "
                    <> formatBytes totalDataRate
                    <> "   Current rate: "
                    <> display rate
                    <> " Pkts/sec "
                    <> formatBytes dataRate
                    <> " Pkts received: " 
                    <> display (pktStats ^. statN)

        go env (Just frameStats) (Just pktStats)


    formatBytes n | n > 1_000_000_000 = display (n / 1_000_000_000) <> " GB/s"
                  | n > 1_000_000     = display (n / 1_000_000) <> " MB/s"
                  | n > 1_000         = display (n / 1_000) <> " KB/s"
                  | otherwise         = display n <> " B/s"
