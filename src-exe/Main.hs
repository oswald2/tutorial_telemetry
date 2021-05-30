module Main where

import           RIO
import qualified RIO.ByteString                as BS
import qualified RIO.Text                      as T

import           Conduit
import           Data.Conduit.Network
import qualified Data.Text.IO                  as T

import           Config


bsLengthC :: (MonadIO m) => ConduitT ByteString Void m ()
bsLengthC =
    awaitForever $ \bs -> liftIO $ T.putStrLn (T.pack (show (BS.length bs)))


main :: IO ()
main = do

    let settings      = clientSettings (fromIntegral (cfgPort cfg)) (encodeUtf8 (cfgHostname cfg))
        cfg           = defaultConfig

        connectClient = do
            res <- try $ runGeneralTCPClient settings $ \appData -> do
                runConduitRes $ appSource appData .| bsLengthC
            case res of
                Left (e :: SomeException) -> do
                    T.putStrLn "Could not connect, reconnecting..."
                    threadDelay 2_000_000
                    connectClient
                Right _ -> connectClient

    connectClient
