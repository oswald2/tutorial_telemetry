module Main where

import qualified MyLib                          ( someFunc )
import           RIO
import qualified RIO.ByteString                as BS
import qualified RIO.Text                      as T

import           Conduit
import           Data.Conduit.Network
import qualified Data.Text.IO                  as T


bsLengthC :: (MonadIO m) => ConduitT ByteString Void m ()
bsLengthC = awaitForever $ \bs -> liftIO $ T.putStrLn (T.pack (show (BS.length bs)))


main :: IO ()
main = do

    let settings = clientSettings 2502 "localhost"

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