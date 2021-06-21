module Chains
    ( prettyShowC
    , runChains
    ) where

import           Classes
import           RIO
import qualified RIO.Text                      as T

import           Conduit
import           Config
import           Data.Conduit.Network
import           Data.Conduit.TQueue
import qualified Data.IntMap.Strict            as M
import           NCTRS
import           NcduToTMFrame
import           TMFrame
import           Text.Show.Pretty


prettyShowC
    :: (MonadIO m, MonadReader env m, HasLogFunc env, Show a)
    => ConduitT a Void m ()
prettyShowC = awaitForever $ \x -> logDebug $ display (T.pack (ppShow x))

prettyShowVcC
    :: (MonadIO m, MonadReader env m, HasLogFunc env, Show a)
    => Int -> ConduitT a Void m ()
prettyShowVcC vcid = awaitForever $ \x -> logDebug $ "VC " <> display vcid <> display '\n' <> display (T.pack (ppShow x))


runNctrsChain
    :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env, HasConfig env)
    => SwitcherMap -> m ()
runNctrsChain switcherMap = do
    cfg <- getConfig <$> ask

    let settings = clientSettings (fromIntegral (cfgPort cfg))
                                  (encodeUtf8 (cfgHostname cfg))

    res <- try $ runGeneralTCPClient settings $ \appData -> do
        runConduitRes
            $  appSource appData
            .| ncduTmC
            .| ncduToTMFrameC
            .| vcSwitcherC switcherMap
    case res of
        Left (_ :: IOException) -> do
            logWarn "Could not connect, reconnecting..."
            threadDelay 2_000_000
            runNctrsChain switcherMap
        Right _ -> runNctrsChain switcherMap




type SwitcherMap = IntMap (TBQueue TMFrameMeta)


vcSwitcherC
    :: (MonadIO m, MonadReader env m, HasLogFunc env)
    => SwitcherMap
    -> ConduitT TMFrameMeta Void m ()
vcSwitcherC switcherMap = awaitForever $ \meta -> do
    let vcid = frHdrVCID . frameHdr . metaFrame $ meta
    case M.lookup (fromIntegral vcid) switcherMap of
        Just queue -> do
            atomically $ writeTBQueue queue meta
        Nothing -> do
            logWarn
                $  "Virtual channel "
                <> display vcid
                <> " not configured, ignoring frame: "
                <> displayShow meta



vcChain
    :: (MonadIO m, MonadReader env m, HasLogFunc env)
    => (Int, TBQueue TMFrameMeta)
    -> ConduitT a Void m ()
vcChain (vcid, queue) = sourceTBQueue queue .| gapCheckC .| prettyShowVcC vcid


queueSize :: Natural
queueSize = 500


runVcChain
    :: (MonadIO m)
    => ConduitT () Void m ()
    -> m ()
runVcChain chain = runConduit chain


runChains
    :: (MonadUnliftIO m, MonadReader env m, HasConfig env, HasLogFunc env) => m ()
runChains = do
    cfg <- getConfig <$> ask

    lst <- mapM createQueue $ cfgVCIDs cfg
    let switcherMap = M.fromList lst

    race_ (forConcurrently_ lst (runVcChain . vcChain))
          (runNctrsChain switcherMap)
  where
    createQueue vcid = do
        queue <- newTBQueueIO queueSize
        pure (fromIntegral vcid, queue)


gapCheckC :: (Monad m) => ConduitT TMFrameMeta TMFrameMeta m () 
gapCheckC = go Nothing 
    where 
        go oldVCFC' = do 
            x <- await 
            case x of 
                Nothing -> pure ()
                Just meta -> do 
                    let !vcfc = frHdrVCFC . frameHdr . metaFrame $ meta
                    case oldVCFC' of 
                        Nothing -> go (Just vcfc)
                        Just oldVCFC -> do 
                            if oldVCFC + 1 == vcfc 
                                then do 
                                    yield meta 
                                    go (Just vcfc)
                                else do 
                                    let !newMeta = meta { metaGap = Just (oldVCFC, vcfc) }
                                    yield newMeta 
                                    go (Just vcfc)

