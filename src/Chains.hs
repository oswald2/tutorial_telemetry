module Chains
    ( prettyShowC
    , runChains
    ) where

import           CRC
import           Classes
import           Conduit
import           Config
import qualified Data.Attoparsec.ByteString    as A
import           Data.Conduit.Network
import           Data.Conduit.TQueue
import qualified Data.IntMap.Strict            as M
import           Data.Time.Clock
import           NCTRS
import           NcduToTMFrame
import           PUSPacket
import           RIO
import qualified RIO.ByteString                as B
import qualified RIO.Text                      as T
import           TMDefinitions
import           TMFrame
import           TMPacket
import           Text.Show.Pretty



prettyShowC
    :: (MonadIO m, MonadReader env m, HasLogFunc env, Show a)
    => ConduitT a Void m ()
prettyShowC = awaitForever $ \x -> logDebug $ display (T.pack (ppShow x))

prettyShowVcC
    :: (MonadIO m, MonadReader env m, HasLogFunc env, Show a)
    => Int
    -> ConduitT a Void m ()
prettyShowVcC vcid = awaitForever $ \x ->
    logDebug $ "VC " <> display vcid <> display '\n' <> display
        (T.pack (ppShow x))


runNctrsChain
    :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env, HasConfig env)
    => SwitcherMap
    -> m ()
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
vcChain (vcid, queue) =
    sourceTBQueue queue .| gapCheckC .| extractPacketsC .| prettyShowVcC vcid


queueSize :: Natural
queueSize = 500


runVcChain
    :: (MonadIO m, MonadReader env m, HasLogFunc env)
    => Int
    -> ConduitT () Void m ()
    -> m ()
runVcChain vcid chain = do
    runConduit chain
    logWarn
        $  "Processing chain for VC"
        <> display vcid
        <> " terminated, restarting."
    runVcChain vcid chain


runChains
    :: (MonadUnliftIO m, MonadReader env m, HasConfig env, HasLogFunc env)
    => m ()
runChains = do
    cfg <- getConfig <$> ask

    lst <- mapM createQueue $ cfgVCIDs cfg
    let switcherMap = M.fromList lst

    race_
        (forConcurrently_ lst (\p@(vcid, _) -> runVcChain vcid (vcChain p)))
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
            Nothing   -> pure ()
            Just meta -> do
                let !vcfc = frHdrVCFC . frameHdr . metaFrame $ meta
                case oldVCFC' of
                    Nothing -> do
                        yield meta
                        go (Just vcfc)
                    Just oldVCFC -> do
                        if oldVCFC + 1 == vcfc
                            then do
                                yield meta
                                go (Just vcfc)
                            else do
                                let !newMeta =
                                        meta { metaGap = Just (oldVCFC, vcfc) }
                                yield newMeta
                                go (Just vcfc)


data PUSPacketMeta = PUSPacketMeta
    { ppMetaERT     :: !UTCTime
    , ppMetaQuality :: !QualityFlag
    , ppMetaPacket  :: !PUSPacket
    }
    deriving Show



extractPacketsC
    :: (MonadIO m, MonadReader env m, HasLogFunc env)
    => ConduitT TMFrameMeta PUSPacketMeta m ()
extractPacketsC = do
    x <- await
    case x of
        Nothing   -> pure ()
        Just meta -> firstFrameC meta
  where
    firstFrameC meta = do
        let !fhp = frHdrFHP . frameHdr . metaFrame $ meta
        if
            | fhp == idleFrameFHP -> extractPacketsC
            | fhp == noFHP -> extractPacketsC
            | fhp == 0 -> processPacketC meta (frameData (metaFrame meta))
            | fhp > 0 -> processPacketC
                meta
                (B.drop (fromIntegral fhp) (frameData (metaFrame meta)))
            | otherwise -> absurd undefined

    getNextFrameC continuation = do
        x <- await
        case x of
            Nothing   -> pure ()
            Just meta -> do
                case metaGap meta of
                    Nothing -> do
                        let !fhp = frHdrFHP . frameHdr . metaFrame $ meta
                        if fhp == idleFrameFHP
                            then getNextFrameC continuation
                            else continuation meta
                    Just (oldVCFC, vcfc) -> do
                        logWarn
                            $  "GAP in TM Frames detected: old VCFC="
                            <> display oldVCFC
                            <> " new VCFC="
                            <> display vcfc
                        firstFrameC meta

    doCheckCRC meta rest (binPacket, packet) = do
        if checkCRC binPacket
            then do
                let newPkt = PUSPacketMeta { ppMetaERT     = metaERT meta
                                           , ppMetaQuality = metaQuality meta
                                           , ppMetaPacket  = packet
                                           }
                yield newPkt
                processPacketC meta rest
            else do
                logError
                    $  "CRC check failed on PUS Packet: "
                    <> displayShow packet
                processPacketC meta rest

    processPacketC meta dat
        | B.null dat = getNextFrameC
            (\m -> processPacketC meta (frameData (metaFrame m)))
        | otherwise = do
            case A.parse (A.match pusPktParser) dat of
                A.Fail _ _ err ->
                    logError $ "Error parsing PUS Packet: " <> display
                        (T.pack err)
                A.Done rest result -> doCheckCRC meta rest result
                A.Partial cont     -> do
                    processContPacketC meta cont

    processContPacketC metaOld cont = getNextFrameC $ \meta -> do
        case cont (frameData (metaFrame meta)) of
            A.Fail _ _ err -> do
                logError $ "Error parsing PUS Packet: " <> display (T.pack err)
            A.Done rest result -> doCheckCRC metaOld rest result
            A.Partial cont1    -> processContPacketC metaOld cont1



convertToTMPacket :: PktIndex -> PUSPacketMeta -> TMPacket
convertToTMPacket = undefined
