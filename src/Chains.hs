module Chains
    ( prettyShowC
    , runChains
    ) where

import           CRC
import           Classes
import           Conduit
import           Config
import qualified Data.Attoparsec.ByteString    as A
import           Data.Bits
import           Data.Conduit.Network
import           Data.Conduit.TQueue
import qualified Data.IntMap.Strict            as M
import           Data.ReinterpretCast
import           Data.Time.Clock
import           NCTRS
import           NcduToTMFrame
import           PUSPacket
import           PUSTypes
import           RIO
import qualified RIO.ByteString                as B
import qualified RIO.HashMap                   as HM
import qualified RIO.Text                      as T
import qualified RIO.Vector                    as V
import           Statistics
import           TMDefinitions
import           TMFrame
import           TMPacket
import           Text.Show.Pretty        hiding ( Value )
import AppState


prettyShowC
    :: (Show a) => ConduitT a Void (RIO AppState) ()
prettyShowC = awaitForever $ \x -> logDebug $ display (T.pack (ppShow x))

prettyShowVcC
    :: (MonadIO m, MonadReader env m, HasLogFunc env, Show a)
    => Int
    -> ConduitT a Void m ()
prettyShowVcC vcid = awaitForever $ \x ->
    logDebug $ "VC " <> display vcid <> display '\n' <> display
        (T.pack (ppShow x))


runNctrsChain
    :: SwitcherMap
    -> RIO AppState ()
runNctrsChain switcherMap = do
    cfg <- getConfig <$> ask

    let settings = clientSettings (fromIntegral (cfgPort cfg))
                                  (encodeUtf8 (cfgHostname cfg))

    res <- try $ runGeneralTCPClient settings $ \appData -> do
        runConduitRes
            $  appSource appData
            .| ncduTmC
            .| ncduToTMFrameC
            .| frameStatC
            .| vcSwitcherC switcherMap
    case res of
        Left (_ :: IOException) -> do
            logWarn "Could not connect, reconnecting..."
            threadDelay 2_000_000
            runNctrsChain switcherMap
        Right _ -> runNctrsChain switcherMap




type SwitcherMap = IntMap (TBQueue TMFrameMeta)


vcSwitcherC
    :: SwitcherMap
    -> ConduitT TMFrameMeta Void (ResourceT (RIO AppState)) ()
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
    :: PktIndex
    -> (Int, TBQueue TMFrameMeta)
    -> ConduitT a Void (RIO AppState) ()
vcChain pktIdx (vcid, queue) =
    sourceTBQueue queue
        .| gapCheckC
        .| extractPacketsC
        .| packetStatC
        .| dropIdlePktsC
        .| convertTMPacketC pktIdx
        .| sinkNull
        -- .| prettyShowVcC vcid


queueSize :: Natural
queueSize = 500


runVcChain
    :: Int
    -> ConduitT () Void (RIO AppState) ()
    -> (RIO AppState) ()
runVcChain vcid chain = do
    runConduit chain
    logWarn
        $  "Processing chain for VC"
        <> display vcid
        <> " terminated, restarting."
    runVcChain vcid chain


runChains
    :: PktIndex
    -> RIO AppState ()
runChains pktIdx = do
    cfg <- getConfig <$> ask

    lst <- mapM createQueue $ cfgVCIDs cfg
    let switcherMap = M.fromList lst

    race_
        (forConcurrently_
            lst
            (\p@(vcid, _) -> runVcChain vcid (vcChain pktIdx p))
        )
        (runNctrsChain switcherMap)
  where
    createQueue vcid = do
        queue <- newTBQueueIO queueSize
        pure (fromIntegral vcid, queue)


gapCheckC :: ConduitT TMFrameMeta TMFrameMeta (RIO AppState) ()
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
    , ppVCID        :: !Word8
    , ppMetaPacket  :: !PUSPacket
    }
    deriving (Show, Generic, NFData)

dropIdlePktsC :: ConduitT PUSPacketMeta PUSPacketMeta (RIO AppState) ()
dropIdlePktsC =
    filterC (\pkt -> idleApid /= (pHdrAPID . pusHdr . ppMetaPacket) pkt)


extractPacketsC
    :: ConduitT TMFrameMeta PUSPacketMeta (RIO AppState) ()
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
                let newPkt = PUSPacketMeta
                        { ppMetaERT     = metaERT meta
                        , ppMetaQuality = metaQuality meta
                        , ppVCID = frHdrVCID . frameHdr . metaFrame $ meta
                        , ppMetaPacket  = packet
                        }
                yield (force newPkt)
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


convertTMPacketC
    :: PktIndex -> ConduitT PUSPacketMeta TMPacket (RIO AppState) ()
convertTMPacketC pktIdx =
    awaitForever $ \metaPkt -> yield $ convertToTMPacket pktIdx metaPkt



convertToTMPacket :: PktIndex -> PUSPacketMeta -> TMPacket
convertToTMPacket pktIdx metaPkt =
    let
        key     = PktKey apid t st
        apid    = pHdrAPID (pusHdr pusPkt)
        pusPkt  = ppMetaPacket metaPkt
        (t, st) = dfhTypes (pusSecHdr pusPkt)
    in
        case HM.lookup key pktIdx of
            Just packetDef ->
                let
                    params = V.map (extractParameter (pusData pusPkt))
                        $ tmpParameterDefs packetDef
                in  TMPacket { tmpName      = tmpDefName packetDef
                             , tmpAPID      = tmpDefApid packetDef
                             , tmpType      = t
                             , tmpSubType   = st
                             , tmpSSC       = pHdrSSC (pusHdr pusPkt)
                             , tmpVCID      = ppVCID metaPkt
                             , tmpTimestamp = pusPktTimestamp pusPkt
                             , tmpERT       = ppMetaERT metaPkt
                             , tmpQuality   = ppMetaQuality metaPkt
                             , tmpParams    = params
                             }
            Nothing ->
                let
                    param = Parameter
                        "Content"
                        ValidityOK
                        (ValOctet (HexBytes (pusData pusPkt)))
                in  TMPacket { tmpName      = "UNKNOWN"
                             , tmpAPID      = apid
                             , tmpType      = t
                             , tmpSubType   = st
                             , tmpSSC       = pHdrSSC (pusHdr pusPkt)
                             , tmpVCID      = ppVCID metaPkt
                             , tmpTimestamp = pusPktTimestamp pusPkt
                             , tmpERT       = ppMetaERT metaPkt
                             , tmpQuality   = ppMetaQuality metaPkt
                             , tmpParams    = V.singleton param
                             }


getParameter :: ParameterDef -> Int -> ByteString -> Value -> Parameter
getParameter (ParameterDef name pos defVal) size dat ~newVal =
    if (fromIntegral pos + size) <= B.length dat
        then Parameter name ValidityOK newVal
        else Parameter name ValidityOutOfBounds defVal


extractParameter :: ByteString -> ParameterDef -> Parameter
extractParameter dat def@(ParameterDef _name pos (ValUInt8 _)) =
    getParameter def 1 dat (ValUInt8 (B.index dat (fromIntegral pos)))
extractParameter dat def@(ParameterDef _name pos (ValUInt16 _)) =
    getParameter def 2 dat (ValUInt16 (getWord16 dat pos))
extractParameter dat def@(ParameterDef _name pos (ValUInt32 _)) =
    getParameter def 4 dat (ValUInt32 (getWord32 dat pos))
extractParameter dat def@(ParameterDef _name pos (ValFloat _)) =
    getParameter def 4 dat (ValFloat (getFloat dat pos))
extractParameter dat def@(ParameterDef _name pos (ValDouble _)) =
    getParameter def 8 dat (ValDouble (getDouble dat pos))
extractParameter dat (ParameterDef name pos defVal@(ValOctet _)) =
    if fromIntegral pos + 1 < B.length dat
        then
            let len       = getWord16 dat pos
                (_, rest) = B.splitAt (fromIntegral pos + 2) dat
                newDat    = B.take (fromIntegral len) rest
                l :: Int
                l = fromIntegral pos + 2 + fromIntegral len
            in  if l <= B.length dat
                    then Parameter name ValidityOK (ValOctet (HexBytes newDat))
                    else Parameter name ValidityOutOfBounds defVal
        else Parameter name ValidityOutOfBounds defVal


getWord16 :: ByteString -> Word16 -> Word16
getWord16 dat offset' =
    let b0      = fromIntegral $ dat `B.index` offset
        b1      = fromIntegral $ dat `B.index` (offset + 1)
        offset  = fromIntegral offset'
        !newVal = (b0 `shiftL` 8) .|. b1
    in  newVal

getWord32 :: ByteString -> Word16 -> Word32
getWord32 dat offset' =
    let
        b0     = fromIntegral $ dat `B.index` offset
        b1     = fromIntegral $ dat `B.index` (offset + 1)
        b2     = fromIntegral $ dat `B.index` (offset + 2)
        b3     = fromIntegral $ dat `B.index` (offset + 3)
        offset = fromIntegral offset'
        !newVal =
            (b0 `shiftL` 24) .|. (b1 `shiftL` 16) .|. (b2 `shiftL` 8) .|. b3
    in
        newVal

getFloat :: ByteString -> Word16 -> Float
getFloat dat offset' = wordToFloat $ getWord32 dat offset'

getDouble :: ByteString -> Word16 -> Double
getDouble dat offset' =
    let b0     = fromIntegral $ dat `B.index` offset
        b1     = fromIntegral $ dat `B.index` (offset + 1)
        b2     = fromIntegral $ dat `B.index` (offset + 2)
        b3     = fromIntegral $ dat `B.index` (offset + 3)
        b4     = fromIntegral $ dat `B.index` (offset + 4)
        b5     = fromIntegral $ dat `B.index` (offset + 5)
        b6     = fromIntegral $ dat `B.index` (offset + 6)
        b7     = fromIntegral $ dat `B.index` (offset + 7)
        offset = fromIntegral offset'
        !newVal =
            (b0 `shiftL` 56)
                .|. (b1 `shiftL` 48)
                .|. (b2 `shiftL` 40)
                .|. (b3 `shiftL` 32)
                .|. (b4 `shiftL` 24)
                .|. (b5 `shiftL` 16)
                .|. (b6 `shiftL` 8)
                .|. b7
    in  wordToDouble newVal




frameStatC
    :: ConduitT TMFrameMeta TMFrameMeta (ResourceT (RIO AppState)) ()
frameStatC = do
    env <- ask
    let frameSize = fromIntegral $ cfgTmFrameLen (getConfig env)
    awaitForever $ \meta -> do
        let frameStatVar = getFrameStats env 
        now <- liftIO $ getCurrentTime 
        atomically $ modifyTVar' frameStatVar (statNewDU now frameSize)
        yield meta


packetStatC
    :: ConduitT PUSPacketMeta PUSPacketMeta (RIO AppState) ()
packetStatC = do
    awaitForever $ \meta -> do
        pktStatVar <- getPacketStats <$> ask
        let pktSize = fromIntegral (pHdrLength . pusHdr . ppMetaPacket $ meta) + 1 + 6
        now <- liftIO $ getCurrentTime 
        atomically $ modifyTVar' pktStatVar (statNewDU now pktSize)
        yield meta
