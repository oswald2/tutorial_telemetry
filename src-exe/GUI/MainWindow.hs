{-# LANGUAGE DataKinds, OverloadedLabels #-}
module GUI.MainWindow
    ( MainWindow
    , initMainWindow
    , mwSetConnected
    , mwAddTmPacket
    , mwLog
    ) where

import           RIO
import qualified RIO.Text                      as T
import qualified RIO.Vector                    as V

import           Data.GI.Base.Attributes
import           Data.GI.Gtk.Threading
import           GI.Gtk                        as Gtk

import           Data.GI.Gtk.ModelView.CellLayout
import           Data.GI.Gtk.ModelView.SeqStore
import           PUSTypes
import           TMPacket

import           GUI.Colors

import           Data.Time.Clock


data MainWindow = MainWindow
    { mwWindow         :: !ApplicationWindow
    , mwConnection     :: !Entry
    , mwTreeviewPacket :: !TreeView
    , mwTreeViewParams :: !TreeView
    , mwLogDisp        :: !TreeView
    , mwPacketModel    :: SeqStore TMPacket
    , mwParamModel     :: SeqStore Parameter
    , mwLogModel       :: SeqStore LogMsg
    }


data LogMsg = LogMsg !UTCTime !LogLevel !Text


initMainWindow :: (MonadIO m) => m MainWindow
initMainWindow = do
    builder   <- builderNewFromResource "/gui/data/interface.ui"

    window    <- getObject builder "mainWindow" ApplicationWindow
    connEntry <- getObject builder "entryConnection" Entry
    tvPacket  <- getObject builder "treeviewPacket" TreeView
    tvParams  <- getObject builder "treeviewParams" TreeView
    logDisp   <- getObject builder "treeviewLogMsgs" TreeView


    setupEntryCSS connEntry

    packetModel <- initPktDisplay tvPacket
    paramModel  <- initParamDisplay tvParams
    logModel    <- initLogDisplay logDisp

    let gui = MainWindow { mwWindow         = window
                         , mwConnection     = connEntry
                         , mwTreeviewPacket = tvPacket
                         , mwTreeViewParams = tvParams
                         , mwLogDisp        = logDisp
                         , mwPacketModel    = packetModel
                         , mwParamModel     = paramModel
                         , mwLogModel       = logModel
                         }

    void $ onWidgetDestroy window Gtk.mainQuit

    void $ Gtk.on tvPacket #rowActivated $ \path _col -> do
        ipath <- treePathGetIndices path
        forM_ ipath $ \idxs -> do
            case idxs of
                (idx : _) -> do
                    val <- seqStoreGetValue packetModel idx
                    let params = tmpParams val
                    displayParams gui params
                [] -> pure ()

    Gtk.widgetShowAll window
    pure gui


mwAddTmPacket :: (MonadIO m) => MainWindow -> TMPacket -> m ()
mwAddTmPacket gui packet = do
    let model = mwPacketModel gui

    n <- seqStoreGetSize model
    when (n > 500) $ do
        seqStoreRemove model (n - 1)
    void $ seqStorePrepend model packet

    row <- treePathNewFromIndices [0]
    treeViewScrollToCell (mwTreeviewPacket gui)
                         (Just row)
                         (Nothing :: Maybe TreeViewColumn)
                         False
                         0
                         0


displayParams :: (MonadIO m) => MainWindow -> Vector Parameter -> m ()
displayParams gui params = do
    let model = mwParamModel gui
    seqStoreClear model
    V.mapM_ (seqStoreAppend model) params

initLogDisplay :: (MonadIO m) => TreeView -> m (SeqStore LogMsg)
initLogDisplay tv = do
    model <- seqStoreNew []
    treeViewSetModel tv (Just model)

    mapM_ (createColumn tv model)
        $ [ ("Time"   , 250 , timeAttrs)
          , ("Level"  , 80  , levelAttrs)
          , ("Message", 8000, \(LogMsg _ _ msg) -> [#text := msg])
          ]
    pure model
  where
    timeAttrs :: LogMsg -> [AttrOp CellRendererText 'AttrSet]
    timeAttrs (LogMsg t _ _) = [#text := T.pack (show t)]

    levelAttrs :: LogMsg -> [AttrOp CellRendererText 'AttrSet]
    levelAttrs (LogMsg _ LevelDebug _) = (#text := "DEBUG") : colors LevelDebug
    levelAttrs (LogMsg _ LevelInfo _) = (#text := "INFO") : colors LevelInfo
    levelAttrs (LogMsg _ LevelWarn _) = (#text := "WARNING") : colors LevelWarn
    levelAttrs (LogMsg _ LevelError _) = (#text := "ERROR") : colors LevelError
    levelAttrs (LogMsg _ l@(LevelOther x) _) = (#text := x) : colors l

    colors :: LogLevel -> [AttrOp CellRendererText 'AttrSet]
    colors LevelDebug = [#backgroundSet := False, #foregroundSet := False]
    colors LevelInfo =
        [ #backgroundSet := True
        , #foregroundSet := True
        , #backgroundRgba := green
        , #foregroundRgba := black
        ]
    colors LevelWarn =
        [ #backgroundSet := True
        , #foregroundSet := True
        , #backgroundRgba := yellow
        , #foregroundRgba := black
        ]
    colors LevelError =
        [ #backgroundSet := True
        , #foregroundSet := True
        , #backgroundRgba := red
        , #foregroundRgba := white
        ]
    colors (LevelOther _) =
        [ #backgroundSet := True
        , #foregroundSet := True
        , #backgroundRgba := red
        , #foregroundRgba := white
        ]


initPktDisplay :: (MonadIO m) => TreeView -> m (SeqStore TMPacket)
initPktDisplay tv = do
    model <- seqStoreNew []
    treeViewSetModel tv (Just model)

    mapM_ (createColumn tv model)
        $ [ ("Time", 250, timeAttrs)
          , ("ERT", 200, \pkt -> [#text := T.pack (show (tmpERT pkt))])
          , ("Name", 180, \pkt -> [#text := textDisplay (tmpName pkt)])
          , ("APID", 60, \pkt -> [#text := textDisplay (tmpAPID pkt)])
          , ("T", 30, \pkt -> [#text := textDisplay (tmpType pkt)])
          , ("ST", 30, \pkt -> [#text := textDisplay (tmpSubType pkt)])
          , ("SSC", 80, \pkt -> [#text := textDisplay (tmpSSC pkt)])
          , ("VC", 30, \pkt -> [#text := textDisplay (tmpVCID pkt)])
          , ("Qual", 60 , qualAttrs)
          ]

    pure model
  where
    timeAttrs :: TMPacket -> [AttrOp CellRendererText 'AttrSet]
    timeAttrs pkt = case tmpTimestamp pkt of
        Just t  -> [#text := T.pack (show t)]
        Nothing -> [#text := "--"]

    qualAttrs :: TMPacket -> [AttrOp CellRendererText 'AttrSet]
    qualAttrs pkt = case tmpQuality pkt of
        Good -> [#text := "GOOD"]
        Bad ->
            [ #text := "BAD"
            , #backgroundSet := True
            , #foregroundSet := True
            , #backgroundRgba := red
            , #foregroundRgba := white
            ]



initParamDisplay :: (MonadIO m) => TreeView -> m (SeqStore Parameter)
initParamDisplay tv = do
    model <- seqStoreNew []
    treeViewSetModel tv (Just model)

    mapM_ (createColumn tv model)
        $ [ ("Name"    , 180, \par -> [#text := textDisplay (paramName par)])
          , ("Value"   , 100, \par -> [#text := textDisplay (paramValue par)])
          , ("Validity", 50, \par -> [#text := textDisplay (paramValidity par)])
          ]

    pure model



createColumn
    :: (MonadIO m)
    => TreeView
    -> SeqStore row
    -> (Text, Int32, row -> [AttrOp CellRendererText 'AttrSet])
    -> m (TreeViewColumn, CellRendererText)
createColumn tv model (name, width, attr) = do
    col <- treeViewColumnNew
    treeViewColumnSetFixedWidth col width
    treeViewColumnSetSizing col TreeViewColumnSizingFixed
    treeViewColumnSetResizable col True
    treeViewColumnSetReorderable col True
    treeViewColumnSetTitle col name

    renderer <- cellRendererTextNew
    cellLayoutPackStart col renderer True
    cellLayoutSetAttributes col renderer model attr
    void $ treeViewAppendColumn tv col
    return (col, renderer)



mwSetConnected :: (MonadIO m) => MainWindow -> Bool -> m ()
mwSetConnected gui True = do
    entrySetText (mwConnection gui) "CONNECTED"
    widgetSetName (mwConnection gui) "green-entry"
mwSetConnected gui False = do
    entrySetText (mwConnection gui) "DISCONNECTED"
    widgetSetName (mwConnection gui) "warn-entry"


css :: ByteString
css =
    "entry { min-height: 0px; }\
    \#error-entry {\
      \   background-color: #ff0000;\
      \   color: white;\
      \   border-radius: 20px;\
      \}\
      \#warn-entry {\
      \   background-color: #ffff00;\
      \   color: black;\
      \   border-radius: 20px;\
      \}\
      \#green-entry {\
      \   background-color: #00BB00;\
      \   color: black;\
      \   border-radius: 20px;\
      \}"


setupEntryCSS :: (MonadIO m) => Entry -> m ()
setupEntryCSS entry = do
    widgetSetSensitive entry False
    provider <- cssProviderNew
    cssProviderLoadFromData provider css
    context <- widgetGetStyleContext entry
    styleContextAddProvider context provider 600



getObject
    :: (MonadIO m, GObject o)
    => Gtk.Builder
    -> Text
    -> (ManagedPtr o -> o)
    -> m o
getObject builder obj gtkConstr = do
    o <- builderGetObject builder obj
    case o of
        Nothing ->
            error $ "GTK: could not find " <> T.unpack obj <> " in Glade file!"
        Just oo -> do
            w <- liftIO $ castTo gtkConstr oo
            case w of
                Nothing -> error $ "GTK: cannot cast widget " <> T.unpack obj
                Just widget -> return widget




mwLog
    :: MainWindow -> CallStack -> LogSource -> LogLevel -> Utf8Builder -> IO ()
mwLog gui _ _ LevelInfo      msg = postGUIASync $ doLog gui LevelInfo msg
mwLog gui _ _ LevelWarn      msg = postGUIASync $ doLog gui LevelWarn msg
mwLog gui _ _ LevelError     msg = postGUIASync $ doLog gui LevelError msg
mwLog gui _ _ (LevelOther x) msg = postGUIASync $ doLog gui (LevelOther x) msg
mwLog _ _ _ _ _ = pure ()


doLog :: MainWindow -> LogLevel -> Utf8Builder -> IO ()
doLog gui level msg = do
    now <- getCurrentTime
    let logMsg  = LogMsg now level (utf8BuilderToText msg)
        logSize = 200
        model   = mwLogModel gui

    n <- seqStoreGetSize model
    when (n > logSize) $ seqStoreRemove model (n - 1)
    void $ seqStorePrepend model logMsg

    row <- treePathNewFromIndices [0]
    treeViewScrollToCell (mwLogDisp gui)
                         (Just row)
                         (Nothing :: Maybe TreeViewColumn)
                         False
                         0
                         0

