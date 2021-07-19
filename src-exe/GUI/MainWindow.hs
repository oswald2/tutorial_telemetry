{-# LANGUAGE DataKinds, OverloadedLabels #-}
module GUI.MainWindow
    ( MainWindow
    , initMainWindow
    , mwSetConnected
    , mwAddTmPacket
    ) where

import           RIO
import qualified RIO.Text                      as T
import qualified RIO.Vector                    as V

import           Data.GI.Base.Attributes
import           GI.Gtk                        as Gtk

import           Data.GI.Gtk.ModelView.CellLayout
import           Data.GI.Gtk.ModelView.SeqStore
import           PUSTypes
import           TMPacket

import           GUI.Colors

data MainWindow = MainWindow
    { mwWindow         :: !ApplicationWindow
    , mwConnection     :: !Entry
    , mwTreeviewPacket :: !TreeView
    , mwTreeViewParams :: !TreeView
    , mwPacketModel    :: SeqStore TMPacket
    , mwParamModel     :: SeqStore Parameter
    }


initMainWindow :: (MonadIO m) => m MainWindow
initMainWindow = do
    builder   <- builderNewFromResource "/gui/data/interface.ui"

    window    <- getObject builder "mainWindow" ApplicationWindow
    connEntry <- getObject builder "entryConnection" Entry
    tvPacket  <- getObject builder "treeviewPacket" TreeView
    tvParams  <- getObject builder "treeviewParams" TreeView

    setupEntryCSS connEntry

    packetModel <- initPktDisplay tvPacket
    paramModel  <- initParamDisplay tvParams


    let gui = MainWindow { mwWindow         = window
                         , mwConnection     = connEntry
                         , mwTreeviewPacket = tvPacket
                         , mwTreeViewParams = tvParams
                         , mwPacketModel    = packetModel
                         , mwParamModel     = paramModel
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


