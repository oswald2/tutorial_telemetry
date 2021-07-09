module GUI.MainWindow
    ( MainWindow
    , initMainWindow
    , mwSetConnected
    ) where

import           RIO
import qualified RIO.Text                      as T

import           GI.Gtk                        as Gtk




data MainWindow = MainWindow
    { mwWindow     :: !ApplicationWindow
    , mwConnection :: !Entry
    }


initMainWindow :: (MonadIO m) => m MainWindow
initMainWindow = do
    builder   <- builderNewFromResource "/gui/data/interface.ui"

    window    <- getObject builder "mainWindow" ApplicationWindow
    connEntry <- getObject builder "entryConnection" Entry

    setupEntryCSS connEntry 

    let gui = MainWindow { mwWindow = window, mwConnection = connEntry }

    void $ onWidgetDestroy window Gtk.mainQuit
    Gtk.widgetShowAll window
    pure gui




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


