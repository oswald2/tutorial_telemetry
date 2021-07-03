module GUI.MainWindow
    ( MainWindow
    , initMainWindow
    ) where

import           RIO
import qualified RIO.Text                      as T

import           GI.Gtk                        as Gtk




data MainWindow = MainWindow
    { mwWindow :: !ApplicationWindow
    }


initMainWindow :: (MonadIO m) => m MainWindow
initMainWindow = do
    builder <- builderNewFromResource "/gui/data/interface.ui"

    window  <- getObject builder "mainWindow" ApplicationWindow

    let gui = MainWindow { mwWindow = window }

    void $ onWidgetDestroy window Gtk.mainQuit
    Gtk.widgetShowAll window
    pure gui




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


