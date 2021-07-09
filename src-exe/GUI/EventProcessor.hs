module GUI.EventProcessor
    ( processEvent
    ) where

import           Data.GI.Gtk.Threading
import           RIO

import           Events

import           GUI.MainWindow

processEvent :: MainWindow -> Event -> IO ()
processEvent gui EventConnected    = postGUIASync $ mwSetConnected gui True
processEvent gui EventDisconnected = postGUIASync $ mwSetConnected gui False
processEvent _   _                 = pure ()
