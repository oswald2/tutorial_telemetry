module AppState
    ( AppState(..)
    , HasConfig(..)
    ) where

import           Config
import           RIO

import           Classes
import           Events


data AppState = AppState
    { appConfig     :: !Config
    , appLogFunc    :: !LogFunc
    , appRaiseEvent :: !RaiseEvent
    }


instance HasLogFunc AppState where
    logFuncL = lens appLogFunc (\st l -> st { appLogFunc = l })


instance HasConfig AppState where
    getConfig = appConfig


instance HasRaiseEvent AppState where 
    raiseEventL = lens appRaiseEvent (\st re -> st { appRaiseEvent = re })