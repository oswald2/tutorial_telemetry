module AppState
    ( AppState(..)
    , HasConfig(..)
    ) where

import           Config
import           RIO

import           Classes


data AppState = AppState
    { appConfig  :: !Config
    , appLogFunc :: !LogFunc
    }


instance HasLogFunc AppState where
    logFuncL = lens appLogFunc (\st l -> st { appLogFunc = l })


instance HasConfig AppState where
    getConfig = appConfig
