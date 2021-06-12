module AppState
    ( AppState(..)
    , HasConfig(..)
    ) where

import           Config
import           RIO


data AppState = AppState
    { appConfig  :: !Config
    , appLogFunc :: !LogFunc
    }


instance HasLogFunc AppState where
    logFuncL = lens appLogFunc (\st l -> st { appLogFunc = l })


class HasConfig a where 
  getConfig :: a -> Config 

instance HasConfig AppState where 
  getConfig = appConfig 
