module AppState
    ( AppState(..)
    , HasConfig(..)
    ) where

import           Classes
import           Config
import           Control.Lens
import           RIO                     hiding ( lens )
import           Statistics


data AppState = AppState
    { appConfig           :: !Config
    , appFrameStatistics  :: TVar Statistics
    , appPacketStatistics :: TVar Statistics
    , appLogFunc          :: !LogFunc
    }


instance HasLogFunc AppState where
    logFuncL = lens appLogFunc (\st l -> st { appLogFunc = l })


instance HasConfig AppState where
    getConfig = appConfig


instance HasStats AppState where 
    getFrameStats = appFrameStatistics  
    getPacketStats = appPacketStatistics