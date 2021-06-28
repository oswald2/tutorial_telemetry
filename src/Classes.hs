module Classes where

--import RIO 

import           Config
import           Statistics

import           UnliftIO.STM

class HasConfig a where
  getConfig :: a -> Config


class HasStats a where
  getFrameStats :: a -> TVar Statistics
  getPacketStats :: a -> TVar Statistics
