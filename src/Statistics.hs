{-# LANGUAGE  TemplateHaskell #-}
module Statistics
    ( Statistics(..)
    , statNewDU
    , statN
    , statBytes
    , statFirst
    , initialStatistics
    , statCalc 
    , statTotal
    ) where

import           Control.Lens
import           Data.Time.Clock
import           RIO                     hiding ( (^.) )

data Statistics = Statistics
    { _statN     :: !Int64
    , _statBytes :: !Int64
    , _statFirst :: Maybe UTCTime
    , _statCurrent :: Maybe UTCTime 
    }
makeLenses ''Statistics

initialStatistics :: Statistics
initialStatistics =
    Statistics { _statN = 0, _statBytes = 0, _statFirst = Nothing, _statCurrent = Nothing }


statNewDU :: UTCTime -> Int64 -> Statistics -> Statistics
statNewDU now size s =
    s & statN +~ 1 &  statBytes +~ size & 
      case s ^. statFirst of
            Nothing -> statFirst ?~ now
            Just _  -> statCurrent ?~ now  
    

statCalc :: Statistics -> Statistics -> (Double, Double)
statCalc !s1 !s2 = 
  let bytes = s2 ^. statBytes - s1 ^. statBytes 
      n = s2 ^. statN - s1 ^. statN 
  in
  case (s1 ^. statCurrent, s2 ^. statCurrent) of 
    (Just t1, Just t2) -> 
        let diff = realToFrac (nominalDiffTimeToSeconds (diffUTCTime t2 t1))
            !duRate = fromIntegral n / diff 
            !dataRate = fromIntegral bytes / diff 
        in if diff == 0.0 then (0, 0) else (duRate, dataRate)
    _ -> (0.0, 0.0)

statTotal :: Statistics -> (Double, Double)
statTotal s = 
  case (s ^. statFirst, s ^. statCurrent) of 
    (Just start, Just end) -> 
      let diff = realToFrac (nominalDiffTimeToSeconds (diffUTCTime end start))
          !duTotal = fromIntegral (s ^. statN) / diff 
          !duTotalRate = fromIntegral (s ^. statBytes) / diff 
      in if diff == 0 then (0, 0) else (duTotal, duTotalRate)          
    _ -> (0, 0)

