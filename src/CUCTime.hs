module CUCTime
    ( CUCTime(..)
    , cucTimeParser
    ) where

import           RIO

import           Data.Attoparsec.Binary
import           Data.Attoparsec.ByteString
import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.Time.Clock.System         ( systemEpochDay )

import           Time

data CUCTime = CUCTime
    { cucSeconds :: !Word32
    , cucSubSec  :: !Word16
    }
    deriving Show


cucTimeParser :: Parser CUCTime
cucTimeParser = CUCTime <$> anyWord32be <*> anyWord16be



instance TimeConversion CUCTime where
    toUTCTime (CUCTime sec subsec) =
        let (days, daysInSec) = sec `quotRem` 86400
            day  = addDays (fromIntegral days) systemEpochDay
            micro :: Double
            micro = fromIntegral subsec * 1_000_000 / 65536
            pico  = fromIntegral daysInSec * 1_000_000_000_000 + round (micro * 1_000_000)
            dtime = picosecondsToDiffTime pico
        in  UTCTime day dtime
