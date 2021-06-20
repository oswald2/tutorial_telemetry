module Time
    ( TimeConversion(..)
    ) where


import           Data.Time.Clock


class TimeConversion a where
  toUTCTime :: a -> UTCTime
