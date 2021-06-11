module CDSTime
    ( CDSTime(..)
    , cdsTimeParser
    , toUTCTime
    ) where


import           RIO


import Data.Attoparsec.ByteString
import Data.Attoparsec.Binary

import Data.Time.Clock




data CDSTime = CDSTime
    { cdsDays  :: !Word16
    , cdsMilli :: !Word32
    , cdsMicro :: !Word16
    } deriving (Show)




cdsTimeParser :: Parser CDSTime
cdsTimeParser = CDSTime <$> anyWord16be <*> anyWord32be <*> anyWord16be



toUTCTime :: CDSTime -> UTCTime
toUTCTime = undefined  