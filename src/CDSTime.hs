module CDSTime
    ( CDSTime(..)
    , cdsTimeParser
    , toUTCTime
    ) where


import           RIO


import           Data.Attoparsec.Binary
import           Data.Attoparsec.ByteString

import           Data.Time.Calendar
import           Data.Time.Clock



data CDSTime = CDSTime
    { cdsDays  :: !Word16
    , cdsMilli :: !Word32
    , cdsMicro :: !Word16
    }
    deriving Show




cdsTimeParser :: Parser CDSTime
cdsTimeParser = CDSTime <$> anyWord16be <*> anyWord32be <*> anyWord16be



toUTCTime :: CDSTime -> UTCTime
toUTCTime (CDSTime days milli micro) =
    let epochDay1958 = ModifiedJulianDay 36204
        day          = addDays (fromIntegral days) epochDay1958
        pico = (fromIntegral milli * 1000 + fromIntegral micro) * 1_000_000
        dtime        = picosecondsToDiffTime pico
    in  UTCTime day dtime
