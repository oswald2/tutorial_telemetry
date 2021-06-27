module TMPacket
    ( TMPacket(..)
    , Parameter(..)
    ) where

import           Data.Time.Clock
import           PUSTypes
import           RIO
import           TMDefinitions




data TMPacket = TMPacket
    { tmpName      :: !Text
    , tmpAPID      :: !Word16
    , tmpType      :: PUSType
    , tmpSubType   :: PUSSubType
    , tmpSSC       :: !Word16
    , tmpVCID      :: !Word8
    , tmpTimestamp :: Maybe UTCTime
    , tmpERT       :: !UTCTime
    , tmpQuality   :: !QualityFlag
    , tmpParams    :: Vector Parameter
    }
    deriving (Show, Generic)



data Validity =
  ValidityOK
  | ValidityOutOfBounds
  deriving (Eq, Ord, Enum, Show, Generic)


data Parameter = Parameter
    { paramName     :: !Text
    , paramValidity :: !Validity
    , paramValue    :: !Value
    }
    deriving (Show, Generic)



