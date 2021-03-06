module TMPacket
    ( TMPacket(..)
    , Parameter(..)
    , Validity(..)
    , isValid
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

isValid :: Validity -> Bool 
isValid ValidityOK = True 
isValid _ = False 


data Parameter = Parameter
    { paramName     :: !Text
    , paramValidity :: !Validity
    , paramValue    :: !Value
    }
    deriving (Show, Generic)



instance Display Validity where 
    display ValidityOK = "OK"
    display ValidityOutOfBounds = "OOB"
