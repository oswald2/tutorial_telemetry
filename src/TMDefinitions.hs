module TMDefinitions
    ( PktKey(..)
    , PktIndex
    , TMPacketDef(..)
    , ParameterDef(..)
    , Value(..)
    , generatePktIdx
    , defaultDefs
    , writeTMDefinitions
    , readTMDefinitions
    ) where

import           Data.Aeson              hiding ( Value )
import           Data.Aeson.Encode.Pretty      as A

import           PUSTypes
import           RIO
import qualified RIO.ByteString.Lazy           as BL
import qualified RIO.HashMap                   as HM
import qualified RIO.Text                      as T
import qualified RIO.Vector                    as V


data PktKey = PktKey !Word16 PUSType PUSSubType
    deriving (Eq, Show, Generic, Hashable)

type PktIndex = HashMap PktKey TMPacketDef


generatePktIdx :: [TMPacketDef] -> PktIndex
generatePktIdx defs = foldl' f HM.empty defs
  where
    f hm def =
        let key = PktKey (tmpDefApid def) (tmpDefType def) (tmpDefSubType def)
        in  HM.insert key def hm



data TMPacketDef = TMPacketDef
    { tmpDefName       :: !Text
    , tmpDefApid       :: !Word16
    , tmpDefType       :: PUSType
    , tmpDefSubType    :: PUSSubType
    , tmpParameterDefs :: Vector ParameterDef
    }
    deriving (Show, Generic, FromJSON, ToJSON)


data ParameterDef = ParameterDef
    { parDefName  :: !Text
    , parDefPos   :: !Word16
    , parDefValue :: !Value
    }
    deriving (Show, Generic, FromJSON, ToJSON)


data Value =
    ValUInt8 !Word8
    | ValUInt16 !Word16
    | ValUInt32 !Word32
    | ValFloat !Float
    | ValDouble !Double
    | ValOctet !HexBytes
    deriving (Show, Generic, FromJSON, ToJSON)


encConfig :: A.Config
encConfig = defConfig { confCompare = compareFunc }
  where
    compareFunc =
        keyOrder
                [ "tmpDefName"
                , "tmpDefApid"
                , "tmpDefType"
                , "tmpDefSubType"
                , "tmpParameterDefs"
                , "parDefName"
                , "parDefPos"
                , "parDefValue"
                ]
            <> compare



writeTMDefinitions :: [TMPacketDef] -> FilePath -> IO ()
writeTMDefinitions defs file = BL.writeFile file (encodePretty' encConfig defs)


readTMDefinitions :: FilePath -> IO (Either Text [TMPacketDef])
readTMDefinitions file = do
    res <- try $ BL.readFile file
    case res of
        Left (e :: IOException) -> do
            pure
                $  Left
                $  T.pack
                $  "Could not open file: "
                <> file
                <> ", reason: "
                <> show e
        Right content -> case eitherDecode content of
            Left  err -> pure $ Left (T.pack err)
            Right cfg -> pure (Right cfg)



defaultDefs :: [TMPacketDef]
defaultDefs = [
  ackPkt (PUSSubType 1)
  , ackPkt (PUSSubType 3)
  , ackPkt (PUSSubType 7)
  , ackPktFail (PUSSubType 2)
  , ackPktFail (PUSSubType 4)
  , ackPktFail (PUSSubType 8)
  , hktm
  ]



ackPkt :: PUSSubType -> TMPacketDef
ackPkt st = TMPacketDef { tmpDefName       = "Ack_1_" <> textDisplay st
                        , tmpDefApid       = 17
                        , tmpDefType       = PUSType 1
                        , tmpDefSubType    = st
                        , tmpParameterDefs = V.fromList [paramDefPktID, paramDefSeqCtrl]
                        }


ackPktFail :: PUSSubType -> TMPacketDef
ackPktFail st = TMPacketDef { tmpDefName       = "AckFail_1_" <> textDisplay st
                        , tmpDefApid       = 17
                        , tmpDefType       = PUSType 1
                        , tmpDefSubType    = st
                        , tmpParameterDefs = V.fromList [paramDefPktID, paramDefSeqCtrl, paramDefFailID]
                        }



hktm :: TMPacketDef
hktm = TMPacketDef { tmpDefName       = "HKTM"
                        , tmpDefApid       = 20 
                        , tmpDefType       = PUSType 3
                        , tmpDefSubType    = PUSSubType 25
                        , tmpParameterDefs = V.fromList [s2ktp501, s2ktp502]
                        }



paramDefPktID :: ParameterDef
paramDefPktID = ParameterDef
    { parDefName  = "PktID"
    , parDefPos   = 0 
    , parDefValue = ValUInt16 0
    }

paramDefSeqCtrl :: ParameterDef
paramDefSeqCtrl = ParameterDef
    { parDefName  = "SeqCtrl"
    , parDefPos   = 2 
    , parDefValue = ValUInt16 0
    }


paramDefFailID :: ParameterDef
paramDefFailID = ParameterDef
    { parDefName  = "FailID"
    , parDefPos   = 6 
    , parDefValue = ValUInt8 0
    }


s2ktp501 :: ParameterDef
s2ktp501 = ParameterDef
    { parDefName  = "S2KTP501"
    , parDefPos   = 30 
    , parDefValue = ValFloat 0
    }


s2ktp502 :: ParameterDef
s2ktp502 = ParameterDef
    { parDefName  = "S2KTP502"
    , parDefPos   = 34 
    , parDefValue = ValDouble 0
    }
