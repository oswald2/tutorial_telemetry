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
import           Data.Aeson.Encode.Pretty       ( encodePretty )
import           PUSTypes
import           RIO
import qualified RIO.ByteString.Lazy           as BL
import qualified RIO.HashMap                   as HM
import qualified RIO.Text                      as T


data PktKey = PktKey !Word16 PUSType PUSSubType
    deriving (Eq, Show, Generic, Hashable)

type PktIndex = HashMap PktKey TMPacketDef


generatePktIdx :: [TMPacketDef] -> PktIndex
generatePktIdx defs = foldl' f HM.empty defs
  where
    f hm def =
        let key = PktKey (tmpDefApid def) (tmpDefType def) (tmpDefSubType def)
        in  HM.insert key def hm


defaultDefs :: [TMPacketDef]
defaultDefs = []


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
    | ValFloat !Float
    | ValDouble !Double
    | ValOctet !HexBytes
    deriving (Show, Generic, FromJSON, ToJSON)




writeTMDefinitions :: [TMPacketDef] -> FilePath -> IO ()
writeTMDefinitions defs file = BL.writeFile file (encodePretty defs)


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
