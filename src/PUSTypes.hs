module PUSTypes
    ( PUSType(..)
    , PUSSubType(..)
    , QualityFlag(..)
    , HexBytes(..)
    , pusTypeParser
    , pusSubTypeParser
    ) where


import           Data.Aeson                    as AE
import qualified Data.Aeson.Encoding           as E
import           Data.Aeson.Types
import           Data.Attoparsec.ByteString    as A
import qualified Data.Attoparsec.Text          as AT
import           Data.Bits
import           Data.Char
import           RIO                     hiding ( Builder )
import qualified RIO.ByteString                as B
import qualified RIO.Text                      as T
import           Text.Builder


data QualityFlag = Good | Bad
  deriving (Show, Generic)




newtype PUSType = PUSType Word8
  deriving (Eq, Show, Generic, Hashable, FromJSON, ToJSON)

pusTypeParser :: A.Parser PUSType
pusTypeParser = PUSType <$> A.anyWord8


newtype PUSSubType = PUSSubType Word8
  deriving (Eq, Show, Generic, Hashable, FromJSON, ToJSON)

pusSubTypeParser :: A.Parser PUSSubType
pusSubTypeParser = PUSSubType <$> A.anyWord8




newtype HexBytes = HexBytes ByteString


hexBytesBuilder :: HexBytes -> Builder
hexBytesBuilder (HexBytes b) = do
    mconcat . map (\x -> padFromLeft 2 '0' (unsignedHexadecimal x)) $ B.unpack b


hexBytesParser :: AT.Parser HexBytes
hexBytesParser = do
    HexBytes . B.pack <$> many parseByte


parseByte :: AT.Parser Word8
parseByte = do
    a <- AT.satisfy isHexDigit
    b <- AT.satisfy isHexDigit
    pure $ fromIntegral ((ord a `shiftL` 4) .|. ord b)


instance Show HexBytes where
    show b = T.unpack $ run $ hexBytesBuilder b


instance ToJSON HexBytes where
    toJSON b = String $ run $ hexBytesBuilder b
    toEncoding b = E.text $ run $ hexBytesBuilder b


instance FromJSON HexBytes where
    parseJSON (String x) = case AT.parseOnly hexBytesParser x of
        Left  err -> fail err
        Right val -> pure val
    parseJSON invalid = prependFailure "parsing HexBytes failed, "
                                       (typeMismatch "String" invalid)
