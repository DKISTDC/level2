module NSO.Data.DateTime where


import NSO.Prelude
import Data.Time.Clock (UTCTime)
import Data.Time.Format.ISO8601
import Data.Morpheus.Client (EncodeScalar(..), DecodeScalar(..), ScalarValue(..))

newtype DateTime = DateTime UTCTime
  deriving (Show, Eq, Generic)
  deriving newtype (ISO8601)

instance EncodeScalar DateTime where
  encodeScalar (DateTime x) = String $ cs $ iso8601Show x

instance DecodeScalar DateTime where
  -- dates do not have the UTC suffix
  decodeScalar (String s) = iso8601ParseM $ cs $ s <> "Z"
  decodeScalar _ = Left "Cannot decode DateTime"
