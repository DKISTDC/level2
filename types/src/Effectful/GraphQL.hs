{-# LANGUAGE OverloadedStrings #-}

module Effectful.GraphQL where

import Data.Aeson (FromJSON (..), Options (..), ToJSON, defaultOptions, eitherDecode, encode, genericParseJSON)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Kind (Type)
import Data.List qualified as L
import Data.Text qualified as T
import Effectful
import Effectful.Dispatch.Dynamic
import GHC.Generics
import GHC.TypeLits
import NSO.Prelude
import Network.HTTP.Req
import Text.URI (mkURI)


data GraphQL :: Effect where
  Query :: (Query a, FromJSON (Result a)) => Service -> a -> GraphQL m [Result a]
type instance DispatchOf GraphQL = 'Dynamic


class Query a where
  type Result a :: Type
  query :: a -> Text
  operationName :: a -> Text


newtype Service = Service (Url 'Http)


data Request = Request
  { operationName :: Text
  , query :: Text
  -- , variables :: Text
  }
  deriving (Generic, ToJSON, FromJSON)


data Response a = Response
  { _data :: [a]
  }
  deriving (Generic)


instance (FromJSON a) => FromJSON (Response a) where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = drop 1}


service :: Text -> Maybe Service
service inp = do
  uri <- mkURI inp
  url <- fst <$> useHttpURI uri
  pure $ Service url


httpsText :: Text -> Url 'Https
httpsText t = https $ T.drop 8 t


runGraphQL
  :: (IOE :> es)
  => Eff (GraphQL : es) a
  -> Eff es a
runGraphQL = interpret $ \_ -> \case
  Query (Service url) q -> do
    let inp = encode $ request q
    let headers = header "Content-Type" "application/json"
    Response r <- runReq defaultHttpConfig $ do
      responseBody <$> req POST url (ReqBodyLbs inp) jsonResponse headers
    pure r


runGraphQLMock
  :: (IOE :> es)
  => (Text -> Request -> IO ByteString)
  -> Eff (GraphQL : es) a
  -> Eff es a
runGraphQLMock mock = interpret $ \_ -> \case
  Query (Service url) q -> liftIO $ do
    resp <- mock (renderUrl url) (request q)
    case eitherDecode resp of
      Left e -> fail $ "Fetch Mock Decode: " <> show e
      Right a -> pure a


request :: (Query a) => a -> Request
request q =
  Request
    { operationName = operationName q
    , query = query q
    }


data NestedFields = NestedFields String [NestedFields]


-- | Return the field names for a given object by proxy
class FieldNames f where
  fieldNames :: Proxy f -> [NestedFields]


instance (FieldNames f, FieldNames g) => FieldNames (f :*: g) where
  fieldNames _ = fieldNames (Proxy :: Proxy f) ++ fieldNames (Proxy :: Proxy g)


instance (FieldNames f, FieldNames g) => FieldNames (f :+: g) where
  fieldNames _ = fieldNames (Proxy :: Proxy f) ++ fieldNames (Proxy :: Proxy g)


instance (KnownSymbol name, FieldNames f) => FieldNames (M1 S ('MetaSel ('Just name) _1 _2 _3) f) where
  fieldNames _ = [NestedFields (symbolVal (Proxy :: Proxy name)) []]


-- instance (KnownSymbol name, FieldNames f) => FieldNames (M1 S ('MetaSel ('Just name) _1 _2 _3) f) where
--   fieldNames _ = [NestedFields (symbolVal (Proxy :: Proxy name)) (fieldNames @f Proxy)]

instance (FieldNames f) => FieldNames (M1 D meta f) where
  fieldNames _ = fieldNames (Proxy :: Proxy f)


instance (FieldNames f) => FieldNames (M1 C meta f) where
  fieldNames _ = fieldNames (Proxy :: Proxy f)


instance FieldNames U1 where
  fieldNames _ = []


instance FieldNames (K1 R f) where
  fieldNames _ = []


-- class DataName f where
--   dataName :: Proxy f -> String
--
-- instance (Datatype d) => DataName (M1 D d f) where
--   dataName _ = datatypeName (undefined :: M1 D d f a)

-- recordFields :: forall a. (Generic a, FieldNames (Rep a)) => Proxy a -> [String]
-- recordFields _ = fmap fst $ fieldNames @(Rep a) Proxy

genQueryFields :: forall a. (Generic a, FieldNames (Rep a)) => Proxy a -> String
genQueryFields _ = allFields $ fieldNames @(Rep a) Proxy
 where
  allFields :: [NestedFields] -> String
  allFields = L.intercalate "\n" . fmap fields

  fields :: NestedFields -> String
  fields (NestedFields nm []) = nm
  fields (NestedFields nm fs) = nm ++ "{" ++ allFields fs ++ "}"
