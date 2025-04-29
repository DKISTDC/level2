{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Effectful.GraphQL where

import Control.Monad.Catch (Exception, throwM)
import Data.Aeson hiding (Result)
import Data.Aeson.Types (Parser, parseEither, parseMaybe)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Char (toLower)
import Data.List qualified as L
import Data.String (fromString)
import Data.String.Interpolate (i)
import Data.Text qualified as T
import Debug.Trace
import Effectful
import Effectful.Debug
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static

-- import Effectful.Log
import GHC.Generics
import NSO.Prelude
import Network.HTTP.Req
import Text.URI (mkURI)


data GraphQL :: Effect where
  Query :: (Query a, FromJSON (Result a)) => Service -> a -> GraphQL m [Result a]
type instance DispatchOf GraphQL = 'Dynamic


class Query a where
  type Result a :: Type


  operationName :: a -> Text
  default operationName :: (Generic a, ConstructorName (Rep a)) => a -> Text
  operationName _ = T.pack $ genConName @a Proxy


  selectorName :: a -> Text
  default selectorName :: a -> Text
  selectorName a = T.pack $ mapFirst toLower $ T.unpack $ operationName a
   where
    mapFirst f (x : xs) = f x : xs
    mapFirst _ xs = xs


  query :: a -> Text
  default query :: (Generic (Result a), FieldNames (Rep (Result a))) => a -> Text
  query a =
    let fields = genQueryFields @(Result a) Proxy
        op = operationName a
        sel = selectorName a
     in [i| query #{op} { #{sel} { #{fields} }}|]


newtype Service = Service (Url 'Http)


data Request = Request
  { operationName :: Text
  , query :: Text
  -- , variables :: Text
  }
  deriving (Generic, ToJSON, FromJSON)


data ServerErrors = ServerErrors
  { errors :: [ServerError]
  }
  deriving (Generic, FromJSON, Show)


data ServerError = ServerError
  { message :: Text
  , locations :: [ServerErrorLocation]
  }
  deriving (Generic, FromJSON, Show)


data ServerErrorLocation = ServerErrorLocation
  { line :: Int
  , column :: Int
  }
  deriving (Generic, FromJSON, Show)


-- parseQueryErrors :: a -> Value -> Parser [ServerErrorMessage]
-- parseQueryErrors = _

parseQueryResponse :: forall a. (FromJSON (Result a), Query a) => a -> Value -> Parser [Result a]
parseQueryResponse q = parseData
 where
  parseData :: Value -> Parser [Result a]
  parseData = withObject "DataResponse" $ \v -> do
    -- return a parse error here!
    -- errors :: Maybe [ServerErrorMessage] <- v .:? "errors"
    -- checkErrors errors
    items <- v .: "data"
    parseItems items

  -- checkErrors :: Maybe [ServerErrorMessage] -> Parser ()
  -- checkErrors = \case
  --   Nothing -> pure ()
  --   Just [] -> pure ()
  --   Just errs -> fail $ "Server Errors: " <> mconcat (fmap (cs . (.message)) errs)

  parseItems :: Value -> Parser [Result a]
  parseItems =
    let sel = cs (selectorName q)
     in withObject ("query " <> cs (operationName q) <> " - response." <> sel) $ \v -> do
          v .: fromString sel


service :: Text -> Maybe Service
service inp = do
  uri <- mkURI inp
  url <- fst <$> useHttpURI uri
  pure $ Service url


httpsText :: Text -> Url 'Https
httpsText t = https $ T.drop 8 t


runGraphQL
  :: (Error GraphQLError :> es, IOE :> es)
  => Eff (GraphQL : es) a
  -> Eff es a
runGraphQL = interpret $ \_ -> \case
  Query (Service url) q -> do
    let inp = encode $ request q
    let headers = header "Content-Type" "application/json"
    v <- runReq defaultHttpConfig $ do
      responseBody <$> req POST url (ReqBodyLbs inp) jsonResponse headers
    checkQueryErrors q v
    res <- case parseEither (parseQueryResponse q) v of
      Left e -> throwError $ GraphQLParseError e
      Right as -> pure as
    pure res
 where
  checkQueryErrors q v =
    case parseMaybe parseJSON v of
      Just errs ->
        throwError $ GraphQLServerError (query q) errs
      Nothing -> pure ()


runGraphQLMock
  :: (IOE :> es)
  => (Text -> Request -> IO ByteString)
  -> Eff (GraphQL : es) a
  -> Eff es a
runGraphQLMock mock = interpret $ \_ -> \case
  Query (Service url) q -> liftIO $ do
    putStrLn "Query MOCK"
    resp <- mock (renderUrl url) (request q)
    case parseEither (parseQueryResponse q) =<< eitherDecode resp of
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


instance (Selector s) => FieldNames (M1 S s f) where
  fieldNames _ = [NestedFields (selName @s undefined) []]


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


genQueryFields :: forall a. (Generic a, FieldNames (Rep a)) => Proxy a -> String
genQueryFields _ = allFields $ fieldNames @(Rep a) Proxy
 where
  allFields :: [NestedFields] -> String
  allFields = L.intercalate "\n" . fmap fields

  fields :: NestedFields -> String
  fields (NestedFields nm []) = nm
  fields (NestedFields nm fs) = nm ++ "{" ++ allFields fs ++ "}"


-- | Return the field names for a given object by proxy
class ConstructorName f where
  constructorName :: Proxy f -> String


instance (Constructor meta) => ConstructorName (M1 C meta f) where
  constructorName _ = conName @meta undefined


instance (ConstructorName f) => ConstructorName (M1 D meta f) where
  constructorName _ = constructorName (Proxy :: Proxy f)


instance (ConstructorName f) => ConstructorName (M1 S meta f) where
  constructorName _ = constructorName (Proxy :: Proxy f)


genConName :: forall a. (Generic a, ConstructorName (Rep a)) => Proxy a -> String
genConName _ = constructorName @(Rep a) Proxy


data GraphQLError
  = GraphQLParseError String
  | GraphQLServerError Text ServerErrors
  deriving (Show, Exception)
