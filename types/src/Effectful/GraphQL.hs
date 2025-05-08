{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Effectful.GraphQL where

import Control.Monad.Catch (Exception)
import Data.Aeson hiding (Result)
import Data.Aeson qualified as A
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types qualified as A
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Char (toLower)
import Data.List qualified as L
import Data.String (fromString)
import Data.String.Interpolate (i)
import Data.Text qualified as T
import Debug.Trace
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import GHC.Generics
import NSO.Prelude
import Network.HTTP.Client hiding (Proxy, Request, Response)
import Network.HTTP.Client qualified as Http
import Network.HTTP.Types (methodPost)


data GraphQL :: Effect where
  Query :: (Request a, FromJSON (Data a)) => Service -> a -> GraphQL m (Data a)
  Mutation :: (Request a, FromJSON (Data a)) => Service -> a -> GraphQL m (Data a)
type instance DispatchOf GraphQL = 'Dynamic


class Request a where
  type Data a :: Type


  rootField :: Text
  default rootField :: (Generic a, ConstructorName (Rep a)) => Text
  rootField = mapFirst toLower $ T.pack $ genConName @a
   where
    mapFirst :: (Char -> Char) -> Text -> Text
    mapFirst f t =
      T.map f (T.take 1 t) <> T.drop 1 t


  parameters :: a -> [(Key, A.Value)]
  default parameters :: (ToJSON a) => a -> [(Key, A.Value)]
  parameters a =
    case toJSON a of
      Object o -> KM.toList o
      _ -> []


  request :: a -> Text
  default request :: (Generic (Data a), FieldNames (Data a)) => a -> Text
  request a =
    let params = parametersText (parameters a)
        fields = requestFields @(Data a)
     in [i|{#{rootField @a}#{params} { #{fields} }}|]


parametersText :: [(Key, A.Value)] -> Text
parametersText [] = ""
parametersText ps = "(" <> T.intercalate "," (fmap paramText ps) <> ")"
 where
  paramText (k, val) = K.toText k <> ": " <> cs (encodeGraphQL val)


encodeGraphQL :: A.Value -> ByteString
encodeGraphQL = \case
  Object km ->
    "{" <> (BL.intercalate "," $ fmap pair $ KM.toList km) <> "}"
  val -> A.encode val
 where
  pair (k, v) = cs (K.toText k) <> ":" <> encodeGraphQL v


class RequestParameters a where
  requestParameters :: a -> [(Text, A.Value)]


class Mutation a


newtype Service = Service Http.Request


-- data Request = Request
--   { operationName :: Text
--   , query :: Text
--   -- , variables :: Text
--   }
--   deriving (Generic, ToJSON, FromJSON)

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


data Response a
  = Errors [ServerError]
  | Data A.Value


instance (Request a) => FromJSON (Response a) where
  parseJSON = withObject "GraphQL Response" $ \o -> do
    Errors <$> o .: "errors" <|> Data <$> parseData o
   where
    parseData o = do
      dat <- o .: "data"
      dat .: fromString (cs $ rootField @a)


-- parseQueryErrors :: a -> Value -> Parser [ServerErrorMessage]
-- parseQueryErrors = _

-- parseResponse :: forall a. (FromJSON (Data a), Request a) => a -> Value -> Parser (Either [ServerError] (Data a))
-- parseResponse q val = Left <$> parseErrors val <|> Right <$> parseData val
--  where
--   parseErrors :: Value -> Parser [ServerError]
--   parseErrors = withObject "GraphQL response errors" $ \o -> do
--     o .: "errors"
--
--   parseData :: Value -> Parser (Data a)
--   parseData = withObject "GraphQL Response data" $ \v -> do
--     dt <- v .: "data"
--     parseRootField dt
--
--   -- checkErrors :: Maybe [ServerErrorMessage] -> Parser ()
--   -- checkErrors = \case
--   --   Nothing -> pure
--   --   Just [] -> pure ()
--   --   Just errs -> fail $ "Server Errors: " <> mconcat (fmap (cs . (.message)) errs)
--
--   parseRootField :: Value -> Parser (Data a)
--   parseRootField =
--     let field = cs $ rootField q
--      in withObject ("GraphQL Response data.rootField: " <> cs field) $ \v -> do
--           v .: fromString field

service :: Text -> Maybe Service
service inp = do
  traceM $ "service: " <> show inp
  req <- parseRequest (cs inp)
  pure $ Service req


-- httpsText :: Text -> Url 'Https
-- httpsText t = https $ T.drop 8 t

runGraphQL
  :: (Error GraphQLError :> es, IOE :> es)
  => Http.Manager
  -> Eff (GraphQL : es) a
  -> Eff es a
runGraphQL mgr = interpret $ \_ -> \case
  Query s q -> sendRequest mgr s "query" q
  Mutation s q -> sendRequest mgr s "mutation" q


newtype ReqType = ReqType Text
  deriving newtype (IsString)


sendRequest :: forall r es. (Request r, FromJSON (Data r), Error GraphQLError :> es, IOE :> es) => Manager -> Service -> ReqType -> r -> Eff es (Data r)
sendRequest mgr (Service sv) rt r = do
  -- liftIO $ putStrLn $ "QUERY: " <> show sv
  let requestHeaders = [("Content-Type", "application/json")]
  putStrLn "REQ"
  putStrLn $ cs $ request r
  let requestBody = RequestBodyLBS $ body rt $ request r
  let req = sv{method = methodPost, requestHeaders, requestBody}
  res <- liftIO $ Http.httpLbs req mgr
  case A.eitherDecode @(Response r) (responseBody res) of
    Left e -> throwError $ GraphQLParseError (request r) e
    Right (Errors es) -> throwError $ GraphQLServerError (request r) es
    Right (Data v) -> do
      case A.parseEither parseJSON v of
        Left e -> throwError $ GraphQLParseError (request r) e
        Right d -> pure d
 where
  body (ReqType typ) rbody =
    -- but it isn't escaped if we do this, right?
    [i|{#{A.encode typ}: #{A.encode rbody}}|]


-- runGraphQLMock
--   :: (IOE :> es)
--   => _
--   -> Eff (GraphQL : es) a
--   -> Eff es a
-- runGraphQLMock mock = interpret $ \_ -> \case
--   Query (Service url) q -> liftIO $ do
--     putStrLn "Query MOCK"
--     resp <- mock (renderUrl url) (request q)
--     case parseEither (parseQueryResponse q) =<< eitherDecode resp of
--       Left e -> fail $ "Fetch Mock Decode: " <> show e
--       Right a -> pure a
--   Mutation (Service url) r -> _

-- request :: (Query a) => a -> Request
-- request q =
--   Request
--     { operationName = operationName q
--     , query = query q
--     }

data NestedFields = NestedFields String [NestedFields]


-- | Return the field names for a given object by proxy
class FieldNames (a :: Type) where
  fieldNames :: Proxy a -> [NestedFields]
  default fieldNames :: (Generic a, GFieldNames (Rep a)) => Proxy a -> [NestedFields]
  fieldNames _ = gFieldNames @(Rep a) Proxy


instance (FieldNames a) => FieldNames [a] where
  fieldNames _ = fieldNames @a Proxy


class GFieldNames (f :: Type -> Type) where
  gFieldNames :: Proxy f -> [NestedFields]


instance (GFieldNames f, GFieldNames g) => GFieldNames (f :*: g) where
  gFieldNames _ = gFieldNames (Proxy :: Proxy f) ++ gFieldNames (Proxy :: Proxy g)


instance (GFieldNames f, GFieldNames g) => GFieldNames (f :+: g) where
  gFieldNames _ = gFieldNames (Proxy :: Proxy f) ++ gFieldNames (Proxy :: Proxy g)


instance (Selector s) => GFieldNames (M1 S s f) where
  gFieldNames _ = [NestedFields (selName @s undefined) []]


-- instance (KnownSymbol name, GFieldNames f) => GFieldNames (M1 S ('MetaSel ('Just name) _1 _2 _3) f) where
--   gFieldNames _ = [NestedFields (symbolVal (Proxy :: Proxy name)) (gFieldNames @f Proxy)]

instance (GFieldNames f) => GFieldNames (M1 D meta f) where
  gFieldNames _ = gFieldNames (Proxy :: Proxy f)


instance (GFieldNames f) => GFieldNames (M1 C meta f) where
  gFieldNames _ = gFieldNames (Proxy :: Proxy f)


instance GFieldNames U1 where
  gFieldNames _ = []


instance GFieldNames (K1 R f) where
  gFieldNames _ = []


requestFields :: forall a. (FieldNames a) => String
requestFields = allFields $ fieldNames @a Proxy
 where
  allFields :: [NestedFields] -> String
  allFields = L.intercalate " " . fmap fields

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


genConName :: forall a. (Generic a, ConstructorName (Rep a)) => String
genConName = constructorName @(Rep a) Proxy


data GraphQLError
  = GraphQLParseError Text String
  | GraphQLServerError Text [ServerError]
  deriving (Show, Exception)
