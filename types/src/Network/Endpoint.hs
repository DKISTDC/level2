module Network.Endpoint where

import Control.Monad.Catch (Exception, throwM)
import Data.Aeson (FromJSON)
import Data.List qualified as L
import Effectful
import Effectful.Environment
import NSO.Prelude
import Network.URI (URI (..), URIAuth (..))
import Network.URI qualified as URI
import System.FilePath ((</>))
import Text.Read (readMaybe)


data Endpoint = Endpoint
  { scheme :: Scheme
  , auth :: EndpointAuth
  , domain :: Domain
  , port :: Port
  , path :: Path
  }
  deriving (Show, Eq)


data EndpointAuth
  = AuthNone
  | AuthToken Text
  | AuthUser Text Text
  deriving (Show, Eq)


newtype Domain = Domain {value :: String}
  deriving newtype (FromJSON, Show, Eq)


newtype Port = Port {value :: Int}
  deriving newtype (FromJSON, Show, Eq)


newtype Scheme = Scheme {value :: String}
  deriving newtype (FromJSON, Show, Eq)


newtype Path = Path {value :: String}
  deriving newtype (FromJSON, Show, Eq, IsString)
instance Semigroup Path where
  Path p1 <> Path p2 = Path $ p1 </> p2
instance Monoid Path where
  mempty = Path mempty


data Mock = Mock
  deriving (Show, Eq)


getEndpointURIMock :: (Environment :> es) => Env -> Eff es (Either Mock Endpoint)
getEndpointURIMock env = do
  val <- getEnv env.value
  case val of
    "MOCK" -> pure $ Left Mock
    _ -> Right <$> endpointURI env val


getEndpointURI :: (Environment :> es) => Env -> Eff es Endpoint
getEndpointURI env = do
  val <- getEnv env.value
  endpointURI env val


endpointURI :: Env -> String -> Eff es Endpoint
endpointURI env val = do
  uri <- maybe (throwM $ EndpointInvalidURI env val) pure $ URI.parseURI val
  base <- maybe (throwM $ EndpointMissingURIAuth env) pure $ uri.uriAuthority
  scheme <- parseScheme uri
  auth <- parseAuth base
  domain <- parseDomain base
  port <- parsePort base
  path <- parsePath uri
  pure $ Endpoint{scheme, auth, domain, port, path}
 where
  parseAuth ua = do
    case L.dropWhileEnd (== '@') ua.uriUserInfo of
      "" -> pure AuthNone
      as ->
        case L.break (/= ':') as of
          (t, "") -> pure $ AuthToken (cs t)
          (u, ':' : p) -> pure $ AuthUser (cs u) (cs p)
          _ -> throwM $ EndpointInvalidAuth env as

  parsePort ua =
    case readMaybe (drop 1 ua.uriPort) of
      Nothing -> throwM $ EndpointInvalidPort env ua.uriPort
      Just p -> pure $ Port p

  parseDomain ua = pure $ Domain ua.uriRegName
  parsePath u = pure $ Path u.uriPath
  parseScheme u = pure $ Scheme u.uriScheme


toURI :: Endpoint -> URI
toURI e =
  URI
    { uriScheme = L.dropWhileEnd (== ':') e.scheme.value <> ":"
    , uriAuthority =
        Just
          URIAuth
            { uriUserInfo = userInfo e.auth
            , uriRegName = e.domain.value
            , uriPort = ":" <> show e.port.value
            }
    , uriPath = e.path.value
    , uriQuery = ""
    , uriFragment = ""
    }
 where
  userInfo = \case
    AuthNone -> ""
    AuthToken t -> cs t <> "@"
    AuthUser u p -> cs u <> ":" <> cs p <> "@"


newtype Env = Env {value :: String}
  deriving newtype (Show, Eq, IsString)


data EndpointError
  = EndpointDoesNotExist String
  | EndpointInvalidURI Env String
  | EndpointUnexpectedMock String
  | EndpointMissingURIAuth Env
  | EndpointInvalidPort Env String
  | EndpointInvalidAuth Env String
  deriving (Show, Eq, Exception)
