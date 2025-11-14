module Network.Endpoint where

import Control.Monad.Catch (Exception, throwM)
import Effectful
import Effectful.Environment
import NSO.Prelude
import Network.URI


data Endpoint = Endpoint String URI
  deriving (Show, Eq)


data Mock = Mock
  deriving (Show, Eq)


envEndpoint :: (Environment :> es) => String -> Eff es Endpoint
envEndpoint env = do
  res <- envEndpointMock env
  case res of
    Left Mock -> throwM $ EndpointUnexpectedMock env
    Right e -> pure e


envEndpointMock :: (Environment :> es) => String -> Eff es (Either Mock Endpoint)
envEndpointMock env = do
  val <- getEnv env
  case val of
    "MOCK" -> pure $ Left Mock
    _ -> maybe (err val) (pure . Right . Endpoint env) $ parseURI val
 where
  err :: String -> Eff es a
  err v = throwM $ EndpointInvalidURI env v


data EndpointError
  = EndpointDoesNotExist String
  | EndpointInvalidURI String String
  | EndpointUnexpectedMock String
  deriving (Show, Eq, Exception)
