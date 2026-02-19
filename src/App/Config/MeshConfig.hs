module App.Config.MeshConfig where

import Control.Monad.Catch (Exception, throwM)
import Data.Aeson (FromJSON (..), Options (..), camelTo2, defaultOptions, eitherDecode, genericParseJSON)
import Data.ByteString.Lazy (ByteString)
import NSO.Prelude
import Network.Endpoint (Domain, Endpoint (..), EndpointAuth, Path, Port, Scheme (..))


-- MESH_CONFIG='{
--   "interservice-bus": {"mesh_address": "interservice-bus.service.sim.consul", "mesh_port": 5672},
--   "internal-api-gateway": {"mesh_address": "internal-api-gateway.service.sim.consul", "mesh_port": 80},
-- '}

data MeshConfig a = MeshConfig
  { interserviceBus :: a
  , internalApiGateway :: a
  }
  deriving (Generic)
instance FromJSON (MeshConfig MeshService) where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = camelTo2 '-'}


data MeshService = MeshService
  { meshAddress :: Domain
  , meshPort :: Port
  }
  deriving (Generic)
instance FromJSON MeshService where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = camelTo2 '_'}


parseMeshConfig :: String -> Eff es (MeshConfig MeshService)
parseMeshConfig json = do
  let bs = cs json
  case eitherDecode bs of
    Left e -> throwM $ MeshConfigInvalid e bs
    Right a -> pure a


meshEndpoints :: MeshConfig EndpointAuth -> MeshConfig MeshService -> MeshConfig Path -> MeshConfig Endpoint
meshEndpoints auths mesh paths =
  MeshConfig
    { interserviceBus = meshEndpoint (Scheme "amqp") mesh.interserviceBus auths.interserviceBus paths.interserviceBus
    , internalApiGateway = meshEndpoint (Scheme "http") mesh.internalApiGateway auths.internalApiGateway paths.internalApiGateway
    }


meshEndpoint :: Scheme -> MeshService -> EndpointAuth -> Path -> Endpoint
meshEndpoint scheme ms auth path =
  Endpoint
    { auth = auth
    , scheme
    , domain = ms.meshAddress
    , port = ms.meshPort
    , path
    }


data MeshConfigError
  = MeshConfigInvalid String ByteString
  deriving (Show, Eq, Exception)
