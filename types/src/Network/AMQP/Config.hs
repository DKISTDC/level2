module Network.AMQP.Config where

import Effectful
import NSO.Prelude
import Network.AMQP.Worker qualified as Worker
import Network.AMQP.Worker.Connection (Connection (..), ConnectionOpts, ExchangeName)
import Network.Endpoint as Endpoint (Endpoint (..), toURI)
import Network.URI (uriToString)


data AMQPConfig = AMQPConfig
  { options :: ConnectionOpts
  , exchangeName :: ExchangeName
  }
  deriving (Show)


initAMQPConfig :: Endpoint -> ExchangeName -> Eff es AMQPConfig
initAMQPConfig endpoint exg = do
  options <- Worker.parseURI $ uriToString id (Endpoint.toURI endpoint) ""
  pure $ AMQPConfig{options, exchangeName = exg}


initAMQPConnection :: (IOE :> es) => AMQPConfig -> Eff es Connection
initAMQPConnection cfg = do
  setExchange <$> Worker.connect cfg.options
 where
  setExchange cnn = cnn{exchange = cfg.exchangeName}
