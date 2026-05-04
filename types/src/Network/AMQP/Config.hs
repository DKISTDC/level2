{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}

module Network.AMQP.Config where

import Data.Char (toLower)
import Effectful
import Effectful.Environment
import NSO.Prelude
import Network.AMQP (ExchangeOpts (..), QueueOpts (..), newExchange, newQueue)
import Network.AMQP.Types (FieldTable (..), FieldValue (..))
import Network.AMQP.Worker (Key, Queue (..), Route)
import Network.AMQP.Worker qualified as AMQP
import Network.AMQP.Worker.Connection (Connection (..), ConnectionOpts, ExchangeName)
import Network.AMQP.Worker.Key (toBindKey)
import Network.AMQP.Worker.Queue qualified as AMQP
import Network.Endpoint as Endpoint (Endpoint (..), toURI)
import Network.URI (uriToString)


data AMQPConfig = AMQPConfig
  { options :: ConnectionOpts
  , exchangeName :: ExchangeName
  , queueType :: QueueType
  }
  deriving (Show)


initAMQPConfig :: Endpoint -> ExchangeName -> QueueType -> Eff es AMQPConfig
initAMQPConfig endpoint exg qt = do
  options <- AMQP.parseURI $ uriToString id (Endpoint.toURI endpoint) ""
  pure $ AMQPConfig{options, exchangeName = exg, queueType = qt}


initAMQPConnection :: (IOE :> es) => AMQPConfig -> Eff es Connection
initAMQPConnection cfg = do
  setExchange <$> AMQP.connect cfg.options
 where
  setExchange cnn = cnn{exchange = cfg.exchangeName}


data QueueType
  = Quorum
  | Classic
  deriving (Show, Read, Eq)


queueType :: (Environment :> es) => String -> Eff es QueueType
queueType qt =
  case qt of
    "quorum" -> pure Quorum
    "classic" -> pure Classic
    _ -> error $ "Invalid Queue Type: " <> qt


bindAMQPQueue :: forall a es. (IOE :> es) => AMQP.Connection -> ExchangeName -> Text -> QueueType -> Key Route a -> Eff es (AMQP.Queue a)
bindAMQPQueue conn exg qname qt key = do
  let qtype = cs $ map toLower $ show qt
  let args = FieldTable [("x-queue-type", FVString qtype)]
  let qopts = newQueue{queueName = qname, queueHeaders = args}
  let q = Queue @a (toBindKey key) qname
  AMQP.bindQueue' conn exchangeWithName qopts key (FieldTable mempty)
  pure q
 where
  exchangeWithName :: ExchangeOpts
  exchangeWithName =
    let ExchangeOpts{..} = newExchange
     in ExchangeOpts{exchangeName = exg, exchangeType = "direct", ..}
