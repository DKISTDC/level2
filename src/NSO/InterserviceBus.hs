module NSO.InterserviceBus where

import Data.Aeson (FromJSON, ToJSON)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.GenRandom (GenRandom)
import Effectful.Log
import NSO.Files.DKIST as DKIST (Publish, inversionDir)
import NSO.Files.Image (L2Asdf, L2Fits)
import NSO.Files.Scratch as Scratch
import NSO.Image.Asdf as Asdf (generatedL2FrameAsdf)
import NSO.Image.Fits.Frame as Frames (generatedL2FrameFits)
import NSO.Image.Headers.Types (Constant (..))
import NSO.Image.Types.Frame (Frames (..))
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.Dataset
import NSO.Types.InstrumentProgram
import NSO.Types.Inversion
import Network.AMQP.Worker (Key, Route, key, word)
import Network.AMQP.Worker qualified as Worker
import Network.AMQP.Worker.Connection (Connection (..), ConnectionOpts, ExchangeName)


data InterserviceBus :: Effect where
  CatalogFits :: Id Conversation -> Bucket -> Path Publish File L2Fits -> InterserviceBus m ()
  CatalogAsdf :: Id Conversation -> Bucket -> Id Inversion -> Path Publish File L2Asdf -> InterserviceBus m ()


type instance DispatchOf InterserviceBus = 'Dynamic


runInterserviceBus
  :: (IOE :> es, Log :> es, GenRandom :> es)
  => BusConnection
  -> Eff (InterserviceBus : es) a
  -> Eff es a
runInterserviceBus bus = interpret $ \_ -> \case
  CatalogFits convId bucket file -> do
    let msg = catalogFrameMessage bucket file convId
    log Debug $ dump (show bus.catalogFrame) msg
    publishMessage bus.catalogFrame msg
  CatalogAsdf convId bucket invId file -> do
    let msg = catalogAsdfMessage bucket file invId convId
    log Debug $ dump (show bus.catalogObject) msg
    publishMessage bus.catalogObject msg
 where
  publishMessage :: (IOE :> es, ToJSON msg) => Key Route msg -> msg -> Eff es ()
  publishMessage k msg =
    liftIO $ Worker.publish bus.connection k msg

  catalogFrameMessage bucket objectName conversationId =
    CatalogFrameMessage
      { bucket
      , objectName
      , incrementDatasetCatalogReceiptCount = False
      , conversationId
      }

  catalogAsdfMessage bucket objectName invId conversationId =
    CatalogObjectMessage
      { bucket
      , objectName = objectName
      , objectType = Constant
      , groupId = invId.fromId
      , groupName = Constant
      , incrementDatasetCatalogReceiptCount = False
      , conversationId
      }


data InterserviceBusConfig = InterserviceBusConfig
  { options :: ConnectionOpts
  , exchangeName :: ExchangeName
  }
  deriving (Show)


data BusConnection = BusConnection
  { connection :: Connection
  , catalogFrame :: Key Route CatalogFrameMessage
  , catalogObject :: Key Route CatalogObjectMessage
  }


initDummyBusConnection :: (IOE :> es) => InterserviceBusConfig -> Eff es BusConnection
initDummyBusConnection _ = pure $ BusConnection (error "Dummy Bus Connection") (key "catalog" & word "frame" & word "m") (key "catalog" & word "object" & word "m")


initBusConnection :: (IOE :> es) => InterserviceBusConfig -> Eff es BusConnection
initBusConnection cfg = do
  cnn <- setExchange cfg.exchangeName <$> Worker.connect cfg.options
  let frameMessagesKey = key "catalog" & word "frame" & word "m"
  _ <- Worker.queueNamed cnn "catalog.frame.q" frameMessagesKey

  let catalogObjectKey = key "catalog" & word "object" & word "m"
  _ <- Worker.queueNamed cnn "catalog.object.q" catalogObjectKey
  pure $
    BusConnection
      { connection = cnn
      , catalogFrame = frameMessagesKey
      , catalogObject = catalogObjectKey
      }
 where
  setExchange exg cnn = cnn{exchange = exg}


initBusConfig :: String -> String -> Eff es InterserviceBusConfig
initBusConfig uri exc = do
  options <- Worker.parseURI uri
  pure $ InterserviceBusConfig{options, exchangeName = cs exc}


data Conversation


-- Catalog FITS Frames -------------------------------------------------------

catalogFitsFrames :: (Error ScratchError :> es, Scratch :> es, InterserviceBus :> es) => Id Conversation -> Bucket -> Id Proposal -> Id Inversion -> Eff es ()
catalogFitsFrames convId bucket propId invId = do
  Frames fnames <- Frames.generatedL2FrameFits propId invId
  let dir = DKIST.inversionDir propId invId
  let fpaths = fmap (filePath dir . publishFileName) fnames
  mapM_ (send . CatalogFits convId bucket) fpaths


data CatalogFrameMessage = CatalogFrameMessage
  { bucket :: Bucket
  , objectName :: Path Publish File L2Fits
  , incrementDatasetCatalogReceiptCount :: Bool
  , conversationId :: Id Conversation
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)


-- Catalog ASDF  -------------------------------------------------------

catalogAsdf :: (Error ScratchError :> es, Scratch :> es, InterserviceBus :> es) => Id Conversation -> Bucket -> Id Proposal -> Id Inversion -> Eff es ()
catalogAsdf convId bucket propId invId = do
  sname :: Path Scratch Filename L2Asdf <- Asdf.generatedL2FrameAsdf propId invId
  let pth :: Path Publish File L2Asdf = filePath (DKIST.inversionDir propId invId) (publishFileName sname)
  send $ CatalogAsdf convId bucket invId pth


publishFileName :: Path Scratch Filename a -> Path Publish Filename a
publishFileName (Path f) = Path f


data CatalogObjectMessage = CatalogObjectMessage
  { bucket :: Bucket
  , objectName :: Path Publish File L2Asdf
  , objectType :: Constant "ASDF"
  , groupId :: Text -- <inversionId>
  , groupName :: Constant "DATASET"
  , incrementDatasetCatalogReceiptCount :: Bool
  , conversationId :: Id Conversation
  }
  deriving (Generic, Show, ToJSON)

-- https://nso.atlassian.net/wiki/spaces/DPD/pages/3670465/06+-+Interservice+Bus
-- catalog.frame.m
-- { "bucket": "<bucket name>", "objectName": "<object name>", "incrementDatasetCatalogReceiptCount": <True/False>, "conversationId" : "<generated\passed thru uuid>" }
--
-- catalog.object.m
-- { "bucket": "<bucket name>", "objectName": "<object name>", "objectType": "<object_type>", "incrementDatasetCatalogReceiptCount": <True/False>, "groupId": "<group_id>", "groupName": "<group_name>", "conversationId" : "<generated\passed thru uuid>" }
