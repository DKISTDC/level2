module NSO.InterserviceBus where

import Control.Monad.Catch (MonadThrow)
import Data.Aeson (FromJSON, ToJSON)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Log
import Effectful.Reader.Dynamic
import NSO.Files.DKIST (Bucketed, Publish)
import NSO.Files.DKIST qualified as DKIST
import NSO.Files.Image (isFits)
import NSO.Files.Image qualified as Files
import NSO.Files.Scratch as Scratch
import NSO.Image.Fits.Frame (L2FrameFits)
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.Dataset
import NSO.Types.InstrumentProgram
import NSO.Types.Inversion
import Network.AMQP.Worker (Bind, Connection, Key, Route, key, word)
import Network.AMQP.Worker qualified as Worker
import Network.AMQP.Worker.Connection (ConnectionOpts)
import Network.AMQP.Worker.Key (Bind, Key, key, word)


data InterserviceBus :: Effect where
  -- send a message FOR EACH frame?
  CatalogFrames :: Id Proposal -> Id Inversion -> Bucket -> InterserviceBus m ()
type instance DispatchOf InterserviceBus = 'Dynamic


runInterserviceBus
  :: (IOE :> es, Scratch :> es, Log :> es)
  => Connection
  -> Eff (InterserviceBus : es) a
  -> Eff es a
runInterserviceBus conn = interpret $ \_ -> \case
  CatalogFrames propId invId bucket -> do
    msgs <- catalogFrameMessages propId invId bucket
    mapM_ publishFrameMessage msgs
 where
  publishFrameMessage :: (IOE :> es, Log :> es) => CatalogFrameMessage -> Eff es ()
  publishFrameMessage msg = do
    log Debug $ dump (show frameMessagesKey) msg
    liftIO $ Worker.publish conn frameMessagesKey msg


catalogFrameMessages :: (Scratch :> es) => Id Proposal -> Id Inversion -> Bucket -> Eff es [CatalogFrameMessage]
catalogFrameMessages propId invId bucket = do
  let dir = Files.outputL2Dir propId invId
  fits <- fmap frameObjectName . filter isFits <$> Scratch.listDirectory dir -- Path Scratch Filename a
  pure $ fmap catalogFrameMessage fits
 where
  frameObjectName :: Path Scratch Filename Inversion -> Path Publish File L2FrameFits
  frameObjectName (Path filename) =
    let fpath :: Path Publish Filename L2FrameFits = Path filename
        dir :: Path Publish Dir Inversion = Files.inversionDir propId invId
     in filePath dir fpath

  catalogFrameMessage :: Path Publish File L2FrameFits -> CatalogFrameMessage
  catalogFrameMessage objectName =
    CatalogFrameMessage
      { bucket
      , objectName
      , incrementDatasetCatalogReceiptCount = False
      , conversationId = Id invId.fromId
      }


newtype InterserviceBusConfig = InterserviceBusConfig ConnectionOpts


initBusConnection :: (IOE :> es) => InterserviceBusConfig -> Eff es Connection
initBusConnection (InterserviceBusConfig opts) = Worker.connect opts


parseConnectionOpts :: (MonadThrow m) => String -> m InterserviceBusConfig
parseConnectionOpts = fmap InterserviceBusConfig <$> Worker.parseURI


data Conversation


data CatalogFrameMessage = CatalogFrameMessage
  { bucket :: Bucket
  , objectName :: Path Publish File L2FrameFits
  , incrementDatasetCatalogReceiptCount :: Bool
  , conversationId :: Id Conversation
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)


-- Keys, we don't need to create or know the queues
frameMessagesKey :: Key Route CatalogFrameMessage
frameMessagesKey = key "catalog" & word "frame" & word "m"

-- I need to name this the same thing as frame cataloger
-- q <- Worker.queue conn "catalog.frame.q" frameMessages

--   CreateInversion :: Inversion -> MetadataInversions m [InversionInventory]
-- type instance DispatchOf InterserviceBus = 'Dynamic

-- https://nso.atlassian.net/wiki/spaces/DPD/pages/3670465/06+-+Interservice+Bus
-- catalog.frame.m
-- { "bucket": "<bucket name>", "objectName": "<object name>", "incrementDatasetCatalogReceiptCount": <True/False>, "conversationId" : "<generated\passed thru uuid>" }
--
-- catalog.object.m
-- { "bucket": "<bucket name>", "objectName": "<object name>", "objectType": "<object_type>", "incrementDatasetCatalogReceiptCount": <True/False>, "groupId": "<group_id>", "groupName": "<group_name>", "conversationId" : "<generated\passed thru uuid>" }
