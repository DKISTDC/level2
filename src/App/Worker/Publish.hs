module App.Worker.Publish where

import App.Effect.Transfer (Transfer, transferPublish)
import App.Effect.Transfer qualified as Transfer
import Control.Monad.Catch (Exception, Handler (..), catches)
import Control.Monad.Loops
import Effectful
import Effectful.Concurrent
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.GenRandom
import Effectful.Globus (GlobusError, Task)
import Effectful.GraphQL (GraphQLError)
import Effectful.Log
import Effectful.Tasks
import Effectful.Time
import NSO.Data.Datasets (Datasets)
import NSO.Data.Datasets qualified as Datasets
import NSO.Data.Inversions as Inversions
import NSO.Files.Scratch as Scratch
import NSO.InterserviceBus as InterserviceBus
import NSO.Metadata as Metadata
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.Dataset (Bucket)
import NSO.Types.InstrumentProgram
import Network.Globus (taskPercentComplete)
import Network.Globus qualified as Globus


data PublishTask = PublishTask {proposalId :: Id Proposal, inversionId :: Id Inversion}
  deriving (Eq, Show)


instance WorkerTask PublishTask where
  type Status PublishTask = PublishStatus
  idle = PublishWaiting


data PublishStatus
  = PublishWaiting
  | PublishStarted
  | PublishTransferring (Id Task)
  | PublishMessages
  | PublishSave
  deriving (Eq, Ord, Show)


startPublish :: (Tasks PublishTask :> es) => Id Proposal -> Id Inversion -> Eff es ()
startPublish propId invId = do
  send $ TaskAdd $ PublishTask propId invId


publishTask
  :: forall es
   . ( Datasets :> es
     , Inversions :> es
     , Time :> es
     , Scratch :> es
     , Log :> es
     , Concurrent :> es
     , Tasks PublishTask :> es
     , Transfer :> es
     , Error GlobusError :> es
     , Error GraphQLError :> es
     , InterserviceBus :> es
     , GenRandom :> es
     , IOE :> es
     , MetadataInversions :> es
     )
  => PublishTask
  -> Eff es ()
publishTask task = do
  res <-
    runErrorNoCallStack @PublishError $
      workWithError
        `catchError` (\_ e -> onCaughtGlobus e)
        `catchError` (\_ e -> onCaughtGraphQL e)
        `catches` [Handler onCaughtError]
  case res of
    Left err -> failed err
    Right a -> pure a
 where
  workWithError :: Eff (Error PublishError : es) ()
  workWithError = do
    logContext ("Publish " <> cs task.inversionId.fromId) $ do
      logStatus "starting"
      send $ TaskSetStatus task PublishStarted

      bucket <- proposalBucket task.proposalId
      taskId <- transferPublish bucket task.proposalId task.inversionId
      send $ TaskSetStatus task $ PublishTransferring taskId

      logStatus "transferring"
      untilM_ (threadDelay (2 * 1000 * 1000)) (isTransferComplete taskId)

      send $ TaskSetStatus task PublishMessages
      logStatus "sending frame messages"

      conversationId <- randomId "l2pub"
      runScratchError $ do
        InterserviceBus.catalogFitsFrames conversationId bucket task.proposalId task.inversionId
        InterserviceBus.catalogAsdf conversationId bucket task.proposalId task.inversionId

      send $ TaskSetStatus task PublishSave
      inv <- loadInversion task.inversionId
      datasets <- Datasets.find $ Datasets.ByIds inv.datasets

      _ <- send $ Metadata.CreateInversion bucket inv datasets

      Inversions.setPublished task.inversionId

  runScratchError = runErrorNoCallStackWith (throwError . ScratchError)

  loadInversion invId = do
    is <- send $ Inversions.ById invId
    case is of
      [inv] -> pure inv
      _ -> throwError $ MissingInversion invId

  failed :: (Show e) => e -> Eff es ()
  failed err = do
    log Err $ dump "Publish Error" err
    Inversions.setError task.inversionId (cs $ show err)


data PublishError
  = TransferFailed (Id Task)
  | MissingProposalDatasets (Id Proposal)
  | MixedProposalBuckets (Id Proposal)
  | GlobusError GlobusError
  | GraphQLError GraphQLError
  | PublishIOError IOError
  | ScratchError ScratchError
  | MissingInversion (Id Inversion)
  deriving (Show, Exception)


isTransferComplete :: (Log :> es, Transfer :> es, Error PublishError :> es) => Id Task -> Eff es Bool
isTransferComplete it = do
  task <- Transfer.transferStatus it
  case task.status of
    Globus.Succeeded -> do
      pure True
    Globus.Failed -> throwError $ TransferFailed it
    _ -> do
      logStatus $ dump "Transfer" $ taskPercentComplete task
      pure False


onCaughtError :: (Log :> es) => IOError -> Eff (Error PublishError : es) a
onCaughtError e = do
  log Err "Catch IO Error"
  throwError $ PublishIOError e


onCaughtGlobus :: (Log :> es) => GlobusError -> Eff (Error PublishError : es) a
onCaughtGlobus e = do
  log Err "Catch GLOBUS"
  throwError $ GlobusError e


onCaughtGraphQL :: (Log :> es) => GraphQLError -> Eff (Error PublishError : es) a
onCaughtGraphQL e = do
  log Err "Catch GraphQL"
  throwError $ GraphQLError e


proposalBucket
  :: (Datasets :> es, Error PublishError :> es)
  => Id Proposal
  -> Eff es Bucket
proposalBucket pid = do
  datasets <- Datasets.find (Datasets.ByProposal pid)
  case datasets of
    [] -> throwError (MissingProposalDatasets pid)
    (d : rest) ->
      if all ((== d.bucket) . (.bucket)) rest
        then pure d.bucket
        else throwError (MixedProposalBuckets pid)
