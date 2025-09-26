module App.Worker.Publish where

import App.Effect.Transfer (Transfer, transferPublish)
import App.Effect.Transfer qualified as Transfer
import Control.Monad.Catch (Exception)
import Control.Monad.Loops
import Effectful
import Effectful.Concurrent
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.Exception (catch)
import Effectful.Globus (GlobusError, Task)
import Effectful.Log
import Effectful.Tasks
import Effectful.Time
import NSO.Data.Datasets (Datasets)
import NSO.Data.Datasets qualified as Datasets
import NSO.Data.Inversions as Inversions
import NSO.Files.Scratch as Scratch
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
  | PublishTransferring (Id Task)
  deriving (Eq, Ord)


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
     , IOE :> es
     )
  => PublishTask
  -> Eff es ()
publishTask task = do
  res <-
    runErrorNoCallStack @PublishError $
      workWithError
        `catchError` (\_ e -> onCaughtGlobus e)
        `catch` onCaughtError
  case res of
    Left err -> failed err
    Right a -> pure a
 where
  workWithError :: Eff (Error PublishError : es) ()
  workWithError = do
    logContext ("Publish " <> cs task.inversionId.fromId) $ do
      logStatus "starting"

      bucket <- proposalBucket task.proposalId
      taskId <- transferPublish bucket task.proposalId task.inversionId
      send $ TaskSetStatus task $ PublishTransferring taskId

      logStatus "transferring"

      untilM_ (threadDelay (2 * 1000 * 1000)) (isTransferComplete taskId)

      logStatus "transferred!"

      Inversions.setPublished task.inversionId

  failed :: (Show e) => e -> Eff es ()
  failed err = do
    log Err $ dump "Publish Error" err
    Inversions.setError task.inversionId (cs $ show err)


data PublishError
  = TransferFailed (Id Task)
  | MissingProposalDatasets (Id Proposal)
  | MixedProposalBuckets (Id Proposal)
  | GlobusError GlobusError
  | PublishIOError IOError
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
