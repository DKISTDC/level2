module App.Worker.Publish where

import App.Effect.Transfer (Transfer, transferSoftPublish)
import App.Effect.Transfer qualified as Transfer
import Control.Monad.Catch (Exception)
import Control.Monad.Loops
import Effectful
import Effectful.Concurrent
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.Globus (GlobusError, Task)
import Effectful.Log
import Effectful.Tasks
import Effectful.Time
import NSO.Data.Inversions as Inversions
import NSO.Files.Scratch as Scratch
import NSO.Prelude
import NSO.Types.Common
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


startSoftPublish :: (Tasks PublishTask :> es) => Id Proposal -> Id Inversion -> Eff es ()
startSoftPublish propId invId = do
  send $ TaskAdd $ PublishTask propId invId


publishTask
  :: forall es
   . ( Transfer :> es
     , Inversions :> es
     , Time :> es
     , Scratch :> es
     , Log :> es
     , Concurrent :> es
     , Tasks PublishTask :> es
     )
  => PublishTask
  -> Eff es ()
publishTask task = do
  res <- runErrorNoCallStack @GlobusError . runErrorNoCallStack @PublishError $ workWithError
  case res of
    Left err -> failed err
    Right (Left err) -> failed err
    Right (Right a) -> pure a
 where
  workWithError :: Eff (Error PublishError : Error GlobusError : es) ()
  workWithError = do
    log Debug "Publish Task"

    taskId <- transferSoftPublish task.proposalId task.inversionId
    send $ TaskSetStatus task $ PublishTransferring taskId

    log Debug " - publish transferring"

    untilM_ (threadDelay (2 * 1000 * 1000)) (isTransferComplete taskId)

    Inversions.setPublished task.inversionId

  failed :: (Show e) => e -> Eff es ()
  failed err = do
    log Err $ dump "Publish Error" err
    Inversions.setError task.inversionId (cs $ show err)


data PublishError
  = TransferFailed (Id Task)
  deriving (Show, Eq, Exception)


isTransferComplete :: (Log :> es, Transfer :> es, Error PublishError :> es) => Id Task -> Eff es Bool
isTransferComplete it = do
  task <- Transfer.transferStatus it
  case task.status of
    Globus.Succeeded -> pure True
    Globus.Failed -> throwError $ TransferFailed it
    _ -> do
      log Debug $ dump "Transfer" $ taskPercentComplete task
      pure False
