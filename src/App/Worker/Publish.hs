module App.Worker.Publish where

import App.Effect.Publish (transferSoftPublish)
import App.Effect.Scratch as Scratch
import App.Globus (Globus, GlobusError, Task, Token, Token' (Access))
import App.Globus qualified as Globus
import Control.Monad.Loops
import Effectful
import Effectful.Concurrent
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.Log
import Effectful.Reader.Dynamic
import Effectful.Tasks
import Effectful.Time
import NSO.Data.Datasets
import NSO.Data.Inversions as Inversions
import NSO.Prelude
import NSO.Types.InstrumentProgram


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
   . ( Reader (Token Access) :> es
     , Globus :> es
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
  res <- runErrorNoCallStack @GlobusError workWithError
  either failed pure res
 where
  workWithError :: Eff (Error GlobusError : es) ()
  workWithError = do
    log Debug "Publish Task"

    taskId <- transferSoftPublish task.proposalId task.inversionId
    send $ TaskSetStatus task $ PublishTransferring taskId

    log Debug " - publish transferring"

    untilM_ (threadDelay (2 * 1000 * 1000)) (Globus.isTransferComplete taskId)

    Inversions.setPublished task.inversionId

  failed err = do
    log Err $ dump "Publish Error" err
    Inversions.setError task.inversionId (cs $ show err)
