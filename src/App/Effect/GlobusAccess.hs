{-# LANGUAGE AllowAmbiguousTypes #-}

module App.Effect.GlobusAccess where

import App.Effect.Auth
import Control.Monad.Loops (untilM_)
import Data.Time.Clock (NominalDiffTime, addUTCTime, secondsToNominalDiffTime)
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.STM
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.Globus hiding (Id)
import Effectful.Globus qualified as Globus
import Effectful.Log
import Effectful.Time
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.User (CurrentAccess (..), User (..))


-- Transfer relies on a single implementation of (GlobusAccess remote) for each remote
-- the way it is implemented can change

-- User Auth: If they have logged in, they have valid globus access, but not a chosen remote folder
data GlobusAccess (remote :: Type) :: Effect where
  GetCurrentAccess :: GlobusAccess remote m CurrentAccess
  TransferStatus :: Id Task -> GlobusAccess remote m Task
type instance DispatchOf (GlobusAccess remote) = 'Dynamic


-- you'll have to run this manually in the functions that use it
runGlobusUserAccess
  :: (Auth :> es, Globus :> es) => Eff (GlobusAccess User : es) a -> Eff es a
runGlobusUserAccess = interpret $ \_ -> \case
  TransferStatus taskId -> do
    u <- send GetUser
    fetchTransfer u.access taskId
  GetCurrentAccess -> do
    u <- send GetUser
    pure u.access


fetchTransfer :: (Globus :> es) => CurrentAccess -> Id Task -> Eff es Task
fetchTransfer access taskId = do
  -- NOTE: use the source access to check the transfer (ignoring dest access). Works for User, etc
  send $ StatusTask access.token (Tagged taskId.fromId)


-- if taskId == Id fakeLocalTask.task_id.unTagged
--   then do
--     threadDelay 1000000
--     pure fakeLocalTask
--   else do
-- Auth.waitForAdmin $ do

transferStatus :: forall remote es. (GlobusAccess remote :> es) => Id Task -> Eff es Task
transferStatus = send . TransferStatus @remote


runGlobusClientAccess
  :: forall remote a es
   . (Globus :> es, Concurrent :> es, Time :> es, Log :> es)
  => ClientAccess
  -> Eff (GlobusAccess remote : es) a
  -> Eff es a
runGlobusClientAccess (ClientAccess var) = interpret $ \_ -> \case
  TransferStatus taskId -> do
    acc <- getAccess
    fetchTransfer acc taskId
  GetCurrentAccess -> getAccess
 where
  getAccess :: Eff es CurrentAccess
  getAccess = do
    now <- currentTime
    macc <- atomically $ do
      -- Wait if empty, means we are busy refreshing it
      ca <- takeTMVar var
      if ca.expires < addUTCTime earlyRefreshSeconds now
        then pure Nothing -- refresh!
        else pure $ Just ca
    acc <- maybe fetchClientToken pure macc
    atomically $ putTMVar var acc
    pure acc

  earlyRefreshSeconds :: NominalDiffTime
  earlyRefreshSeconds = 3600 -- 1h = 3600s


-- TODO: is this right? Should we be using a refresh token instead?
fetchClientToken :: (Time :> es, Globus :> es, Concurrent :> es, Log :> es) => Eff es CurrentAccess
fetchClientToken = do
  log Debug "Fetch Globus Client Access Token"
  now <- currentTime
  tokenClient <- send GetClientAuth
  let expires = clientExpires now tokenClient
  let access :: CurrentAccess = CurrentAccess tokenClient.access_token expires
  pure access
 where
  clientExpires :: UTCTime -> TokenClient -> UTCTime
  clientExpires now tok =
    let expiresDiff = secondsToNominalDiffTime $ fromIntegral tok.expires_in
     in addUTCTime expiresDiff now


newtype ClientAccess = ClientAccess (TMVar CurrentAccess)


initGlobusClientAccess :: (Concurrent :> es, Time :> es, Globus :> es, Log :> es) => Eff es ClientAccess
initGlobusClientAccess = do
  -- immediately fetch a globus access token
  access <- fetchClientToken
  var <- newTMVarIO access
  pure $ ClientAccess var


getCurrentAccess :: forall remote es. (GlobusAccess remote :> es) => Eff es CurrentAccess
getCurrentAccess = send (GetCurrentAccess @remote)


-- helpers ------------------------------------------------------

waitForTransfer
  :: forall remote err es
   . (Concurrent :> es, GlobusAccess remote :> es, Error err :> es, Show err)
  => (Task -> err)
  -> Id Globus.Task
  -> Eff es ()
waitForTransfer toError taskId = do
  untilM_ delay2s taskComplete
 where
  taskComplete :: Eff es Bool
  taskComplete = do
    tsk <- transferStatus @remote taskId
    case tsk.status of
      Failed -> throwError @err $ toError tsk
      Succeeded -> pure True
      _ -> pure False

  delay2s = threadDelay $ 2 * 1000 * 1000
