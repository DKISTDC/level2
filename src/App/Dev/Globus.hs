module App.Dev.Globus where

import App.Effect.Scratch as Scratch
import App.Globus
import App.Route
import Control.Monad (forM_)
import Data.List.NonEmpty
import Data.Text (Text)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Globus
import NSO.Types.Common (Path' (..), PathType (..), isPathAbsolute, (</>))
import Network.Globus.Auth
import Web.Hyperbole
import Prelude


runGlobusDev
  :: (Scratch :> es, IOE :> es)
  => Path' Dir DKIST
  -> Eff (Globus : es) a
  -> Eff es a
runGlobusDev dkistDir = interpret $ \_ -> \case
  AuthUrl red _ _ -> do
    pure $ Uri Https "localhost" (routePath $ Dev DevAuth) (Query [("redirect", Just $ renderUri red)])
  GetUserInfo _ -> do
    pure $
      UserInfoResponse
        { info = UserInfo "sub" 10
        , email = Just $ UserEmail "shess@nso.edu"
        , profile = Nothing
        }
  GetAccessTokens _ _ -> do
    pure $
      singleton $
        TokenItem
          { scope = Scopes $ TransferAll :| [Identity OpenId]
          , access_token = Tagged "access"
          , expires_in = 3600
          , state = State "state"
          }
  SubmissionId _ -> do
    pure "submission"
  Transfer _ tr -> do
    liftIO $ print tr.source_endpoint
    liftIO $ print tr.destination_endpoint
    forM_ tr.data_ $ \ti -> do
      -- the source may be absolute: /Users/seanhess/Data/pid_1_118/etc....
      -- or it may be local, but from the main store
      liftIO $ putStrLn $ "TRANSFER ITEM: " ++ show ti.source_path ++ " " ++ show ti.destination_path ++ " " ++ show ti.recursive
      -- liftIO $ putStrLn $ "  :> " ++ show (localSourcePath tr.source_endpoint src)
      -- Scratch.copyFile (localSourcePath tr.source_endpoint src) (Path ti.destination_path)
      localCopy tr.source_endpoint ti.source_path ti.destination_path

    pure $
      TransferResponse
        { task_id = "task"
        , submission_id = "submission"
        , message = "message"
        , resource = "resource"
        , request_id = "request"
        }
  StatusTask _ _ -> do
    pure
      Task
        { status = Succeeded
        , task_id = "task"
        , label = "label"
        , files = 10
        , directories = 10
        , files_skipped = 0
        , files_transferred = 10
        , bytes_transferred = 100
        , bytes_checksummed = 100
        , effective_bytes_per_second = 100
        , nice_status = Nothing
        , source_endpoint_id = "source"
        , destination_endpoint_id = "destination"
        }
  StatusTasks _ _ -> do
    error "Not Implemented"
 where
  localCopy :: (Scratch :> es) => Id Collection -> FilePath -> FilePath -> Eff es ()
  localCopy cl src dest = do
    if cl == dkistEndpoint
      then Scratch.symLink (dkistDir </> Path src) (Path dest)
      else Scratch.copyFile (Path src) (Path dest)


globusDevAuth :: (Hyperbole :> es) => Eff es Response
globusDevAuth = do
  red <- url <$> reqParam @Text "redirect"
  redirect $ red{query = [("code", Just "dev_auth")]}


data DKIST
