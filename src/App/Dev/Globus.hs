module App.Dev.Globus where

import App.Effect.Scratch as Scratch
import App.Effect.Transfer
import Data.List.NonEmpty
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Globus
import Effectful.Log
import NSO.Prelude
import NSO.Types.Common (Path' (..), PathType (..), (</>))
import Network.Globus.Auth
import Network.Globus.Types qualified as Globus
import Network.URI.Static (uri)
import Web.Hyperbole
import Web.View.Types.Url qualified as Url


runGlobusDev
  :: (Scratch :> es, Log :> es)
  => Path' Dir DKIST
  -> Eff (Globus : es) a
  -> Eff es a
runGlobusDev dkistDir = interpret $ \_ -> \case
  AuthUrl red _ _ -> do
    pure $ Tagged [uri|https://localhost/dev?|] & Globus.param "redirect" (renderUri red)
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
  GetSubmissionId _ -> do
    pure "submission"
  Transfer _ tr -> do
    forM_ tr.data_ $ \ti -> do
      -- the source may be absolute: /Users/seanhess/Data/pid_1_118/etc....
      -- or it may be local, but from the main store
      -- liftIO $ putStrLn $ "  :> " ++ show (localSourcePath tr.source_endpoint src)
      -- Scratch.copyFile (localSourcePath tr.source_endpoint src) (Path ti.destination_path)
      localCopy tr.source_endpoint (Path ti.source_path) (Path ti.destination_path)

    pure $
      TransferResponse
        { task_id = "globus-dev-task"
        , submission_id = Tagged "submission"
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
  localCopy :: (Scratch :> es, Log :> es) => Id Collection -> Path' any src -> Path' any dest -> Eff es ()
  localCopy cl src dest = do
    log Debug $ "(Globus Dev) localCopy: " <> "\n\tsrc= " <> src.filePath <> "\n\tdst= " <> dest.filePath
    if cl == dkistEndpoint
      then symLinkDir (dkistDir </> src) dest
      else do
        isDir <- send $ DirExists (Path src.filePath)
        if isDir
          then symLinkDir src dest
          else localCopyFile src dest

  symLinkDir (Path src) (Path dest) = do
    log Debug $ "(Globus Dev) symLinkDir " <> src <> " - " <> dest
    Scratch.symLink (Path src) (Path dest)

  localCopyFile (Path src) (Path dest) = do
    log Debug $ "(Globus Dev) localCopyFile " <> src <> " - " <> dest
    Scratch.copyFile (Path src) (Path dest)


globusDevAuth :: (Hyperbole :> es) => Eff es Response
globusDevAuth = do
  red <- url <$> param @Text "redirect"
  redirect $ red{Url.query = [("code", Just "dev_auth")]}


data DKIST
