module NSO.Files.RemoteFolder where

import Effectful.Globus hiding (Id)
import Effectful.Globus qualified as Globus
import NSO.Prelude
import NSO.Types.Common as App
import Network.URI qualified as URI


data Source
data Dest


-- | The location files on different systems.
data Remote sys = Remote
  { collection :: Globus.Id Collection
  , directory :: Path sys Dir ()
  }
  deriving (Show)


remotePath :: Remote sys -> Path sys f a -> Path sys f a
remotePath r p =
  r.directory </> p


parseGlobusRemoteURI :: (MonadFail m) => String -> m (Remote sys)
parseGlobusRemoteURI uriStr = do
  uri <- case URI.parseURI uriStr of
    Nothing -> fail err
    Just u -> pure u
  collection <- case URI.uriScheme uri of
    "globus:" -> do
      auth <- maybe (fail err) pure (URI.uriAuthority uri)
      pure $ Tagged (cs $ URI.uriRegName auth)
    _ -> fail err

  let path = URI.uriPath uri
  when (null path) (fail err)
  pure Remote{collection, directory = Path $ cs path}
 where
  err = "Invalid remote folder URI: " <> uriStr
