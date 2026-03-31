module NSO.Files.Scratch where

import Data.ByteString (ByteString)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Exception (Exception)
import Effectful.FileSystem (FileSystem)
import Effectful.FileSystem qualified as FS
import Effectful.FileSystem.IO.ByteString qualified as FS
import Effectful.Log
import NSO.Files.RemoteFolder
import NSO.Prelude
import NSO.Types.Common
import System.Directory (removePathForcibly)
import System.FilePath (takeDirectory)
import System.Posix.Files (createSymbolicLink)


data Ingest
data Output


data Config s = Config
  { mount :: Path s Dir ()
  , remote :: Remote s
  }


data Scratches = Scratches
  { ingest :: Config Ingest
  , output :: Config Output
  }


data Mounted a


data Scratch s :: Effect where
  ListDirectory :: Path s Dir a -> Scratch s m [Path s Filename a]
  ReadFile :: Path s File a -> Scratch s es ByteString
  WriteFile :: Path s File a -> ByteString -> Scratch s es ()
  CopyFile :: Path s File a -> Path s File a -> Scratch s es ()
  PathExists :: Path s File a -> Scratch s es Bool
  DirExists :: Path s File a -> Scratch s es Bool
  CreateSymbolicLink :: Path s f a -> Path s f b -> Scratch s es ()
  RemoveDir :: Path s Dir a -> Scratch s es ()
  GetConfig :: Scratch s es (Config s)
  MountedPath :: Path s x a -> Scratch s es (Path s (Mounted x) a)
type instance DispatchOf (Scratch s) = 'Dynamic


runScratch
  :: forall s a es
   . (FileSystem :> es, Log :> es, IOE :> es)
  => Config s
  -> Eff (Scratch s : es) a
  -> Eff es a
runScratch cfg = interpret $ \_ -> \case
  ListDirectory dir -> do
    fs <- FS.listDirectory $ absolutePath dir
    pure $ fmap Path fs
  ReadFile f ->
    FS.readFile $ absolutePath f
  WriteFile f cnt -> do
    FS.createDirectoryIfMissing True $ takeDirectory (absolutePath f)
    FS.writeFile (absolutePath f) cnt
  CopyFile src dest -> do
    FS.createDirectoryIfMissing True $ takeDirectory (absolutePath dest)
    FS.copyFile (absolutePath src) (absolutePath dest)
  CreateSymbolicLink src dest -> do
    FS.createDirectoryIfMissing True $ takeDirectory (absolutePath dest)
    exists <- FS.doesPathExist (absolutePath dest)
    liftIO $ do
      when exists $ do
        liftIO $ removePathForcibly (absolutePath dest)
      createSymbolicLink (absolutePath src) (absolutePath dest)
  PathExists src -> do
    FS.doesPathExist (absolutePath src)
  DirExists src -> do
    FS.doesDirectoryExist (absolutePath src)
  RemoveDir dir -> do
    exists <- FS.doesDirectoryExist (absolutePath dir)
    when exists $ do
      FS.removeDirectoryRecursive (absolutePath dir)
  GetConfig -> pure cfg
  MountedPath p -> do
    log Debug $ "MountedPath " <> show p <> " " <> show cfg.mount <> " "
    pure $ mounted p
 where
  mounted :: Path s x b -> Path s (Mounted x) b
  mounted p =
    cfg.mount </> p

  absolutePath :: Path s x b -> FilePath
  absolutePath p = (mounted p).filePath


readFile :: (Scratch s :> es) => Path s File a -> Eff es ByteString
readFile = send . ReadFile


writeFile :: (Scratch s :> es) => Path s File a -> ByteString -> Eff es ()
writeFile f cnt = send $ WriteFile f cnt


copyFile :: (Scratch s :> es) => Path s File a -> Path s File a -> Eff es ()
copyFile s d = send $ CopyFile s d


listDirectory :: (Scratch s :> es) => Path s Dir a -> Eff es [Path s Filename a]
listDirectory = send . ListDirectory


pathExists :: (Scratch s :> es) => Path s File a -> Eff es Bool
pathExists s = send $ PathExists s


symLink :: (Scratch s :> es, Log :> es) => Path s f a -> Path s f a -> Eff es ()
symLink src dest = do
  send $ CreateSymbolicLink src dest


mountedPath :: (Scratch s :> es) => Path s x a -> Eff es (Path s (Mounted x) a)
mountedPath = send . MountedPath


-- Remote Folder ----------------------------------------

remote :: (Scratch s :> es) => Eff es (Remote s)
remote = do
  cfg <- send GetConfig
  pure cfg.remote


data ScratchError
  = ScratchPathMissing FilePath String
  deriving (Show, Eq, Exception)
