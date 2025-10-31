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


data Config = Config
  { mount :: Path Scratch Dir ()
  , remote :: Remote Scratch
  }


data Mounted a


data Scratch :: Effect where
  ListDirectory :: Path Scratch Dir a -> Scratch m [Path Scratch Filename a]
  ReadFile :: Path Scratch File a -> Scratch es ByteString
  WriteFile :: Path Scratch File a -> ByteString -> Scratch es ()
  CopyFile :: Path Scratch File a -> Path Scratch File a -> Scratch es ()
  PathExists :: Path Scratch File a -> Scratch es Bool
  DirExists :: Path Scratch File a -> Scratch es Bool
  CreateSymbolicLink :: Path Scratch f a -> Path Scratch f b -> Scratch es ()
  RemoveDir :: Path Scratch Dir a -> Scratch es ()
  Globus :: Scratch es (Remote Scratch)
  MountedPath :: Path Scratch x a -> Scratch es (Path Scratch (Mounted x) a)
type instance DispatchOf Scratch = 'Dynamic


runScratch
  :: (FileSystem :> es, Log :> es, IOE :> es)
  => Config
  -> Eff (Scratch : es) a
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
  Globus -> pure cfg.remote
  MountedPath p -> do
    log Debug $ "MountedPath " <> show p <> " " <> show cfg.mount <> " "
    pure $ mounted p
 where
  mounted :: Path Scratch x a -> Path Scratch (Mounted x) a
  mounted p =
    cfg.mount </> p

  absolutePath :: Path Scratch x a -> FilePath
  absolutePath p = (mounted p).filePath


readFile :: (Scratch :> es) => Path Scratch File a -> Eff es ByteString
readFile = send . ReadFile


writeFile :: (Scratch :> es) => Path Scratch File a -> ByteString -> Eff es ()
writeFile f cnt = send $ WriteFile f cnt


copyFile :: (Scratch :> es) => Path Scratch File a -> Path Scratch File a -> Eff es ()
copyFile s d = send $ CopyFile s d


listDirectory :: (Scratch :> es) => Path Scratch Dir a -> Eff es [Path Scratch Filename a]
listDirectory = send . ListDirectory


pathExists :: (Scratch :> es) => Path Scratch File a -> Eff es Bool
pathExists s = send $ PathExists s


symLink :: (Scratch :> es, Log :> es) => Path Scratch f a -> Path Scratch f a -> Eff es ()
symLink src dest = do
  log Debug "EH?"
  send $ CreateSymbolicLink src dest


mountedPath :: (Scratch :> es) => Path Scratch x a -> Eff es (Path Scratch (Mounted x) a)
mountedPath = send . MountedPath


-- Remote Folder ----------------------------------------

remote :: (Scratch :> es) => Eff es (Remote Scratch)
remote = send Globus


data ScratchError
  = ScratchPathMissing FilePath String
  deriving (Show, Eq, Exception)
