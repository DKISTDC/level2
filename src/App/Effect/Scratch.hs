module App.Effect.Scratch where

import Data.ByteString (ByteString)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.FileSystem (FileSystem)
import Effectful.FileSystem qualified as FS
import Effectful.FileSystem.IO.ByteString qualified as FS
import Effectful.Log
import NSO.Image.Asdf (L2Asdf, filenameL2Asdf)
import NSO.Image.Frame (L2Frame, filenameL2Frame)
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.Dataset
import NSO.Types.InstrumentProgram
import NSO.Types.Inversion
import Network.Globus (Id' (Collection))
import Network.Globus qualified as Globus
import System.FilePath (takeDirectory)


data Config = Config
  { collection :: Globus.Id Collection
  , mount :: Path' Dir Scratch
  }


data Scratch :: Effect where
  ListDirectory :: Path' Dir a -> Scratch m [Path' Filename a]
  ReadFile :: Path a -> Scratch es ByteString
  WriteFile :: Path a -> ByteString -> Scratch es ()
  CopyFile :: Path a -> Path a -> Scratch es ()
  PathExists :: Path a -> Scratch es Bool
  DirExists :: Path a -> Scratch es Bool
  CreateDirectoryLink :: Path' Dir a -> Path' Dir b -> Scratch es ()
  Globus :: Scratch es (Id Collection)
type instance DispatchOf Scratch = 'Dynamic


runScratch
  :: (FileSystem :> es, Log :> es)
  => Config
  -> Eff (Scratch : es) a
  -> Eff es a
runScratch cfg = interpret $ \_ -> \case
  ListDirectory dir -> do
    fs <- FS.listDirectory $ mounted dir
    pure $ fmap Path fs
  ReadFile f ->
    FS.readFile $ mounted f
  WriteFile f cnt -> do
    FS.createDirectoryIfMissing True $ takeDirectory (mounted f)
    FS.writeFile (mounted f) cnt
  CopyFile src dest -> do
    FS.createDirectoryIfMissing True $ takeDirectory (mounted dest)
    FS.copyFile (mounted src) (mounted dest)
  CreateDirectoryLink src dest -> do
    FS.createDirectoryIfMissing True $ takeDirectory (mounted dest)
    exists <- FS.doesDirectoryExist (mounted dest)
    when exists $ do
      FS.removeDirectoryLink (mounted dest)
    FS.createDirectoryLink (mounted src) (mounted dest)
  PathExists src -> do
    FS.doesPathExist (mounted src)
  DirExists src -> do
    FS.doesDirectoryExist (mounted src)
  Globus -> pure $ Id cfg.collection.unTagged
 where
  mounted :: Path' x a -> FilePath
  mounted p = (cfg.mount </> p).filePath


readFile :: (Scratch :> es) => Path a -> Eff es ByteString
readFile = send . ReadFile


writeFile :: (Scratch :> es) => Path a -> ByteString -> Eff es ()
writeFile f cnt = send $ WriteFile f cnt


copyFile :: (Scratch :> es) => Path a -> Path a -> Eff es ()
copyFile s d = send $ CopyFile s d


listDirectory :: (Scratch :> es) => Path' Dir a -> Eff es [Path' Filename a]
listDirectory = send . ListDirectory


pathExists :: (Scratch :> es) => Path a -> Eff es Bool
pathExists s = send $ PathExists s


symLink :: (Scratch :> es) => Path' Dir a -> Path' Dir a -> Eff es ()
symLink s d = send $ CreateDirectoryLink s d


baseDir :: Path' Dir Scratch
baseDir = Path "level2"


input :: Path' Dir Input
input = baseDir </> "input"


generated :: Path' Dir Generate
generated = baseDir </> "generated"


dataset :: Dataset -> Path' Dir Dataset
dataset d =
  input </> Path (cs d.primaryProposalId.fromId) </> Path (cs d.instrumentProgramId.fromId) </> Path (cs d.datasetId.fromId)


blanca :: Id Proposal -> Id Inversion -> Path' Dir BLANCA
blanca ip ii =
  input </> Path (cs ip.fromId) </> Path (cs ii.fromId)


outputL2Dir :: Id Proposal -> Id Inversion -> Path' Dir L2Frame
outputL2Dir ip ii =
  generated </> Path (cs ip.fromId) </> Path (cs ii.fromId)


outputL2Frame :: Id Proposal -> Id Inversion -> UTCTime -> Path L2Frame
outputL2Frame ip ii dt =
  filePath (outputL2Dir ip ii) $ filenameL2Frame ii dt


outputL2Asdf :: Id Proposal -> Id Inversion -> Path L2Asdf
outputL2Asdf ip ii =
  filePath (outputL2Dir ip ii) $ filenameL2Asdf ip ii


collection :: (Scratch :> es) => Eff es (Globus.Id Collection)
collection = do
  Id c <- send Globus
  pure $ Tagged c


data Input


-- data Timestamps
data BLANCA


data UploadFiles t = UploadFiles
  { profileFit :: Path' t InvProfileFit
  , profileOrig :: Path' t InvProfileOrig
  , quantities :: Path' t InvQuantities
  -- , timestamps :: Path' t Timestamps
  }
  deriving (Generic, Show)


inversionUploads :: Path' Dir BLANCA -> UploadFiles File
inversionUploads bdir =
  let quantities = bdir </> fileQuantities
      profileFit = bdir </> fileProfileFit
      profileOrig = bdir </> fileProfileOrig
   in UploadFiles{quantities, profileFit, profileOrig}


-- timestamps = bdir </> fileTimestamps

fileQuantities :: Path' Filename InvQuantities
fileQuantities = Path "inv_res_mod.fits"


fileProfileFit :: Path' Filename InvProfileFit
fileProfileFit = Path "inv_res_pre.fits"


fileProfileOrig :: Path' Filename InvProfileOrig
fileProfileOrig = Path "per_ori.fits"

-- fileTimestamps :: Path' Filename Timestamps
-- fileTimestamps = Path "timestamps.tsv"
