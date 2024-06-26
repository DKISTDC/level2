module App.Scratch where

import Data.ByteString (ByteString)
import Effectful.FileSystem (FileSystem)
import Effectful.FileSystem qualified as FS
import Effectful.FileSystem.IO.ByteString qualified as FS
import Effectful.Reader.Dynamic
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.Dataset
import NSO.Types.Inversion
import Network.Globus (Id' (Collection))
import Network.Globus qualified as Globus
import System.FilePath (takeDirectory)


data Scratch = Scratch
  { collection :: Globus.Id Collection
  , mount :: Path' Abs Scratch
  }


-- needs to know where the

-- DONE: ABS dir of scratch dir (App.scratch)
-- TODO: relative path from scratch globus

-- Path' Abs Scratch: the location of the scratch folder. Build locations on top of this
-- Path' Globus Scratch: the relative location of the scratch folder, from within the globus dir

-- do we control the layout of the scratch space? We sure do!

baseDir :: Path' Dir Scratch
baseDir = Path "level2"


dataset :: Dataset -> Path' Dir Dataset
dataset d =
  baseDir </> Path (cs d.primaryProposalId.fromId) </> Path (cs d.datasetId.fromId)


inversion :: Id Inversion -> Path' Dir Inversion
inversion ii =
  baseDir </> Path (cs ii.fromId)


outputL2 :: Id Inversion -> Path' Dir L2Frame
outputL2 ii = inversion ii </> Path "output"


data InvProfile
data InvResults
data OrigProfile
data Timestamps
data L2Frame


data UploadFiles t = UploadFiles
  { invProfile :: Path' t InvProfile
  , invResults :: Path' t InvResults
  , origProfile :: Path' t OrigProfile
  , timestamps :: Path' t Timestamps
  }
  deriving (Generic, Show)


mounted :: (Reader Scratch :> es) => Path' x a -> Eff es (Path' Abs a)
mounted p = do
  s <- ask @Scratch
  pure $ s.mount </> p


inversionUploads :: (Reader Scratch :> es) => Path' Dir Inversion -> Eff es (UploadFiles Abs)
inversionUploads inv = do
  invResults <- mounted $ inv </> fileInvResults
  invProfile <- mounted $ inv </> fileInvProfile
  origProfile <- mounted $ inv </> fileOrigProfile
  timestamps <- mounted $ inv </> fileTimestamps
  pure $ UploadFiles{invResults, invProfile, origProfile, timestamps}


fileInvResults :: Path' Filename InvResults
fileInvResults = Path "inv_res_mod.fits"


fileInvProfile :: Path' Filename InvProfile
fileInvProfile = Path "inv_res_pre.fits"


fileOrigProfile :: Path' Filename OrigProfile
fileOrigProfile = Path "per_ori.fits"


fileTimestamps :: Path' Filename Timestamps
fileTimestamps = Path "timestamps.tsv"


listDirectory :: (FileSystem :> es, Reader Scratch :> es) => Path' Dir a -> Eff es [Path' Filename a]
listDirectory dir = do
  d <- mounted dir
  fmap Path <$> FS.listDirectory d.filePath


readFile :: (FileSystem :> es) => Path' Abs a -> Eff es ByteString
readFile (Path f) = do
  FS.readFile f


writeFile :: (FileSystem :> es) => Path' Abs a -> ByteString -> Eff es ()
writeFile (Path f) cnt = do
  FS.createDirectoryIfMissing True $ takeDirectory f
  FS.writeFile f cnt
