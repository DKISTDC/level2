module NSO.Image.Files where

import Data.Text qualified as T
import NSO.Data.Scratch qualified as Scratch
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.Dataset (Dataset, Dataset' (..))
import NSO.Types.InstrumentProgram (Proposal)
import NSO.Types.Inversion (Generate, InvProfileFit, InvProfileOrig, InvQuantities, Inversion)
import System.FilePath (takeExtension)


-- UPLOADS --------------------------------------------------------------------------

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

-- BLANCA INPUTS --------------------------------------------------------------------------------------------

data Input
data BLANCA


input :: Path' Dir Input
input = Scratch.baseDir </> "input"


dataset :: Dataset -> Path' Dir Dataset
dataset d =
  input </> Path (cs d.primaryProposalId.fromId) </> Path (cs d.instrumentProgramId.fromId) </> Path (cs d.datasetId.fromId)


isAsdf :: Path' Filename Dataset -> Bool
isAsdf p = do
  takeExtension p.filePath == ".asdf"


blanca :: Id Proposal -> Id Inversion -> Path' Dir BLANCA
blanca ip ii =
  input </> Path (cs ip.fromId) </> Path (cs ii.fromId)


-- GENRATED OUTPUTS -------------------------------------------------------------------------------------------

generated :: Path' Dir Generate
generated = Scratch.baseDir </> "generated"


outputL2Dir :: Id Proposal -> Id Inversion -> Path' Dir Inversion
outputL2Dir ip ii =
  generated </> Path (cs ip.fromId) </> Path (cs ii.fromId)


data L2Asdf


outputL2AsdfPath :: Id Proposal -> Id Inversion -> Path L2Asdf
outputL2AsdfPath ip ii =
  filePath (outputL2Dir ip ii) $ filenameL2Asdf ip ii


filenameL2Asdf :: Id Proposal -> Id Inversion -> Path' Filename L2Asdf
filenameL2Asdf _ ii =
  Path $ cs (T.toUpper $ T.map toUnderscore ii.fromId) <> "_L2_metadata.asdf"
 where
  toUnderscore :: Char -> Char
  toUnderscore '.' = '_'
  toUnderscore c = c
