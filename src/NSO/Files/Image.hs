module NSO.Files.Image where

import Data.Text qualified as T
import NSO.Files.Scratch (Scratch)
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.Dataset (Dataset, Dataset' (..))
import NSO.Types.InstrumentProgram (Proposal)
import NSO.Types.Inversion (InvProfileFit, InvProfileOrig, InvQuantities, Inversion)
import System.FilePath (takeExtension)


-- the file paths here are independent of where they might be mounted

-- UPLOADS --------------------------------------------------------------------------

fileQuantities :: Path s Filename InvQuantities
fileQuantities = Path "inv_res_mod.fits"


fileProfileFit :: Path s Filename InvProfileFit
fileProfileFit = Path "inv_res_pre.fits"


fileProfileOrig :: Path s Filename InvProfileOrig
fileProfileOrig = Path "per_ori.fits"


-- Inversion INPUTS --------------------------------------------------------------------------------------------

data Input


dataset :: Dataset -> Path Scratch f Dataset
dataset d =
  "input" </> Path (cs d.primaryProposalId.fromId) </> Path (cs d.datasetId.fromId)


blancaInput :: Id Proposal -> Id Inversion -> Path Scratch Dir Inversion
blancaInput ip ii =
  "input" </> Path (cs ip.fromId) </> Path (cs ii.fromId)


blancaFile :: Path Scratch Dir Inversion -> Path s Filename a -> Path Scratch File Inversion
blancaFile folder (Path filename) = folder </> Path filename


isAsdf :: Path Scratch Filename Dataset -> Bool
isAsdf p = do
  takeExtension p.filePath == ".asdf"


-- GENRATED OUTPUTS -------------------------------------------------------------------------------------------

outputL2Dir :: Id Proposal -> Id Inversion -> Path Scratch Dir Inversion
outputL2Dir ip ii =
  "generated" </> Path (cs ip.fromId) </> Path (cs ii.fromId)


data L2Asdf


outputL2AsdfPath :: Id Proposal -> Id Inversion -> Path Scratch File L2Asdf
outputL2AsdfPath ip ii =
  filePath (outputL2Dir ip ii) $ filenameL2Asdf ip ii


filenameL2Asdf :: Id Proposal -> Id Inversion -> Path Scratch Filename L2Asdf
filenameL2Asdf _ ii =
  Path $ cs (T.toUpper $ T.map toUnderscore ii.fromId) <> "_L2_metadata.asdf"
 where
  toUnderscore :: Char -> Char
  toUnderscore '.' = '_'
  toUnderscore c = c
