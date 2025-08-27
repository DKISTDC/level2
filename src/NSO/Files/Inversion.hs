module NSO.Files.Inversion where

import Data.List qualified as L
import NSO.Files.Image qualified as Files
import NSO.Files.Scratch (Scratch)
import NSO.Prelude
import NSO.Types.Common as App
import NSO.Types.Inversion
import Web.FormUrlEncoded qualified as FUE
import Web.Hyperbole


data InversionFiles f t = InversionFiles
  { quantities :: Field f (Path Scratch t InvQuantities)
  , profileFit :: Field f (Path Scratch t InvProfileFit)
  , profileOrig :: Field f (Path Scratch t InvProfileOrig)
  -- , timestamps :: Path' t Timestamps
  }
  deriving (Generic)
instance Show (InversionFiles Maybe Filename) where
  show (InversionFiles q pf po) = "UploadFiles " <> show q <> " " <> show pf <> " " <> show po
instance FromForm (InversionFiles Maybe Filename) where
  fromForm f = do
    fs <- files f
    let quantities = findFile Files.fileQuantities fs
    let profileFit = findFile Files.fileProfileFit fs
    let profileOrig = findFile Files.fileProfileOrig fs
    let inv = InversionFiles{quantities, profileFit, profileOrig}
    case inv of
      InversionFiles Nothing Nothing Nothing ->
        Left "Must provide at least one file"
      _ -> pure inv
   where
    files :: FUE.Form -> Either Text [Path s Filename ()]
    files frm = do
      f0 <- FUE.parseMaybe "file[0]" frm
      f1 <- FUE.parseMaybe "file[1]" frm
      f2 <- FUE.parseMaybe "file[2]" frm
      f3 <- FUE.parseMaybe "file[3]" frm
      pure $ catMaybes [f0, f1, f2, f3]

    findFile :: Path s Filename a -> [Path s Filename ()] -> Maybe (Path s Filename a)
    findFile file fs = do
      Path ff <- L.find (isFile file) fs
      pure $ Path ff

    isFile :: Path s Filename a -> Path s Filename () -> Bool
    isFile (Path fa) (Path fb) = fa == fb


inversionFiles :: Path Scratch Dir Inversion -> InversionFiles Identity File
inversionFiles bdir =
  let quantities = bdir </> Files.fileQuantities
      profileFit = bdir </> Files.fileProfileFit
      profileOrig = bdir </> Files.fileProfileOrig
   in InversionFiles{quantities, profileFit, profileOrig}
