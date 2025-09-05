module App.Worker.Generate.Error where

import Control.Exception (Exception, SomeException)
import Effectful
import Effectful.Error.Static
import Effectful.Log
import NSO.Data.Datasets as Datasets
import NSO.Data.Inversions as Inversions
import NSO.Image.Blanca (BlancaError (..))
import NSO.Image.Fits as Fits
import NSO.Image.Fits.Quantity (QuantityError)
import NSO.Image.Primary (PrimaryError)
import NSO.Prelude
import NSO.Types.Common
import Network.Globus (GlobusError)
import Network.Globus qualified as Globus
import Telescope.Asdf.Error (AsdfError)
import Telescope.Data.Parser (ParseError)


onCaughtError :: (Log :> es) => IOError -> Eff (Error GenerateError : es) a
onCaughtError e = do
  log Err "Catch IO Error"
  throwError $ GenIOError e


onCaughtGlobus :: (Log :> es) => GlobusError -> Eff (Error GenerateError : es) a
onCaughtGlobus e = do
  log Err "Catch GLOBUS"
  throwError $ GlobusError e


generateFailed :: (Log :> es, Inversions :> es) => Id Inversion -> GenerateError -> Eff es ()
generateFailed iid err = do
  log Err $ dump "GenerateError" err
  Inversions.setError iid (cs $ show err)


----------------------------------------------------------------
-- Generate Error
-----------------------------------------------------------------

data FrameSizes = FrameSizes {quantities :: Int, fit :: Int, original :: Int, l1 :: Int}
  deriving (Show, Eq)


data FetchError
  = NoCanonicalDataset [Id Dataset]
  | NoDatasets [Id Dataset]
  | MissingFrames (Id Dataset)
  | MissingL1HDU FilePath
  | L1AsdfParse AsdfError
  | MissingL1Asdf FilePath
  | FetchParse FilePath ParseError
  deriving (Show, Exception, Eq)


data GenerateError
  = L1TransferFailed (Id Globus.Task)
  | L1FetchError FetchError
  | GlobusError GlobusError
  | MissingInversion (Id Inversion)
  | ProfileError ProfileError
  | QuantityError QuantityError
  | PrimaryError PrimaryError
  | ParseError FilePath ParseError
  | AsdfError AsdfError
  | BlancaError BlancaError
  | MismatchedFrames FrameSizes
  | NoFrames FrameSizes
  | GenIOError IOError
  | MissingL2Fits
  | InvalidSliceKeys ParseError
  deriving (Show, Exception)


type GenerateErrors es = (Error ProfileError : Error QuantityError : Error FetchError : Error PrimaryError : Error AsdfError : Error BlancaError : es)


runGenerateError
  :: (Error GenerateError :> es)
  => Eff (GenerateErrors es) a
  -> Eff es a
runGenerateError =
  runErrorNoCallStackWith @BlancaError (throwError . BlancaError)
    . runErrorNoCallStackWith @AsdfError (throwError . AsdfError)
    . runErrorNoCallStackWith @PrimaryError (throwError . PrimaryError)
    . runErrorNoCallStackWith @FetchError (throwError . L1FetchError)
    . runErrorNoCallStackWith @QuantityError (throwError . QuantityError)
    . runErrorNoCallStackWith @ProfileError (throwError . ProfileError)
