module NSO.Fits.Generate.Error where

import Control.Exception
import Data.Massiv.Array
import NSO.Prelude
import App.Globus qualified as Globus
import NSO.Types.Common
import NSO.Types.InstrumentProgram


-- Error -------------------------------------------------------------
data GenerateError
  = InvalidFrameShape (Sz Ix3)
  | InvalidFits String
  | FrameOutOfBounds (Sz Ix4) Int
  | MissingProfileExtensions String
  | InvalidWavelengthGroups
  | NoCanonicalDataset (Id InstrumentProgram)
  | L1TransferFailed (Id Globus.Task)
  deriving (Show, Eq, Exception)
