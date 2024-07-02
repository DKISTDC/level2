module NSO.Fits.Generate.Error where

import Control.Exception
import Data.Massiv.Array
import NSO.Fits.Generate.Headers.LiftL1
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.InstrumentProgram
import NSO.Types.Inversion
import Network.Globus qualified as Globus


-- Error -------------------------------------------------------------
data GenerateError
  = InvalidFrameShape (Sz Ix3)
  | InvalidFits String
  | FrameOutOfBounds (Sz Ix4) Int
  | MissingProfileExtensions String
  | InvalidWavelengthGroups
  | NoCanonicalDataset (Id InstrumentProgram)
  | L1TransferFailed (Id Globus.Task)
  | MissingInversion (Id Inversion)
  | InvalidTimestamp Text
  | ZeroValidTimestamps FilePath
  | LiftL1 LiftL1Error
  | MismatchedFrames [Int]
  | GenIOError IOError
  deriving (Show, Eq, Exception)
