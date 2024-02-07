module NSO.DataStore.Inversions
  ( Inversions (..)
  , AllInversions (..)
  , module NSO.Types.Inversion
  , Id (..)
  , send
  , empty
  ) where

import Data.Diverse.Many hiding (select)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.GenRandom
import Effectful.Time
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.InstrumentProgram
import NSO.Types.Inversion


-- Put all the operations here?
data Inversions :: Effect where
  All :: Inversions m AllInversions
  ByProgram :: Id InstrumentProgram -> Inversions m [Inversion]
  Create :: Id InstrumentProgram -> Inversions m Inversion


type instance DispatchOf Inversions = 'Dynamic


-- | Provenance of EVERY Instrument Program
newtype AllInversions = AllInversions [Inversion]


empty :: (Time :> es, GenRandom :> es) => Id InstrumentProgram -> Eff es Inversion
empty ip = do
  now <- currentTime
  i <- randomId
  let start = Started now :: Started
  pure $
    Inversion
      { inversionId = i
      , programId = ip
      , step = StepStarted (start ./ nil)
      }
