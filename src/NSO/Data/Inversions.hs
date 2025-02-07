module NSO.Data.Inversions
  ( module NSO.Data.Inversions.Effect
  , module NSO.Data.Inversions.Update
  , module NSO.Data.Inversions.Commit
  , module NSO.Types.Inversion
  , isComplete
  , isError
  , isGenerated
  , isInverted
  , findByProgram
  , inversionStep
  , InversionStep (..)
  ) where

import Data.List qualified as L
import Data.Maybe (isJust)
import Data.Ord (Down (..))
import Effectful
import Effectful.Dispatch.Dynamic
import NSO.Data.Inversions.Commit
import NSO.Data.Inversions.Effect hiding (inversions)
import NSO.Data.Inversions.Update
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.InstrumentProgram (InstrumentProgram)
import NSO.Types.Inversion


isComplete :: Inversion -> Bool
isComplete inv = isJust inv.published


isError :: Inversion -> Bool
isError inv = isJust inv.invError


isGenerated :: Inversion -> Bool
isGenerated inv =
  let g = inv.generate
   in isJust g.fits
        && isJust g.asdf
        && isJust g.transfer


isInverted :: Inversion -> Bool
isInverted inv =
  let i = inv.invert
   in not (L.null i.datasets)
        && isJust i.commit
        && isJust i.profileFit
        && isJust i.profileOrig
        && isJust i.quantities


data InversionStep
  = StepInvert
  | StepGenerate
  | StepPublish
  | StepComplete


inversionStep :: Inversion -> InversionStep
inversionStep inv
  | isComplete inv = StepComplete
  | isGenerated inv = StepPublish
  | isInverted inv = StepGenerate
  | otherwise = StepInvert


findByProgram :: (Inversions :> es) => Id InstrumentProgram -> Eff es [Inversion]
findByProgram ip = do
  invs <- send $ ByProgram ip
  pure $ sortOn latest invs
 where
  latest = Down . (.updated)
