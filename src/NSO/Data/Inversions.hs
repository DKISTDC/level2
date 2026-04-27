module NSO.Data.Inversions
  ( module NSO.Data.Inversions.Effect
  , module NSO.Data.Inversions.Update
  , module NSO.Data.Inversions.Commit
  , module NSO.Types.Inversion
  , isPublished
  , isError
  , isGenerated
  , isInverted
  , inversionStatus
  , distinctProgramIds
  , loadDistinctProposalIds
  , InvStatus (..)
  , Generated (..)
  , generated
  ) where

import Data.List qualified as L
import Data.Maybe (isJust)
import Effectful
import Effectful.Dispatch.Dynamic
import NSO.Data.Inversions.Commit
import NSO.Data.Inversions.Effect hiding (inversions)
import NSO.Data.Inversions.Update
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.InstrumentProgram (InstrumentProgram)
import NSO.Types.Inversion
import NSO.Types.Proposal
import NSO.Types.Status (InvStatus (..))


isPublished :: Inversion -> Bool
isPublished inv = isJust inv.published


isError :: Inversion -> Bool
isError inv =
  -- all inversions that exist in the database should be uploaded
  isJust inv.invError


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


inversionStatus :: Inversion -> InvStatus
inversionStatus inv
  | inv.deleted = InvDeleted
  | isError inv = InvError
  | isPublished inv = StepComplete
  | isGenerated inv = StepPublish
  | isInverted inv = StepGenerate
  | otherwise = StepInvert


-- findByProgram :: (Inversions :> es) => Id InstrumentProgram -> Eff es [Inversion]
-- findByProgram ip = do
--   invs <- send $ ByProgram ip
--   pure $ sortOn latest $ filter (\i -> not i.deleted) invs
--  where
--   latest = Down . (.updated)

distinctProgramIds :: [Inversion] -> [Id InstrumentProgram]
distinctProgramIds inv =
  L.nub $ fmap (.programId) inv


loadDistinctProposalIds :: (Inversions :> es) => Eff es [Id Proposal]
loadDistinctProposalIds = do
  -- TODO: use a distinct query instead
  AllInversions ivs <- send All
  pure $ L.nub $ fmap (.proposalId) ivs


data Generated = Generated
  { genFits :: UTCTime
  , genAsdf :: UTCTime
  , genTransfer :: UTCTime
  }


generated :: Inversion -> Maybe Generated
generated inv = do
  genFits <- inv.generate.fits
  genAsdf <- inv.generate.asdf
  genTransfer <- inv.generate.transfer
  pure Generated{genFits, genAsdf, genTransfer}
