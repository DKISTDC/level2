{-# LANGUAGE UndecidableInstances #-}

module App.Page.Proposals where

import App.Colors
import App.Effect.Auth
import App.Route as Route
import App.Style qualified as Style
import App.View.Common as View
import App.View.DataRow (dataRows)
import App.View.Layout
import App.View.ProposalDetails (viewProgramRow)
import Data.Grouped as G
import Data.Ord (Down (..))
import Data.Text qualified as T
import Effectful
import Effectful.Log
import Effectful.Time
import NSO.Data.Datasets as Datasets
import NSO.Data.Inversions as Inversions
import NSO.Data.Programs as Programs
import NSO.Prelude
import NSO.Types.InstrumentProgram
import NSO.Types.Status
import Text.Read (readMaybe)
import Web.HttpApiData
import Web.Hyperbole as H


page
  :: (Log :> es, Hyperbole :> es, Datasets :> es, Inversions :> es, Time :> es, Auth :> es)
  => Eff es (Page '[AllProposals, ProgramRow])
page = do
  fs <- query

  props <- Programs.loadAll
  now <- currentTime

  appLayout Proposals $ do
    hyper AllProposals $ do
      viewProposals now fs props


newtype ShowVISP = ShowVISP {value :: Bool}
  deriving newtype (ToParam, FromParam, Eq, Read, Show)
instance DefaultParam ShowVISP where
  defaultParam = ShowVISP True


data Filters = Filters
  { status :: InversionFilter
  , visp :: ShowVISP
  , vbi :: Bool
  , cryo :: Bool
  }
  deriving (Show, Read, Generic, ToQuery, FromQuery)


-----------------------------------------------------
-- Query URL
-----------------------------------------------------

-----------------------------------------------------
-- Proposals --------------------------------------
-----------------------------------------------------

data AllProposals = AllProposals
  deriving (Show, Read, ViewId)


instance (Datasets :> es, Inversions :> es, Time :> es) => HyperView AllProposals es where
  data Action AllProposals = Filter Filters
    deriving (Show, Read, ViewAction)


  type Require AllProposals = '[ProgramRow]


  update (Filter fs) = do
    setQuery fs
    props <- Programs.loadAll
    now <- currentTime
    pure $ viewProposals now fs props


data InversionFilter
  = Any
  | Qualified
  | Active
  | Complete
  deriving (Show, Read, Eq, ToParam, FromParam)
instance DefaultParam InversionFilter where
  defaultParam = Any


instance FromHttpApiData InversionFilter where
  parseQueryParam t =
    case readMaybe (cs t) of
      Nothing -> Left $ "Invalid InversionFilter: " <> t
      Just a -> pure a


viewProposals :: UTCTime -> Filters -> [Proposal] -> View AllProposals ()
viewProposals now fs props = do
  let sorted = sortOn (\p -> Down p.proposal.proposalId) exs
  el (pad 15 . gap 20 . big flexRow . small flexCol . grow) $ do
    row (big aside . gap 5) $ do
      viewFilters fs

    col (gap 40 . grow . minWidth 0) $ do
      forM_ sorted viewProposal
 where
  aside = width 250 . flexCol
  big = media (MinWidth 1000)
  small = media (MaxWidth 1000)

  viewProposal :: ProposalPrograms -> View AllProposals ()
  viewProposal e = do
    let shown = filter applyFilters $ G.toList e.programs
    proposalPrograms e shown

  proposalPrograms :: ProposalPrograms -> [InstrumentProgramStatus] -> View AllProposals ()
  proposalPrograms _ [] = none
  proposalPrograms pp ipss = do
    let p = pp.proposal
    proposalCard pp.proposal $ do
      tableInstrumentPrograms now $ fmap (\ips -> ips.program.programId) ipss

      let ignored = length pp.programs - length ipss
      when (ignored > 0) $ do
        route (Route.Proposal p.proposalId PropRoot) (fontSize 14 . color Black . gap 5) $ do
          text $ cs (show ignored)
          text " Hidden Instrument Programs"

  applyFilters :: InstrumentProgramStatus -> Bool
  applyFilters ip = checkInstrument ip && checkInvertible fs.status ip.status

  checkInstrument :: InstrumentProgramStatus -> Bool
  checkInstrument ip =
    case ip.program.instrument of
      VBI -> fs.vbi
      VISP -> fs.visp.value
      CRYO_NIRSP -> False

  checkInvertible :: InversionFilter -> ProgramStatus -> Bool
  checkInvertible Any _ = True
  checkInvertible Qualified StatusQualified = True
  checkInvertible Active (StatusInversion (StepPublish (StepPublished _))) = False
  checkInvertible Active (StatusInversion _) = True
  checkInvertible Active (StatusError _) = True
  checkInvertible Complete (StatusInversion (StepPublish (StepPublished _))) = True
  checkInvertible _ _ = False


proposalCard :: Proposal -> View c () -> View c ()
proposalCard p content = do
  col (Style.card . gap 15 . pad 15) $ do
    row id $ do
      el bold $ do
        text "Proposal "
        route (Route.Proposal p.proposalId PropRoot) Style.link $ do
          text p.proposalId.fromId
      space
      el_ $ do
        text $ showDate p.startTime

    View.hr (color Gray)

    col (gap 10) $ do
      el truncate $ text $ T.take 100 p.description
      content


viewFilters :: Filters -> View AllProposals ()
viewFilters fs =
  col (gap 10) $ do
    el bold "Instrument"

    row (gap 5) $ do
      toggle (Filter fs{visp = ShowVISP $ not fs.visp.value}) fs.visp.value id "VISP"
      toggle (Filter fs{vbi = not fs.vbi}) fs.vbi id "VBI"
      toggle (Filter fs{cryo = not fs.cryo}) fs.cryo id "Cryo-NIRSP"

    el bold "Status"
    dropdown (\i -> Filter $ setStatus i fs) (== fs.status) (pad 5) $ do
      option Any "Any"
      option Qualified "Qualified"
      option Active "Active"
      option Complete "Complete"
 where
  toggle action sel f =
    button action (f . Style.btn (if sel then on else off))

  setStatus s Filters{visp, vbi, cryo} = Filters{status = s, visp, vbi, cryo}

  on = Primary
  off = Gray


tableInstrumentPrograms :: UTCTime -> [Id InstrumentProgram] -> View AllProposals ()
tableInstrumentPrograms _ ips = do
  let sorted = ips
  col id $ do
    dataRows sorted $ \progId -> do
      hyper (ProgramRow progId) $ rowInstrumentProgramLoad progId


-----------------------------------------------------
-- ProgramRow
-----------------------------------------------------

data ProgramRow = ProgramRow (Id InstrumentProgram)
  deriving (Show, Read, ViewId)


instance (Datasets :> es, Inversions :> es, Time :> es) => HyperView ProgramRow es where
  data Action ProgramRow = Details
    deriving (Show, Read, ViewAction)


  update Details = do
    ProgramRow progId <- viewId
    ps <- Programs.loadProgram progId
    now <- currentTime
    pure $ mapM_ (rowInstrumentProgram now) ps


rowInstrumentProgramLoad :: Id InstrumentProgram -> View ProgramRow ()
rowInstrumentProgramLoad _progId = do
  el (onLoad Details 100) $ text "..."


rowInstrumentProgram :: UTCTime -> InstrumentProgramStatus -> View ProgramRow ()
rowInstrumentProgram now psm = do
  let p = psm.program
  route (Route.Proposal p.proposalId $ Program p.programId) id $ do
    viewProgramRow now psm
