{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module App.Page.Proposals where

import App.Colors
import App.Effect.Auth
import App.Error (expectFound)
import App.Route as Route
import App.Style qualified as Style
import App.View.Common as View
import App.View.DataRow (dataRows)
import App.View.Icons (skeleton, xCircle)
import App.View.Layout
import App.View.Loading (loadingCard)
import App.View.ProposalDetails (viewProgramRow)
import Data.Ord (Down (..))
import Data.Text qualified as T
import Effectful
import Effectful.Debug
import Effectful.Dispatch.Dynamic
import Effectful.Log
import Effectful.Time
import NSO.Data.Datasets as Datasets
import NSO.Data.Inversions as Inversions
import NSO.Data.Programs as Programs
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.InstrumentProgram
import Web.Atomic.CSS
import Web.Hyperbole as H hiding (content)
import Web.Hyperbole.HyperView.Handled


page
  :: (Log :> es, Hyperbole :> es, Datasets :> es, Inversions :> es, Time :> es, Auth :> es, Debug :> es)
  => Page es '[ProposalFilters, ProposalCard, ProgramRow]
page = do
  fs <- query
  props <- Programs.loadAllProposals

  appLayout Proposals $ do
    script "/proposals.js"
    viewProposals fs props


viewProposals :: (HyperViewHandled ProposalFilters any, HyperViewHandled ProposalCard any) => Filters -> [Proposal] -> View any ()
viewProposals fs props = do
  let sorted = sortOn (\p -> Down p.proposalId) props
  el ~ pad 15 . gap 20 . big flexRow . small flexCol . grow $ do
    el ~ small flexRow . big aside . gap 10 $ do
      hyper ProposalFilters $ viewFilters fs

    col ~ grow . minWidth 0 $ do
      forM_ sorted $ \prop ->
        hyper (ProposalCard prop.proposalId) $ viewProposalLoad fs prop
 where
  aside = width 315 . flexCol
  big = media (MinWidth 1000)
  small = media (MaxWidth 1000)


-----------------------------------------------------
-- Filters: Query
-----------------------------------------------------

newtype ShowVISP = ShowVISP {value :: Bool}
  deriving newtype (ToParam, FromParam, Eq, Read, Show, ToJSON, FromJSON)
instance Default ShowVISP where
  def = ShowVISP True


data Filters = Filters
  { status :: InversionFilter
  , visp :: ShowVISP
  , vbi :: Bool
  , cryo :: Bool
  , searchTerm :: Text
  }
  deriving (Generic, Show, ToQuery, FromQuery, ToJSON, FromJSON)


data InversionFilter
  = Any
  | Qualified
  | Active
  | Complete
  deriving (Show, Eq, ToParam, FromParam, Generic, ToJSON, FromJSON)
instance Default InversionFilter where
  def = Any


-----------------------------------------------------
-- Filters
-----------------------------------------------------

data ProposalFilters = ProposalFilters
  deriving (Generic, ViewId)


instance (Log :> es) => HyperView ProposalFilters es where
  data Action ProposalFilters
    = FilterInstrument Instrument Bool
    | FilterStatus InversionFilter
    | FilterProposal Text
    deriving (Generic, ViewAction)


  update = \case
    FilterInstrument instrument shown -> do
      fs <- query
      filterProposals $ setInstrumentFilter instrument shown fs
    FilterStatus status -> do
      fs <- query
      filterProposals $ setStatus status fs
    FilterProposal term -> do
      fs <- query
      filterProposals $ fs{searchTerm = term}
   where
    setInstrumentFilter instr b fs =
      case instr of
        VBI -> fs{vbi = b}
        VISP -> fs{visp = ShowVISP b}
        CRYO_NIRSP -> fs{cryo = b}

    setStatus status Filters{visp, vbi, cryo, searchTerm} =
      Filters{..}

    filterProposals fs = do
      setQuery fs
      pushEvent "proposal-filters" fs
      -- we don't need to reload all the proposals...
      -- props <- Programs.loadAllProposals

      -- we DO need to re-render the filters though!
      -- but we could make the filters their own hyperview
      pure $ viewFilters fs


viewFilters :: Filters -> View ProposalFilters ()
viewFilters fs = do
  col ~ gap 10 $ do
    el ~ bold $ "Proposal Id"
    el ~ stack $ do
      search FilterProposal 500 @ placeholder "1 118" . value fs.searchTerm ~ border 1 . pad 10 . grow
      clearButton

  col ~ gap 10 $ do
    el ~ bold $ "Instrument"
    row ~ gap 5 $ do
      View.toggleBtn (FilterInstrument VISP) fs.visp.value "VISP"
      View.toggleBtn (FilterInstrument VBI) fs.vbi "VBI"
      View.toggleBtn (FilterInstrument CRYO_NIRSP) fs.cryo "Cryo-NIRSP"

  col ~ gap 10 $ do
    el ~ bold $ "Status"
    dropdown FilterStatus (== fs.status) ~ pad 5 $ do
      option Any "Any"
      option Qualified "Qualified"
      option Active "Active"
      option Complete "Complete"
 where
  clearButton = do
    let shown = if T.null fs.searchTerm then display None else id
    el ~ popup (R 0) . pad 10 . shown $ do
      button (FilterProposal "") ~ width 24 . hover (color (light Secondary)) $ xCircle


-----------------------------------------------------
-- ProposalCard
-----------------------------------------------------

data ProposalCard = ProposalCard (Id Proposal)
  deriving (Generic, ViewId)


instance (Datasets :> es, Inversions :> es, Time :> es, Log :> es) => HyperView ProposalCard es where
  data Action ProposalCard = ProposalDetails Filters
    deriving (Generic, ViewAction)


  type Require ProposalCard = '[ProgramRow]


  update (ProposalDetails filts) = do
    ProposalCard propId <- viewId
    now <- currentTime
    ds <- Datasets.find (Datasets.ByProposal propId)
    invs <- send $ Inversions.ByProposal propId
    prop <- proposalFromDataset . head <$> expectFound ds
    let progs = programFamilies invs ds

    pure $ viewProposalDetails filts now prop progs


viewProposalLoad :: Filters -> Proposal -> View ProposalCard ()
viewProposalLoad filts prop = do
  -- proposalCard prop $ el $ loadingCard
  proposalCard prop $ el @ onLoad (ProposalDetails filts) 150 $ loadingCard


viewProposalDetails :: Filters -> UTCTime -> Proposal -> [ProgramFamily] -> View ProposalCard ()
viewProposalDetails fs now prop progs = do
  if isProposalShown fs.searchTerm prop
    then viewProp
    else none
 where
  viewProp = do
    let shownProgs = filter applyFilters progs
    proposalCard prop $ do
      tableInstrumentPrograms now $ fmap (\prog -> prog.program.programId) shownProgs

      -- how many total programs?
      let ignored = length progs - length shownProgs
      when (ignored > 0) $ do
        appRoute (Route.Proposal prop.proposalId PropRoot) ~ fontSize 14 . color Black . gap 5 $ do
          text $ cs (show ignored)
          text " Hidden Instrument Programs"

  applyFilters :: ProgramFamily -> Bool
  applyFilters ip = checkInstrument ip && checkInvertible fs.status ip.status

  checkInstrument :: ProgramFamily -> Bool
  checkInstrument ip =
    case ip.program.instrument of
      VBI -> fs.vbi
      VISP -> fs.visp.value
      CRYO_NIRSP -> fs.cryo

  checkInvertible :: InversionFilter -> ProgramStatus -> Bool
  checkInvertible Any _ = True
  checkInvertible Qualified StatusQualified = True
  checkInvertible Active (StatusError _) = True
  checkInvertible Active (StatusInversion inv) = not $ isPublished inv
  checkInvertible Complete (StatusInversion inv) = isPublished inv
  checkInvertible _ _ = False

  cleanPropId = T.replace " " "_"

  isProposalShown term p =
    cleanPropId term `T.isInfixOf` p.proposalId.fromId


tableInstrumentPrograms :: UTCTime -> [Id InstrumentProgram] -> View ProposalCard ()
tableInstrumentPrograms _ progIds = do
  col $ do
    dataRows progIds $ \progId -> do
      hyper (ProgramRow progId) $ rowInstrumentProgramLoad progId


proposalCard :: Proposal -> View c () -> View c ()
proposalCard prop content = do
  col ~ Style.card . gap 15 . pad 15 $ do
    row $ do
      el ~ bold $ do
        text "Proposal "
        appRoute (Route.Proposal prop.proposalId PropRoot) ~ Style.link $ do
          text prop.proposalId.fromId
      space
      el $ do
        text $ showDate prop.startTime

    View.hr ~ color Gray

    col ~ gap 10 $ do
      el ~ overflow Hidden $ text $ T.take 200 prop.description
      content
  space ~ height 40


-----------------------------------------------------
-- ProgramRow
-----------------------------------------------------

data ProgramRow = ProgramRow (Id InstrumentProgram)
  deriving (Generic, ViewId)


instance (Datasets :> es, Inversions :> es, Time :> es) => HyperView ProgramRow es where
  data Action ProgramRow = ProgramDetails
    deriving (Generic, ViewAction)


  update ProgramDetails = do
    ProgramRow progId <- viewId
    ps <- Programs.loadProgram progId
    now <- currentTime
    pure $ mapM_ (rowInstrumentProgram now) ps


rowInstrumentProgramLoad :: Id InstrumentProgram -> View ProgramRow ()
rowInstrumentProgramLoad _progId = do
  el @ onLoad ProgramDetails 150 $ skeleton


rowInstrumentProgram :: UTCTime -> ProgramFamily -> View ProgramRow ()
rowInstrumentProgram now psm = do
  let p = psm.program
  skeleton ~ display None . whenLoading (display Block)
  appRoute (Route.Proposal p.proposalId $ Program p.programId Prog) ~ whenLoading (display None) $ do
    viewProgramRow now psm
