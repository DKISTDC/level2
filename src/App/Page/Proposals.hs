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
import App.View.Layout
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
import NSO.Types.InstrumentProgram
import Web.Hyperbole as H


page
  :: (Log :> es, Hyperbole :> es, Datasets :> es, Inversions :> es, Time :> es, Auth :> es, Debug :> es)
  => Eff es (Page '[AllProposals, ProposalCard, ProgramRow])
page = do
  fs <- query
  props <- Programs.loadAllProposals

  appLayout Proposals $ do
    hyper AllProposals $ do
      viewProposals fs props


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
-- Proposals
-----------------------------------------------------

data AllProposals = AllProposals
  deriving (Generic, ViewId)


instance (Datasets :> es, Inversions :> es) => HyperView AllProposals es where
  data Action AllProposals
    = FilterInstrument Instrument Bool
    | FilterStatus InversionFilter
    | FilterProposal Text
    deriving (Generic, ViewAction)


  type Require AllProposals = '[ProposalCard]


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
      props <- Programs.loadAllProposals
      pure $ viewProposals fs props


viewProposals :: Filters -> [Proposal] -> View AllProposals ()
viewProposals fs props = do
  let sorted = sortOn (\p -> Down p.proposalId) $ filter (applyFilters fs.searchTerm) props
  el (pad 15 . gap 20 . big flexRow . small flexCol . grow) $ do
    row (big aside . gap 10) $ do
      viewFilters fs

    col (gap 40 . grow . minWidth 0) $ do
      forM_ sorted $ \prop ->
        hyper (ProposalCard prop.proposalId) $ viewProposalLoad fs prop
 where
  aside = width 250 . flexCol
  big = media (MinWidth 1000)
  small = media (MaxWidth 1000)

  cleanPropId = T.replace " " "_"

  applyFilters term prop =
    cleanPropId term `T.isInfixOf` prop.proposalId.fromId


viewFilters :: Filters -> View AllProposals ()
viewFilters fs = do
  col (gap 10) $ do
    el bold "Proposal Id"
    stack id $ do
      layer id $ search FilterProposal 500 (att "placehodler" "1 118" . border 1 . pad 10 . grow)

  col (gap 10) $ do
    el bold "Instrument"
    row (gap 5) $ do
      View.toggleBtn (FilterInstrument VISP) fs.visp.value id "VISP"
      View.toggleBtn (FilterInstrument VBI) fs.vbi id "VBI"
      View.toggleBtn (FilterInstrument CRYO_NIRSP) fs.cryo id "Cryo-NIRSP"

  col (gap 10) $ do
    el bold "Status"
    dropdown FilterStatus (== fs.status) (pad 5) $ do
      option Any "Any"
      option Qualified "Qualified"
      option Active "Active"
      option Complete "Complete"


-- clearButton =
--   layer (popup (R 0)) $ do
--     el (pad (XY 5 10) . shownIfTerm fs.propSearch) $ do
--       button (FilterProposal "") (width 24 . hover (color (light Secondary))) Icons.xCircle
--
-- shownIfTerm "" = hide
-- shownIfTerm _ = display Block

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
  proposalCard prop $ el (onLoad (ProposalDetails filts) 100) ""


viewProposalDetails :: Filters -> UTCTime -> Proposal -> [ProgramFamily] -> View ProposalCard ()
viewProposalDetails fs now prop progs = do
  let shown = filter applyFilters progs
  proposalCard prop $ do
    tableInstrumentPrograms now $ fmap (\prog -> prog.program.programId) shown

    -- how many total programs?
    let ignored = length progs - length shown
    when (ignored > 0) $ do
      appRoute (Route.Proposal prop.proposalId PropRoot) (fontSize 14 . color Black . gap 5) $ do
        text $ cs (show ignored)
        text " Hidden Instrument Programs"
 where
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


tableInstrumentPrograms :: UTCTime -> [Id InstrumentProgram] -> View ProposalCard ()
tableInstrumentPrograms _ progIds = do
  col id $ do
    dataRows progIds $ \progId -> do
      hyper (ProgramRow progId) $ rowInstrumentProgramLoad progId


proposalCard :: Proposal -> View c () -> View c ()
proposalCard prop content = do
  col (Style.card . gap 15 . pad 15) $ do
    row id $ do
      el bold $ do
        text "Proposal "
        appRoute (Route.Proposal prop.proposalId PropRoot) Style.link $ do
          text prop.proposalId.fromId
      space
      el_ $ do
        text $ showDate prop.startTime

    View.hr (color Gray)

    col (gap 10) $ do
      el truncate $ text $ T.take 200 prop.description
      content


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
  el (onLoad ProgramDetails 100) $ text "..."


rowInstrumentProgram :: UTCTime -> ProgramFamily -> View ProgramRow ()
rowInstrumentProgram now psm = do
  let p = psm.program
  appRoute (Route.Proposal p.proposalId $ Program p.programId Prog) id $ do
    viewProgramRow now psm
