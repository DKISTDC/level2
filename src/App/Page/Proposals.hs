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
import Effectful.Dispatch.Dynamic
import Effectful.Log
import Effectful.Time
import NSO.Data.Datasets as Datasets
import NSO.Data.Inversions as Inversions
import NSO.Data.Programs as Programs
import NSO.Prelude
import NSO.Types.InstrumentProgram
import Text.Read (readMaybe)
import Web.HttpApiData
import Web.Hyperbole as H


page
  :: (Log :> es, Hyperbole :> es, Datasets :> es, Inversions :> es, Time :> es, Auth :> es)
  => Eff es (Page '[AllProposals, ProposalCard, ProgramRow])
page = do
  fs <- query
  props <- Programs.loadAllProposals

  appLayout Proposals $ do
    hyper AllProposals $ do
      viewProposals fs props


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


instance (Datasets :> es, Inversions :> es) => HyperView AllProposals es where
  data Action AllProposals = Filter Filters
    deriving (Show, Read, ViewAction)


  type Require AllProposals = '[ProposalCard]


  update (Filter fs) = do
    setQuery fs
    props <- Programs.loadAllProposals
    pure $ viewProposals fs props


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


viewProposals :: Filters -> [Proposal] -> View AllProposals ()
viewProposals fs props = do
  let sorted = sortOn (\p -> Down p.proposalId) props
  el (pad 15 . gap 20 . big flexRow . small flexCol . grow) $ do
    row (big aside . gap 5) $ do
      viewFilters fs

    col (gap 40 . grow . minWidth 0) $ do
      forM_ sorted $ \prop ->
        hyper (ProposalCard prop.proposalId) $ viewProposalLoad fs prop
 where
  aside = width 250 . flexCol
  big = media (MinWidth 1000)
  small = media (MaxWidth 1000)


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


-----------------------------------------------------
-- ProposalCard
-----------------------------------------------------

data ProposalCard = ProposalCard (Id Proposal)
  deriving (Show, Read, ViewId)


instance (Datasets :> es, Inversions :> es, Time :> es, Log :> es) => HyperView ProposalCard es where
  data Action ProposalCard = ProposalDetails Filters
    deriving (Show, Read, ViewAction)


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
      route (Route.Proposal prop.proposalId PropRoot) (fontSize 14 . color Black . gap 5) $ do
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
  checkInvertible Active (StatusInversion (StepPublish (StepPublished _))) = False
  checkInvertible Active (StatusInversion _) = True
  checkInvertible Active (StatusError _) = True
  checkInvertible Complete (StatusInversion (StepPublish (StepPublished _))) = True
  checkInvertible _ _ = False


tableInstrumentPrograms :: UTCTime -> [Id InstrumentProgram] -> View ProposalCard ()
tableInstrumentPrograms _ progIds = do
  -- TODO: sort instrument program statuses
  col id $ do
    dataRows progIds $ \progId -> do
      hyper (ProgramRow progId) $ rowInstrumentProgramLoad progId


proposalCard :: Proposal -> View c () -> View c ()
proposalCard prop content = do
  col (Style.card . gap 15 . pad 15) $ do
    row id $ do
      el bold $ do
        text "Proposal "
        route (Route.Proposal prop.proposalId PropRoot) Style.link $ do
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
  deriving (Show, Read, ViewId)


instance (Datasets :> es, Inversions :> es, Time :> es) => HyperView ProgramRow es where
  data Action ProgramRow = ProgramDetails
    deriving (Show, Read, ViewAction)


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
  route (Route.Proposal p.proposalId $ Program p.programId) id $ do
    viewProgramRow now psm
