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
import Web.Hyperbole.Data.QueryData (DefaultParam (..))


page
  :: (Log :> es, Hyperbole :> es, Datasets :> es, Inversions :> es, Time :> es, Auth :> es)
  => Eff es (Page '[PView])
page = do
  fs <- query
  log Debug (dump "Filters" fs)

  exs <- Programs.loadAllProposals
  now <- currentTime

  appLayout Proposals $ do
    hyper PView $ do
      viewProposals now fs exs


newtype ShowVISP = ShowVISP {value :: Bool}
  deriving newtype (ToParam, FromParam, Eq, Read, Show)
instance DefaultParam ShowVISP where
  defaultParam = ShowVISP True


data Filters = Filters
  { status :: InversionFilter
  , visp :: ShowVISP
  , vbi :: Bool
  }
  deriving (Show, Read, Generic, ToQuery, FromQuery)


loading :: View c ()
loading = el_ "loading..."


-----------------------------------------------------
-- Query URL
-----------------------------------------------------

-----------------------------------------------------
-- Proposals --------------------------------------
-----------------------------------------------------

data PView = PView
  deriving (Show, Read, ViewId)


instance (Datasets :> es, Inversions :> es, Time :> es) => HyperView PView es where
  data Action PView = Filter Filters
    deriving (Show, Read, ViewAction)


  update (Filter fs) = do
    setQuery fs
    exs <- Programs.loadAllProposals
    now <- currentTime
    pure $ viewProposals now fs exs


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


viewProposals :: UTCTime -> Filters -> [ProposalPrograms] -> View PView ()
viewProposals now fs exs = do
  let sorted = sortOn (\p -> Down p.proposal.proposalId) exs
  el (pad 15 . gap 20 . big flexRow . small flexCol . grow) $ do
    row (big aside . gap 5) $ do
      viewFilters fs

    col (gap 40 . grow) $ do
      forM_ sorted viewProposal
 where
  aside = width 250 . flexCol
  big = media (MinWidth 1000)
  small = media (MaxWidth 1000)

  viewProposal :: ProposalPrograms -> View PView ()
  viewProposal e = do
    let shown = filter applyFilters $ G.toList e.programs
    proposalPrograms e shown

  proposalPrograms :: ProposalPrograms -> [InstrumentProgramStatus] -> View PView ()
  proposalPrograms _ [] = none
  proposalPrograms pp ipss = do
    let p = pp.proposal
    proposalCard pp.proposal $ do
      tableInstrumentPrograms now ipss

      let ignored = length pp.programs - length ipss
      when (ignored > 0) $ do
        route (Route.Proposal p.proposalId PropRoot) (fontSize 14 . color Black) $ do
          text $ cs (show ignored)
          text "Hidden Instrument Programs"

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
      -- row (gap 5) $ do
      --   el bold "Start Time:"
      --   el_ $ text $ showDate ds1.startTime
      el truncate $ text p.description
      content


-- applyFilter :: Filter -> (InstrumentProgram -> Bool) -> (InstrumentProgram -> Bool)
-- applyFilter Invertible f ip = f ip && Qualify.isQualified ip
-- applyFilter (IsInstrument i) f ip = f ip && ip.instrument == i

viewFilters :: Filters -> View PView ()
viewFilters fs = do
  el (item . bold) "Instrument"

  row (gap 5) $ do
    toggle (Filter fs{visp = ShowVISP $ not fs.visp.value}) fs.visp.value id "VISP"
    toggle (Filter fs{vbi = not fs.vbi}) fs.vbi id "VBI"

  -- dropdown (\i -> Filter $ fs{isInstrument = i}) (== fs.isInstrument) $ do
  --   option Nothing id ""
  --   option (Just VISP) id "VISP"
  --   option (Just VBI) id "VBI"

  el (item . bold) "Status"
  dropdown (\i -> Filter $ setStatus i fs) (== fs.status) (item . pad 5) $ do
    option Any "Any"
    option Qualified "Qualified"
    option Active "Active"
    option Complete "Complete"
 where
  toggle action sel f =
    button action (f . item . pad (XY 10 5) . Style.btn (if sel then on else off))

  setStatus s Filters{visp, vbi} = Filters{status = s, visp, vbi}

  item = pad (XY 0 5)

  on = Primary
  off = Gray


tableInstrumentPrograms :: UTCTime -> [InstrumentProgramStatus] -> View PView ()
tableInstrumentPrograms now ips = do
  let sorted = ips
  col id $ do
    dataRows sorted $ \ip -> do
      rowInstrumentProgram now ip


-----------------------------------------------------
-- IPRow
-----------------------------------------------------

-- newtype IPRow = IPRow (Id InstrumentProgram)
--   deriving newtype (Show, Read, Param)
--
-- data IPEvent
--   = Expand
--   | Collapse
--   deriving (Show, Read, Param)
--
-- instance LiveView IPRow IPEvent
--
-- handleIPRow :: (Page :> es, Rel8 :> es) => IPRow -> IPEvent -> Eff es (View IPRow ())
-- handleIPRow (IPRow i) a = do
--   dss <- queryProgram i
--   ps <- loadProvenance i
--   case dss of
--     [] -> pure $ el_ "Mising Datasets!"
--     (d : ds) -> do
--       let ip = instrumentProgram (d :| ds)
--       let psm = programSummary ip ps
--       action psm a
--  where
--   action psm Expand =
--     pure $ viewInstrumentProgram psm.program
--   action psm Collapse =
--     pure $ rowInstrumentProgram Expand psm

rowInstrumentProgram :: UTCTime -> InstrumentProgramStatus -> View c ()
rowInstrumentProgram now psm = do
  let p = psm.program
  -- liveButton onClick id $ do
  route (Route.Proposal p.proposalId $ Program p.programId) id $ do
    viewProgramRow now psm

-- viewInstrumentProgram :: ProgramSummary -> View IPRow ()
-- viewInstrumentProgram psm = do
--   let ds = NE.toList psm.programm.datasets
--   el (transition 500 (Height (viewHeight ds)) . truncate) $ do
--     rowInstrumentProgram Collapse psm
--
--     InstrumentProgramSummary.viewCriteria psm
--
--     DatasetsTable.datasetsTable ds
--  where
--   viewHeight ds =
--     dataRowHeight
--       + ((PxRem (length ds) + 1) * DatasetsTable.rowHeight)
--       + InstrumentProgramSummary.summaryHeight
