module App.View.ProposalDetails
  ( viewExperimentDescription
  , viewProgramRow
  , viewCriteria
  , viewProgramSummary
  ) where

import App.Colors
import App.Route as Route
import App.Style qualified as Style
import App.View.Common as View (hr)
import App.View.DataRow (dataCell, tagCell)
import App.View.DatasetsTable as DatasetsTable
import App.View.Icons as Icons
import App.View.Inversions (inversionStatusLabel)
import Data.Grouped
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as Text
import NSO.Data.Datasets
import NSO.Data.Programs
import NSO.Data.Qualify
import NSO.Prelude
import NSO.Types.Inversion
import Web.Hyperbole


viewExperimentDescription :: Text -> View c ()
viewExperimentDescription t = do
  let ps = Text.splitOn "\\n" t
  col (gap 10) $ do
    mapM_ (el_ . text) ps


viewProgramRow :: UTCTime -> InstrumentProgram -> View c ()
viewProgramRow now ip = row (gap 10 . textAlign Center) $ do
  statusTag ip.status

  -- el dataCell $ text $ showDate ip.startTime
  -- el dataCell $ text $ showDate ip.startTime
  el dataCell $ text $ cs $ show ip.instrument
  -- not worth showing Stokes in the row. They seem to be present for all VISP
  -- el dataCell $ text $ cs $ show ip.stokesParameters

  row (dataCell . gap 5 . fontSize 14) $ do
    maybe none embargoTag ip.embargo
    if ip.onDisk then diskTag else none
    mapM_ lineTag ip.spectralLines
    mapM_ midTag $ sortOn id ip.otherWavelengths
 where
  lineTag :: SpectralLine -> View c ()
  lineTag s = tag "pre" (dataTag . Style.tagOutline (light Secondary)) $ text $ cs $ show s

  diskTag = el (dataTag . Style.tagOutline (light Primary)) "On Disk"

  embargoTag utc =
    if utc > now
      then el (dataTag . Style.tagOutline (dark Warning)) "Embargoed"
      else none

  midTag mid =
    tag "pre" (pad 2 . color (light Secondary)) $ text $ cs (show (round mid :: Integer) <> "nm")

  dataTag :: Mod
  dataTag = pad (XY 6 1)

  statusTag StatusInvalid = el (tagCell . color (light Secondary)) $ text "-"
  statusTag StatusQualified = el (stat Primary) $ text "Qualified"
  statusTag (StatusInversion (StepPublished _)) = el (stat Success) $ text "Complete"
  statusTag (StatusInversion step) = el (stat Info) $ text $ inversionStatusLabel step
  statusTag (StatusError _) = el (stat Danger) $ text "Error"

  stat c = tagCell . Style.tag c


-- statusTag Queued = el (dataCell . bg Warning) $ text "Queued"
-- statusTag Inverted = el (dataCell . bg SecondaryLight) $ text "Complete"

viewCriteria :: InstrumentProgram -> Grouped InstrumentProgram Dataset -> View c ()
viewCriteria ip gd = do
  col (pad 8) $ do
    case ip.instrument of
      VISP -> vispCriteria gd ip.spectralLines
      VBI -> vbiCriteria
 where
  vispCriteria :: Grouped InstrumentProgram Dataset -> [SpectralLine] -> View c ()
  vispCriteria ds sls = do
    el (bold . height criteriaRowHeight) "VISP Criteria"
    criteria "Stokes IQUV" $ qualifyStokes ds
    criteria "On Disk" $ qualifyOnDisk ds
    criteria "Spectra: FeI" $ qualifyLine FeI sls
    criteria "Spectra: CaII 854" $ qualifyLine (CaII CaII_854) sls
    criteria "Health" $ qualifyHealth ds
    criteria "GOS Status" $ qualifyGOS ds
    criteria "AO Lock" $ qualifyAO ds

  vbiCriteria = do
    el bold "VBI Criteria"
    criteria "Not Supported" False

  criteriaRowHeight :: Length
  criteriaRowHeight = 32

  criteria :: Text -> Bool -> View c ()
  criteria msg b =
    row (gap 6 . height criteriaRowHeight . color (if b then Success else Danger)) $ do
      el (pad 4) checkmark
      el (pad 4) (text msg)
   where
    checkmark =
      el (width 24 . height 24) $
        if b
          then Icons.checkCircle
          else Icons.xMark


-- viewProgramSummary :: UTCTime -> WithDatasets -> View c ()
-- viewProgramSummary now wdp = do
--   col (gap 10) $ do
--     col (bg White . gap 10 . pad 10) $ do
--       route (Proposal wdp.program.proposalId $ Program wdp.program.programId) flexRow $ do
--         viewProgramRow now wdp.program
--       row (gap 10) $ do
--         route (Proposal wdp.program.proposalId $ Program wdp.program.programId) Style.link $ do
--           text wdp.program.programId.fromId
--         space
--         forM_ wdp.datasets.items $ \d -> do
--           route (Dataset d.datasetId) Style.link $ do
--             text d.datasetId.fromId
--

viewProgramSummary :: (HyperViewHandled ProgramDatasets c) => UTCTime -> ProgramFamily -> View c ()
viewProgramSummary now pf = do
  let ds = pf.datasets.items
  let p = pf.program
  col Style.card $ do
    route (Proposal p.proposalId $ Program p.programId) (Style.cardHeader Secondary) $ text $ "Instrument Program - " <> pf.program.programId.fromId
    col (gap 15 . pad 15) $ do
      viewProgramDetails p now (NE.filter (.latest) ds)
      hyper (ProgramDatasets pf.program.programId) $ DatasetsTable.datasetsTable ByLatest (NE.toList ds)


viewProgramDetails :: InstrumentProgram -> UTCTime -> [Dataset] -> View c ()
viewProgramDetails _ _ [] = none
viewProgramDetails p now (d : ds) = do
  let gd = Grouped (d :| ds)

  row (textAlign Center) $ do
    route (Proposal p.proposalId $ Program p.programId) id $ viewProgramRow now p

  View.hr (color Gray)

  viewCriteria p gd
