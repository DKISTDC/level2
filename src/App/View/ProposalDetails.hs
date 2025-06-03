module App.View.ProposalDetails
  ( viewExperimentDescription
  , viewProgramRow
  , viewCriteria
  , viewProgramDetails
  , viewProgramDetails'
  , viewProgramStats
  ) where

import App.Colors
import App.Route as Route
import App.Style qualified as Style
import App.View.Common (showTimestamp)
import App.View.Common as View (hr)
import App.View.DataRow (dataCell, tagCell)
import App.View.Icons as Icons
import App.View.Inversion (inversionStepTag)
import Data.Grouped
import Data.List qualified as L
import Data.Text qualified as Text
import Effectful.Time
import NSO.Data.Datasets
import NSO.Data.Programs
import NSO.Data.Qualify
import NSO.Prelude
import Web.Hyperbole


viewExperimentDescription :: Text -> View c ()
viewExperimentDescription t = do
  let ps = Text.splitOn "\\n" t
  col (gap 10) $ do
    mapM_ (el_ . text) ps


viewDataRow :: View c () -> View c ()
viewDataRow =
  row (gap 10 . textAlign AlignCenter . grow)


viewProgramRow :: forall c. UTCTime -> ProgramFamily -> View c ()
viewProgramRow now fam = viewDataRow $ do
  statusTag fam.status
  viewProgramStats now fam


viewProgramStats :: forall c. UTCTime -> ProgramFamily -> View c ()
viewProgramStats now prog = viewDataRow $ do
  let ip = prog.program
  -- el dataCell $ text $ showDate ip.startTime
  -- el dataCell $ text $ showDate ip.startTime
  el dataCell $ text $ cs $ show ip.instrument
  -- not worth showing Stokes in the row. They seem to be present for all VISP
  -- el dataCell $ text $ cs $ show ip.stokesParameters

  code (cell . color Secondary) $ cs $ showTimestamp ip.startTime

  row (dataCell . gap 5 . fontSize 14) $ do
    maybe none embargoTag ip.embargo
    if ip.onDisk then diskTag else none
    mapM_ lineTag $ L.sort ip.spectralLines
    mapM_ midTag $ sortOn id ip.otherWavelengths

  space

  code (cellData . color Secondary . minWidth 0) ip.programId.fromId
 where
  cellData = fontSize 14 . pad 2
  cell = dataCell . cellData

  lineTag :: SpectralLine -> View c ()
  lineTag s = tag "pre" (dataTag . Style.tagOutline (light Secondary)) $ text $ cs $ show s

  diskTag = el (dataTag . Style.tagOutline (light Primary)) "On Disk"

  embargoTag utc =
    if utc > now
      then el (dataTag . Style.tagOutline (dark Warning)) "Embargoed"
      else none

  midTag mid =
    code (pad 2 . color (light Secondary)) $ cs (show (round mid :: Integer) <> "nm")

  dataTag :: Mod c
  dataTag = pad (XY 6 1)


statusTag :: ProgramStatus -> View c ()
statusTag = \case
  StatusInvalid -> el (tagCell . color (light Secondary)) $ text "-"
  StatusQualified -> el (stat Primary) $ text "Qualified"
  StatusInversion step -> inversionStepTag step
  StatusError _ -> el (stat Danger) $ text "Error"
 where
  stat c = tagCell . Style.tag c


-- statusTag Queued = el (dataCell . bg Warning) $ text "Queued"
-- statusTag Inverted = el (dataCell . bg SecondaryLight) $ text "Complete"

viewCriteria :: ProgramFamily -> Group (Id InstrumentProgram) Dataset -> View c ()
viewCriteria ip gd = do
  col id $ do
    case ip.program.instrument of
      VISP -> vispCriteria gd ip.program.spectralLines
      VBI -> vbiCriteria
      CRYO_NIRSP -> cryoCriteria
 where
  vispCriteria :: Group (Id InstrumentProgram) Dataset -> [SpectralLine] -> View c ()
  vispCriteria ds sls = do
    col (gap 10) $ do
      el bold "VISP Criteria"
      row (gap 10 . Style.flexWrap) $ do
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

  cryoCriteria = do
    el bold "CRYO NIRSP Criteria"
    criteria "Not Supported" False

  -- criteriaRowHeight :: Length
  -- criteriaRowHeight = 32

  criteria :: Text -> Bool -> View c ()
  criteria msg b =
    row (gap 2 . color (if b then Success else Danger)) $ do
      el id checkmark
      el id (text msg)
   where
    checkmark =
      el (width 24 . height 24) $
        if b
          then Icons.checkCircle
          else Icons.xMark


viewProgramDetails :: ProgramFamily -> UTCTime -> Group (Id InstrumentProgram) Dataset -> View c ()
viewProgramDetails prog now ds = do
  viewProgramDetails' (viewProgramRow now) prog ds


viewProgramDetails' :: (ProgramFamily -> View c ()) -> ProgramFamily -> Group (Id InstrumentProgram) Dataset -> View c ()
viewProgramDetails' progRow prog gd = do
  let p = prog.program :: InstrumentProgram

  row (textAlign AlignCenter . pad 10) $ do
    appRoute (Proposal p.proposalId $ Program p.programId Prog) grow $ do
      progRow prog

  View.hr (color Gray)

  col (pad 15) $ do
    viewCriteria prog gd
