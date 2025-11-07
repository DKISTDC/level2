module App.View.ProposalDetails
  ( viewExperimentDescription
  , viewProgramRow
  , viewProgramRowLink
  , viewCriteria
  , viewProgramStats
  , ionTag
  , viewFriedHistogram
  , viewWavelengthRanges
  ) where

import App.Colors
import App.Route as Route
import App.Style (noWrap)
import App.Style qualified as Style
import App.View.Common (showDate)
import App.View.DataRow (dataCell, tagCell)
import App.View.Datasets (boxPlot)
import App.View.Icons as Icons
import App.View.Inversion (inversionStepTag)
import Data.Grouped
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Data.Text qualified as Text
import Effectful.Time
import NSO.Data.Datasets
import NSO.Data.Programs
import NSO.Data.Qualify
import NSO.Data.Spectra qualified as Spectra
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.Wavelength
import Numeric (showFFloat)
import Web.Atomic.CSS
import Web.Hyperbole


viewExperimentDescription :: Text -> View c ()
viewExperimentDescription t = do
  let ps = Text.splitOn "\\n" t
  col ~ gap 10 $ do
    mapM_ (el . text) ps


viewDataRow :: View c () -> View c ()
viewDataRow =
  row ~ gap 10 . textAlign AlignCenter . grow


viewProgramRowLink :: forall c. UTCTime -> ProgramFamily -> View c ()
viewProgramRowLink now prog = do
  let p = prog.program :: InstrumentProgram
  row ~ textAlign AlignCenter . pad 10 $ do
    appRoute (Proposal p.proposalId $ Program p.programId Prog) ~ grow $ do
      viewProgramRow now prog


viewProgramRow :: forall c. UTCTime -> ProgramFamily -> View c ()
viewProgramRow now fam = viewDataRow $ do
  statusTag fam.status
  viewProgramStats now fam


viewProgramStats :: forall c. UTCTime -> ProgramFamily -> View c ()
viewProgramStats now prog = viewDataRow $ do
  let ip = prog.program
  -- el dataCell $ text $ showDate ip.startTime
  -- el dataCell $ text $ showDate ip.startTime
  el ~ dataCell $ text $ cs $ show ip.instrument
  -- not worth showing Stokes in the row. They seem to be present for all VISP
  -- el dataCell $ text $ cs $ show ip.stokesParameters

  code (cs $ showDate ip.startTime) ~ cell . noWrap . color Secondary

  row ~ dataCell . gap 5 . fontSize 14 $ do
    maybe none embargoTag ip.embargo
    -- if ip.onDisk then diskTag else none
    mapM_ ionTag $ L.sort $ L.nub $ fmap (.ion) ip.spectralLines
  -- mapM_ wavTag $ fmap (.wavelength) $ ip.spectralLines

  space

  code ip.programId.fromId ~ color (light Secondary) . minWidth 0 . fontSize 12 . pad 2
 where
  cellData :: (Styleable h) => CSS h -> CSS h
  cellData = fontSize 14 . pad 2

  cell :: (Styleable h) => CSS h -> CSS h
  cell = dataCell . cellData

  unknownIon s =
    case s.ion of
      Ion _ -> True
      _ -> False

  -- diskTag = el ~ dataTag . noWrap . Style.tagOutline (light Primary) $ "On Disk"

  embargoTag utc =
    if utc > now
      then el ~ dataTag . Style.tagOutline (dark Warning) $ "Embargoed"
      else none


ionTag :: Ion -> View c ()
ionTag i = ionTag' i ""


ionTag' :: Ion -> Text -> View c ()
ionTag' i suffix =
  tag "pre" ~ dataTag . Style.tagOutline (light Secondary) $ text $ cs (show i) <> suffix


wavTag :: Wavelength Nm -> View c ()
wavTag w =
  code (cs (show (round w :: Integer) <> "nm")) ~ (pad 2 . color (light Secondary))


dataTag :: (Styleable h) => CSS h -> CSS h
dataTag = pad (XY 6 1)


statusTag :: ProgramStatus -> View c ()
statusTag = \case
  StatusInvalid -> el ~ tagCell . color (light Secondary) $ text "-"
  StatusQualified -> el ~ stat Primary $ text "Qualified"
  StatusInversion step -> inversionStepTag step
  StatusError _ -> el ~ stat Danger $ text "Error"
 where
  stat c = tagCell . Style.tag c


-- statusTag Queued = el (dataCell . bg Warning) $ text "Queued"
-- statusTag Inverted = el (dataCell . bg SecondaryLight) $ text "Complete"

viewCriteria :: ProgramFamily -> Group (Id InstrumentProgram) Dataset -> View c ()
viewCriteria ip gd = do
  col $ do
    case ip.program.instrument of
      VISP -> vispCriteria gd ip.program.spectralLines
      VBI -> vbiCriteria
      CRYO_NIRSP -> cryoCriteria
 where
  vispCriteria :: Group (Id InstrumentProgram) Dataset -> [SpectralLine] -> View c ()
  vispCriteria ds sls = do
    el ~ bold $ "VISP Criteria"
    row ~ gap 10 . flexWrap Wrap $ do
      criteria "Stokes IQUV" $ qualifyStokes ds
      criteria "On Disk" $ qualifyOnDisk ds
      criteria "Spectra: FeI 630" $ qualifyLine FeI sls
      criteria "Spectra: CaII 854" $ qualifyLine CaII sls
      criteria "Health" $ qualifyHealth ds
      criteria "GOS Status" $ qualifyGOS ds
      criteria "AO Lock" $ qualifyAO ds

  vbiCriteria = do
    el ~ bold $ "VBI Criteria"
    criteria "Not Supported" False

  cryoCriteria = do
    el ~ bold $ "CRYO NIRSP Criteria"
    criteria "Not Supported" False

  criteria :: Text -> Bool -> View c ()
  criteria msg b =
    row ~ gap 2 . color (if b then Success else Danger) $ do
      el checkmark
      el (text msg)
   where
    checkmark =
      el ~ width 24 . height 24 $
        if b
          then Icons.checkCircle
          else Icons.xMark


viewFriedHistogram :: Maybe Distribution -> View c ()
viewFriedHistogram Nothing = none
viewFriedHistogram (Just fried) = do
  el ~ friedColor fried.med . bold $ "R0 Fried Parameter"
  boxPlot (\f -> showFFloat (Just 1) (f * 100) "") 0.15 fried
 where
  friedColor :: (Styleable h) => Float -> CSS h -> CSS h
  friedColor r0
    | r0 >= 0.07 = color Success
    | r0 >= 0.05 = color (dark Warning)
    | otherwise = color Danger


viewWavelengthRanges :: NonEmpty Dataset -> View c ()
viewWavelengthRanges ds = do
  col ~ gap 4 $ do
    el ~ bold $ "Spectral Lines"

--    row ~ gap 5 $ do
--      forM_ (sortOn fst $ mapMaybe datasetWithLine $ NE.toList ds) $ \(l, d) -> do
--        row $ do
--          let clr = lineColor d l
--          ionTag l ~ fontSize 14 . color clr . borderColor clr
-- where
--  datasetWithLine d = do
--    case d.spectralLines of
--      (s : _) -> Just (s, d)
--      [] -> Nothing
--
--  lineColor d l = Success
--  lineWarning d l = ""
