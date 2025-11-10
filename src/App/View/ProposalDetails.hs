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
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as Text
import Effectful.Time
import NSO.Data.Datasets
import NSO.Data.Programs
import NSO.Data.Qualify
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
  -- not worth showing Stokes in the row. They seem to be present for all ViSP
  -- el dataCell $ text $ cs $ show ip.stokesParameters

  code (cs $ showDate ip.startTime) ~ cell . noWrap . color Secondary

  row ~ dataCell . gap 5 . fontSize 14 $ do
    maybe none embargoTag ip.embargo

    let dlines :: NonEmpty [SpectralLine] = fmap (.spectralLines) prog.datasets.items
    mapM_ lineTag $ sortOn linesIon $ NE.filter isKnownIon dlines
    mapM_ lineTag $ NE.filter (not . isKnownIon) dlines

  space

  code ip.programId.fromId ~ color (light Secondary) . minWidth 0 . fontSize 12 . pad 2
 where
  linesIon :: [SpectralLine] -> Ion
  linesIon [] = UnknownIon
  linesIon (s : _) = s.ion

  isKnownIon :: [SpectralLine] -> Bool
  isKnownIon = \case
    (s : _) ->
      case s.ion of
        Ion _ -> True
        UnknownIon -> False
        _ -> True
    _ -> False

  cellData :: (Styleable h) => CSS h -> CSS h
  cellData = fontSize 14 . pad 2

  cell :: (Styleable h) => CSS h -> CSS h
  cell = dataCell . cellData

  -- diskTag = el ~ dataTag . noWrap . Style.tagOutline (light Primary) $ "On Disk"

  embargoTag utc =
    if utc > now
      then el ~ dataTag . Style.tagOutline (dark Warning) $ "Embargoed"
      else none


lineTag :: [SpectralLine] -> View c ()
lineTag [] = none
lineTag (s : _) =
  case s.ion of
    Ion _ -> lineTag' s
    UnknownIon -> wavTag s.wavelength
    _ -> lineTag' s


lineTag' :: SpectralLine -> View c ()
lineTag' s =
  tag "pre" ~ dataTag . Style.tagOutline (light Secondary) $ text $ do
    spectralLineShort s


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
      VISP -> vispCriteria gd
      VBI -> vbiCriteria
      CRYO_NIRSP -> cryoCriteria
 where
  vispCriteria :: Group (Id InstrumentProgram) Dataset -> View c ()
  vispCriteria ds = do
    el ~ bold $ "ViSP Criteria"
    row ~ gap 10 . flexWrap Wrap $ do
      vcriteria "Stokes IQUV" $ qualifyStokes ds
      vcriteria "On Disk" $ qualifyOnDisk ds
      -- criteria require "Spectra: FeI" $ qualifyLine FeI sls
      -- criteria warning "Spectra: CaII" $ qualifyLine CaII sls
      -- criteria warning "Spectra: NaI" $ qualifyLine NaI sls
      vcriteria "Health" $ qualifyHealth ds
      vcriteria "GOS Status" $ qualifyGOS ds
      vcriteria "AO Lock" $ qualifyAO ds

  vbiCriteria = do
    el ~ bold $ "VBI Criteria"
    vcriteria "Not Supported" False

  cryoCriteria = do
    el ~ bold $ "CRYO NIRSP Criteria"
    vcriteria "Not Supported" False

  vcriteria msg b =
    criteria (if b then Good else Fail) msg


data Criteria
  = Fail
  | Warn
  | Good
instance Semigroup Criteria where
  Fail <> _ = Fail
  _ <> Fail = Fail
  Warn <> _ = Warn
  _ <> Warn = Warn
  Good <> Good = Good


criteria :: Criteria -> Text -> View c ()
criteria c msg =
  row ~ gap 2 . color clr $ do
    el checkmark
    el (text msg)
 where
  clr =
    case c of
      Fail -> colorValue Danger
      Warn -> colorValue $ dark Warning
      Good -> colorValue Success

  checkmark =
    el ~ width 24 . height 24 $
      case c of
        Fail -> Icons.xMark
        Warn -> Icons.minus
        Good -> Icons.checkCircle


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


viewWavelengthRanges :: [SpectralLine] -> NonEmpty Dataset -> View c ()
viewWavelengthRanges sls _ = do
  col ~ gap 4 $ do
    el ~ bold $ "ViSP Spectral Lines"
    specCriteria validateIron FeI
    specCriteria (validateOptional CaII) CaII
    specCriteria (validateOptional NaI) NaI
 where
  specCriteria allCriteria i = do
    let c = runCriteria allCriteria
    let t = ionName i
    criteria c $ do
      case c of
        Good -> t
        Warn -> t <> " Missing"
        Fail -> t <> " Missing"

  runCriteria :: Either Criteria Criteria -> Criteria
  runCriteria c =
    case c of
      Left l -> l
      Right r -> r

  validateIron :: Either Criteria Criteria
  validateIron = do
    lineRequired FeI
    pure Good

  validateOptional ion = do
    lineOptional ion
    pure Good

  lineRequired l
    | qualifyLine l sls = pure ()
    | otherwise = Left Fail

  lineOptional l
    | qualifyLine l sls = pure ()
    | otherwise = Left Warn
