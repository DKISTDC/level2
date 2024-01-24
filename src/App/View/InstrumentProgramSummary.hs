module App.View.InstrumentProgramSummary where

import App.Colors
import App.View.Common
import App.View.DataRow (dataCell)
import App.View.Icons as Icons
import Data.Grouped
import NSO.Data.Datasets
import NSO.Data.Programs
import NSO.Data.Qualify
import NSO.Prelude
import NSO.Types.InstrumentProgram
import NSO.Types.Wavelength
import Numeric (showFFloat)
import Web.View
import Web.View.Style (Align (Center))


viewRow :: UTCTime -> InstrumentProgram -> View c ()
viewRow now ip = row (gap 10 . textAlign Center) $ do
  statusTag ip.status

  el dataCell $ text $ showDate ip.startTime
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
  lineTag s = tag "pre" (dataTag . bg SecondaryLight) $ text $ cs $ show s

  diskTag = el (dataTag . bg Success) "On Disk"

  embargoTag utc =
    if utc > now
      then el (dataTag . bg Warning) "Embargoed"
      else none

  midTag mid =
    tag "pre" (pad 2 . color GrayDark) $ text $ cs (show (round mid :: Integer) <> "nm")

  dataTag :: Mod
  dataTag = pad (XY 6 2) . rounded 3

  statusTag Invalid = el (dataCell . color GrayLight) $ text "-"
  statusTag Qualified = el (dataCell . bg Success) $ text "Qualified"
  statusTag Queued = el (dataCell . bg Warning) $ text "Queued"
  statusTag Inverted = el (dataCell . bg SecondaryLight) $ text "Complete"


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


criteriaRowHeight :: PxRem
criteriaRowHeight = 32


criteria :: Text -> Bool -> View c ()
criteria msg b =
  row (gap 6 . height criteriaRowHeight . color (if b then SuccessDark else ErrorDark)) $ do
    el (pad 4) checkmark
    el (pad 4) (text msg)
 where
  checkmark =
    el (width 24 . height 24)
      $ if b
        then Icons.checkCircle
        else Icons.xMark


radiusBoundingBox :: Maybe BoundingBox -> View c ()
radiusBoundingBox Nothing = none
radiusBoundingBox (Just b) = row (gap 5) $ do
  space
  forM_ (boundingPoints b) $ \c ->
    code . cs $ showFFloat (Just 0) (boxRadius c) ""
  space


code :: Text -> View c ()
code = pre (fontSize 14)
