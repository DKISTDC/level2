module App.View.InstrumentProgramSummary where

import App.Colors
import App.View.DataRow (dataCell)
import App.View.Icons as Icons
import Data.Either (partitionEithers)
import Data.List.NonEmpty qualified as NE
import NSO.Data.Dataset
import NSO.Data.Qualify as Qualify
import NSO.Data.Types
import NSO.Prelude
import Numeric (showFFloat)
import Web.UI

viewRow :: InstrumentProgram -> View c ()
viewRow ip = do
  statusTag

  el dataCell $ text $ showDate ip.createDate
  -- el dataCell $ text $ showDate ip.startTime
  el dataCell $ text $ cs $ show ip.instrument
  -- not worth showing Stokes in the row. They seem to be present for all VISP
  -- el dataCell $ text $ cs $ show ip.stokesParameters

  -- TODO: on disk
  row (dataCell . gap 5 . fontSize 14) $ do
    diskTag ip.datasets
    let (mids, lns) = partitionEithers $ map identify $ NE.toList ip.datasets
    mapM_ lineTag lns
    mapM_ midTag $ sortOn id mids
 where
  identify :: Dataset -> Either (Wavelength Nm) SpectralLine
  identify d =
    case Qualify.identifyLine d.wavelengthMin d.wavelengthMax of
      Nothing -> Left (midWave d)
      Just l -> Right l

  midWave :: Dataset -> Wavelength Nm
  midWave d = d.wavelengthMin + d.wavelengthMax / 2

  lineTag s = tag "pre" (dataTag . bg SecondaryLight) $ text $ cs $ show s

  diskTag ds =
    if all (\d -> isOnDisk d.boundingBox) ds
      then el (dataTag . bg Success) "On Disk"
      else none

  midTag mid =
    tag "pre" (pad 2 . color GrayDark) $ text $ cs (showFFloat (Just 0) mid "nm")

  dataTag :: Mod
  dataTag = pad (XY 6 2) . rounded 3

  statusTag =
    case qualify ip of
      Left _ -> el (dataCell . color GrayLight) $ text "-"
      Right _ -> el (dataCell . bg Success) $ text "Invertible"

viewCriteria :: InstrumentProgram -> View c ()
viewCriteria ip = do
  let ds = NE.toList ip.datasets
  let sls = identifyLines ds
  col (pad 8) $ do
    case (head ip.datasets).instrument of
      VISP -> vispCriteria ds sls
      VBI -> vbiCriteria
 where
  vispCriteria ds sls = do
    el bold "VISP Criteria"
    criteria "On Disk" $ qualifyOnDisk ds
    criteria "Stokes IQUV" $ qualifyStokes ds
    criteria "Spectra: FeI" $ qualifyLine FeI sls
    criteria "Spectra: CaII 854" $ qualifyLine (CaII CaII_854) sls

  vbiCriteria = do
    el bold "VBI Criteria"
    criteria "Not Supported" False

criteria :: Text -> Bool -> View c ()
criteria msg b =
  row (gap 6 . color (if b then SuccessDark else GrayDark)) $ do
    el (pad 4) checkmark
    el (pad 4) (text msg)
 where
  checkmark =
    el (width 24 . height 24)
      $ if b
        then Icons.checkCircle
        else Icons.xMark
