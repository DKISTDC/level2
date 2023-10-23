module App.View.InstrumentProgramSummary where

import App.Colors
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
      Left _ -> el (dataCell . color GrayDark) $ text "-"
      Right _ -> el (dataCell . bg Success) $ text "Invertible"

viewCriteria :: InstrumentProgram -> View c ()
viewCriteria ip = do
  let ds = NE.toList ip.datasets
  let sls = identifyLines ds
  col (pad 8) $ do
    criteria "On Disk" $ all (\d -> isOnDisk d.boundingBox) ip.datasets
    criteria "VISP" $ all (\d -> d.instrument == VISP) ip.datasets
    criteria "FeI" $ FeI `elem` sls
    criteria "CaII 854" $ CaII CaII_854 `elem` sls
 where
  criteria msg b =
    row (gap 6) $ do
      el (pad 4) $ checkmark b
      el (pad 4) msg

  checkmark True = "✅"
  checkmark False = "✖"

dataRows :: [a] -> (a -> View c ()) -> View c ()
dataRows as viewRow = forM_ (zip (cycle [True, False]) as) $ \(b, a) ->
  el (dataRow . alternateColor b) $ viewRow a
 where
  alternateColor b = if b then bg Light else id

dataCell :: Mod
dataCell = minWidth 100

dataRow :: Mod
dataRow = gap 10 . pad (All dataRowPadding)

dataRowPadding :: PxRem
dataRowPadding = 5

dataRowHeight :: PxRem
dataRowHeight = 16 + 2 * dataRowPadding
