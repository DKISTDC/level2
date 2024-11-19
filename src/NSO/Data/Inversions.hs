module NSO.Data.Inversions
  ( module NSO.Data.Inversions.Effect
  , module NSO.Data.Inversions.Update
  , module NSO.Data.Inversions.Commit
  , module NSO.Types.Inversion
  , isActive
  , isComplete
  , inversionStep
  , inverting
  , downloadedDatasetIds
  ) where

import NSO.Data.Inversions.Commit
import NSO.Data.Inversions.Effect hiding (inversions)
import NSO.Data.Inversions.Update
import NSO.Prelude
import NSO.Types.Common
import NSO.Types.Dataset
import NSO.Types.Inversion


isActive :: InversionStep -> Bool
isActive = \case
  StepPublish (StepPublished _) -> False
  _ -> True


isComplete :: InversionStep -> Bool
isComplete = \case
  StepPublish (StepPublished _) -> True
  _ -> False


inversionStep :: Inversion -> InversionStep
inversionStep inv
  | isDownloading = StepDownload inv.download
  | isInverting = StepInvert inv.invert
  | isGenerating = StepGenerate inv.generate
  | otherwise = StepPublish inv.publish
 where
  isDownloading :: Bool
  isDownloading =
    case inv.download of
      StepDownloadNone -> True
      StepDownloading _ -> True
      StepDownloaded _ -> False

  isInverting :: Bool
  isInverting = do
    case inv.invert of
      StepInvertNone -> True
      StepInverting _ -> True
      StepInverted _ -> False

  isGenerating :: Bool
  isGenerating = do
    case inv.generate of
      StepGenerateNone -> True
      StepGenerateWaiting -> True
      StepGenerateError _ -> True
      StepGeneratingFits _ -> True
      StepGeneratingAsdf _ -> True
      StepGenerated _ -> False


downloadedDatasetIds :: Inversion -> [Id Dataset]
downloadedDatasetIds inv =
  case inv.download of
    StepDownloadNone -> []
    StepDownloading dwn -> dwn.datasets
    StepDownloaded dwn -> dwn.datasets


inverting :: StepInvert -> Inverting
inverting = \case
  StepInverting inv -> inv
  StepInverted inv -> Inverting (Just inv.transfer) (Just inv.commit)
  _ -> Inverting Nothing Nothing
