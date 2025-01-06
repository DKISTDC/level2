module App.View.Inversions where

import App.Colors
import App.Style qualified as Style
import App.View.DataRow (tagCell)
import NSO.Data.Inversions as Inversions
import NSO.Prelude
import Web.View


inversionStepLabel :: InversionStep -> Text
inversionStepLabel = \case
  StepDownload _ -> "Downloading"
  StepInvert _ -> "Inverting"
  StepGenerate (StepGenerateError _) -> "Error"
  StepGenerate _ -> "Generating"
  StepPublish (StepPublished _) -> "Complete"
  StepPublish _ -> "Publishing"


inversionStepTagColor :: InversionStep -> AppColor
inversionStepTagColor = \case
  StepDownload _ -> Info
  StepInvert _ -> Info
  StepGenerate (StepGenerateError _) -> Danger
  StepGenerate _ -> Info
  StepPublish (StepPublished _) -> Success
  StepPublish _ -> Info


inversionStepTag :: InversionStep -> View c ()
inversionStepTag step =
  el (textAlign AlignCenter . stat (inversionStepTagColor step)) (text $ inversionStepLabel step)
 where
  stat c = tagCell . Style.tag c
