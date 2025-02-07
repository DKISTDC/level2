module App.View.Inversions where

import App.Colors
import App.Style qualified as Style
import App.View.DataRow (tagCell)
import NSO.Data.Inversions as Inversions
import NSO.Prelude
import Web.View


inversionStepLabel :: Inversion -> Text
inversionStepLabel inv
  | isError inv = "Error"
  | otherwise = stepLabel (inversionStep inv)
 where
  stepLabel = \case
    StepComplete -> "Complete"
    StepPublish -> "Publish"
    StepGenerate -> "Generate"
    StepInvert -> "Invert"


inversionStepColor :: Inversion -> AppColor
inversionStepColor inv
  | isError inv = Danger
  | isComplete inv = Success
  | otherwise = Info


inversionStepTag :: Inversion -> View c ()
inversionStepTag inv =
  el (textAlign AlignCenter . stat (inversionStepColor inv)) (text $ inversionStepLabel inv)
 where
  stat c = tagCell . Style.tag c
