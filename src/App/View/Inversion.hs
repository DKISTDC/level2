module App.View.Inversion where

import App.Colors
import App.Route as Route
import App.Style qualified as Style
import App.View.Common (showDate)
import App.View.DataRow (dataCell, tagCell)
import App.View.Icons qualified as Icons
import NSO.Data.Inversions as Inversions
import NSO.Prelude
import NSO.Types.Common (Id (..))
import Web.View


inversionStepLabel :: Inversion -> Text
inversionStepLabel inv
  | isError inv = "Error"
  | inv.deleted = "Archived"
  | otherwise = stepLabel (inversionStep inv)
 where
  stepLabel = \case
    StepComplete -> "Complete"
    StepPublish -> "Publishing"
    StepGenerate -> "Generating"
    StepInvert -> "Inverting"


inversionStepColor :: Inversion -> AppColor
inversionStepColor inv
  | inv.deleted = Secondary
  | isError inv = Danger
  | isPublished inv = Success
  | otherwise = Info


inversionStepTag :: Inversion -> View c ()
inversionStepTag inv =
  el (textAlign AlignCenter . stat (inversionStepColor inv)) (text $ inversionStepLabel inv)
 where
  stat c = tagCell . Style.tag c


viewInversionContainer :: Inversion -> View c () -> View c ()
viewInversionContainer inv =
  viewInversionContainer' (inversionStepColor inv)


viewInversionContainer' :: AppColor -> View c () -> View c ()
viewInversionContainer' clr cnt =
  col (Style.card . gap 15) $ do
    el (Style.cardHeader clr) "Inversion"
    col (gap 0 . pad 15) $ do
      cnt


rowInversion :: Inversion -> View id ()
rowInversion inv = do
  appRoute (Route.inversion inv.proposalId inv.inversionId) id $ do
    row (gap 10) $ do
      inversionStepTag inv
      el (Style.link . width 100) $ text $ cs inv.inversionId.fromId
      -- el (width 150) $ text $ cs inv.programId.fromId
      space
      el dataCell $ text $ cs $ showDate inv.updated


-------------------------------------------------------------------
-- VERTICAL STEP INDICATORS
-------------------------------------------------------------------

data Step
  = StepActive
  | StepNext
  | StepDone
  | StepError


viewStep :: Int -> Text -> Step -> View c () -> View c ()
viewStep num stepName step = viewStep' num stepName step (stepLine step)


viewStepEnd :: Int -> Text -> Step -> View c () -> View c ()
viewStepEnd num stepName step = viewStep' num stepName step none


viewStep' :: Int -> Text -> Step -> View c () -> View c () -> View c ()
viewStep' num stepName step line content =
  row (gap 10 . stepEnabled) $ do
    col id $ do
      stepCircle step num
      line
    col (gap 10 . grow . pad (TRBL 0 0 40 0)) $ do
      el (bold . color (stepColor step) . fontSize 22) (text stepName)
      content
 where
  stepEnabled =
    case step of
      StepNext -> Style.disabled
      _ -> id


stepLine :: Step -> View c ()
stepLine = \case
  StepActive -> line Info
  StepNext -> line Secondary
  StepDone -> line Success
  StepError -> line Danger
 where
  line clr = el (grow . border (TRBL 0 2 0 0) . width 18 . borderColor clr) ""


stepCircle :: Step -> Int -> View c ()
stepCircle step num =
  el (circle . bg (stepColor step)) stepIcon
 where
  circle = rounded 50 . pad 5 . color White . textAlign AlignCenter . width 34 . height 34
  stepIcon =
    case step of
      StepDone -> Icons.check
      StepError -> "!"
      _ -> text $ cs $ show num


stepColor :: Step -> AppColor
stepColor = \case
  StepActive -> Info
  StepNext -> Secondary
  StepDone -> Success
  StepError -> Danger


stepUpload :: Step -> View c () -> View c ()
stepUpload = viewStep 1 "Upload"


stepMetadata :: Step -> View c () -> View c ()
stepMetadata = viewStep 2 "Metadata"


stepGenerate :: Step -> View c () -> View c ()
stepGenerate = viewStep 3 "Generate"


stepPublish :: Step -> View c () -> View c ()
stepPublish = viewStepEnd 4 "Publish"
