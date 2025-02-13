module App.View.Inversion where

import App.Colors
import App.Route qualified as Route
import App.Style qualified as Style
import App.View.Common (showDate)
import App.View.DataRow (dataCell, tagCell)
import NSO.Data.Inversions as Inversions
import NSO.Prelude
import NSO.Types.Common (Id (..))
import Web.Hyperbole (route)
import Web.View


inversionStepLabel :: Inversion -> Text
inversionStepLabel inv
  | isError inv = "Error"
  | otherwise = stepLabel (inversionStep inv)
 where
  stepLabel = \case
    StepComplete -> "Complete"
    StepPublish -> "Publishing"
    StepGenerate -> "Generating"
    StepInvert -> "Inverting"


inversionStepColor :: Inversion -> AppColor
inversionStepColor inv
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
  route (Route.inversion inv.proposalId inv.inversionId) id $ do
    row (gap 10) $ do
      inversionStepTag inv
      el dataCell $ text $ cs $ showDate inv.updated
      -- el (width 150) $ text $ cs inv.programId.fromId
      space
      el (Style.link . width 100) $ text $ cs inv.inversionId.fromId
