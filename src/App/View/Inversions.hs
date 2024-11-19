module App.View.Inversions where

import NSO.Data.Inversions (inversionStep)
import NSO.Prelude
import NSO.Types.Inversion
import Web.View


inversionStatusTag :: Inversion -> View c ()
inversionStatusTag i =
  case i.invError of
    Nothing -> text $ inversionStatusLabel (inversionStep i)
    Just _ -> text "Error"


inversionStatusLabel :: InversionStep -> Text
inversionStatusLabel = \case
  StepDownload _ -> "Downloading"
  StepInvert _ -> "Inverting"
  StepGenerate _ -> "Generating"
  StepPublish _ -> "Complete"
