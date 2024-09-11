module App.View.Inversions where

import NSO.Prelude
import NSO.Types.Inversion
import Web.View


inversionStatusTag :: Inversion -> View c ()
inversionStatusTag i =
  case i.invError of
    Nothing -> text $ inversionStatusLabel i.step
    Just _ -> text "Error"


inversionStatusLabel :: InversionStep -> Text
inversionStatusLabel = \case
  StepCreated _ -> "Downloading"
  StepDownloading _ -> "Downloading"
  StepDownloaded _ -> "Preprocessing"
  StepPreprocessed _ -> "Inverting"
  StepInverting _ -> "Inverting"
  StepInverted _ -> "Generating"
  StepGenTransfer _ -> "Generating"
  StepGenerating _ -> "Generating"
  StepGenerated _ -> "Publishing"
  StepPublished _ -> "Complete"
