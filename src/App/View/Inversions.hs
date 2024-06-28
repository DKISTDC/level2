module App.View.Inversions where

import NSO.Prelude
import NSO.Types.Status


inversionStatusLabel :: InversionStep -> Text
inversionStatusLabel (StepCreated _) = "Downloading"
inversionStatusLabel (StepDownloading _) = "Downloading"
inversionStatusLabel (StepDownloaded _) = "Preprocessing"
inversionStatusLabel (StepPreprocessed _) = "Inverting"
inversionStatusLabel (StepInverting _) = "Inverting"
inversionStatusLabel (StepInverted _) = "Generating"
inversionStatusLabel (StepGenTransfer _) = "Generaeing"
inversionStatusLabel (StepGenerating _) = "Generating"
inversionStatusLabel (StepGenerated _) = "Publishing"
inversionStatusLabel (StepPublished _) = "Complete"
