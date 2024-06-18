module App.View.Inversions where

import NSO.Prelude
import NSO.Types.Status


inversionStatusLabel :: InversionStep -> Text
inversionStatusLabel (StepCreated _) = "Download"
inversionStatusLabel (StepDownloading _) = "Download"
inversionStatusLabel (StepDownloaded _) = "Preprocess"
inversionStatusLabel (StepPreprocessed _) = "Invert"
inversionStatusLabel (StepInverting _) = "Invert"
inversionStatusLabel (StepInverted _) = "Generate"
inversionStatusLabel (StepGenerating _) = "Generate"
inversionStatusLabel (StepGenerated _) = "Publish"
inversionStatusLabel (StepPublished _) = "Complete"
