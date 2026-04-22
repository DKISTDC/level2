module NSO.Types.Status where

import NSO.Prelude


data ProgramStatus
  = StatusInvalid
  | StatusQualified
  | StatusError Text
  | -- we have a "latest" inversion, use its status
    StatusInversion InversionStep
  deriving (Eq, Show)


data InversionStep
  = StepInvert
  | StepGenerate
  | StepPublish
  | StepComplete
  deriving (Eq, Show)
