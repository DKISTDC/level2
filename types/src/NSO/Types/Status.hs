module NSO.Types.Status where

import NSO.Prelude


data ProgramStatus
  = StatusInvalid
  | StatusQualified
  | StatusInversion InvStatus
  deriving (Eq, Show)


data InvStatus
  = InvError
  | InvDeleted
  | StepInvert
  | StepGenerate
  | StepPublish
  | StepComplete
  deriving (Eq, Show)
