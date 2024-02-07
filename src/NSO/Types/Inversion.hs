{-# LANGUAGE StrictData #-}

module NSO.Types.Inversion
  ( Inversion (..)
  , module NSO.Types.Status
  ) where

import NSO.Types.Common
import NSO.Types.InstrumentProgram
import NSO.Types.Status


data Inversion = Inversion
  { inversionId :: Id Inversion
  , programId :: Id InstrumentProgram
  , step :: InversionStep
  }
