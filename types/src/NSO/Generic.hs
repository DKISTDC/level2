{-# LANGUAGE DerivingVia #-}

module NSO.Generic
  ( NoFields
  , module Generic.Data.Microsurgery
  ) where

import Generic.Data.Microsurgery
  ( Derecordify
  , Generically (..)
  , Surgery
  , Surgery' (..)
  )


-- import NSO.Prelude

type NoFields a = Surgery Derecordify a
