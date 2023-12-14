module NSO.Types.Common where

import Data.Aeson
import GHC.Real (Real)
import NSO.Prelude
import Rel8 (DBEq, DBType)
import Web.Hyperbole (Param (..), Route)


newtype Id a = Id {fromId :: Text}
  deriving newtype (Show, Read, Eq, Ord, DBType, FromJSON, Route, DBEq)
  deriving (Generic)


instance Param (Id a) where
  toParam (Id t) = t
  parseParam t = pure $ Id t


type Coordinate a = (a, a)


newtype Kilometers = Kilometers { value :: Double}
  deriving newtype (Eq, Show, Read, RealFloat, Floating, RealFrac, Fractional, Real, Num, Ord)


newtype Arcseconds = Arcseconds Double
  deriving newtype (Eq, Show, Read, RealFloat, Floating, RealFrac, Fractional, Real, Num, Ord)


newtype Radians = Radians Double
  deriving newtype (Eq, Show, Read, RealFloat, Floating, RealFrac, Fractional, Real, Num, Ord)


radiansToArcseconds :: Radians -> Arcseconds
radiansToArcseconds (Radians r) = Arcseconds $ r * 206265
