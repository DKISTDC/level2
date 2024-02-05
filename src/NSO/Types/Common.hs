module NSO.Types.Common where

import Control.Monad (replicateM)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Effectful.GenRandom (GenRandom, randomFromList)
import GHC.Real (Real)
import NSO.Prelude
import Rel8 (DBEq, DBType, TypeInformation, parseTypeInformation, typeInformation)
import Web.Hyperbole (Param (..), Route)


newtype Id a = Id {fromId :: Text}
  deriving newtype (Show, Read, Eq, Ord, DBType, FromJSON, Route, DBEq)
  deriving (Generic)


randomId :: (GenRandom :> es) => Eff es (Id a)
randomId = do
  let chars = ['A' .. 'Z'] ++ ['0' .. '9']
  sid <- replicateM 6 (randomFromList chars)
  pure $ Id (cs sid)


instance Param (Id a) where
  toParam (Id t) = t
  parseParam t = pure $ Id t


type Coordinate a = (a, a)


newtype Kilometers = Kilometers {value :: Double}
  deriving newtype (Eq, Show, Read, RealFloat, Floating, RealFrac, Fractional, Real, Num, Ord)


newtype Arcseconds = Arcseconds Double
  deriving newtype (Eq, Show, Read, RealFloat, Floating, RealFrac, Fractional, Real, Num, Ord)


newtype Radians = Radians Double
  deriving newtype (Eq, Show, Read, RealFloat, Floating, RealFrac, Fractional, Real, Num, Ord)


radiansToArcseconds :: Radians -> Arcseconds
radiansToArcseconds (Radians r) = Arcseconds $ r * 206265


jsonTypeInfo :: (FromJSON a, ToJSON a) => TypeInformation a
jsonTypeInfo = parseTypeInformation (parseEither parseJSON) toJSON typeInformation
