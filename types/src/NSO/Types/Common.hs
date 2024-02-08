{-# LANGUAGE DerivingVia #-}

module NSO.Types.Common where

import Control.Monad (replicateM)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.List qualified as L
import Effectful.GenRandom (GenRandom, randomFromList)
import GHC.Real (Real)
import NSO.Prelude
import Rel8 (DBEq, DBType, ReadShow (..), TypeInformation, parseTypeInformation, typeInformation)
import Web.Hyperbole (Param (..), Route)


newtype Id a = Id {fromId :: Text}
  deriving newtype (Show, Read, Eq, Ord, DBType, FromJSON, Route, DBEq)
  deriving (Generic)


randomId :: (GenRandom :> es) => Text -> Eff es (Id a)
randomId pre = do
  let chars = ['A' .. 'Z'] ++ ['0' .. '9']
  sid <- replicateM 6 (randomFromList chars)
  pure $ Id (pre <> "." <> cs sid)


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


data Stokes = I | Q | U | V
  deriving (Show, Read, Eq, Ord)
  deriving (DBType) via ReadShow Stokes


newtype StokesParameters = StokesParameters [Stokes]
  deriving newtype (DBType, Eq, Ord, Monoid)


instance Show StokesParameters where
  show (StokesParameters ss) = mconcat $ fmap show ss


instance FromJSON StokesParameters where
  parseJSON = withText "Stokes Params" $ \t -> do
    sps <- mapM parseChar $ cs t
    pure $ StokesParameters sps
   where
    parseChar 'I' = pure I
    parseChar 'Q' = pure Q
    parseChar 'U' = pure U
    parseChar 'V' = pure V
    parseChar c = fail $ "Expected Stokes param (IQUV) but got: " <> [c]


instance Semigroup StokesParameters where
  (StokesParameters a) <> (StokesParameters b) = StokesParameters . L.nub $ a <> b


data Instrument
  = VBI
  | VISP
  deriving (Show, Ord, Eq, Read, Param)
  deriving (DBType) via ReadShow Instrument
