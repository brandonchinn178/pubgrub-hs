{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module PubGrub.Error (
  PubGrubError,
  PubGrubError' (..),

  -- * DerivationTree
  DerivationTree' (..),
  ExternalIncompat' (..),
  DerivedIncompat' (..),
) where

import Data.Map (Map)
import PubGrub.DependencyProvider qualified as DP
import PubGrub.Term (Term')

type PubGrubError provider =
  PubGrubError'
    (DP.Package provider)
    (DP.Version provider)
    (DP.VersionRange provider)
    (DP.ResolutionError provider)
    (DP.ResolutionError provider)

data PubGrubError' package version range unavailErr err
  = NoSolution (DerivationTree' package version range unavailErr)
  | RetrieveDepsError
      { package :: package
      , version :: version
      , source :: err
      }
  | ChooseVersionError
      { package :: package
      , source :: err
      }
  | ShouldCancelError err
  deriving (Show, Eq)

-- | Derivation tree resulting in the impossibility to solve the dependencies of our root package.
data DerivationTree' package version range unavailErr
  = DerivationTreeExternal (ExternalIncompat' package version range unavailErr)
  | DerivationTreeDerived (DerivedIncompat' package version range unavailErr)
  deriving (Show, Eq)

-- | Incompatibility that is not derived from other incompatibilities.
data ExternalIncompat' package version range unavailErr
  = -- | Initial incompatibility aiming at picking the root package for the first decision.
    NotRoot package version
  | -- | There are no versions in the given set for this package.
    NoVersions package range
  | -- | Incompatibility coming from the dependencies of a given package.
    FromDependencyOf (package, range) (package, range)
  | -- | The package is unusable for reasons outside pubgrub.
    Custom package range unavailErr
  deriving (Show, Eq)

-- | Incompatibility derived from two others.
data DerivedIncompat' package version range unavailErr = DerivedIncompat'
  { terms :: Map package (Term' package range)
  , sharedId :: Maybe Int
  , cause1 :: DerivationTree' package version range unavailErr
  , cause2 :: DerivationTree' package version range unavailErr
  }
  deriving (Show, Eq)
