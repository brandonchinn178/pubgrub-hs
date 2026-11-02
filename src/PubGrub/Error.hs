{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module PubGrub.Error (
  Error (..),

  -- * DerivationTree
  DerivationTree (..),
  ExternalIncompat (..),
  DerivedIncompat (..),
) where

import Data.Map (Map)
import PubGrub.DependencyProvider (
  DependencyProvider,
  Package,
  ResolutionError,
  UnavailableError,
  Version,
  VersionRange,
 )
import PubGrub.Term (Term)

data Error provider
  = NoSolution (DerivationTree provider)
  | RetrieveDepsError
      { package :: Package provider
      , version :: Version provider
      , source :: ResolutionError provider
      }
  | ChooseVersionError
      { package :: Package provider
      , source :: ResolutionError provider
      }
  | ShouldCancelError (ResolutionError provider)

deriving instance (DependencyProvider provider) => Show (Error provider)
deriving instance (DependencyProvider provider) => Eq (Error provider)

-- | Derivation tree resulting in the impossibility to solve the dependencies of our root package.
data DerivationTree provider
  = DerivationTreeExternal (ExternalIncompat provider)
  | DerivationTreeDerived (DerivedIncompat provider)
  deriving (Eq)

deriving instance
  ( Show (ExternalIncompat provider)
  , Show (DerivedIncompat provider)
  ) =>
  Show (DerivationTree provider)

-- | Incompatibility that is not derived from other incompatibilities.
data ExternalIncompat provider
  = -- | Initial incompatibility aiming at picking the root package for the first decision.
    NotRoot (Package provider) (Version provider)
  | -- | There are no versions in the given set for this package.
    NoVersions (Package provider) (VersionRange provider)
  | -- | Incompatibility coming from the dependencies of a given package.
    FromDependencyOf
      (Package provider, VersionRange provider)
      (Package provider, VersionRange provider)
  | -- | The package is unusable for reasons outside pubgrub.
    Custom (Package provider) (VersionRange provider) (UnavailableError provider)

deriving instance
  ( Show (Package provider)
  , Show (Version provider)
  , Show (VersionRange provider)
  , Show (UnavailableError provider)
  , Show (ResolutionError provider)
  ) =>
  Show (ExternalIncompat provider)
deriving instance (DependencyProvider provider) => Eq (ExternalIncompat provider)

-- | Incompatibility derived from two others.
data DerivedIncompat provider = DerivedIncompat
  { terms :: Map (Package provider) (Term (VersionRange provider))
  , sharedId :: Maybe Int
  , cause1 :: DerivationTree provider
  , cause2 :: DerivationTree provider
  }

deriving instance
  ( Show (Package provider)
  , Show (VersionRange provider)
  , Show (DerivationTree provider)
  ) =>
  Show (DerivedIncompat provider)
deriving instance (DependencyProvider provider) => Eq (DerivedIncompat provider)
