{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module PubGrub.Error (
  PubGrubError (..),

  -- * DerivationTree
  DerivationTree (..),
  ExternalIncompat (..),
  DerivedIncompat (..),
) where

import Data.Map (Map)
import PubGrub.DependencyProvider (IsDependencyProvider)
import PubGrub.DependencyProvider qualified as DP
import PubGrub.Term (Term)

data PubGrubError provider
  = NoSolution (DerivationTree provider)
  | RetrieveDepsError
      { package :: DP.Package provider
      , version :: DP.Version provider
      , source :: DP.ResolutionError provider
      }
  | ChooseVersionError
      { package :: DP.Package provider
      , source :: DP.ResolutionError provider
      }
  | ShouldCancelError (DP.ResolutionError provider)

deriving instance (IsDependencyProvider provider) => Show (PubGrubError provider)
deriving instance (IsDependencyProvider provider) => Eq (PubGrubError provider)

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
    NotRoot (DP.Package provider) (DP.Version provider)
  | -- | There are no versions in the given set for this package.
    NoVersions (DP.Package provider) (DP.VersionRange provider)
  | -- | Incompatibility coming from the dependencies of a given package.
    FromDependencyOf
      (DP.Package provider, DP.VersionRange provider)
      (DP.Package provider, DP.VersionRange provider)
  | -- | The package is unusable for reasons outside pubgrub.
    Custom (DP.Package provider) (DP.VersionRange provider) (DP.UnavailableError provider)

deriving instance
  ( Show (DP.Package provider)
  , Show (DP.Version provider)
  , Show (DP.VersionRange provider)
  , Show (DP.UnavailableError provider)
  , Show (DP.ResolutionError provider)
  ) =>
  Show (ExternalIncompat provider)
deriving instance (IsDependencyProvider provider) => Eq (ExternalIncompat provider)

-- | Incompatibility derived from two others.
data DerivedIncompat provider = DerivedIncompat
  { terms :: Map (DP.Package provider) (Term provider)
  , sharedId :: Maybe Int
  , cause1 :: DerivationTree provider
  , cause2 :: DerivationTree provider
  }

deriving instance
  ( Show (DP.Package provider)
  , Show (DP.VersionRange provider)
  , Show (DerivationTree provider)
  ) =>
  Show (DerivedIncompat provider)
deriving instance (IsDependencyProvider provider) => Eq (DerivedIncompat provider)
