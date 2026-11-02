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
import PubGrub.VersionSet (Term, Version)

data Error package versionSet incompat err
  = NoSolution (DerivationTree package versionSet incompat)
  | RetrieveDepsError
      { package :: package
      , version :: (Version versionSet)
      , source :: err
      }
  | ChooseVersionError
      { package :: package
      , source :: err
      }
  | ShouldCancelError err

deriving instance
  ( Show package
  , Show versionSet
  , Show (Version versionSet)
  , Show incompat
  , Show err
  ) =>
  Show (Error package versionSet incompat err)

deriving instance
  ( Eq package
  , Eq versionSet
  , Eq (Version versionSet)
  , Eq incompat
  , Eq err
  ) =>
  Eq (Error package versionSet incompat err)

-- | Derivation tree resulting in the impossibility to solve the dependencies of our root package.
data DerivationTree package versionSet incompat
  = DerivationTreeExternal (ExternalIncompat package versionSet incompat)
  | DerivationTreeDerived (DerivedIncompat package versionSet incompat)

deriving instance
  ( Show package
  , Show versionSet
  , Show (Version versionSet)
  , Show incompat
  ) =>
  Show (DerivationTree package versionSet incompat)

deriving instance
  ( Eq package
  , Eq versionSet
  , Eq (Version versionSet)
  , Eq incompat
  ) =>
  Eq (DerivationTree package versionSet incompat)

-- | Incompatibility that is not derived from other incompatibilities.
data ExternalIncompat package versionSet incompat
  = -- | Initial incompatibility aiming at picking the root package for the first decision.
    NotRoot package (Version versionSet)
  | -- | There are no versions in the given set for this package.
    NoVersions package versionSet
  | -- | Incompatibility coming from the dependencies of a given package.
    FromDependencyOf (package, versionSet) (package, versionSet)
  | -- | The package is unusable for reasons outside pubgrub.
    Custom package versionSet incompat

deriving instance
  ( Show package
  , Show versionSet
  , Show (Version versionSet)
  , Show incompat
  ) =>
  Show (ExternalIncompat package versionSet incompat)

deriving instance
  ( Eq package
  , Eq versionSet
  , Eq (Version versionSet)
  , Eq incompat
  ) =>
  Eq (ExternalIncompat package versionSet incompat)

-- | Incompatibility derived from two others.
data DerivedIncompat package versionSet incompat = DerivedIncompat
  { terms :: Map package (Term versionSet)
  , sharedId :: Maybe Int
  , cause1 :: DerivationTree package versionSet incompat
  , cause2 :: DerivationTree package versionSet incompat
  }

deriving instance
  ( Show package
  , Show versionSet
  , Show (Version versionSet)
  , Show incompat
  ) =>
  Show (DerivedIncompat package versionSet incompat)

deriving instance
  ( Eq package
  , Eq versionSet
  , Eq (Version versionSet)
  , Eq incompat
  ) =>
  Eq (DerivedIncompat package versionSet incompat)
