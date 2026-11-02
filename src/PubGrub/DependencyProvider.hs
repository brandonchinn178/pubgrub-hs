{-# LANGUAGE NoFieldSelectors #-}

module PubGrub.DependencyProvider (
  DependencyProvider (..),
  IsVersionSet (..),
  Dependencies (..),

  -- * PackageResolutionStats
  PackageResolutionStats,
  conflictCount,
) where

import PubGrub.Internal.PackageResolutionStats (PackageResolutionStats, conflictCount)
import PubGrub.VersionSet (IsVersionSet (..), Version)

-- | A record-of-functions that's polymorphic over the caller's package/version
-- types, which allows flexibility for how package names, version requirements,
-- version numbers, etc. are represented.
data DependencyProvider package versionSet incompat err m
  = forall priority.
  (Ord priority) =>
  DependencyProvider
  { prioritize :: package -> versionSet -> PackageResolutionStats -> m priority
  , chooseVersion :: package -> versionSet -> m (Either err (Maybe (Version versionSet)))
  , getDependencies :: package -> Version versionSet -> m (Either err (Dependencies package versionSet incompat))
  , shouldCancel :: m (Either err ())
  }

data Dependencies package versionSet incompat
  = Available [(package, versionSet)]
  | Unavailable incompat
  deriving (Show, Eq)
