{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

module PubGrub.DependencyProvider (
  DependencyProvider (..),
  IsVersionSet (..),
  Dependencies (..),

  -- * PackageResolutionStats
  PackageResolutionStats,
  conflictCount,

  -- * Simple dependency provider
  mkOfflineDependencyProvider,
) where

import Data.Functor.Identity (Identity)
import Data.List (find)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Void (Void)
import PubGrub.Internal.PackageResolutionStats (PackageResolutionStats, conflictCount)
import PubGrub.Logger (Logger (..))
import PubGrub.VersionSet (IsVersionSet (..), Version)
import PubGrub.VersionSet qualified as VersionSet

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
  , logger :: Logger m
  }

data Dependencies package versionSet incompat
  = Available [(package, versionSet)]
  | Unavailable incompat
  deriving (Show, Eq)

-- | A simple instance of 'DependencyProvider'.
mkOfflineDependencyProvider ::
  (Ord package, IsVersionSet versionSet) =>
  Map package (Map (Version versionSet) [(package, versionSet)]) ->
  DependencyProvider package versionSet Text Void Identity
mkOfflineDependencyProvider deps = DependencyProvider{..}
 where
  prioritize package range stats = do
    let versionCount =
          case Map.lookup package deps of
            Nothing -> 0
            Just versions -> length . filter id . (`VersionSet.elemMany` range) $ Map.keys versions
    pure (stats.conflictCount, Down versionCount)

  chooseVersion package range = pure . Right $ do
    versions <- Map.lookup package deps
    find (`VersionSet.elem` range) . reverse $ Map.keys versions

  getDependencies package version = pure . Right . fromMaybe unavailable $ do
    versions <- Map.lookup package deps
    deps' <- Map.lookup version versions
    pure $ Available deps'
  unavailable = Unavailable "its dependencies could not be determined"

  shouldCancel = pure (Right ())

  logger = Logger (\_ _ -> pure ())
