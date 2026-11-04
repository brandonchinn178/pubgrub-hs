{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoFieldSelectors #-}

module PubGrub.DependencyProvider (
  IsDependencyProvider (..),
  Version,
  Dependencies,
  Dependencies' (..),

  -- * Constraint helpers
  IsPackage,
  IsUnavailableError,
  IsResolutionError,
  IsPriority,

  -- * PackageResolutionStats
  PackageResolutionStats,
  conflictCount,

  -- * Built-in dependency provider implementations
  DependencyProvider (..),
  toImpl,
  OfflineDependencyProvider (..),
) where

import Data.Functor.Identity (Identity)
import Data.Hashable (Hashable)
import Data.Kind (Type)
import Data.List (find)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Void (Void)
import Data.Word (Word32)
import PubGrub.Internal.PackageResolutionStats (PackageResolutionStats, conflictCount)
import PubGrub.Logger (Logger (..))
import PubGrub.VersionRange (IsVersionRange)
import PubGrub.VersionRange qualified as VersionRange

-- | The interface for package resolution.
--
-- All types are fully polymorphic so that the caller has full control over how
-- package names, version numbers and requirements, etc. are represented.
class
  ( IsPackage (Package provider)
  , IsVersionRange (VersionRange provider)
  , IsUnavailableError (UnavailableError provider)
  , IsResolutionError (ResolutionError provider)
  , Monad (ResolveMonad provider)
  , IsPriority (Priority provider)
  ) =>
  IsDependencyProvider provider
  where
  type Package provider
  type VersionRange provider
  type UnavailableError provider
  type ResolutionError provider
  type ResolveMonad provider :: Type -> Type
  type Priority provider

  prioritize ::
    provider ->
    Package provider ->
    VersionRange provider ->
    PackageResolutionStats ->
    ResolveMonad provider (Priority provider)

  chooseVersion ::
    provider ->
    Package provider ->
    VersionRange provider ->
    ResolveMonad provider (Either (ResolutionError provider) (Maybe (Version provider)))

  getDependencies ::
    provider ->
    Package provider ->
    Version provider ->
    ResolveMonad provider (Either (ResolutionError provider) (Dependencies provider))

  shouldCancel :: provider -> ResolveMonad provider (Either (ResolutionError provider) ())
  shouldCancel _ = pure (Right ())

  logger :: provider -> Logger (ResolveMonad provider)
  logger _ = Logger (\_ _ -> pure ())

type IsPackage package =
  ( Show package
  , Ord package
  , Hashable package
  )

type IsUnavailableError unavailErr =
  ( Show unavailErr
  , Eq unavailErr
  )

type IsResolutionError err =
  ( Show err
  , Eq err
  )

type IsPriority priority =
  ( Show priority
  , Eq priority
  )

type Version provider = VersionRange.Version (VersionRange provider)

type Dependencies provider =
  Dependencies'
    (Package provider)
    (VersionRange provider)
    (UnavailableError provider)

data Dependencies' package range unavailErr
  = Available [(package, range)]
  | Unavailable unavailErr
  deriving (Show, Eq)

-- | A record-of-functions for specifying a DependencyProvider instance in-line.
data DependencyProvider provider = DependencyProvider
  { prioritize ::
      Package provider ->
      VersionRange provider ->
      PackageResolutionStats ->
      ResolveMonad provider (Priority provider)
  , chooseVersion ::
      Package provider ->
      VersionRange provider ->
      ResolveMonad provider (Either (ResolutionError provider) (Maybe (Version provider)))
  , getDependencies ::
      Package provider ->
      Version provider ->
      ResolveMonad provider (Either (ResolutionError provider) (Dependencies provider))
  , shouldCancel ::
      ResolveMonad provider (Either (ResolutionError provider) ())
  , logger ::
      Logger (ResolveMonad provider)
  }

toImpl :: (IsDependencyProvider provider) => provider -> DependencyProvider provider
toImpl provider =
  DependencyProvider
    { prioritize = prioritize provider
    , chooseVersion = chooseVersion provider
    , getDependencies = getDependencies provider
    , shouldCancel = shouldCancel provider
    , logger = logger provider
    }

instance
  (IsDependencyProvider provider) =>
  IsDependencyProvider (DependencyProvider provider)
  where
  type Package (DependencyProvider provider) = Package provider
  type VersionRange (DependencyProvider provider) = VersionRange provider
  type UnavailableError (DependencyProvider provider) = UnavailableError provider
  type ResolutionError (DependencyProvider provider) = ResolutionError provider
  type ResolveMonad (DependencyProvider provider) = ResolveMonad provider
  type Priority (DependencyProvider provider) = Priority provider

  prioritize = (.prioritize)
  chooseVersion = (.chooseVersion)
  getDependencies = (.getDependencies)
  shouldCancel = (.shouldCancel)
  logger = (.logger)

newtype OfflineDependencyProvider package range
  = OfflineDependencyProvider
      (Map package (Map (VersionRange.Version range) [(package, range)]))

instance
  ( IsPackage package
  , IsVersionRange range
  ) =>
  IsDependencyProvider (OfflineDependencyProvider package range)
  where
  type Package (OfflineDependencyProvider package range) = package
  type VersionRange (OfflineDependencyProvider package range) = range
  type UnavailableError (OfflineDependencyProvider package range) = Text
  type ResolutionError (OfflineDependencyProvider package range) = Void
  type ResolveMonad (OfflineDependencyProvider package range) = Identity
  type Priority (OfflineDependencyProvider package range) = (Word32, Down Int)

  prioritize (OfflineDependencyProvider deps) package range stats = do
    let versionCount =
          case Map.lookup package deps of
            Nothing -> 0
            Just versions -> length . filter (`VersionRange.elem` range) $ Map.keys versions
    pure (stats.conflictCount, Down versionCount)

  chooseVersion (OfflineDependencyProvider deps) package range = do
    pure . Right $ do
      versions <- Map.lookup package deps
      find (`VersionRange.elem` range) . reverse $ Map.keys versions

  getDependencies (OfflineDependencyProvider deps) package version = do
    pure . Right . fromMaybe unavailable $ do
      versions <- Map.lookup package deps
      deps' <- Map.lookup version versions
      pure $ Available deps'
   where
    unavailable = Unavailable "package dependencies are not provided"
