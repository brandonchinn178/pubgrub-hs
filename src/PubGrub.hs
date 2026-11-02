{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module PubGrub (
  resolve,

  -- * DependencyProvider
  DependencyProvider (..),
  IsVersionSet (..),
  Dependencies (..),

  -- ** PackageResolutionStats
  PackageResolutionStats,
  conflictCount,
) where

import Data.Foldable qualified as Seq (toList)
import Data.Hashable (Hashable)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import PubGrub.DependencyProvider (
  Dependencies (..),
  DependencyProvider (..),
  IsVersionSet (..),
  PackageResolutionStats,
  conflictCount,
 )
import PubGrub.Error (Error)
import PubGrub.Internal.Arena.HashArena (HashArena)
import PubGrub.Internal.Arena.HashArena qualified as HashArena
import PubGrub.Internal.Arena.IdMap (Id)
import PubGrub.Internal.Arena.SeqArena (SeqArena)
import PubGrub.Internal.Arena.SeqArena qualified as SeqArena
import PubGrub.Internal.Incompatibility (Incompatibility)
import PubGrub.Internal.Incompatibility qualified as Incompatibility
import PubGrub.Internal.PartialSolution (PartialSolution)
import PubGrub.Internal.PartialSolution qualified as PartialSolution

-- | Finds a set of packages satisfying dependency bounds for a given package +
-- version pair.
--
-- It consists in efficiently finding a set of packages and versions that
-- satisfy all the constraints of a given project dependencies. In addition,
-- when that is not possible, PubGrub tries to provide a very human-readable and
-- clear explanation as to why that failed.
resolve ::
  forall package versionSet incompat err m.
  (Hashable package, IsVersionSet versionSet) =>
  DependencyProvider package versionSet incompat err m ->
  package ->
  Version versionSet ->
  m (Either (Error package versionSet incompat err) (Map package (Version versionSet)))
resolve provider package version =
  go
    state0
    Map.empty
    newAddedDeps
    state0.rootPackage
 where
  state0 = initState @_ @versionSet package version

  go = undefined provider (insertAddedDeps undefined version)

-- go state conflictTracker addedDeps next = undefined

{----- State -----}

data State package versionSet incompat priority = State
  { rootPackage :: !(Id package)
  , rootVersion :: !(Version versionSet)
  , allIncompats :: !(Map (Id package) [Id (Incompatibility package versionSet incompat)])
  , mergedDeps :: !(MergedDeps package (Id (Incompatibility package versionSet incompat)))
  , partialSolution :: !(PartialSolution package versionSet incompat priority)
  , incompatStore :: !(SeqArena (Incompatibility package versionSet incompat))
  , packageStore :: !(HashArena package)
  , unitPropBuffer :: ![Id package]
  }

type MergedDeps package incompatId = Map (DependencyKey package) [incompatId]

data DependencyKey package

-- = DependencyKey
-- { dependent :: Id package
-- , dependency :: Id package
-- , rangeHash :: Int
-- }

initState ::
  forall package versionSet incompat priority.
  (Hashable package, IsVersionSet versionSet) =>
  package ->
  Version versionSet ->
  State package versionSet incompat priority
initState rootPackage rootVersion =
  State
    { rootPackage = rootPackageId
    , rootVersion
    , allIncompats
    , partialSolution = PartialSolution.empty
    , incompatStore
    , packageStore
    , unitPropBuffer = []
    , mergedDeps = Map.empty
    }
 where
  packageStore0 = HashArena.empty
  (packageStore, rootPackageId) = HashArena.alloc rootPackage packageStore0

  incompatStore0 = SeqArena.empty
  (incompatStore, notRootId) = SeqArena.alloc (Incompatibility.notRoot rootPackageId rootVersion) incompatStore0

  allIncompats = Map.singleton rootPackageId [notRootId]

{----- AddedDependencies -----}

-- | Tracks package versions whose dependency incompatibilities have already
-- been added.
--
-- Before the first backtrack, entries are stored in a compact history because
-- decisions cannot be revisited. The history is promoted to a deduplicating map
-- only once revisits become possible.
data AddedDependencies package version = AddedDependencies
  { beforeFirstBacktrack :: !(Seq (Id package, version))
  , afterFirstBacktrack :: !(Maybe (Map (Id package) (Set version)))
  }

newAddedDeps :: AddedDependencies package version
newAddedDeps =
  AddedDependencies
    { beforeFirstBacktrack = Seq.empty
    , afterFirstBacktrack = Nothing
    }

insertAddedDeps ::
  (Ord version) =>
  Id package ->
  version ->
  Bool ->
  AddedDependencies package version ->
  (AddedDependencies package version, Bool)
insertAddedDeps package version hasBacktracked addedDeps
  | not hasBacktracked =
      let addedDeps' =
            addedDeps
              { beforeFirstBacktrack = addedDeps.beforeFirstBacktrack Seq.|> (package, version)
              }
       in (addedDeps', True)
  | otherwise =
      let (packages, addedDeps') =
            case addedDeps.afterFirstBacktrack of
              Nothing ->
                ( fromBeforeFirstBacktrack addedDeps.beforeFirstBacktrack
                , addedDeps{beforeFirstBacktrack = Seq.empty}
                )
              Just pkgs -> (pkgs, addedDeps)
          (isNew, packages') = insertDepReturnIsNew package packages
       in (addedDeps'{afterFirstBacktrack = Just packages'}, isNew)
 where
  fromBeforeFirstBacktrack = fmap Set.singleton . Map.fromList . Seq.toList
  insertDepReturnIsNew =
    Map.alterF
      ( fmap Just . \case
          Nothing -> (True, Set.singleton version)
          Just versions -> insertSetReturnIsNew version versions
      )
  insertSetReturnIsNew = Set.alterF (\present -> (not present, True))
