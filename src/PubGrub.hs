{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
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

import Control.Monad.Trans.Class qualified as Except
import Control.Monad.Trans.Except qualified as Except
import Data.Bifunctor (first)
import Data.Foldable qualified as Seq (toList)
import Data.Hashable (Hashable)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import PubGrub.DependencyProvider (
  Dependencies (..),
  DependencyProvider (..),
  IsVersionSet (..),
  PackageResolutionStats,
  conflictCount,
 )
import PubGrub.Error (Error (..))
import PubGrub.Internal.AddedDependencies qualified as AddedDeps
import PubGrub.Internal.Arena.HashArena (HashArena)
import PubGrub.Internal.Arena.HashArena qualified as HashArena
import PubGrub.Internal.Arena.IdMap (Id)
import PubGrub.Internal.Arena.SeqArena (SeqArena)
import PubGrub.Internal.Arena.SeqArena qualified as SeqArena
import PubGrub.Internal.Incompatibility (Incompatibility)
import PubGrub.Internal.Incompatibility qualified as Incompatibility
import PubGrub.Internal.PartialSolution (PartialSolution)
import PubGrub.Internal.PartialSolution qualified as PartialSolution
import PubGrub.Internal.ResolveState qualified as ResolveState

-- | Finds a set of packages satisfying dependency bounds for a given package +
-- version pair.
--
-- It consists in efficiently finding a set of packages and versions that
-- satisfy all the constraints of a given project dependencies. In addition,
-- when that is not possible, PubGrub tries to provide a very human-readable and
-- clear explanation as to why that failed.
resolve ::
  forall package versionSet incompat err m.
  ( (Show package, Hashable package)
  , IsVersionSet versionSet
  , Monad m
  ) =>
  DependencyProvider package versionSet incompat err m ->
  package ->
  Version versionSet ->
  m (Either (Error package versionSet incompat err) (Map package (Version versionSet)))
resolve provider package version =
  go
    state0
    Map.empty
    AddedDeps.new
    state0.rootPackage
 where
  state0 = ResolveState.init @_ @versionSet package version

  mapError f = fmap (first f)
  fromEitherWith f = Except.ExceptT . mapError f

  showT :: (Show a) => a -> Text
  showT = Text.pack . show

  -- TODO: Replace ExceptT with CPS monad
  go state0 conflictTracker addedDeps next = Except.runExceptT $ do
    fromEitherWith ShouldCancelError provider.shouldCancel

    Except.lift . provider.logger.info . Text.concat $
      [ "unit_propagation: " <> showT next
      , " = "
      , "'" <> showT (state0.packageStore HashArena.! next) <> "'"
      ]

    (state1, satisfierCauses) <-
      fromEitherWith NoSolution $ ResolveState.unitPropagation provider.logger next state0

    undefined

--         for (affected, incompat) in satisfier_causes {
--             conflict_tracker
--                 .entry(affected)
--                 .or_default()
--                 .unit_propagation_affected += 1;
--             for conflict_package in state.conflict_packages(&incompat) {
--                 if conflict_package == affected {
--                     continue;
--                 }
--                 conflict_tracker
--                     .entry(conflict_package)
--                     .or_default()
--                     .unit_propagation_culprit += 1;
--             }
--         }
--
--         debug!(
--             "Partial solution after unit propagation: {}",
--             state.partial_solution.display(&state.package_store)
--         );
--
--         let Some((highest_priority_pkg, term_intersection)) =
--             state.partial_solution.pick_highest_priority_pkg(|p, r| {
--                 dependency_provider.prioritize(
--                     &state.package_store[p],
--                     r,
--                     conflict_tracker.entry(p).or_default(),
--                 )
--             })
--         else {
--             return Ok(SelectedDependencies(
--                 state
--                     .partial_solution
--                     .extract_solution()
--                     .map(|(p, v)| (state.package_store[p].clone(), v))
--                     .collect(),
--             ));
--         };
--         next = highest_priority_pkg;
--
--         let decision = dependency_provider
--             .choose_version(&state.package_store[next], term_intersection)
--             .map_err(|err| PubGrubError::ErrorChoosingVersion {
--                 package: state.package_store[next].clone(),
--                 source: err,
--             })?;
--
--         info!(
--             "DP chose: {:?} = '{}' @ {:?}",
--             next, state.package_store[next], decision
--         );
--
--         // Pick the next compatible version.
--         let v = match decision {
--             None => {
--                 let versions = term_intersection.clone();
--                 state.add_no_versions(next, versions);
--                 continue;
--             }
--             Some(x) => x,
--         };
--
--         if !term_intersection.contains(&v) {
--             panic!(
--                 "`choose_version` picked an incompatible version for package {}, {} is not in {}",
--                 state.package_store[next], v, term_intersection
--             );
--         }
--
--         let is_new_dependency =
--             added_dependencies.insert(next, v.clone(), state.partial_solution.has_backtracked());
--
--         if is_new_dependency {
--             // Retrieve that package dependencies.
--             let p = next;
--             let dependencies = dependency_provider
--                 .get_dependencies(&state.package_store[p], &v)
--                 .map_err(|err| PubGrubError::ErrorRetrievingDependencies {
--                     package: state.package_store[p].clone(),
--                     version: v.clone(),
--                     source: err,
--                 })?;
--
--             let dependencies = match dependencies {
--                 Dependencies::Unavailable(reason) => {
--                     state.add_unavailable(p, DP::VS::singleton(v), reason);
--                     continue;
--                 }
--                 Dependencies::Available(x) => x,
--             };
--
--             // Add that package and version if the dependencies are not problematic.
--             let versions = <DP::VS as VersionSet>::singleton(v.clone());
--             if let Some(conflict) =
--                 state.add_package_version_dependencies(p, v, versions, dependencies)
--             {
--                 conflict_tracker.entry(p).or_default().dependencies_affected += 1;
--                 for incompat_package in state.conflict_packages(&conflict) {
--                     if incompat_package == p {
--                         continue;
--                     }
--                     conflict_tracker
--                         .entry(incompat_package)
--                         .or_default()
--                         .dependencies_culprit += 1;
--                 }
--             }
--         } else {
--             // `dep_incompats` are already in `incompatibilities` so we know there are not satisfied
--             // terms and can add the decision directly.
--             info!(
--                 "add_decision (not first time): {:?} = '{}' @ {}",
--                 next, state.package_store[next], v
--             );
--             state.partial_solution.add_decision(next, v);
--         }
--     }
