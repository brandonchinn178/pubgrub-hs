{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoFieldSelectors #-}

module PubGrub.Internal.ResolveState (
  ResolveState (..),
  MergedDeps,
  DependencyKey (..),
  init,
  unitPropagation,
) where

import Data.Hashable (Hashable)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import PubGrub.Error (DerivationTree)
import PubGrub.Internal.Arena.HashArena (HashArena)
import PubGrub.Internal.Arena.HashArena qualified as HashArena
import PubGrub.Internal.Arena.IdMap (Id)
import PubGrub.Internal.Arena.SeqArena (SeqArena)
import PubGrub.Internal.Arena.SeqArena qualified as SeqArena
import PubGrub.Internal.Incompatibility (Incompatibility)
import PubGrub.Internal.Incompatibility qualified as Incompatibility
import PubGrub.Internal.PartialSolution (PartialSolution)
import PubGrub.Internal.PartialSolution qualified as PartialSolution
import PubGrub.Logger (Logger)
import PubGrub.VersionSet (IsVersionSet, Version)
import Prelude hiding (init)

data ResolveState package versionSet incompat priority = ResolveState
  { rootPackage :: !(Id package)
  , rootVersion :: !(Version versionSet)
  , allIncompats :: !(Map (Id package) [Id (Incompatibility package versionSet incompat)])
  , mergedDeps :: !(MergedDeps package (Id (Incompatibility package versionSet incompat)))
  , partialSolution :: !(PartialSolution package versionSet incompat priority)
  , incompatStore :: !(SeqArena (Incompatibility package versionSet incompat))
  , packageStore :: !(HashArena package)
  }

type MergedDeps package incompatId = Map (DependencyKey package) [incompatId]

data DependencyKey package = DependencyKey
  { dependent :: Id package
  , dependency :: Id package
  , rangeHash :: Int
  }

init ::
  forall package versionSet incompat priority.
  (Hashable package, IsVersionSet versionSet) =>
  package ->
  Version versionSet ->
  ResolveState package versionSet incompat priority
init rootPackage rootVersion =
  ResolveState
    { rootPackage = rootPackageId
    , rootVersion
    , allIncompats
    , partialSolution = PartialSolution.empty
    , incompatStore
    , packageStore
    , mergedDeps = Map.empty
    }
 where
  packageStore0 = HashArena.empty
  (packageStore, rootPackageId) = HashArena.alloc rootPackage packageStore0

  incompatStore0 = SeqArena.empty
  (incompatStore, notRootId) = SeqArena.alloc (Incompatibility.notRoot rootPackageId rootVersion) incompatStore0

  allIncompats = Map.singleton rootPackageId [notRootId]

newtype ConflictId package versionSet incompat = ConflictId (Id (Incompatibility package versionSet incompat))

-- | Unit propagation is the core mechanism of the solving algorithm.
--
-- Reference: https://github.com/dart-lang/pub/blob/master/doc/solver.md#unit-propagation
--
-- For each package with a satisfied incompatibility, return the package and the
-- root cause conflict.
unitPropagation ::
  (Monad m) =>
  Logger m ->
  Id package ->
  ResolveState package versionSet incompat priority ->
  m
    ( Either
        (DerivationTree package versionSet incompat)
        (ResolveState package versionSet incompat priority, [(Id package, ConflictId package versionSet incompat)])
    )
unitPropagation package initialState = go initialState [] [package]
 where
  go state0 satisfierCauses = \case
    [] -> pure $ Right (state0, map (fmap ConflictId) satisfierCauses)
    currentPkg : buf -> do
      conflictId <- findFirstConflict _
      case conflictId of
        Nothing -> pure ()
        --            if let Some(incompat_id) = conflict_id {
        --                let (package_almost, root_cause) = self
        --                    .conflict_resolution(incompat_id, &mut satisfier_causes)
        --                    .map_err(|terminal_incompat_id| {
        --                        self.build_derivation_tree(terminal_incompat_id)
        --                    })?;
        --                self.unit_propagation_buffer.clear();
        --                self.unit_propagation_buffer.push(package_almost);
        --                // Add to the partial solution with incompat as cause.
        --                self.partial_solution.add_derivation(
        --                    package_almost,
        --                    root_cause,
        --                    &self.incompatibility_store,
        --                );
        --                // After conflict resolution and the partial solution update,
        --                // the root cause incompatibility is now contradicted.
        --                self.partial_solution
        --                    .mark_contradicted(&mut self.incompatibility_store[root_cause]);
        --            }
        Just _ -> pure ()
      go _ _ _

  findFirstConflict _ = do

--            // Iterate over incompatibilities in reverse order
--            // to evaluate first the newest incompatibilities.
--            let mut conflict_id = None;
--            // We only care about incompatibilities if it contains the current package.
--            for &incompat_id in self.incompatibilities[&current_package].iter().rev() {
--                if self
--                    .partial_solution
--                    .is_contradicted(&self.incompatibility_store[incompat_id])
--                {
--                    continue;
--                }
--                let current_incompat = &self.incompatibility_store[incompat_id];
--                match self.partial_solution.relation(current_incompat) {
--                    // If the partial solution satisfies the incompatibility
--                    // we must perform conflict resolution.
--                    Relation::Satisfied => {
--                        log::info!(
--                            "Start conflict resolution because incompat satisfied:\n   {}",
--                            current_incompat.display(&self.package_store)
--                        );
--                        conflict_id = Some(incompat_id);
--                        break;
--                    }
--                    Relation::AlmostSatisfied(package_almost) => {
--                        // Add `package_almost` to the `unit_propagation_buffer` set.
--                        // Putting items in `unit_propagation_buffer` more than once waste cycles,
--                        // but so does allocating a hash map and hashing each item.
--                        // In practice `unit_propagation_buffer` is small enough that we can just do a linear scan.
--                        if !self.unit_propagation_buffer.contains(&package_almost) {
--                            self.unit_propagation_buffer.push(package_almost);
--                        }
--                        // Add (not term) to the partial solution with incompat as cause.
--                        self.partial_solution.add_derivation(
--                            package_almost,
--                            incompat_id,
--                            &self.incompatibility_store,
--                        );
--                        // With the partial solution updated, the incompatibility is now contradicted.
--                        self.partial_solution
--                            .mark_contradicted(&mut self.incompatibility_store[incompat_id]);
--                    }
--                    Relation::Contradicted(_) => {
--                        self.partial_solution
--                            .mark_contradicted(&mut self.incompatibility_store[incompat_id]);
--                    }
--                    _ => {}
--                }
--            }
