{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module PubGrub (
  resolve,
  Resolution,

  -- * IsDependencyProvider
  IsDependencyProvider (..),
  Dependencies,
  Dependencies' (..),

  -- ** PackageResolutionStats
  PackageResolutionStats,
  conflictCount,
) where

import Data.Foldable (foldlM)
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Set qualified as Set
import PubGrub.DependencyProvider (
  Dependencies,
  Dependencies' (..),
  IsDependencyProvider,
  PackageResolutionStats,
  conflictCount,
 )
import PubGrub.DependencyProvider qualified as DP
import PubGrub.Error (PubGrubError)
import PubGrub.Solver.Incompatibilities qualified as Incompatibilities
import PubGrub.Solver.Incompatibility (Incompatibility)
import PubGrub.Solver.Incompatibility qualified as Incompability
import PubGrub.Solver.Incompatibility qualified as Incompatibility
import PubGrub.Solver.Monad (
  SolverM,
  SolverState (..),
  getState,
  logger,
  runSolverM,
  setState,
 )
import PubGrub.Solver.PartialSolution qualified as PartialSolution
import PubGrub.Term (Term, Term' (..))
import PubGrub.Utils.Text (showT)

-- | A successful resolution mapping package to their resolved version.
type Resolution provider = Map (DP.Package provider) (DP.Version provider)

-- | Finds a set of packages satisfying dependency bounds for a given package +
-- version pair.
--
-- It consists in efficiently finding a set of packages and versions that
-- satisfy all the constraints of a given project dependencies. In addition,
-- when that is not possible, PubGrub tries to provide a very human-readable and
-- clear explanation as to why that failed.
resolve ::
  forall provider.
  (IsDependencyProvider provider) =>
  provider ->
  DP.Package provider ->
  DP.Version provider ->
  DP.ResolveMonad provider (Either (PubGrubError provider) (Resolution provider))
resolve provider rootPackage rootVersion =
  -- Begin by adding an incompatibility indicating that the current version of
  -- the root package must be selected (for example, `{not root 1.0.0}`). Note
  -- that although there's only one version of the root package, this is just an
  -- incompatibility, not an assignment.
  runSolverM
    provider
    SolverState
      { partialSolution = PartialSolution.empty
      , incompats = Incompatibilities.singleton rootIncompat
      }
    (go rootPackage)
 where
  rootIncompat = Incompatibility.notRoot @_ @(DP.VersionRange provider) rootPackage rootVersion

  go next = do
    -- Perform unit propagation on next to find new derivations.
    --
    -- If this causes an incompatibility to be satisfied by the partial
    -- solution, we have a conflict. Unit propagation will try to resolve the
    -- conflict. If this fails, version solving has failed and an error should
    -- be reported.
    logger.info $ "starting unit propagation: " <> showT next
    newDerivations <- unitPropagation $ Set.singleton next
    --
    --     Once there are no more derivations to be found, make a decision and set next to the package name returned by the decision-making process. Note that the first decision will always select the single available version of the root package.
    --         Decision making may determine that there's no more work to do, in which case version solving is done and the partial solution represents a total solution.
    undefined next provider newDerivations

  unitPropagation changed = case Set.minView changed of
    Nothing -> undefined
    Just (package, changed') -> do
      state <- getState
      let packageIncompats = state.incompats.getOrdered package
      foldlM checkIncompatibility changed' packageIncompats >>= unitPropagation

checkIncompatibility ::
  (IsDependencyProvider provider) =>
  Set (DP.Package provider) ->
  Incompatibility provider ->
  SolverM provider (Set (DP.Package provider))
checkIncompatibility changed incompat = do
  state <- getState
  case state.partialSolution.getSatisfiability incompat of
    Incompability.Satisfies -> do
      (rootIncompat, term) <- conflictResolution
      setState state{partialSolution = state.partialSolution.add term.negated rootIncompat}
      pure $ Set.singleton term.package
    Incompability.AlmostSatisfies term -> do
      setState state{partialSolution = state.partialSolution.add term.negated incompat}
      pure $ Set.insert term.package changed
    _ -> pure changed

conflictResolution :: SolverM provider (Incompatibility provider, Term provider)
conflictResolution = pure (undefined, undefined)
