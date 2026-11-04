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

import Control.Applicative (asum)
import Control.Monad (when)
import Data.Foldable (foldlM)
import Data.Foldable qualified as Seq (toList)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import PubGrub.DependencyProvider (
  Dependencies,
  Dependencies' (..),
  IsDependencyProvider,
  PackageResolutionStats,
  conflictCount,
 )
import PubGrub.DependencyProvider qualified as DP
import PubGrub.Error (PubGrubError, PubGrubError' (..))
import PubGrub.Solver.Incompatibilities qualified as Incompatibilities
import PubGrub.Solver.Incompatibility (Incompatibility)
import PubGrub.Solver.Incompatibility qualified as Incompatibility
import PubGrub.Solver.Monad (
  SolverM,
  SolverState (..),
  logger,
  modifyState,
  readState,
  runSolverM,
  throwError,
 )
import PubGrub.Solver.PartialSolution qualified as PartialSolution
import PubGrub.Term (Term' (..))
import PubGrub.Term qualified as Term
import PubGrub.Utils.Text (showT)
import PubGrub.VersionRange qualified as VersionRange

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
      { rootPackage
      , rootVersion
      , partialSolution = PartialSolution.empty
      , incompats = Incompatibilities.singleton rootIncompat
      }
    -- Let `next` be the name of the root package.
    (resolveLoop rootPackage)
 where
  rootIncompat = Incompatibility.notRoot @_ @(DP.VersionRange provider) rootPackage rootVersion

resolveLoop ::
  (IsDependencyProvider provider) =>
  DP.Package provider ->
  SolverM provider (Resolution provider)
-- In a loop:
resolveLoop next = do
  -- Perform unit propagation on `next` to find new derivations.
  --
  -- If this causes an incompatibility to be satisfied by the partial solution,
  -- we have a conflict. Unit propagation will try to resolve the conflict. If
  -- this fails, version solving has failed and an error should be reported.
  logger.info $ "starting unit propagation: " <> showT next
  unitPropagation next
  readState (.partialSolution) >>= \partialSolution ->
    logger.debug $ "partial solution after unit propagation: " <> showT partialSolution

  -- Once there are no more derivations to be found, make a decision and set
  -- `next` to the package name returned by the decision-making process. Note
  -- that the first decision will always select the single available version of
  -- the root package.
  decisionMaking >>= \case
    Just next' -> resolveLoop next'
    -- Decision making may determine that there's no more work to do, in which
    -- case version solving is done and the partial solution represents a total
    -- solution.
    Nothing -> readState (.partialSolution.totalSolution)

-- | Perform unit propagation to discover new derivations.
--
-- Unit propagation combines the partial solution with the known
-- incompatibilities to derive new assignments. Given an incompatibility, if the
-- partial solution is inconclusive for exactly one term @t@ in that
-- incompatibility and satisfies the rest, then @t@ must be contradicted in
-- order for the incompatibility to be contradicted. Thus, we add @not t@ to the
-- partial solution as a derivation.
unitPropagation ::
  (IsDependencyProvider provider) =>
  DP.Package provider ->
  SolverM provider ()
unitPropagation =
  -- Let `changed` be a set containing the input package name.
  go1 . Set.singleton
 where
  -- While changed isn't empty:
  -- Remove an element from `changed`. Call it `package`.
  go1 changed = case Set.minView changed of
    Nothing -> pure ()
    Just (package, changed') -> do
      -- For each `incompatibility` that refers to `package` from newest to
      -- oldest (since conflict resolution tends to produce more general
      -- incompatibilities later on):
      packageIncompats <- readState $ \s -> s.incompats.getOrdered package
      foldlM go2 changed' packageIncompats >>= go1

  go2 changed incompat = do
    getSatisfiability incompat >>= \case
      -- If `incompatibility` is satisfied by the partial solution:
      Incompatibility.Satisfies -> do
        -- Run conflict resolution with `incompatibility`. If this succeeds, it
        -- returns an incompatibility that's guaranteed to be almost satisfied
        -- by the partial solution. Call this incompatibility's unsatisfied term
        -- `term`.
        logger.info $ "starting conflict resolution because incompatibility satisfied: " <> showT incompat
        rootIncompat <- conflictResolution incompat
        term <-
          getSatisfiability rootIncompat >>= \case
            Incompatibility.AlmostSatisfies t -> pure t
            satisfiability -> error $ "conflict resolution unexpectedly returned incompatibility that does not almost satisfy the partial solution: " <> show satisfiability

        -- Add `not term` to the partial solution with `incompatibility` as its
        -- cause.
        modifyState $ \s -> s{partialSolution = s.partialSolution.addDerivation term.negated rootIncompat}

        -- Replace `changed` with a set containing only `term`'s package name.
        pure $ Set.singleton term.package

      -- Otherwise, if the partial solution almost satisfies `incompatibility`:
      -- Call `incompatibility`'s unsatisfied term `term`.
      Incompatibility.AlmostSatisfies term -> do
        -- Add `not term` to the partial solution with `incompatibility` as its
        -- cause.
        modifyState $ \s -> s{partialSolution = s.partialSolution.addDerivation term.negated incompat}

        -- Add `term`'s package name to `changed`.
        pure $ Set.insert term.package changed

      -- <ignore, continue looping>
      _ -> pure changed

  getSatisfiability incompat = readState $ \s -> s.partialSolution.getSatisfiability incompat

-- | Run conflict resolution to find the root cause of the conflict.
--
-- When an incompatibility is satisfied by the partial solution, that indicates
-- that the partial solution's decisions aren't a subset of any total solution.
-- So we find the root cause of the conflict between the incompatibility and the
-- partial solution, backtrack the partial solution to before that point, and
-- return the root cause.
conflictResolution ::
  (IsDependencyProvider provider) =>
  Incompatibility provider ->
  SolverM provider (Incompatibility provider)
conflictResolution incompat0 = go incompat0
 where
  go incompat = do
    -- If `incompatibility` contains no terms, or if it contains a single
    -- positive term that refers to the root package version, that indicates
    -- that the root package can't be selected and thus that version solving has
    -- failed. Report an error with `incompatibility` as the root
    -- incompatibility.
    checkNoSolution incompat

    -- Find the earliest assignment in the partial solution such that
    -- `incompatibility` is satisfied by the partial solution up to and
    -- including that assignment. Call this `satisfier`, and call the term in
    -- `incompatibility` that refers to the same package `term`.
    assignments <- readState (.partialSolution.assignments)
    (satisfierIndex, satisfier) <-
      case asum . fmap (traverse (getSatisfier incompat)) . indexed . Seq.inits $ assignments of
        Nothing -> error $ "conflict resolution unexpectedly failed to find satisfier: " <> show (incompat, assignments)
        Just (i, satisfier) -> pure (i, satisfier)
    term <-
      case incompat.get satisfier.package of
        Nothing -> error $ "conflict resolution unexpectedly failed to get satisfier term: " <> show (incompat, satisfier)
        Just term -> pure term

    -- Find the earliest assignment in the partial solution _before_ `satisfier`
    -- such that `incompatibility` is satisfied by the partial solution up to
    -- and including that assignment plus `satisfier`. Call this
    -- `previousSatisfier`.
    let previousSatisfier =
          asum
            . fmap (getSatisfier incompat . (Seq.|> satisfier))
            . Seq.inits
            . Seq.take satisfierIndex
            $ assignments

    -- Let `previousSatisfierLevel` be `previousSatisfier`'s decision level, or
    -- decision level 1 if there is no `previousSatisfier`.
    let previousSatisfierLevel = maybe 1 (.decisionLevel) previousSatisfier

    -- If `satisfier` is a decision or if `previousSatisfierLevel` is different
    -- than `satisfier`'s decision level:
    if satisfier.isDecision || previousSatisfierLevel /= satisfier.decisionLevel
      then do
        -- If `incompatibility` is different than the original input, add it to
        -- the solver's incompatibility set. (If the conflicting incompatibility
        -- was added lazily during decision making, it may not have a distinct
        -- root cause.)
        when (incompat /= incompat0) $ do
          modifyState $ \s -> s{incompats = undefined s.incompats}

        -- Backtrack by removing all assignments whose decision level is greater
        -- than `previousSatisfierLevel` from the partial solution.
        _ <- undefined

        -- Return `incompatibility`.
        pure incompat
      else do
        -- Otherwise, let `priorCause` be the union of the terms in
        -- `incompatibility` and the terms in `satisfier`'s cause, minus the
        -- terms referring to `satisfier`'s package.
        priorCause <- undefined

        -- If `satisfier` doesn't satisfy `term`, add `not (satisfier \ term)`
        -- to `priorCause`.
        _ <- undefined term

        -- Set `incompatibility` to `priorCause`.
        go priorCause

  checkNoSolution incompat = do
    isRootTerm <- readState $ \s term -> term.package == s.rootPackage && term.range == VersionRange.singleton s.rootVersion
    let isNoSolution =
          case incompat.toList of
            [] -> True
            [term@Term{sign = Term.Positive}] -> isRootTerm term
            _ -> False
    when isNoSolution $ do
      throwError . NoSolution . Incompatibility.toDerivationTree $ incompat

  getSatisfier incompat assignments
    | Incompatibility.Satisfies <- assignments.getSatisfiability incompat =
        fmap NonEmpty.last . NonEmpty.nonEmpty . Seq.toList $ assignments
    | otherwise = Nothing

  indexed :: Seq a -> Seq (Int, a)
  indexed s = Seq.zip (Seq.fromFunction (Seq.length s) id) s

decisionMaking :: SolverM provider (Maybe (DP.Package provider))
decisionMaking = do
  -- Let `package` be a package with a positive derivation but no decision in
  -- the partial solution, and let `term` be the intersection of all assignments
  -- in the partial solution referring to that package.
  package <- undefined
  term <- undefined

  -- Let `version` be a version of `package` that matches `term`.
  version_ <- undefined package term

  case version_ of
    Nothing -> do
      -- If there is no such `version`, add an incompatibility `{term}` to the
      -- incompatibility set and return `package`'s name. This tells Pubgrub to
      -- avoid this range of versions in the future.
      _ <- undefined term
      pure package
    Just version -> do
      -- Add each `incompatibility` from `version`'s dependencies to the
      -- incompatibility set if it's not already there.
      _ <- undefined version

      -- Add `version` to the partial solution as a decision, unless this would
      -- produce a conflict in any of the new incompatibilities.
      _ <- undefined version

      -- Return `package`'s name.
      pure package
