{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoFieldSelectors #-}

module PubGrub.Solver.PartialSolution (
  PartialSolution,
  PartialSolution' (..),
  Assignment,
  Assignment' (..),

  -- * Constructors
  empty,
) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Word (Word32)
import GHC.Records (HasField (..))
import PubGrub.DependencyProvider qualified as DP
import PubGrub.Solver.Incompatibility (Incompatibility')
import PubGrub.Solver.Incompatibility qualified as Incompatibility
import PubGrub.Term (Term')
import PubGrub.VersionRange (IsVersionRange)

type PartialSolution provider =
  PartialSolution' (DP.Package provider) (DP.Version provider) (DP.VersionRange provider)

-- | An ordered list of assignments representing PubGrub's current best guess
-- about what's true for the eventual set of package versions that will comprise
-- the total solution. The solver continuously modifies its partial solution as
-- it progresses through the search space.
newtype PartialSolution' package version range
  = PartialSolution (Map package (Assignment' package version range))

type Assignment provider =
  Assignment' (DP.Package provider) (DP.Version provider) (DP.VersionRange provider)

data Assignment' package version range
  = -- | A decision is an assignment that selects individual package versions.
    -- It represents guesses PubGrub has made about versions that might work.
    Assignment_Decision !version !DecisionLevel
  | -- | A derivation is an assignment that selects version ranges. It
    -- represents terms that must be true given the previous assignments in the
    -- partial solution and any incompatibilities we know about.
    Assignment_Derivation !(Term' package range) !DecisionLevel

-- | The number of decisions at or before it in the partial solution other than
-- the root package.
--
-- This is used to determine how far back to look for root causes during
-- conflict resolution, and how far back to jump when a conflict is found.
type DecisionLevel = Word32

empty :: PartialSolution' package version range
empty = PartialSolution Map.empty

instance
  HasField
    "add"
    (PartialSolution' package version range)
    ( Term' package range ->
      Incompatibility' package range ->
      PartialSolution' package version range
    )
  where
  getField _ _ _ = undefined

-- TODO: test
instance
  (Eq package, IsVersionRange range) =>
  HasField
    "getSatisfiability"
    (PartialSolution' package version range)
    ( Incompatibility' package range ->
      Incompatibility.Satisfiability' package range
    )
  where
  getField = Incompatibility.getSatisfiability . toTerms
    where
      toTerms = undefined
