{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoFieldSelectors #-}

module PubGrub.Solver.PartialSolution (
  PartialSolution,
  PartialSolution',
  Assignment,
  Assignment' (..),
  AssignmentDecision,
  AssignmentDecision' (..),
  AssignmentDerivation,
  AssignmentDerivation' (..),

  -- * Constructors
  empty,
) where

import Data.Foldable qualified as Seq (toList)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Word (Word32)
import GHC.Records (HasField (..))
import PubGrub.DependencyProvider qualified as DP
import PubGrub.Solver.Incompatibility (Incompatibility')
import PubGrub.Solver.Incompatibility qualified as Incompatibility
import PubGrub.Term (Term')
import PubGrub.Term qualified as Term
import PubGrub.VersionRange (IsVersionRange)
import PubGrub.VersionRange qualified as VersionRange

type PartialSolution provider =
  PartialSolution'
    (DP.Package provider)
    (DP.Version provider)
    (DP.VersionRange provider)

-- | An ordered list of assignments representing PubGrub's current best guess
-- about what's true for the eventual set of package versions that will comprise
-- the total solution. The solver continuously modifies its partial solution as
-- it progresses through the search space.
data PartialSolution' package version range
  = PartialSolution
  { _assignments :: Seq (Assignment' package version range)
  , currDecisionLevel :: DecisionLevel
  -- ^ Invariant: currDecisionLevel = length (filter isDecision assignments)
  }
  deriving (Show, Eq)

type Assignment provider =
  Assignment'
    (DP.Package provider)
    (DP.Version provider)
    (DP.VersionRange provider)

data Assignment' package version range
  = Assignment_Decision (AssignmentDecision' package version)
  | Assignment_Derivation (AssignmentDerivation' package range)
  deriving (Show, Eq)

instance HasField "isDecision" (Assignment' package version range) Bool where
  getField = \case
    Assignment_Decision{} -> True
    _ -> False
instance HasField "decisionLevel" (Assignment' package version range) DecisionLevel where
  getField = \case
    Assignment_Decision assignment -> assignment.decisionLevel
    Assignment_Derivation assignment -> assignment.decisionLevel
instance HasField "package" (Assignment' package version range) package where
  getField = \case
    Assignment_Decision assignment -> assignment.package
    Assignment_Derivation assignment -> assignment.term.package

type AssignmentDecision provider =
  AssignmentDecision' (DP.Package provider) (DP.Version provider)

-- | An assignment that selects individual package versions.
--
-- It represents guesses PubGrub has made about versions that might work.
data AssignmentDecision' package version
  = AssignmentDecision
  { package :: package
  , version :: version
  , decisionLevel :: DecisionLevel
  }
  deriving (Show, Eq)

type AssignmentDerivation provider =
  AssignmentDerivation' (DP.Package provider) (DP.VersionRange provider)

-- | An assignment that selects version ranges.
--
-- It represents terms that must be true given the previous assignments in the
-- partial solution and any incompatibilities we know about.
data AssignmentDerivation' package range
  = AssignmentDerivation
  { term :: Term' package range
  , cause :: Incompatibility' package range
  , decisionLevel :: DecisionLevel
  }
  deriving (Show, Eq)

-- | The number of decisions at or before it in the partial solution other than
-- the root package.
--
-- This is used to determine how far back to look for root causes during
-- conflict resolution, and how far back to jump when a conflict is found.
type DecisionLevel = Word32

{----- PartialSolution API -----}

empty :: PartialSolution' package version range
empty =
  PartialSolution
    { _assignments = Seq.empty
    , currDecisionLevel = 0
    }

instance
  HasField
    "assignments"
    (PartialSolution' package version range)
    (Seq (Assignment' package version range))
  where
  getField = (._assignments)

instance
  (Ord package) =>
  HasField
    "totalSolution"
    (PartialSolution' package version range)
    (Map package version)
  where
  getField partialSolution =
    Map.fromList
      [ (a.package, a.version)
      | Assignment_Decision a <- Seq.toList partialSolution.assignments
      ]

instance
  HasField
    "addDerivation"
    (PartialSolution' package version range)
    ( Term' package range ->
      Incompatibility' package range ->
      PartialSolution' package version range
    )
  where
  getField partialSolution term cause =
    partialSolution
      { _assignments = partialSolution.assignments Seq.|> Assignment_Derivation newDerivation
      }
   where
    newDerivation =
      AssignmentDerivation
        { term
        , cause
        , decisionLevel = partialSolution.currDecisionLevel
        }

instance
  (Eq package, IsVersionRange range, version ~ VersionRange.Version range) =>
  HasField
    "getSatisfiability"
    (PartialSolution' package version range)
    ( Incompatibility' package range ->
      Incompatibility.Satisfiability' package range
    )
  where
  getField = Incompatibility.getSatisfiability . toTerms
   where
    toTerms = map toTerm . Seq.toList . (.assignments)
    toTerm = \case
      Assignment_Decision a -> fromDecision a
      Assignment_Derivation a -> a.term
    fromDecision dec = Term.new dec.package (VersionRange.singleton dec.version)

{----- Assignment API -----}

instance
  (Eq package, IsVersionRange range, version ~ VersionRange.Version range) =>
  HasField
    "getSatisfiability"
    (Seq (Assignment' package version range))
    ( Incompatibility' package range ->
      Incompatibility.Satisfiability' package range
    )
  where
  getField = Incompatibility.getSatisfiability . map toTerm . Seq.toList
   where
    toTerm = \case
      Assignment_Decision a -> fromDecision a
      Assignment_Derivation a -> a.term
    fromDecision dec = Term.new dec.package (VersionRange.singleton dec.version)
