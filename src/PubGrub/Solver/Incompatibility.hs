{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData #-}

module PubGrub.Solver.Incompatibility (
  Incompatibility,
  Incompatibility',
  fromList,
  notRoot,

  -- * Satisfiability
  Satisfiability,
  Satisfiability' (..),
  getSatisfiability,

  -- * Derivation tree
  toDerivationTree,
) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import GHC.Records (HasField (..))
import PubGrub.DependencyProvider qualified as DP
import PubGrub.Error (DerivationTree')
import PubGrub.Term (Term')
import PubGrub.Term qualified as Term
import PubGrub.VersionRange (IsVersionRange, Version)
import PubGrub.VersionRange qualified as VersionRange

type Incompatibility provider =
  Incompatibility' (DP.Package provider) (DP.VersionRange provider)

newtype Incompatibility' package range
  = Incompatibility
      -- Invariant: key == term.package
      (Map package (Term' package range))
  deriving (Show, Eq)

fromList :: (Ord package) => [Term' package range] -> Incompatibility' package range
fromList = Incompatibility . Map.fromList . map (\t -> (t.package, t))

instance HasField "toList" (Incompatibility' package range) [Term' package range] where
  getField (Incompatibility terms) = Map.elems terms

instance
  (Ord package) =>
  HasField
    "get"
    (Incompatibility' package range)
    (package -> Maybe (Term' package range))
  where
  getField (Incompatibility terms) = flip Map.lookup terms

notRoot ::
  forall package range.
  (IsVersionRange range) =>
  package -> Version range -> Incompatibility' package range
notRoot package version = Incompatibility $ Map.singleton package term.negated
 where
  term = Term.new package (VersionRange.singleton version)

{----- Satisfiability -----}

type Satisfiability provider = Satisfiability' (DP.Package provider) (DP.VersionRange provider)

data Satisfiability' package range
  = Satisfies
  | AlmostSatisfies (Term' package range)
  | Contradicts
  | Inconclusive
  deriving (Show, Eq)

getSatisfiability ::
  (Eq package, IsVersionRange range) =>
  [Term' package range] ->
  Incompatibility' package range ->
  Satisfiability' package range
getSatisfiability terms (Incompatibility packageTerms) =
  go [] (Map.elems packageTerms)
 where
  go unsatisfiedTerms = \case
    term : rest ->
      case terms `Term.satisfies` term of
        Just False -> Contradicts
        Just True -> go unsatisfiedTerms rest
        Nothing -> go (term : unsatisfiedTerms) rest
    [] ->
      case unsatisfiedTerms of
        [] -> Satisfies
        [term] -> AlmostSatisfies term
        _ -> Inconclusive

{----- Derivation tree -----}

toDerivationTree ::
  Incompatibility' package range ->
  DerivationTree' package version range unavailErr
toDerivationTree = undefined
