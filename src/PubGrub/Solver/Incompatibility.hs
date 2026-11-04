{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module PubGrub.Solver.Incompatibility (
  Incompatibility,
  Incompatibility' (..),
  notRoot,

  -- * Satisfiability
  Satisfiability,
  Satisfiability' (..),
  getSatisfiability,
) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import PubGrub.DependencyProvider qualified as DP
import PubGrub.Term (Term')
import PubGrub.Term qualified as Term
import PubGrub.VersionRange (IsVersionRange, Version)
import PubGrub.VersionRange qualified as VersionRange

type Incompatibility provider =
  Incompatibility' (DP.Package provider) (DP.VersionRange provider)

newtype Incompatibility' package range = Incompatibility (Map package (Term' package range))

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
  | AlmostSatisfies !(Term' package range)
  | Contradicts
  | Inconclusive

-- TODO: test
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
