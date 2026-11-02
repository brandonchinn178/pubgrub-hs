{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module PubGrub.Internal.Incompatibility (
  Incompatibility,
  notRoot,

  -- * ContradictionCache
  ContradictionCache,
  notContradicted,
  isContradicted,
) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import PubGrub.Internal.Arena.IdMap (Id)
import PubGrub.Internal.PartialSolution (DecisionLevel)
import PubGrub.VersionSet (IsVersionSet, Term (..), Version)
import PubGrub.VersionSet qualified as VersionSet

data Incompatibility package versionSet incompat = Incompatibility
  { packageTerms :: !(Map (Id package) (Term versionSet))
  , kind :: !(IncompatKind package versionSet incompat)
  , contradictionCache :: !ContradictionCache
  }

data IncompatKind package versionSet incompat
  = IncompatNotRoot (Id package) (Version versionSet)

data ContradictionCache = ContradictionCache
  { decisionLevel :: DecisionLevel
  -- ^ The decision level where this incompatibility became contradicted.
  , backtrackGeneration :: Int
  -- ^ The backtrack generation for that decision level.
  }

notContradicted :: ContradictionCache
notContradicted =
  ContradictionCache
    { decisionLevel = maxBound
    , backtrackGeneration = 0
    }

isContradicted :: [DecisionLevel] -> ContradictionCache -> Bool
isContradicted lastValidDecisionLevels cache =
  maybe True (cache.decisionLevel <=) $
    safeIndex lastValidDecisionLevels cache.backtrackGeneration
 where
  safeIndex = \cases
    (x : _) 0 -> Just x
    (_ : xs) i | i > 0 -> safeIndex xs (i - 1)
    _ _ -> Nothing

notRoot :: (IsVersionSet versionSet) => Id package -> Version versionSet -> Incompatibility package versionSet incompat
notRoot package version =
  Incompatibility
    { packageTerms = Map.singleton package (NegativeTerm (VersionSet.singleton version))
    , kind = IncompatNotRoot package version
    , contradictionCache = notContradicted
    }
