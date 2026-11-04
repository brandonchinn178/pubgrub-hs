{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

module PubGrub.Term (
  Term,
  Term' (..),
  TermSign (..),

  -- * Constructors
  new,

  -- * API
  satisfies,
) where

import Data.Foldable1 (foldr1)
import Data.List.NonEmpty qualified as NonEmpty
import GHC.Records (HasField (..))
import PubGrub.DependencyProvider qualified as DP
import PubGrub.VersionRange (IsVersionRange)
import PubGrub.VersionRange qualified as VersionRange
import Prelude hiding (foldr1, negate)

type Term provider = Term' (DP.Package provider) (DP.VersionRange provider)

-- | A positive or negative expression regarding a set of versions.
--
-- @Term{sign = Positive, range = range}@ and
-- @Term{sign = Negative, range = complement range}@ are not equivalent:
--
--   * @Term{sign = Positive, range = range}@ is satisfied if the package is
--     selected AND the selected version is in @range@.
--   * @Term{sign = Negative, range = complement range}@ is satisfied if the
--     package is not selected OR the selected version is in @range@.
--
-- A @Positive@ term in the partial solution requires a version to be selected,
-- but a @Negative@ term allows for a solution that does not have that package
-- selected. Specifically, @Term{sign = Positive, range = empty}@ means that
-- there was a conflict (we need to select a version for the package but can’t
-- pick any), while @Term{sign = Negative, range = full}@ would mean it is fine
-- as long as we don’t select the package.
data Term' package range = Term
  { sign :: TermSign
  , package :: package
  , range :: range
  }
  deriving (Show, Eq)

data TermSign
  = Positive
  | Negative
  deriving (Show, Eq)

new :: package -> range -> Term' package range
new package range = Term{sign = Positive, ..}

{----- API -----}

instance HasField "negated" (Term' package range) (Term' package range) where
  getField term = term{sign = negate term.sign}
   where
    negate = \case
      Positive -> Negative
      Negative -> Positive

satisfies ::
  forall package range.
  (Eq package, IsVersionRange range) =>
  [Term' package range] ->
  Term' package range ->
  Maybe Bool
otherTerms `satisfies` term =
  case otherTermIntersection of
    (Positive, other)
      | term.sign == Negative && term.range == VersionRange.empty -> Just True
      | other `VersionRange.isSubsetOf` term.range -> Just (resolve True)
      | VersionRange.isDisjoint other term.range -> Just (resolve False)
      | otherwise -> Nothing
    (Negative, other)
      | term.range `VersionRange.isSubsetOf` other -> Just (resolve True)
      | otherwise -> Nothing
 where
  resolve = case term.sign of
    Positive -> id
    Negative -> not

  otherTermIntersection =
    maybe (Positive, VersionRange.empty) (foldr1 intersect) . NonEmpty.nonEmpty $
      [ (other.sign, other.range)
      | other <- otherTerms
      , other.package == term.package
      ]

  intersect :: (TermSign, range) -> (TermSign, range) -> (TermSign, range)
  intersect = \cases
    (Positive, r1) (Positive, r2) -> (Positive, VersionRange.intersection r1 r2)
    (Positive, p) (Negative, n) -> (Positive, p VersionRange.\\ n)
    (Negative, n) (Positive, p) -> (Positive, p VersionRange.\\ n)
    (Negative, r1) (Negative, r2) -> (Negative, VersionRange.union r1 r2)
