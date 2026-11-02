{-# LANGUAGE TypeFamilies #-}

module PubGrub.VersionSet (
  IsVersionSet (..),
  Term (..),
) where

import Prelude hiding (elem)

class
  ( Eq versionSet
  , Ord (Version versionSet)
  ) =>
  IsVersionSet versionSet
  where
  type Version versionSet

  empty :: versionSet
  singleton :: Version versionSet -> versionSet
  complement :: versionSet -> versionSet
  intersection :: versionSet -> versionSet -> versionSet
  elem :: Version versionSet -> versionSet -> Bool

  elemMany :: [Version versionSet] -> versionSet -> [Bool]
  elemMany vs range = map (`elem` range) vs

  full :: versionSet
  full = complement empty

  union :: versionSet -> versionSet -> versionSet
  union range1 range2 = complement (intersection range1 range2)

  (\\) :: versionSet -> versionSet -> versionSet
  range1 \\ range2 = intersection range1 (complement range2)

  isDisjoint :: versionSet -> versionSet -> Bool
  isDisjoint range1 range2 = intersection range1 range2 == empty

  isSubsetOf :: versionSet -> versionSet -> Bool
  range1 `isSubsetOf` range2 = range1 == intersection range1 range2

-- | A positive or negative expression regarding a set of versions.
--
-- @PositiveTerm range@ and @NegativeTerm (complement range)@ are not equivalent:
--
--   * the term @PositiveTerm range@ is satisfied if the package is selected AND
--     the selected version is in @range@.
--   * the term @NegativeTerm (complement range)@ is satisfied if the package is
--     not selected OR the selected version is in @range@.
--
-- A @PositiveTerm@ in the partial solution requires a version to be selected,
-- but a @NegativeTerm@ allows for a solution that does not have that package
-- selected. Specifically, @PositiveTerm empty@ means that there was a conflict
-- (we need to select a version for the package but can’t pick any), while
-- @NegativeTerm full@ would mean it is fine as long as we don’t select the
-- package.
data Term versionSet
  = -- | For example, @1.0.0 <= v < 2.0.0@ is a positive expression that is
    -- evaluated True if a version is selected and comprised between version
    -- 1.0.0 and version 2.0.0.
    PositiveTerm versionSet
  | -- | The term @not (v < 3.0.0)@ is a negative expression that is evaluated
    -- True if a version >= 3.0.0 is selected or if no version is selected at
    -- all.
    NegativeTerm versionSet
  deriving (Show, Eq)
