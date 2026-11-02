{-# LANGUAGE TypeFamilies #-}

module PubGrub.Term (
  Term (..),
) where

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
data Term range
  = -- | For example, @1.0.0 <= v < 2.0.0@ is a positive expression that is
    -- evaluated True if a version is selected and comprised between version
    -- 1.0.0 and version 2.0.0.
    PositiveTerm range
  | -- | The term @not (v < 3.0.0)@ is a negative expression that is evaluated
    -- True if a version >= 3.0.0 is selected or if no version is selected at
    -- all.
    NegativeTerm range
  deriving (Show, Eq)
