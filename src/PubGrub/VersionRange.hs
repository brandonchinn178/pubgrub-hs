{-# LANGUAGE TypeFamilies #-}

module PubGrub.VersionRange (
  IsVersionRange (..),
) where

import Prelude hiding (elem)

class
  ( Show range
  , Eq range
  , Show (VersionType range)
  , Ord (VersionType range)
  ) =>
  IsVersionRange range
  where
  type VersionType range

  empty :: range
  singleton :: VersionType range -> range
  complement :: range -> range
  intersection :: range -> range -> range
  elem :: VersionType range -> range -> Bool

  elemMany :: [VersionType range] -> range -> [Bool]
  elemMany vs range = map (`elem` range) vs

  full :: range
  full = complement empty

  union :: range -> range -> range
  union range1 range2 = complement (intersection range1 range2)

  (\\) :: range -> range -> range
  range1 \\ range2 = intersection range1 (complement range2)

  isDisjoint :: range -> range -> Bool
  isDisjoint range1 range2 = intersection range1 range2 == empty

  isSubsetOf :: range -> range -> Bool
  range1 `isSubsetOf` range2 = range1 == intersection range1 range2
