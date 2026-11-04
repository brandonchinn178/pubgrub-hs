{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module PubGrub.VersionRange (
  IsVersionRange (..),

  -- * Example implementation
  VersionRange (..),
  makeRange,
) where

import Data.Interval (Interval)
import Data.Interval qualified as Interval
import Data.IntervalSet (IntervalSet)
import Data.IntervalSet qualified as IntervalSet
import Data.Version qualified as Base
import Prelude hiding (elem)

class
  ( Show range
  , Eq range
  , Show (Version range)
  , Ord (Version range)
  ) =>
  IsVersionRange range
  where
  type Version range

  empty :: range
  singleton :: Version range -> range
  complement :: range -> range
  intersection :: range -> range -> range
  elem :: Version range -> range -> Bool

  full :: range
  full = complement empty

  union :: range -> range -> range
  union range1 range2 = complement (intersection (complement range1) (complement range2))

  (\\) :: range -> range -> range
  range1 \\ range2 = intersection range1 (complement range2)

  isDisjoint :: range -> range -> Bool
  isDisjoint range1 range2 = intersection range1 range2 == empty

  isSubsetOf :: range -> range -> Bool
  range1 `isSubsetOf` range2 = range1 == intersection range1 range2

-- | An example implementation of 'IsVersionRange'
newtype VersionRange = VersionRange {unVersionRange :: IntervalSet Base.Version}
  deriving (Show, Eq)

makeRange :: Interval Base.Version -> VersionRange
makeRange = VersionRange . IntervalSet.singleton

instance IsVersionRange VersionRange where
  type Version VersionRange = Base.Version
  empty = VersionRange IntervalSet.empty
  singleton = VersionRange . IntervalSet.singleton . Interval.singleton
  complement = VersionRange . IntervalSet.complement . unVersionRange
  intersection (VersionRange i1) (VersionRange i2) = VersionRange (IntervalSet.intersection i1 i2)
  elem v = IntervalSet.member v . unVersionRange
