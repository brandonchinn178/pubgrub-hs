{-# LANGUAGE LambdaCase #-}

module PubGrub.TestUtils.VersionRange (
  rangeGTE,
  rangeLT,
  rangeCaret,

  -- * Property tests
  genVersionRange,
) where

import Data.Interval qualified as Interval
import Data.IntervalSet qualified as IntervalSet
import Data.Version (makeVersion)
import PubGrub.VersionRange (VersionRange (..), makeRange)
import Skeletest (Gen)
import Skeletest.Prop.Gen qualified as Gen
import Skeletest.Prop.Range qualified as Range

rangeGTE :: [Int] -> VersionRange
rangeGTE v = makeRange $ Interval.Finite (makeVersion v) Interval.<=..< Interval.PosInf

rangeLT :: [Int] -> VersionRange
rangeLT v = makeRange $ Interval.NegInf Interval.<..< Interval.Finite (makeVersion v)

rangeCaret :: [Int] -> VersionRange
rangeCaret = \case
  v : vs ->
    let lo = Interval.Finite . makeVersion $ v : vs
        hi = Interval.Finite . makeVersion $ v + 1 : map (const 0) vs
     in makeRange $ lo Interval.<=..< hi
  _ -> error "rangeCaret got an empty version"

genVersionRange :: Gen VersionRange
genVersionRange = VersionRange . IntervalSet.fromList <$> genIntervals
 where
  genIntervals = Gen.list (Range.linear 0 5) $ genInterval genVersion
  genVersion = makeVersion <$> Gen.list (Range.linear 1 4) (Gen.int (Range.linear 0 100))

genInterval :: (Ord a) => Gen a -> Gen (Interval.Interval a)
genInterval g =
  Gen.frequency
    [ -- Whole
      freq 1 $ do
        pure Interval.whole
    , -- Empty
      freq 1 $ do
        pure Interval.empty
    , -- Point !r
      freq 1 $ do
        x <- g
        pure $ Interval.Finite x Interval.<=..<= Interval.Finite x
    , -- LessThan !r
      freq 5 $ do
        x <- g
        pure $ Interval.NegInf Interval.<..< Interval.Finite x
    , -- LessOrEqual !r
      freq 5 $ do
        x <- g
        pure $ Interval.NegInf Interval.<..<= Interval.Finite x
    , -- GreaterThan !r
      freq 5 $ do
        x <- g
        pure $ Interval.Finite x Interval.<..< Interval.PosInf
    , -- GreaterOrEqual !r
      freq 5 $ do
        x <- g
        pure $ Interval.Finite x Interval.<=..< Interval.PosInf
    , -- BothClosed !r !r
      freq 5 $ do
        lo <- g
        hi <- g
        pure $ Interval.Finite lo Interval.<=..<= Interval.Finite hi
    , -- LeftOpen !r !r
      freq 5 $ do
        lo <- g
        hi <- g
        pure $ Interval.Finite lo Interval.<..<= Interval.Finite hi
    , -- RightOpen !r !r
      freq 5 $ do
        lo <- g
        hi <- g
        pure $ Interval.Finite lo Interval.<=..< Interval.Finite hi
    , -- BothOpen !r !r
      freq 5 $ do
        lo <- g
        hi <- g
        pure $ Interval.Finite lo Interval.<..< Interval.Finite hi
    ]
 where
  freq = (,)
