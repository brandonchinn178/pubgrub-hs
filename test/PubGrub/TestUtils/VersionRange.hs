{-# LANGUAGE LambdaCase #-}

module PubGrub.TestUtils.VersionRange (
  rangeGTE,
  rangeLT,
  rangeCaret,
) where

import Data.Interval qualified as Interval
import Data.Version (makeVersion)
import PubGrub.VersionRange (VersionRange, makeRange)

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
