{-# LANGUAGE TypeFamilies #-}

module PubGrub.VersionRangeSpec (spec) where

import Control.Monad (forM_)
import Data.Interval qualified as Interval
import Data.IntervalSet (IntervalSet)
import Data.IntervalSet qualified as IntervalSet
import PubGrub.VersionRange
import Skeletest
import Prelude hiding (elem)

spec :: Spec
spec = do
  describe "IsVersionRange" $ do
    describe "full" $ do
      it "provides correct default implementation" $ do
        forM_ [0, 1, 2, 5, 10, 100] $ \v -> do
          (v `elem` full @RangeMinimal) `shouldBe` True

    describe "union" $ do
      it "provides correct default implementation" $ do
        let range = singleton 1 `union` singleton 3 :: RangeMinimal
        (0 `elem` range) `shouldBe` False
        (1 `elem` range) `shouldBe` True
        (2 `elem` range) `shouldBe` False
        (3 `elem` range) `shouldBe` True
        (4 `elem` range) `shouldBe` False

    describe "(\\\\)" $ do
      it "provides correct default implementation" $ do
        let range = mkRangeMinimal (1, 3) \\ singleton 2
        (0 `elem` range) `shouldBe` False
        (1 `elem` range) `shouldBe` True
        (2 `elem` range) `shouldBe` False
        (3 `elem` range) `shouldBe` True
        (4 `elem` range) `shouldBe` False

    describe "isDisjoint" $ do
      it "provides correct default implementation" $ do
        isDisjoint (singleton 1) (singleton 1 :: RangeMinimal)
          `shouldBe` False
        isDisjoint (singleton 1) (singleton 2 :: RangeMinimal)
          `shouldBe` True
        isDisjoint (mkRangeMinimal (1, 3)) (singleton 2)
          `shouldBe` False

    describe "isSubsetOf" $ do
      it "provides correct default implementation" $ do
        singleton @RangeMinimal 1 `isSubsetOf` singleton 1
          `shouldBe` True
        singleton @RangeMinimal 1 `isSubsetOf` singleton 2
          `shouldBe` False
        mkRangeMinimal (1, 3) `isSubsetOf` mkRangeMinimal (2, 4)
          `shouldBe` False
        mkRangeMinimal (2, 3) `isSubsetOf` mkRangeMinimal (1, 4)
          `shouldBe` True

newtype RangeMinimal = RangeMinimal {unRangeMinimal :: IntervalSet Int}
  deriving (Show, Eq)

mkRangeMinimal :: (Int, Int) -> RangeMinimal
mkRangeMinimal (lo, hi) = RangeMinimal . IntervalSet.singleton $ Interval.Finite lo Interval.<=..<= Interval.Finite hi

instance IsVersionRange RangeMinimal where
  type Version RangeMinimal = Int
  empty = RangeMinimal IntervalSet.empty
  singleton = RangeMinimal . IntervalSet.singleton . Interval.singleton
  complement = RangeMinimal . IntervalSet.complement . unRangeMinimal
  intersection (RangeMinimal i1) (RangeMinimal i2) = RangeMinimal (IntervalSet.intersection i1 i2)
  elem v = IntervalSet.member v . unRangeMinimal
