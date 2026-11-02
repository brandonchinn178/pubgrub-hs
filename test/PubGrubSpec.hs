module PubGrubSpec (spec) where

import Skeletest

spec :: Spec
spec = do
  it "smoketest" $ do
    1 + 1 `shouldBe` (2 :: Int)
