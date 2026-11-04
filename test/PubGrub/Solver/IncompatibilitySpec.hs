module PubGrub.Solver.IncompatibilitySpec (spec) where

import Data.Text (Text)
import PubGrub.Solver.Incompatibility
import PubGrub.Term qualified as Term
import PubGrub.TestUtils.VersionRange (genVersionRange, rangeCaret, rangeGTE)
import PubGrub.VersionRange (VersionRange)
import Skeletest
import Skeletest.Prop.Gen qualified as Gen
import Skeletest.Prop.Range qualified as Range

spec :: Spec
spec = do
  spec_getSatisfiability

spec_getSatisfiability :: Spec
spec_getSatisfiability = do
  describe "getSatisfiability" $ do
    prop "returns Satisfies on empty terms" $ do
      incompat <- forAll genIncompatibility
      getSatisfiability [] incompat `shouldBe` Satisfies

    it "returns Satisfies if set of terms satisfies incompatibility" $ do
      let terms =
            [ Term.new "foo" (rangeCaret [1])
            , Term.new "bar" (rangeCaret [2])
            , Term.new "baz" (rangeCaret [3])
            ]
          incompat =
            fromList
              [ Term.new "foo" (rangeGTE [1]) -- satisfies
              , Term.new "bar" (rangeGTE [2]) -- satisfies
              , Term.new "baz" (rangeGTE [3]) -- satisfies
              ]
      getSatisfiability terms incompat `shouldBe` Satisfies

    it "returns Contradicts if set of terms contradicts incompatibility" $ do
      let terms =
            [ Term.new "foo" (rangeCaret [1])
            , Term.new "bar" (rangeCaret [2])
            , Term.new "baz" (rangeCaret [3])
            ]
          incompat =
            fromList
              [ Term.new "foo" (rangeGTE [1]) -- satisfies
              , Term.new "bar" (rangeGTE [3]) -- contradicts
              , Term.new "baz" (rangeGTE [4]) -- contradicts
              ]
      getSatisfiability terms incompat `shouldBe` Contradicts

    it "returns AlmostSatisfies if set of terms almost satisfies incompatibility" $ do
      let terms =
            [ Term.new "foo" (rangeCaret [1])
            , Term.new "bar" (rangeCaret [2])
            , Term.new "baz" (rangeGTE [0])
            ]
          incompat =
            fromList
              [ Term.new "foo" (rangeGTE [1]) -- satisfies
              , Term.new "bar" (rangeGTE [2]) -- satisfies
              , Term.new "baz" (rangeCaret [1]) -- inconclusive
              ]
      getSatisfiability terms incompat `shouldBe` AlmostSatisfies (Term.new "baz" (rangeCaret [1]))

    it "returns Inconclusive if set of terms is inconclusive for incompatibility" $ do
      let terms =
            [ Term.new "foo" (rangeCaret [1])
            , Term.new "bar" (rangeGTE [0])
            , Term.new "baz" (rangeGTE [0])
            ]
          incompat =
            fromList
              [ Term.new "foo" (rangeGTE [1]) -- satisfies
              , Term.new "bar" (rangeCaret [1]) -- inconclusive
              , Term.new "baz" (rangeCaret [1]) -- inconclusive
              ]
      getSatisfiability terms incompat `shouldBe` Inconclusive

{----- Property tests -----}

genIncompatibility :: Gen (Incompatibility' Text VersionRange)
genIncompatibility = fmap fromList $ do
  Gen.list (Range.linear 0 10) $ do
    package <- Gen.text (Range.linear 1 20) Gen.unicode
    range <- genVersionRange
    pure $ Term.new package range
