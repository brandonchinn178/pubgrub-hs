{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeFamilies #-}

module PubGrub.TermSpec (spec) where

import PubGrub.Term
import PubGrub.TestUtils.VersionRange (rangeCaret, rangeGTE, rangeLT)
import Skeletest

spec :: Spec
spec = do
  spec_satisfies

spec_satisfies :: Spec
spec_satisfies = do
  describe "satisfies" $ do
    examples

    it "Empty set satisfies all terms" $ do
      [] `satisfies` new "foo" (rangeCaret [100]) `shouldBe` Just True

    it "ignores terms for different packages" $ do
      let terms =
            [ new "bar" (rangeGTE [10]) -- If this were "foo", it would return False
            ]
      terms `satisfies` new "foo" (rangeCaret [1]) `shouldBe` Just True
 where
  -- https://github.com/dart-lang/pub/blob/master/doc/solver.md#term
  examples = do
    it "{foo >=1.0.0, foo <2.0.0} `satisfies` foo ^1.0.0 ==> Just True" $ do
      let terms =
            [ new "foo" (rangeGTE [1, 0, 0])
            , new "foo" (rangeLT [2, 0, 0])
            ]
      terms `satisfies` new "foo" (rangeCaret [1, 0, 0]) `shouldBe` Just True

    it "{foo ^1.5.0} `satisfies` not foo ^1.0.0 ==> Just False" $ do
      let terms =
            [ new "foo" (rangeCaret [1, 5, 0])
            ]
      terms `satisfies` (new "foo" (rangeCaret [1, 0, 0])).negated `shouldBe` Just False

    it "{foo ^1.0.0} `satisfies` foo ^1.5.0 ==> Nothing" $ do
      let terms =
            [ new "foo" (rangeCaret [1, 0, 0])
            ]
      terms `satisfies` new "foo" (rangeCaret [1, 5, 0]) `shouldBe` Nothing
