import hspec (describe, context, it, shouldBe)
import Lib

main = hspec $ do
  describe "wordNumber" $ do
    -- wordNumber works only on Int, so we don't have to worry about decimals.
    it "converts single digits to words without surrounding dashes" $ do
      wordNumber 0 `shouldBe` "zero"
      wordNumber 1 `shouldBe` "one"
      wordNumber 2 `shouldBe` "two"
      wordNumber 3 `shouldBe` "three"
      wordNumber 4 `shouldBe` "four"
      wordNumber 5 `shouldBe` "five"
      wordNumber 6 `shouldBe` "six"
      wordNumber 7 `shouldBe` "seven"
      wordNumber 8 `shouldBe` "eight"
      wordNumber 9 `shouldBe` "nine"
    it "intersperses dashes for multi-digit outputs" $ do
      wordNumber 123 `shouldBe` "one-two-three"
      wordNumber 88 `shouldBe` "eight-eight"
      wordNumber 007 `shouldBe` "zero-zero-seven"
      wordNumber 123456 `shouldBe` "one-two-three-two-four-five-four-six"
    it "prefixes 'negative' when the input is a negative number" $ do
      wordNumber (-98) `shouldBe` "negative-nine-eight"
      -- Since (-0) resolves to 0 before function application, we don't need a special case for it.
