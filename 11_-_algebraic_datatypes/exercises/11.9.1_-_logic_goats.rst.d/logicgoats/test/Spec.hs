import Test.Hspec
import Lib

main = hspec $ do
  describe "Question 1" $ do
    it "tooMany (41,\"dude\") ==> False" $ do
      tooMany (42 :: Int,"dude") `shouldBe` False
    it "tooMany (42,\"dude\") ==> True" $ do
      tooMany (43 :: Int,"dude") `shouldBe` True
    it "tooMany (LivestockNumNamePair (42,\"dude\")) ==> False" $ do
      tooMany (LivestockNumNamePair (42,"dude")) `shouldBe` False
    it "tooMany (LivestockNumNamePair (43,\"dude\")) ==> True" $ do
      tooMany (LivestockNumNamePair (43,"dude")) `shouldBe` True
