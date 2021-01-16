import Test.Hspec
import Lib

main = hspec $ do
  describe "mkWord" $ do
    it "\"eke\" ==> Nothing" $ do
      mkWord "eke" `shouldBe` Nothing
    it "\"platypus\" ==> Just (Word' \"platypus\")" $ do
      mkWord "platypus" `shouldBe` Just (Word' "platypus")
