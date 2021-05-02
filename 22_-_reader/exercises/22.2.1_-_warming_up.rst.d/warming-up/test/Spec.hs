import Test.Hspec
import Lib

main = hspec $ do
  describe "Question 1" $ do
    it "composed and fmapped" $ do
      composed "Julie" `shouldBe` "EILUJ"
      fmapped "Chris" `shouldBe` "SIRHC"
    it "tupled and tupled'" $ do
      tupled  "Julie" `shouldBe` ("JULIE", "eiluJ")
      tupled' "Julie" `shouldBe` ("eiluJ", "JULIE")
