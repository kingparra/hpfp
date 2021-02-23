import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "" $ do
    it "" $ do
      True `shouldBe` True
