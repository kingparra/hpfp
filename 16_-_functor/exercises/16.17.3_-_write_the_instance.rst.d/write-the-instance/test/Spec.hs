import Test.Hspec
import Test.QuickCheck
import Data.Monoid (Sum(..))
import Lib

main :: IO ()
main = hspec $ do
  describe "Question 6" $ do
    context "DaWrappa" $ do
      it "Can fmap into wrapper tycons" $ do
        fmap (+ (3 :: Int)) (DaWrappa (Sum 8) (Sum 12)) `shouldBe`
          (DaWrappa (Sum {getSum = 11}) (Sum {getSum = 15}))
