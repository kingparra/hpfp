import Test.Hspec
import Test.QuickCheck
import Data.Monoid (Sum(..))
import Lib

main :: IO ()
main = hspec $ do
  describe "Question 6" $ do
    context "DaWrappa" $ do
      it "Can fmap into wrapper tycons" $ do
        fmap (+ (3 :: Int)) (DaWrappa (Sum 8) (Sum 12)) 
        `shouldBe` (DaWrappa (Sum {getSum = 11}) (Sum {getSum = 15}))
  describe "Question 9" $ do
    context "List a" $ do
      it "Can map into List a" $ do
        fmap (+1000) (Cons 3 (Cons 8 (Cons 12 Nil))) 
        `shouldBe` (Cons 1003 (Cons 1008 (Cons 1012 Nil)))
      it "preserves structure" $ do
        fmap id (Cons 3 (Cons 8 (Cons 12 Nil))) 
        `shouldBe` (Cons 3 (Cons 8 (Cons 12 Nil)))

