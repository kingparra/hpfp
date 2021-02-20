import Test.Hspec (hspec,describe,context,it,shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Data.Semigroup
import Data.Monoid
import Lib


main :: IO ()
main = hspec $ do
  -- Question 1
  describe "Trivial" $ do
    context "Monoid laws" $ do
      prop "(<>) is associative (semigroup law)"
        ((\a b c -> a <> (b <> c) == (a <> b) <> c)
         :: Trivial 
         -> Trivial 
         -> Trivial 
         -> Bool)
      it "mempty exists for Trivial, and is Trivial" $ do
        (mempty :: Trivial) `shouldBe` Trivial
  -- Question 2
  describe "Identity" $ do
    context "Semigroup laws" $ do
      prop "(<>) is associative"
        ((\a b c -> a <> (b <> c) == (a <> b) <> c)
         :: Identity (Sum Int) 
         -> Identity (Sum Int) 
         -> Identity (Sum Int) 
         -> Bool)
  -- Question 3
  describe "Two a b" $ do
    context "Semigroup laws" $ do
      prop "(<>) is associative for Two"
        ((\a b c -> a <> (b <> c) == (a <> b) <> c)
         :: Two (Sum Int) (Sum Int)
         -> Two (Sum Int) (Sum Int)
         -> Two (Sum Int) (Sum Int)
         -> Bool)
