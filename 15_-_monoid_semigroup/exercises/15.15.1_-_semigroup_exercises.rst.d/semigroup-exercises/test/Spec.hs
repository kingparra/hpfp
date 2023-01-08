import Test.Hspec (hspec,describe,context,it,shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Data.Monoid (Sum, mappend)
import Lib


main :: IO ()
main = hspec $ do
  -- Question 1
  describe "Trivial" $ do
    context "Semigroup laws" $ do
      prop "(<>) is associative"
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
  -- Question 4
  describe "Three a b c" $ do
    context "Semigroup laws" $ do
      prop "(<>) is associative for Three"
        ((\a b c -> a <> (b <> c) == (a <> b) <> c)
         :: Three (Sum Int) (Sum Int) (Sum Int)
         -> Three (Sum Int) (Sum Int) (Sum Int)
         -> Three (Sum Int) (Sum Int) (Sum Int)
         -> Bool)
  -- Question 5
  describe "Four a b c d" $ do
    context "Semigroup laws" $ do
      prop "(<>) is associative for Four"
        ((\a b c d -> a <> (b <> (c <> d)) == ((a <> b) <> c) <> d)
         :: Four (Sum Int) (Sum Int) (Sum Int) (Sum Int)
         -> Four (Sum Int) (Sum Int) (Sum Int) (Sum Int)
         -> Four (Sum Int) (Sum Int) (Sum Int) (Sum Int)
         -> Four (Sum Int) (Sum Int) (Sum Int) (Sum Int)
         -> Bool)
  -- Question 6
  {- For some reason I'm getting hairy type errors from this test -}
  describe "BoolConj" $ do
    context "True && True is True" $ do
      (BoolConj True) <> (BoolConj True) `shouldBe` (BoolConj True)
    context "True && False is False" $ do
      (BoolConj True) <> (BoolConj False) `shouldBe` (BoolConj False)
