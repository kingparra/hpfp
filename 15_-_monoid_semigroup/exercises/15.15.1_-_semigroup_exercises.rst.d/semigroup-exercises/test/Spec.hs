import Test.Hspec (hspec,describe,context,it,shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Lib


main :: IO ()
main = hspec $ do
  describe "Trivial" $ do
    context "Semigroup laws" $ do
      prop "(<>) is associative"
        ((\a b c -> a <> (b <> c) == (a <> b) <> c)
         :: Trivial -> Trivial -> Trivial -> Bool)
