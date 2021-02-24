import Test.Hspec
import Test.QuickCheck
import Lib

-- main :: IO ()
-- main = hspec $ do
--   -- Question 1
--   describe "More b a" $ do
--     it "f applies to the first and third args of L" $ do
--       fmap (+1) (L 1 2 3) `shouldBe` (L 2 2 4)
--     it "f applies f to the second arg of R" $ do
--       fmap (+1) (R 1 2 3) `shouldBe` (R 1 3 3)
