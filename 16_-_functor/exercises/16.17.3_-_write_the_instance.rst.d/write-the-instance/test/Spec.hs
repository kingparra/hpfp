import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Lib

{-# ANN main "Hlint: Ignore: refract:Functor law" #-}
-- main :: IO ()
-- main = hspec $ do

--   describe "Question 1" $ do
--     context "instance for (Quant a)" $ do
--       it "identity law" $ do
--         fmap id (Finance :: Quant Int Int)
--           `shouldBe` (Finance :: Quant Int Int)
--         fmap id (Desk 3 :: Quant Int Int)
--           `shouldBe` (Desk 3 :: Quant Int Int)
--         fmap id (Bloor 88 :: Quant Int Int)
--           `shouldBe` (Bloor 88 :: Quant Int Int)
--       it "composition law" $ do
--         let  f = (*3)
--         let  g = (^4)
--         fmap f . fmap g $ (Bloor 88 :: Quant Int Int)
--           `shouldBe` fmap (f . g) (Bloor 88 :: Quant Int Int)
--       prop "composition property"
--         (\x -> let f = (*3); g = (^4) in
--                  fmap f . fmap g $ (x :: Quant Int Int)
--               == fmap (f . g)      (x :: Quant Int Int))

main = verboseCheck prop_Quant
  where prop_Quant =
          property (\x ->
            let f = (*3); g = (^4)
            in (fmap f . fmap g $ (x :: Quant Int Int)) ==
               (fmap (f . g)      (x :: Quant Int Int)))
