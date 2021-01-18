import Test.Hspec
import Lib

main :: IO ()
main = hspec $ do


  describe "Question 1" $ do

    context "lefts'" $ do
      it "[Left 1, Right 3, Left 88] ==> [1,88]" $ do
        lefts' ([Left 1, Right 3, Left 88] :: [Either Int Int]) `shouldBe` ([1,88] :: [Int])
      it "[Right 8] ==> []" $ do
        lefts' ([Right 8] :: [Either Int Int]) `shouldBe` ([] :: [Int])
      it "[] ==> []" $ do
        lefts' ([] :: [Either Int Int]) `shouldBe` ([] :: [Int])

  describe "Question 2" $ do

    context "rights'" $ do
      it "[Left 1, Right 3, Left 88] ==> [3]" $ do
        rights' ([Left 1, Right 3, Left 88] :: [Either Int Int]) `shouldBe` ([3] :: [Int])
      it "[Right 8] ==> [8]" $ do
        rights' ([Right 8] :: [Either Int Int]) `shouldBe` ([8] :: [Int])
      it "[] ==> []" $ do
        rights' ([] :: [Either Int Int]) `shouldBe` ([] :: [Int])


  describe "Question 3" $ do

    context "partitionEithers'" $ do
      it "[Left 1, Right 3, Left 88] ==> ([1,88],[3])" $ do
        let input  = [Left 1, Right 3, Left 88] :: [Either Int Int]
        let output = ([1,88],[3])               :: ([Int],[Int])
        partitionEithers' input `shouldBe` output

  describe "Question 4" $ do

    context "eitherMaybe'" $ do
      it "(+7) (Left 3) ==> Nothing" $ do
        eitherMaybe' (+7) (Left 3 :: Either Int Int) `shouldBe` (Nothing :: Maybe Int)
      it "(+7) (Right 8) ==> 15" $ do
        eitherMaybe' (+ (7 :: Int)) (Right 8 :: Either Int Int) `shouldBe` (Just 15 :: Maybe Int)

  describe "Question 5" $ do

    context "either'" $ do
      let f = (+8)        :: Int -> Int
      let g = subtract 1  :: Int -> Int
      it "(+8) (-1) (Right 8) ==> 16" $ do
        either' f g (Right 8) `shouldBe` 7
      it "(+8) (-1) (Left 20)" $ do
        either' f g (Left 20) `shouldBe` 28

  describe "Question 6" $ do

     context "eitherMaybe''" $ do
       let f = (+8)        :: Int -> Int
       it "(+8) (Left 8) ==> Nothing" $ do
         eitherMaybe'' f (Left 8 :: Either Int Int) `shouldBe` (Nothing :: Maybe Int)
       it "(+8) (Right 8) ==> Just 16" $ do
         eitherMaybe'' f (Right 8 :: Either Int Int) `shouldBe` (Just 16 :: Maybe Int)
