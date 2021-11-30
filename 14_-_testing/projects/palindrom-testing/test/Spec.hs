import Test.QuickCheck
import Test.Hspec
import Data.Char (isPunctuation)
import Lib


prop_punctuationInvariant text =
  preprocess text == preprocess noPuncText
  where noPuncText = filter (not . isPunctuation) text


main :: IO ()
main = quickCheckWith stdArgs { maxSuccess = 10000 } prop_punctuationInvariant
