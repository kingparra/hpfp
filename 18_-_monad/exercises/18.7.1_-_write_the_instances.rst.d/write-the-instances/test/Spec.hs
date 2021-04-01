import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Lib

-- type SSS = (String, String, String)
type III = (Int, Int, Int)

main = do
  quickBatch $ functor (NopeDotJpg :: Nope III)
  quickBatch $ applicative (NopeDotJpg :: Nope III)
  quickBatch $ monad (NopeDotJpg :: Nope III)
  -- quickBatch $ functor (undefined :: PhhhbbtttEither III III)
  -- quickBatch $ applicative (undefined :: PhhhbbtttEither III III)
  -- quickBatch $ monad (undefined :: PhhhbbtttEither III III)
  -- quickBatch $ functor (undefined :: Identity III)
  -- quickBatch $ applicative (undefined :: Identity III)
  -- quickBatch $ monad (undefined :: Identity III)
  -- quickBatch $ functor (undefined :: List III)
  -- quickBatch $ applicative (undefined :: List III)
  -- quickBatch $ monad (undefined :: List III)
