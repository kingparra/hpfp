import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Lib

-- Functor Identity
prop_FuncIdComp :: Identity Int -> Bool
prop_FuncIdComp xs =
  let { f = (+1); g = (*2) } in
  (fmap (f . g) xs) == (fmap f . fmap g $ xs)


main =
  quickBatch (functor (undefined :: Identity Int))
  -- quickCheck prop_FuncIdComp
