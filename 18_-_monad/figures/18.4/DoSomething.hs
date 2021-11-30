module DoSomething where
-- page 757


-- If your do syntax looks like this, you
-- can rewrite it using Applicative.
doSomething = do
  a <- f
  b <- g
  c <- h
  pure (a,b,c)


-- This function requires Monad, since g
-- and h are operating on nested monadic
-- structure.
doSomething' n = do
  a <- f n
  b <- g a
  c <- h b
  pure (a,b,c)


f :: Integer -> Maybe Integer
f 0 = Nothing
f n = Just n


g :: Integer -> Maybe Integer
g i | even i    = Just (i+1)
    | otherwise = Nothing


h :: Integer -> Maybe String
h i = Just ("10191" ++  show i)
