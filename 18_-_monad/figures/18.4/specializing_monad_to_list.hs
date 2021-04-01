-- 18.4 Examples of Monad use
-- 18.4.1 List
-- 18.4.1.1 Specializing the types
-- page 760
(>>=) :: Monad m => m  a -> (a ->  m  b)  -> m  b
(>>=) ::           [ ] a -> (a -> [ ] b) -> [ ] b

-- same as pure
return :: Monad m => a -> m a
return :: a -> [ ] a

-- or more syntactically common
(>>=) :: [a] -> (a -> [b]) -> [b]
return :: a -> [a]

