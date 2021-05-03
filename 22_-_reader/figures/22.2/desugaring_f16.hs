boop = (*2)
doop = (+10)


bip = boop . doop


boopDoop :: Integer -> Integer
boopDoop = do
  a <- boop
  b <- doop
  return (a + b)


{- Given that,
 -
 -  instance Monad ((->) r) where
 -    return   =  const
 -    f >>= k  =  \r -> k (f r) r
 -
 - desugar the do block in boopDoop.
 -}


-- [ do { ; } := (>>=) ]
boopDoop' =
  boop >>= (\a ->
    doop >>= (\b ->
      return (a + b)))


-- [ return := const ]
boopDoop'' =
  boop >>= (\a ->
    doop >>= (\b ->
      const (a + b)))


--   f >>= k  =  \r -> k (f r) r

boopDoop''' =
    boop >>= (\a -> doop >>= (\b -> const (a + b)))


--     f >>= k  =  \r -> k (f r) r


--  [ f := boop ]
--
--     boop >>= k  =  \r -> k (boop r) r


--  [ k := (\a -> doop >>= (\b -> const (a + b))) ]
--
--     \r -> (\a -> doop >>= (\b -> const (a + b))) (boop r) r

--     \r -> (\a ->
--        doop >>= (\b -> const (a + b))
--        ) (boop r) r
--
--  [
--    doop >>= (\b -> const (a + b))
--    :=
--    (\v -> (\b -> const (a + b)) (doop v) v)
--  ]
--

{-# ANN boopDoopDesugared "Hlint: Ignore redundant lambda"  #-}
boopDoopDesugared =
   (\r ->
     (\a ->
       (\v ->
          (\b -> const (a + b))
          (doop v)
          v
       )
     )
     (boop r)
     r
   )
