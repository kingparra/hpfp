*********
 Summary
*********

Superclasses::

  Applicative m => Monad m where {...}

Operations::

  (>>=)  :: m a  ->  (a -> m b)  ->  m b
  (>>)   :: m a  ->     m b      ->  m b
  return ::   a  ->     m b

Minimal pragma::

  (>>=)

Laws::

  +
             left identity
          return a >>= k  ≡  k a

             right identity
            m >>= return  ≡  m

              associativity
  m >>= (\x -> k x >>= h) ≡  (m >>= k)

The monad laws can be phrased in terms of map and join::

  -- First we'll define map and join in terms of bind and return.
  map :: (a -> b) -> (m a -> m b)
  map f m = m >>= (\a -> return (f a))

  -- Join is like the monad version of Data.List.concat
  join :: m (m a) -> m a
  join z = z >>= (\m -> m)

  -- laws
  map id ≡ id
  map (f . g) ≡ map f . map g

  map f . return      ≡  return . f
  map f . join        ≡  join . map (map f)

  join . return       ≡  id
  join . map return   ≡  id
  join . map join     ≡  join . join
  m >>= k             ≡  join (map k m)


Notice the resemblance between these two operations::


  -- this _almost_ looks like map!
  flip (>>=)    :: Monad m => (a -> m b) -> m a -> m b
  Data.List.map ::            (a -> b)   -> [a] ->  [b]
  -- let's change our b to [b] and see if we can get closer
  Data.List.map ::〈b := [b]〉(a -> [b]) -> [a] -> [[b]]
  -- now we need to collapse the return value to match the signature
  concat        :: [[a]] -> [a]
  -- putting it all together
  concat . map  ::            (a -> [b]) -> [a] ->  [b]

  -- next to each other
  concat . map  ::            (a -> [b]) -> [a] ->  [b]
  flip (>>=)    :: Monad m => (a -> m b) -> m a -> m b

  -- flip (>>=) is alsow known as Control.Monad((=<<))

Do blocks and their equivalent expressions::

  -- do and bind
  do
    x <- action1
    y <- action2
    return (x + y)

  action1 >>= (\x ->
    action2 >>= (\y ->
      return (x + y)))

  -- do and >=>
  -- Control.Monad.((>=>))
  (bs >=> cs) a

  do b <- bs a
     cs b

In the above code, the <- symbol in the do
notation represents the binding of a monadic
value to a variable.
