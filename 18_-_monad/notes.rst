*******************
 Chapter 18: Monad
*******************


Monads form one of the core components for constructing
Haskell programs. In their most general form monads are
an algebraic building block that can give rise to ways
of structuring control flow, handling data structures,
and orchestrating logic. Monads are a very general
algebraic way of structuring code and have a certain
reputation for being confusing. However their power and
flexibility have become foundational to the way modern
Haskell programs are structured.

There is a singluar truth to keep in mind when learning
monads. A monad is just its algebraic laws. Nothing
more, nothing less. Anologies and metaphors will not
lead to understanding.

~ Stephen Dhiel

A monad is the combination of three things:
1. Some data in a wrapper. 
2. Something to wrap the data up.
3. Something to apply a function that works on the wrapped value.

~ Some dude on YouTube comments


18.1 Monad
----------
Older implementations of Haskell didn't use monads for
constructing and transforming IO actions.

In this chapter we will:

* Define ``Monad``, its operations, and its laws.
* Look at several examples of monads in practice.
* Write the ``Monad`` instances for various types.
* Address some misinformation about monads.


18.2 Sorry - a monad is not a burrito
-------------------------------------
You can think of monads as another way of applying
functions over structure, with a couple additional
features.

::

  ·∾ :info Monad
  type Monad :: (* -> *) -> Constraint
  class Applicative m => Monad m where
    (>>=)  :: m a -> (a -> m b) -> m b
    (>>)   :: m a -> m b -> m b
    return ::   a -> m a
    {-# MINIMAL (>>=) #-}

Applicative m
^^^^^^^^^^^^^
The chain of inheritance for monad is ``Functor`` -> ``Applicative`` -> ``Monad``. 
You can implement ``Functor`` and ``Applicative`` in terms of ``Monad``.

Here is and example of how to implement fmap in terms of bind.

::

  ·∾ :type \f xs -> xs >>= return . f
  \f xs -> xs >>= return . f :: Monad m => (a -> b) -> m a -> m b

Or without the point-free:

::

  ·∾ fmapm = \f xs -> (flip (>>=)) (\x -> return (f x)) xs
  ·∾ :type fmapm
  fmapm :: Monad m => (t -> b) -> m t -> m b

**One interesting question to ask is: "What class methods are in Monad
that can't be derived from methods in Functor or Applicative?"**

You may think that the bind operation is unique, but with some restructuring
you'll find that bind can be derived from ``join`` and ``map``.
Only the ``join`` operation isn't inherited.

::

  ·∾ (flip (>>=)) return [1,2,3] -- id
  [1,2,3]

  ·∾ (flip (>>=)) id [[1,2,3],[4,5,6]] -- concat (or join)
  [1,2,3,4,5,6]

  ·∾ (flip (>>=)) (return . (+3)) [1..5] -- map
  [4,5,6,7,8]

You may remember seeing something resembling ``concat`` in semigroup and monoid,
but Monad doesn't inherit from those typeclasses.

If you specialize the type signatures, you can write bind in terms of fmap and
join.

::

  ·∾ -- b ~ Monad m => m b
  ·∾ mapm = fmap :: Monad m => (a -> m b) -> m a -> m (m b)
  ·∾ :type join
  join :: Monad m => m (m a) -> m a
  ·∾ :type (\f xs -> join (mapm f xs))
  (\f xs -> join (mapm f xs)) :: Monad m => (a1 -> m a2) -> m a1 -> m a2

Why doesn't ``Monad`` inherit from ``Semigroup``? It could use ``(<>)`` and
``fmap`` to derive a default class method implementation of ``(>>=)``.

What Monad is not
^^^^^^^^^^^^^^^^^
Monad is not..

* Impure
* An embedded language for imperative programming.
* A value.
* About strictness.

The ``Monad`` typeclass is generilized structure manipulation with some laws to
make it sensible. Just like ``Functor`` and ``Applicative``.

Monad also lifts!
^^^^^^^^^^^^^^^^^
::

  ·∾ -- Monad also lifts!

  ·∾ :type liftA
  liftA :: Applicative f => (a -> b) -> f a -> f b
  ·∾ :type liftM
  liftM :: Monad m => (a1 -> r) -> m a1 -> m r

  ·∾ :type liftA2
  liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
  ·∾ :type liftM2
  liftM2 :: Monad m => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r

  ·∾ :type zipWith
  zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
  ·∾ :type liftA2
  liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c

  ·∾ zipWith (+) [3,4] [5,6]
  [8,10]
  ·∾ liftA2 (+) [3,4] [5,6]
  [8,9,9,10]

The differing behavior between ``zipWith`` and ``liftA2`` has
to do with wich lift monoid is being used.

::

  ·∾ liftM3 (,,) [1,2] [3] [5,6]
  [(1,3,5),(1,3,6),(2,3,5),(2,3,6)]
  ·∾ 
  ·∾ zipWith3 (,,) [1,2] [3] [5,6]
  [(1,3,5)]


18.3 do syntax and monads
-------------------------
``do { putStrLn "a"; putStrLn "b" }`` ≡ 
``putStrLn "a" >> putStrLn "b"`` ≡ 
``putStrLn "a" *> putStrLn "b"``.

::

  do
    name <- getLine
    putStrLn name

  ≡ 

  getLine >>= putStrLn

  ≡

  getLine >>= (\name -> putStrLn name)

  ·∾ :type \f ma -> do { name <- ma; f name }
  \f ma -> do { name <- ma; f name } :: Monad m => (t -> m b) -> m t -> m b

  ·∾ :type \f ma -> ma >>= f
  \f ma -> ma >>= f :: Monad m => (a -> m b) -> m a -> m b

When fmap alone isn't enough
^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Why would I think that ``putStrLn <$> getLine`` could possibly work in the first place?
What am I supposed to be learning here?


One of the strengths of Haskell is that we can refer to, compose, and map over
effectful computations without performing them.

::

  ·∾ printOne = putStrLn "1"
  ·∾ printTwo = putStrLn "2"
  ·∾ twoActions = (printOne, printTwo)
  ·∾ :type twoActions 
  twoActions :: (IO (), IO ())
  ·∾ fst twoActions 
  1
  ·∾ snd twoActions 
  2
  ·∾ fst twoActions 
  1

Note that we are able to evaluate IO actions multiple times.
This will be significant later.

::

  bindingAndSequencing :: IO ()
  bindingAndSequencing = do
    putStrLn "name pls:"
    name <- getLine
    putStrLn ("y hello thar: " ++ name)

::

  bindingAndSequencing :: IO ()
  bindingAndSequencing =
    putStrLn "name pls:" >>
    getLine >>=
    \name -> 
      putStrLn ("y hello thar: " ++ name)

::

  twoBinds :: IO ()
  twoBinds = do
    putStrLn "name pls:"
    name <- getLine
    putStrLn "age pls:"
    age <- getLine
    putStrLn ("y helo thar: " ++ name ++ " who is: "
              ++ age ++ " years old.")

  twoBinds' :: IO ()
  twoBinds' =
    putStrLn "name pls:" >>
    getLine >>=
    \name ->
      putStrLn "age pls:" >>
      getLine >>=
      \age ->
        putStrLn ("y helo thar: "
                  ++ name ++ " who is: "
                  ++ age ++ " years old.")


18.4 Examples of Mondad use
---------------------------

List
^^^^
::

  ·∾ :{
   ⋮ twiceWhenEven :: [Integer] -> [Integer]
   ⋮ twiceWhenEven xs = do
   ⋮   x <- xs
   ⋮   if even x
   ⋮   then [x*x, x*x]
   ⋮   else [x*x]
   ⋮ :}
  ·∾ 
  ·∾ twiceWhenEven [1..3]
  [1,4,4,9]

  ·∾ twiceWhenEven xs = do { x <- xs; if even x then [x*x,x*x] else [] }
  ·∾ twiceWhenEven [1..3]
  [4,4]

Maybe Monad
^^^^^^^^^^^
.. TODO Pick up on page 746

::

  ·∾ -- Using the Maybe monad
  ·∾ 
  ·∾ :{
   ⋮ data Cow = Cow {
   ⋮     name :: String
   ⋮   , age  :: Int
   ⋮   , weight :: Int
   ⋮   } deriving (Eq, Show)
   ⋮ :}
  ·∾ 
  ·∾ :{
   ⋮ noEmpty :: String -> Maybe String
   ⋮ noEmpty "" = Nothing
   ⋮ noEmpty str = Just str
   ⋮ :}
  ·∾ 
  ·∾ :{
   ⋮ noNegative :: Int -> Maybe Int
   ⋮ noNegative n | n >= 0 = Just n
   ⋮              | otherwise = Nothing
   ⋮ :}
  ·∾ 
  ·∾ :{
   ⋮ weightCheck :: Cow -> Maybe Cow
   ⋮ weightCheck c =
   ⋮   let { w = weight c; n = name c } in
   ⋮   if n == "Bess" && w > 499
   ⋮   then Nothing
   ⋮   else Just c
   ⋮ :}
  ·∾ 
  ·∾ :{
   ⋮ mkSphericalCow :: String -> Int -> Int -> Maybe Cow
   ⋮ mkSphericalCow name' age' weight' =
   ⋮   case noEmpty name' of
   ⋮     Nothing -> Nothing
   ⋮     Just nammy ->
   ⋮       case noNegative age' of
   ⋮         Nothing -> Nothing
   ⋮         Just agey ->
   ⋮           case noNegative weight' of
   ⋮             Nothing -> Nothing
   ⋮             Just weighty ->
   ⋮               weight
   ⋮               weightCheck (Cow nammy agey weighty)
   ⋮ :}
  ·∾ 
  ·∾ 
  ·∾ mkSphericalCow "Bess" 5 499
  Just (Cow {name = "Bess", age = 5, weight = 499})
  ·∾ 
  ·∾ mkSphericalCow "Bess" 5 500
  Nothing
  ·∾ 
  ·∾ :{
   ⋮ mkSphericalCow' name' age' weight' = do
   ⋮   nammy   <- noEmpty name'
   ⋮   agey    <- noNegative age'
   ⋮   weighty <- noNegative weight'
   ⋮   weightCheck (Cow nammy agey weighty)
   ⋮ :}
  ·∾ 
  ·∾ mkSphericalCow' "Bess" 5 500
  Nothing
  ·∾ 
  ·∾ mkSphericalCow' "Bess" 5 499
  Just (Cow {name = "Bess", age = 5, weight = 499})

If your do syntax looks like this:

::

  doSomething = do
    a <- f
    b <- g
    c <- h
    pure (a,b,c)

You can rewrite it using ``Applicative``. On the other hand, if you have
something like this:

::

  doSomething' n = do
    a <- f n
    b <- g a
    c <- h b
    pure (a,b,c)

Then it won't work... for reasons. 
Reasons that I don't understand.
The long and short of it:

1. With the maybe applicative, each maybe computation fails or succeds
   independely of one another. You're lifting function that are also ``Just``
   or ``Nothing`` over ``Maybe`` values.

2. With the maybe monad, computation contributing to the final result can
   choose to return nothing based on previous computations.

What does that mean? I don't know.

Exploding a shperical cow
^^^^^^^^^^^^^^^^^^^^^^^^^
Here's how simple an instance of Monad can be.

::

  instance Monad Maybe where
    return x = Just x
    (Just x) >>= k = k x
    Nothing  >>= _ = Nothing

