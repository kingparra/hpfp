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

Monads are just a wrapper around function application.
The bind operator takes something in a structure on the left
and a callback function to apply on the right. The callback
function is operates on an element within the structure,
but it doesn't do any destructuring itself.
The wrapper component performs checks and then extracts the
element from the structure.
Whenever you take the term out of the structure with bind,
the instance of Monad with the checks is implicitly executed.

~ Me, committing the sin of explaining by analogy


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

There are commutative monads that do not order actions.

The ``Monad`` typeclass is generalized structure manipulation with some laws to
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
to do with which monoid is being used.

::

  ·∾ liftM3 (,,) [1,2] [3] [5,6]
  [(1,3,5),(1,3,6),(2,3,5),(2,3,6)]

  ·∾ zipWith3 (,,) [1,2] [3] [5,6]
  [(1,3,5)]


18.3 do syntax and monads
-------------------------
These are all equivalent:

::

  do
    putStrLn "a"
    putStrLn "b" 

  (
    putStrLn "a" >>
    putStrLn "b"
  )

  (
    putStrLn "a" *>
    putStrLn "b"
  )

And these are equivalent to each other, too::

  do
    name <- getLine
    putStrLn name

  getLine >>= (\name -> putStrLn name)

  getLine >>= putStrLn

Here are the rewrite rules that GHC uses
to desugar ``do`` blocks.

::

  do { a <- f; m }  ≡  f >>= (\a -> do { m })
  do { f; m }       ≡  f >> do { m }
  do { m }          ≡  m


Law 1::

  do 
    y <- return x
    f y

  ≡

  do f x

Law 2::

  do
    x <- m
    return x

  ≡

  do m

Law 3::

  do
    b <- do { a <- m; f a }
    g b

  ≡

  do
    a <- m
    b <- f a
    g b

  ≡

  do
    a <- m
    do { b <- f a; g b }

Two-line do notation
A two-line do block desugars to the infix (>>=) operator::

  do x <- m
     e

  -- ... desugars to:
  m >>= (\x ->
  e )

One-line do notation
For a one-line do block, you can just remove the do::

  main = do putStrLn "Hello, world!"

  -- ... desugars to:
  main = putStrLn "Hello, world!"

Multi-line do notation
do notation of more than two lines is equivalent to multiple nested dos::

  do x <- mx
     y <- my
     z

  -- ... is equivalent to:
  do x <- mx
     do y <- my
        z

  -- ... desugars to:
  mx >>= (\x ->
  my >>= (\y ->
  z ))

let in do notation
Non-recursive let in a do block desugars to a lambda::

  do let x = y
     z

  -- ... desugars to:
  (\x -> z) y

When fmap alone isn't enough
^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Consider this example...

::

  ·∾ putStrLn <$> getLine
  this
    
What is ``putStrLn <$> getLine`` intended to do?
Why would mapping ``putStrLn`` over ``getLine`` print the line?
Why would I think that ``putStrLn <$> getLine`` could 
possibly work in the first place?
I guess the intent is to print the line we got, like ``getLine >>= putStrLn``.
I guess we're trying to get the ``String`` out of ``IO String`` so we can
use it with ``putStrLn :: String -> IO ()``, but that doesn't match our typesig.
In the line "what join does here is merge the effects of getLine and
putStrLn into a single IO action", what does merge mean?
What is the internal structure of an ``IO ()``?
How does the ``putStrLn <$> getLine`` example relate to
desugaring the do block?
What am I supposed to be learning here?
This is confusing and I don't feel like I learned 
anything from it.

One of the strengths of Haskell is that we can
refer to, compose, and map over effectful
computations without performing them.

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

Note that we are able to evaluate IO actions
multiple times. This will be significant later.

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
  ·∾ noNegative n | n >= 0 = Just n | otherwise = Nothing
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


.. topic:: Flow-style case and guards

   Did you know that you can write guards in flow-style? Check it out!

   ::

      ·∾ testNoNeg n | n >= 0 = Just n | otherwise = Nothing
      ·∾ noNegative n = case n of { n | n >= 0 -> Just n | otherwise -> Nothing }

One interesting thing is that because of laziness and how monads work is that
the bind operator can be short-cuirting, like this:

::

  ·∾ Nothing >>= undefined
  Nothing

  ·∾ (Just 1) >>= undefined
  *** Exception: Prelude.undefined
  CallStack (from HasCallStack):
    error, called at libraries/base/GHC/Err.hs:74:14 in base:GHC.Err
    undefined, called at <interactive>:263:14 in interactive:Ghci46


18.5 Monad laws
---------------
Here are the Monad laws in their most commonly used representations.

::
 
  .
          m >>= return   ≡  m
   return x >>= f        ≡  f x
  (m >>= f) >>= g        ≡  m >>= (\x -> f x >>= g)

You can also represent them using do notation, like this:

::

  do { x <- m; return x }       ≡  m
  do { y <- return x; f y }     ≡  f x
  do { a <- m; b <- f a; g b }  ≡  m >>= (\x -> f x >>= g)

Monad and applicative operations should relate as follows:

::
  
  pure ≡ return
  m1 <*> m2      ≡      m1 >>= (\x1 -> m2 >>= (\x2 -> return (x1 x2)))

The above laws imply:

::

  fmap f xs ≡ xs >>= return . f
  (>>) ≡ (*>)

Here is a short exceprt from "Monad for Functional Programming" by Philip Wadler.
Lists form a monad, and for this monad map applies a function to each element
of a list, and join concatenates a list of lists.

::

  map id ≡ id
  map (f . g) ≡ map f . map g

  map f . return ≡ return . f
  map f . join   ≡ join . map (map f)

  join . return      ≡ id
  join . map return  ≡ id
  join . map join    ≡ join . join

  m >>= k ≡ join (map k m)


18.7 Chapter exercises
----------------------
Write ``Monad`` instances for the following types.
Use the ``QuickCheck`` properties we showed you to validate your instances.

1. Welcome to the ``Nope Monad``, where nothing happens and nobody cares:

   ::

     data Nope a = NopeDotJpg

   Ok, I'll give it a shot:

   ::

     data Nope a = NopeDotJpg
     instance Functor Nope where { fmap _ NopeDotJpg = NopeDotJpg }
     instance Applicative Nope where { fmap _ NopeDotJpg = NopeDotJpg }
     instance Monad Nope where { _ >>= _ = NopeDotJpg }

2. Problem

   ::

     data BahEither b a = PLeft | PRight b

   Here's my attempt

   ::

     data BahEither b a = PLeft | PRight b
     instance Functor (BahEither b) where { fmap f (PRight x) = PRight x; fmap _ PLeft = PLeft  }


3. Write a ``Monad`` instance for ``Identity``:

   ::

     newtype Identity a = Identity a
       deriving (Eq, Ord, Show)

     instance Functor Identity where
       fmap = undefined

     instance Applicative Identity where
       pure = undefined
       (<*>) = undefined

     instance Monad Identity where
       return = pure
       (>>=)  = undefined

4. This one should be easier than the ``Applicative`` instance was.
   Remember to use the ``Functor`` that ``Monad`` requires, then see
   where the chips fall:

   ::

     data List a = Nil | Cons a (List a)
   Write the following functions using the methods provided by ``Monad`` and
   ``Functor``. Using stuff like identity and composition is fine, but it has
   to type chack with the types provided.

   1. ``j :: Monad m => m (m a) -> m a``
      Expecting the following behaviour:

      ::

        >>> j [[1,2],[],[3]]
        [1,2,3]
        >>> j (Just (Just 1))
        Just 1
        >>> j (Just Nothing)
        Nothing
        >>> j Nothing
        Nothing

   2. ``l1 :: Monad m => (a -> b) -> m a -> m b``

   3. ``l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c``

   4. ``a :: Monad m => m a -> m (a -> b) -> m b``

   5. You'll need recursion for this one: ``meh :: Monad m => [a] -> (a -> m b) -> m [b]``

   6. Hint: reuse ``meh``: ``flipType :: (Monad m) => [m a] -> m [a]``

.. pick up on 19.6
