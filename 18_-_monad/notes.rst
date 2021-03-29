*******************
 Chapter 18: Monad
*******************


18.1 Monad
----------
In this chapter, we:

* define Monad, its operations and laws;
* look at several examples of monads in practice;
* write the Monad instances for various types;
* address some misinformation about monads.

.. For a quick overview, check out this video:
.. "Haskell for imperative programmers",
.. "#17 Monad"
.. https://www.youtube.com/watch?v=IBB7JpbClo8
.. &list=PLe7Ei6viL6jGp1Rfu0dil1JH1SHk9bgDV
.. &index=17


18.2 Sorry -- a monad is not a burrito
--------------------------------------
A monad is an applicative functor with
some unique features that make it a bit
more powerful than either alone.

You can think of monad as another way of
applying functions over structure, with
a couple of additional features.

First let's take a look at the type class
definition:

  .. include:: figures/18.2/monad_typeclass_definition.rst

18.2.1 Applicative m
^^^^^^^^^^^^^^^^^^^^
You can derive applicative and functor in
terms of monad, just as you can derive
functor in terms of applicative.

What does this mean? It means that, for
example, that you can write ``fmap`` using
monadic operations and it works:

  ``fmap f xs``  :math:`=`  ``xs >>= return . f``

For example::

  ·∾ fmap (+1) [1,2,3]
  [2,3,4]

  ·∾ [1,2,3] >>= return . (+1)
  [2,3,4]

Try it for yourself:

.. raw:: html

   <script id="asciicast-tn9lH3aOJcsmX84oyM0Df30TS"
   src="https://asciinema.org/a/tn9lH3aOJcsmX84oyM0Df30TS.js"
   async></script>

It's important to understand this chain of
dependency:

  ``Functor`` -> ``Applicative`` -> ``Monad``

So whenever you implement an instance of
monad for a type, you necessarily have an
applicative and a functor as well.

18.2.2 Core operations
^^^^^^^^^^^^^^^^^^^^^^
The ``Monad`` type class defines three core
operations, although you only need to
define ``(>>=)`` for a minimally complete
instance.

Let's look at all three:

  | (**>>=**)  ::  **m a** -> (**a** -> m b) -> m b
  | (**>>**)   ::  m a -> m b -> m b
  | **return** ::  **a** -> **m a**

(Notice something?)

.. topic:: Operator pronounciation guide

   * ``(>>=)`` is pronounced as "bind"
   * ``(>>)`` is sometimes called the
     "sequencing operator", though there is
     no offical name for it.
   * ``return`` is read as ... return. What
     did you expect?

``return`` puts something inside a context, it
has a default class method of ``pure = return``.

``(>>)`` sequences two actions while
discarding any resulting value of the first
action.

``(>>=)`` is what we'll talk about next.

18.2.3 The novel part of Monad
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
What's unique to monad, at least from the
point of view of types?

We already saw that it's not ``return``.
It also isn't ``(>>)``. And it also isn't
``(>>=)``, at least not in its entirety.

The type of ``(>>=)`` is visibly similar to
that of ``fmap`` and ``(<*>)``, which makes
sense since monads are applicative functors.

For the sake of making this maximally
similar, we're going to change the *m* of
monad to *f*:

.. include:: figures/18.2/fmap_ap_bind_similarities.hs
   :code:

So, the idea of mapping a function over a
value while bypassing its surrounding
structure is not unique to monad.

We can demonstrate this by fmapping a
function of type ``(a -> m b)`` to make it
more like ``(>>=)``, and it will work:

.. include:: figures/18.2/specialized_fmap_vs_bind.txt
   :code:

After mapping a function that generates
additional monadic structure in its return
type, we want a way to discard on layer of
that structure. So, how do we accomplish
that?

Well, at least with lists, we already know
how::

  ·∾ :type concat
  concat :: Foldable t => t (t a) -> t a

The module ``Control.Monad`` provides a
similar function, ``join``::

  ·∾ import Control.Monad (join)

  ·∾ :type join
  join :: Monad m => m (m a) -> m a

Monad, in a sense, is a generalization of
``concat``! ::

  ·∾ import Control.Monad (join)

  ·∾ :set -XTypeApplications

  ·∾ :type concat @[]
  concat @[] :: [[a]] -> [a]

  ·∾ :type join @[]
  join @[] :: [[a]] -> [a]

Allowing the function itself to alter the
structure is something we've not seen in
``Functor`` and ``Applicative``. The ability
to flatten those two layers of structure
into one is what makes ``Monad`` special.

**By putting that** ``join`` **function together with**
``fmap``, **we can create bind.**

**So how do we get bind?**

::

  ·∾ :{
   ⋮ bind :: Monad m => (a -> m b) -> m a -> m b
   ⋮ bind f ma = join (fmap f ma)
   ⋮ :}

  ·∾ :type bind
  bind :: Monad m => (a -> m b) -> m a -> m b

  ·∾ :type flip (>>=)
  flip (>>=) :: Monad m => (a -> m b) -> m a -> m b

  ·∾ :type join
  join :: Monad m => m (m a) -> m a

  ·∾ -- a ~ m a

.. topic:: Deriving join from bind

   What if we go the other way: from ``(>>=)``
   to ``join``?

   Let's start with the type signature for
   ``(>>=)``, and then substitute in what we
   need to get there::

     -- Here is our starting point, bind.
     --
     (>>=) :: Monad m =>     m a -> (a -> m b) -> m b
     --
     -- ...and here is the destination, join.
     --
     join :: Monad m =>  m (m a) -> m a
     --
     -- First off, the input type isn't specific enough,
     -- so let's specialize a into (m b).
     --
     (>>=) :: Monad m => m (m b) -> (m b -> m b) -> m b
     --
     -- At this point, we almost have the type signature
     -- for join!
     --
     (>>=) :: Monad m => m (m b) -> (m b -> m b) ->  m b
     join  :: Monad m => m (m b)         ->          m b
     --                  ^^^^^^                      ^^^
     --
     -- The final input and output are already correct.
     -- But we have to eliminate the function in the middle.
     --
     (>>=) :: Monad m => m (m a) -> (m a -> m b) ->  m b
     join  :: Monad m => m (m a)         ->          m a
     --                              ^^^^^^^^^^
     -- What function fits this hole? ... id!

     ·∾ :{
      ⋮ join' :: Monad m => m (m a) -> m a
      ⋮ join' x = x >>= id
      ⋮ :}
     ·∾

     ·∾ join' ["one "," two"," and three!"]
     "one two and three!"

     ·∾ join ["one "," two"," and three!"]
     "one two and three!"

18.2.4 What Monad is not
^^^^^^^^^^^^^^^^^^^^^^^^
A monad is not:

1. Impure. Monadic functions are pure
   functions.
2. An embedded language for imperative
   programming. While monads are often used
   for sequencing actions in a way that
   looks like imperative programming, there
   are commutative monads that do not order
   operations.
3. A value. Monads are type classes (or
   algebras, as a general concept), not
   values.
4. About strictness. The monadic operations
   of bind and return are nonstrict.

The ``Monad`` type class is generalized
structure manipulation with some laws to
make it sensible. Just like ``Functor`` and
``Applicative``. That's all there is to it.

18.2.5 Monad also lifts!
^^^^^^^^^^^^^^^^^^^^^^^^
The monad type class also includes a set of
``lift`` functions that are the same as the
ones we already saw in ``Applicative``. They
don't do anything different, but they are
still around because some libraries used
them before applicatives were discovered.

.. raw:: html

   <script id="asciicast-rrxHSW0M3cacRlq9Sut5Fm8et"
   src="https://asciinema.org/a/rrxHSW0M3cacRlq9Sut5Fm8et.js"
   async></script>


18.3 Do syntax and monads
-------------------------
Do syntax works with any monad, not just IO.

This section is going to talk about why
``do`` is sugar and demonstrate what the
``join`` of ``Monad`` can do for us.

To begin with, let's look at some
correspondences::

  (*>) :: Applicative f => f a -> f b -> f b
  (>>) :: Monad m =>       m a -> m b -> m b

For our purposes, ``(*>)`` and ``(>>)`` are
the same thing, sequencing functions, but
with two different constraints.

We can see what do syntax looks like after
the compiler desugars it for us by manually
transforming it ourselves:

.. include:: figures/18.3/SequencingAndBinding.hs
   :code:

18.3.1 When fmap alone isn't enough
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
This won't work::

  ·∾ import Control.Monad

  ·∾ putStrLn <$> getLine
  Will this print?

Why? Look at the type signature::

  ·∾ :type putStrLn <$> getLine
  putStrLn <$> getLine :: IO (IO ())

This will fix it::

  ·∾ join (putStrLn <$> getLine)
  Will *this* print?
  Will *this* print?

**What join did here is merge the effects
of** ``getLine`` **and** ``putStrLn`` **into
a single IO action.** As it happens, the
cleanest way to express ordering in a lambda
calculus is through nesting expressions.

A more thorough exploration:

.. raw:: html

   <script id="asciicast-ZAeJbhDpDB4VPC6hNZFpvAyIl"
   src="https://asciinema.org/a/ZAeJbhDpDB4VPC6hNZFpvAyIl.js"
   async></script>

Let's get back to desugaring ``do`` syntax
with our now-enriched understanding of what
monads do for us:

.. include:: figures/18.3/BindingAndSequencing.hs
   :code:

**One key insight is how name binding with the
assignment arrow like** ``do { name <- getLine;
putStrLn name }`` **translates into bind, like**
``getLine >>= (\name -> putStrLn name)`` **.**
When the function to be sequenced doesn't return
a value that is later used, the "semicolon"
translates into ``(>>)``.


18.4 Examples of Monad use
--------------------------
What we need now is to see how monads work
in code, with ``Monad``'s other than IO.

18.4.1 List
^^^^^^^^^^^

18.4.1.1 Specializing the types
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.. include:: figures/18.4/specializing_monad_to_list.hs
   :code:

18.4.1.2 Example of the List Monad in use
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
::

  twiceWhenEven :: [Integer] -> [Integer]
  twiceWhenEven xs = do
    x <- xs
    if even x
    then [x*x, x*x]
    else [x*x]

The ``x <- xs`` line binds individual
values out of the list input, like a list
comprehension, giving us an ``a`` for use
with ``(>>=)``. The if-then-else is our
``(a -> m b)``.

::

  ·∾ :load figures/18.4/TwiceWhenEven.hs
  [1 of 1] Compiling TwiceWhenEven
  ( figures/18.4/TwiceWhenEven.hs, interpreted )
  Ok, one module loaded.

  ·∾ twiceWhenEven [1,2,8,5]
  [1,4,4,64,64,25]

18.4.2 Maybe Monad
^^^^^^^^^^^^^^^^^^

18.4.2.1 Specializing the types
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.. include:: figures/18.4/specializing_monad_to_maybe.hs
   :code:

18.4.2.2 Using the Maybe Monad
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Whenever you interact with something within
the ``Maybe`` monad, there is a potential for
the value to be ``Nothing``. **Using the Maybe
Monads do notation, each subsequent line (or
action) in the do block depends on the
previous action not being** ``Nothing`` **to
continue evaluating the** ``Just`` **value on
the next line.**

The files ``figures/18.4/MaybeMonadV{1,2,3}.hs``
demonstrate a few ways of writing this. But
here are snippets of the changed function
between all three versions of the module, for
convenience::

  module MaybeMonadV1 where
    . . .
  mkSphericalCow :: String -> Int -> Int -> Maybe Cow
  mkSphericalCow name' age' weight' =
    case noEmpty name' of
      Nothing -> Nothing
      Just nammy ->
        case noNegative age' of
          Nothing -> Nothing
          Just agey ->
            case noNegative weight' of
              Nothing -> Nothing
              Just weighty -> weightCheck
                (Cow nammy agey weighty)
    . . .


  module MaybeMonadV2 where
    . . .
  -- With do syntax things are much more concise.
  mkSphericalCow' :: String -> Int -> Int -> Maybe Cow
  mkSphericalCow' name' age' weight' = do
    nammy   <- noEmpty name'
    agey    <- noNegative age'
    weighty <- noNegative weight'
    weightCheck (Cow nammy agey weighty)
    . . .


  module MaybeMonadV3 where
    . . .
  -- Here it is, rewritten to use (>>=), just because.
  mkSphericalCow'' :: String -> Int -> Int -> Maybe Cow
  mkSphericalCow'' name' age' weight' =
    noEmpty name' >>=
    \nammy ->
      noNegative age' >>=
      \agey ->
        noNegative weight' >>=
        \weighty ->
          weightCheck (Cow nammy agey weighty)

Here's a terminal recording of me interacting
with those functions. Spoiler alert: they do
the same thing.

.. raw:: html

   <script id="asciicast-LPfj7n9kelcJqpfo35eTBQDOV"
   src="https://asciinema.org/a/LPfj7n9kelcJqpfo35eTBQDOV.js"
   async></script>

At this point, you may have noticed a
similarity between how the Maybe monad behaves
and how Maybe works with applicative. Can we
use ``Applicative``, instead of ``Monad``?

Well, if your do syntax looks like this::

  doSomething = do
    a <- f
    b <- g
    c <- h
    pure (a, b, c)

Then you can rewrite it using ``Applicative``.

On the other hand, if you have something like
this::

  doSomething' = do
    a <- f n
    b <- g a
    c <- h b
    pure (a, b, c)

You're going to need a ``Monad`` because ``g``
and ``h`` are producing monadic structure
based on values that can only be obtained by
depending on values generated from monadic
structure.

The long and short of it:

* With the ``Maybe`` ``Applicative`` each
  ``Maybe`` computation fails or succeeds
  independently of each other. You're lifting
  functions that are also ``Just`` or
  ``Nothing`` over ``Maybe`` values.
* **With the** ``Maybe`` ``Monad``,
  **computations contributing to the final
  result can choose to return** ``Nothing``
  **based on previous computations.**

18.4.3 Either
^^^^^^^^^^^^^

18.4.3.1 Specializing the types
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.. include:: exercises/18.4/specializing_monad_to_either.hs
   :code:

Why do we keep on doing this? To remind you
that the types always show you the way, once
you've figured them out.

18.4.3.2 Using the Either Monad
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
As you can see, ``Either`` always short-circuits
on the first thing to have failed. It must, because
in the monad later values can depend on previous ones.

.. raw:: html

   <script id="asciicast-yBFOhd4PYby56ljpIkynEGqKt"
   src="https://asciinema.org/a/yBFOhd4PYby56ljpIkynEGqKt.js"
   async></script>

.. include:: exercises/18.4.3.3_-_either_monad.rst


18.5 Monad laws
---------------

18.5.1 Identity laws
^^^^^^^^^^^^^^^^^^^^
Basically both of these laws are saying that
``return`` should be neutral and not perform
any computation.

* **Right identity**: ``m >>= return`` :math:`=` ``m``
* **Left identity**: ``return x >>= f`` :math:`=` ``f x``

18.5.2 Associativity
^^^^^^^^^^^^^^^^^^^^
* **Associativity**:

  ``(m >>= f) >>= g`` :math:`=` ``m >>= (\x -> f x >>= g)``

  Rewritten for more visual similarity::

    (m >>= (\x -> g x)) >>= (\y -> h y)
                    ≡
     m >>= (\x -> g x >>= (\y -> h y))

  ...this time, using ``Control.Monad.((>=>))``,
  instead of the ``(>>=)`` operator:

  ``(f >=> g) >=> h`` :math:`=` ``f >=> (g >=> h)``
