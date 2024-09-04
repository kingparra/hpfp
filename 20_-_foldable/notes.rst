**********************
 Chapter 20: Foldable
**********************

.. conjecture --> argument (proof) --> theorem


20.1 Foldable
-------------
This chapter will expand on the idea of
catamorphisms and generalize it to many
datatypes. The folds we wrote previously
mostly relied on implicit monoidal operations.
As we'll see in this chapter, generalizing
catamorphisms to other datatypes depends on
understanding the monoids for those structures
and, in some cases, making them explicit.

This chapter will cover:

* the ``Foldable`` class and its core operations;
* the monoidal nature of folding;
* standard operations deriving from folding.


20.2 The Foldable class
-----------------------
The Foldable class represents data structures
that can be reduced to a summary value one
element at a time.

Here is the info output from GHCi:

.. include:: figures/20.3/foldable_info.txt
   :code:

As you can see, only ``foldMap`` or ``foldr``
are required for a minimally complete class
definition. All of the other class methods are
implemented in terms of them, as default class
method declarations.

The first line of the class declaration
includes the kind signature for k ``(t :: * ->
*)``. **Types that take more than one type
argument, such as tuples and ``Either``, will
necessarily have their first type argument
included as part of their structure.**

...wait, I thought that instances operated on
the outermost type argument, not the first?

To follow along with the examples in this
chapter, import ``Data.Foldable`` and
``Data.Monoid``.


20.3 Revenge of the monoids
---------------------------
Folding necessarily implies a binary
associative operation that has an identity
value (a monoid).

The first two operations defined in
``Foldable`` make this explicit::

  class Foldable (t :: * -> *) where

    fold    :: Monoid m => t m -> m

    foldMap :: Monoid m => (a -> m) -> t a -> m

Where is the input function to fold, you ask?
Look, that's what I'm saying: You don't
need a function argument. Instead, the function
for combining elements is inferred to be the
``(<>)`` operation of the monoid that those
elements have for their datatype.

In the following example::

  ·∾ fold ["Hello ","world!"]
  "Hello world!"

...the monoidal operation ``(<>)`` was used,
which is the same as ``(++)``, or concatenation.

Here is the haddock documentation for fold,
which has a few more examples:

  **fold**

  Given a structure with elements whose type is
  a ``Monoid``, combine them via the monoid's
  ``(<>)`` operator. This fold is right-associative
  and lazy in the accumulator. When you need a
  strict left-associative ``fold``, use ``foldMap'``
  instead, with ``id`` as the map.

  **Examples**

  Basic usage::

    >>> fold [[1, 2, 3], [4, 5], [6], []]
    [1,2,3,4,5,6]

    >>> fold $ Node (Leaf (Sum 1)) (Sum 3) (Leaf (Sum 5))
    Sum {getSum = 9}

  Folds of unbounded structures do not terminate
  when the monoid's (<>) operator is strict::

    >>> fold (repeat Nothing)
    * Hangs forever *

  Lazy corecursive folds of unbounded structures are fine::

    >>> take 12 $ fold $ map (\i -> [i..i+2]) [0..]
    [0,1,2,1,2,3,2,3,4,3,4,5]

    >>> sum $ take 4000000 $ fold $ map (\i -> [i..i+2]) [0..]
    2666668666666


20.3.1 And now for something different
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
::

  ·∾ import Data.Monoid
  ·∾ import Data.Foldable
  ·∾
  ·∾ -- The first arg to foldMap must map each element to a Monoid.
  ·∾ 
  ·∾ foldMap Sum [1,2,3,4]
  Sum {getSum = 10}
  ·∾ 
  ·∾ foldMap Product [1,2,3,4]
  Product {getProduct = 24}
  ·∾ 
  ·∾ foldMap All [True,False,True]
  All {getAll = False}
  ·∾ 
  ·∾ foldMap Any [(3==4),(9>5)]
  Any {getAny = True}
  ·∾ 
  ·∾ xs = [Just 1, Nothing, Just 5]
  ·∾ foldMap First xs
  First {getFirst = Just 1}
  ·∾ foldMap Last xs
  Last {getLast = Just 5}
  ·∾ 
  ·∾ -- foldMap can also have a function to map that's different from the Monoid it's using
  ·∾ xs = map Product [1..3]
  ·∾ 
  ·∾ foldMap (*5) xs
  Product {getProduct = 750}
  ·∾ 
  ·∾ xs = map S
  Semigroup  Show       ShowS      String     Sum
  ·∾ xs = map Sum [1..3]
  ·∾ foldMap (*5) xs
  Sum {getSum = 30}
  ·∾ 
  ·∾ -- It can map the function to each value first and then use the Monoid instance
  ·∾ -- to reduce them to one value.
  ·∾ -- Compare this to foldr, where the function has the Monoid instance baked in:
  ·∾ foldr (*) 5 [1,2,3]
  30
  ·∾ sumXs = map Sum [2..4]
  ·∾ sumXs
  [Sum {getSum = 2},Sum {getSum = 3},Sum {getSum = 4}]
  ·∾ 
  ·∾ foldr (*) 3 sumXs -- this doesn't seem right, is it a typo?
  Sum {getSum = 72}
  ·∾ foldr (*) 1 sumXs -- this doesn't seem right, is it a typo?
  Sum {getSum = 24}
  ·∾ -- [2, 6, 24]
  ·∾ 
  ·∾ -- It's worth pointing out that if what you're trying to fold only contains
  ·∾ -- one value, declaring a Monoid instance won't change the behavior of foldMap
  ·∾ fm = foldMap (*5)
  ·∾ fm (Just 100) :: Product Integer 
  Product {getProduct = 500}
  ·∾ fm (Just 5) :: Sum Integer 
  Sum {getSum = 25}
  ·∾ 
  ·∾ fm Nothing :: Sum Integer 
  Sum {getSum = 0}
  ·∾ fm Nothing :: Product Integer 
  Product {getProduct = 1}


20.4 Demonstrating Foldable instances
-------------------------------------
Let's turn our attention to implementing
``Foldable`` instances for different types.

20.4.1 Identity
^^^^^^^^^^^^^^^
::

  ·∾ data Identity a = Identity a deriving (Eq, Show)

  ·∾ :{
   ⋮ instance Foldable Identity where
   ⋮   foldr f z (Identity x) = f x z
   ⋮   foldl f z (Identity x) = f z x
   ⋮   foldMap f (Identity x) = f x
   ⋮ :}

  ·∾ foldr (*) 1 (Identity 5)
  5

  ·∾ foldr (*) 5 (Identity 5)
  25

  ·∾ foldMap (*5) (Identity 100) :: Product Integer
  Product {getProduct = 500}


20.4.2 Maybe
^^^^^^^^^^^^
This one is a little more interesting because,
unlike with ``Identity``, we have to account
for the ``Nothing`` cases. When the ``Maybe``
value that we're folding is ``Nothing``, we
need to be able to return some "zero" value.

For ``foldr`` and ``foldl``, that zero value
is the start value provided::

  ·∾ foldr (+) 1 Nothing
  1

On the other hand, for ``foldMap`` we use the
``Monoid``'s identity value as our zero::

  ·∾ foldMap (+1) Nothing :: Sum Integer
  Sum {getSum = 0}

When the value is a ``Just`` value, though, we
need to apply the folding function to the
value and, again, dispose of the structure::

  ·∾ foldr (+) 1 (Just 3)
  4

  ·∾ foldMap (+1) (Just 3) :: Sum Integer
  Sum {getSum = 4}

Now let's look at the instance::


  ·∾ data Optional a = Nada | Yep a deriving (Eq, Ord, Show)

  ·∾ :{
   ⋮ instance Foldable Optional where
   ⋮   foldr _ z Nada = z
   ⋮   foldr f z (Yep x) = f x z
   ⋮   foldl _ z Nada = z
   ⋮   foldl f z (Yep x) = f z x
   ⋮   foldMap _ Nada = mempty
   ⋮   foldMap f (Yep a) = f a
   ⋮ :}

  ·∾ foldMap (+1) (Just 1) :: Sum Int
  Sum {getSum = 2}

With a ``Nada`` and a declared type of ``Sum
Int``, ``foldMap`` gives us ``Sum 0`` because
that was the ``mepmpty`` or identity for ``Sum``::

  ·∾ foldMap (+1) Nada :: Sum Int
  Sum {getSum = 0}

Same idea for ``Product``, which has an
identity of ``1``::

  ·∾ foldMap (+1) Nada :: Product  Int
  Product {getProduct = 1}


20.5 Some basic derived operations
----------------------------------
Here are some examples of several of the other
class methods, which have default definitions
in the class declaration

``toList``
^^^^^^^^^^
::

  ·∾ :type toList
  toList :: Foldable t => t a -> [a]

  ·∾ :doc toList
   List of elements of a structure, from left to right.

  ·∾ toList (Just 1)
  [1]

  ·∾ map toList [Just 1,Just 2,Just 3]
  [[1],[2],[3]]

  ·∾ concatMap toList [Just 1,Just 2,Just 3]
  [1,2,3]

  ·∾ concatMap toList [Just 1,Just 2,Nothing]
  [1,2]

  ·∾ toList (1,2)
  [2]


``null``
^^^^^^^^
::

  ·∾ :type null
  null :: Foldable t => t a -> Bool

  ·∾ :doc null
   Test whether the structure is empty. The
   default implementation is optimized for
   structures that are similar to cons-lists,
   because there is no general way to do
   better.

  ·∾ null (Left 3)
  True

  ·∾ null []
  True

  ·∾ null Nothing
  True

  ·∾ null (1,2)
  False

  ·∾ fmap null [Just 1,Just 2,Nothing]
  [False,False,True]

``length``
^^^^^^^^^^
::

  ·∾ :type length
  length :: Foldable t => t a -> Int

  ·∾ :doc length
   Returns the size/length of a finite
   structure as an 'Int'. The default
   implementation is optimized for structures
   that are similar to cons-lists, because
   there is no general way to do better.

  ·∾ length [(1,2),(3,4),(5,6)]
  3

  ·∾ fmap length [(1,2),(3,4),(5,6)]
  [1,1,1]


  ·∾ fmap length Just [1,2,3]
  1

  ·∾ fmap length (Just [1,2,3])
  Just 3

  ·∾ fmap length Just [1,2,3]
  1
  ·∾ fmap length (Just [1,2,3])
  Just 3
  ·∾ (\x -> Just (length x)) [1,2,3]
  Just 3
  ·∾ (length . Just) [1,2,3]
  1
  ·∾ (\x -> length (Just x)) [1,2,3]
  1

  ·∾ fmap length [Just 1,Just 2,Nothing]
  [1,1,0]


``elem``
^^^^^^^^
::

  ·∾ :type elem
  elem :: (Foldable t, Eq a) => a -> t a -> Bool

  ·∾ :doc elem
   Does the element occur in the structure?

  ·∾ elem 2 (Just 3)
  False

  ·∾ elem True (Left False)
  False

  ·∾ elem True (Left True)
  False

  ·∾ elem True (Right False)
  False

  ·∾ elem True (Right True)
  True

  ·∾ xs = [Right 1,Right 2,Right 3]

  ·∾ fmap (elem 3) xs
  [False,False,True]


``maximum`` and ``minimum``
^^^^^^^^^^^^^^^^^^^^^^^^^^^
Here, notice that ``Left`` and ``Nothing``
(and similar) values are empty for the
purposes of these functions::

  ·∾ maximum [10,12,33,5]
  33

  ·∾ xs = [Just 2, Just 10, Just 4]

  ·∾ fmap maximum xs
  [2,10,4]

  ·∾ fmap maximum [Just 2, Just 10, Just 4]
  [2,10,4]

  ·∾ fmap maximum (Just [3,7,10,2])
  Just 10

  ·∾ minimum "julie"
  'e'

  ·∾ fmap minimum (Just "julie")
  Just 'e'

  ·∾ xs = map Just "jul"
  ·∾ xs
  [Just 'j',Just 'u',Just 'l']

  ·∾ fmap minimum xs
  "jul"

  ·∾ xs = [Just 4, Just 3, Nothing]

  ·∾ fmap minimum xs 
  [4,3,*** Exception: minimum: empty structure

  ·∾ minimum (Left 3)
  *** Exception: minimum: empty structure

``sum`` and ``product``
^^^^^^^^^^^^^^^^^^^^^^^
::

  ·∾ sum (7,5)
  5

  ·∾ fmap sum [(7,5),(3,4)]
  [5,4]

  ·∾ fmap sum (Just [1,2,3,4,5])
  Just 15

  ·∾ product Nothing 
  1

  ·∾ fmap product (Just [])
  Just 1

  ·∾ fmap product (Right [1,2,3])
  Right 6

.. include:: exercises/20.5.7_-_library_functions.rst
