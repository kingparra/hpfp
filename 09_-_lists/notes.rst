******************
 Chapter 9: Lists
******************


9.1 Lists
---------
The list datatype is perhaps the most used composite data structure in
Haskell. If you consider the emphasis on immutability in pure functional
languages, this begins to make sense.

What if you modify an element in the middle of a list? Since the previous
elements "next" link can't be mutated, and this restriction cascades, you
copy the beginning of the list, and tail-share the rest.

In contrast, modifying an array would require either mutation (which is
illegal in pure FP), or a new deep copy of the entire array for each change.
Doubly-linked lists would also require a deep copy of the entire structure,
since modifications to cons cells on either side would cascade through the
entire structure.

So, what can you use Lists for? Lists in Haskell may represent either a finite
collection of values or infinite series of them. Infinite series can be used to
model streams of data; in which case they are usually generated incrementally by
some function.

Another novel use of lists is in combination with higher order functions that
act as control flow constructs. (Like ``map`` or ``fold``.) In this capacity
they serve roughly the same purpose as generators/iterators do with Pythons
``for`` and ``while`` loops.

In this chapter, we will:

* explain list’s datatype and how to pattern match on lists;
* practice many standard library functions for operating on lists;
* learn about the underlying representations of lists;
* see what that representation means for their evaluation;
* and do a whole bunch of exercises!


9.2 The list datatype
---------------------
First off, let's look at the definition of the list type::

  --        infix data constructor
  --       that takes two arguments
  --                 |
  --                 v
  data [] a = [] | a : [a]
  --                    ^
  --                    |
  --    *Type* constructor, not a data
  --    constructor. This is synonmous
  --    with the [] a in data [] a ...
  --    Writing the a between brackets
  --    is syntactic sugar. [a] can be
  --    replaced at the term level by
  --    the [] or : data constructors.
  --
  -- Written differently this may look like...
  --
  --
  data List a = Nil | Cons a (List a)
  --                            ^
  --                            |
  --                can be replaced by Nil
  --               or Cons data constructor
  --
  --   ... which I think makes the distinction
  -- between data and type constructor clearer.

In terms of implementation, Haskell lists have similar
performance characteristics as singly-linked lists, although
average case performance in some situations changes due to
nonstrict evaluation.

.. topic:: Notes about speed

   Fast operations

   * Prepend an element, using ``(:)``
   * Get the first element, using ``head``,
   * Remove last element, using ``tail``,

   Slower operations

   Any function that does something with the :math:`n`'th element, or the first
   :math:`n` elements get slower as :math:`n` increases.

   * Taking a number of elements, starting from index :math:`n`, using ``take n xs``
   * Dropping a number of elements, ``drop n xs``
   * Retreiving an element at index :math:`n`, using ``xs !! n``
   * Splitting at an index, with ``splitAt n xs``

   Any function that needs to process the entire list gets slower as the list
   grows longer. Eg. ``length``, ``(++)``, ``last``, ``map``, ``filter``,
   ``zip``, ``elem``, ``sum``, ``minimum``, and ``maximum``.

   `GHC.List <https://hackage.haskell.org/package/base-4.14.0.0/docs/GHC-List.html>`_
   also has notes about the time complexity of a few of these functions.

   -- from `How to work on lists <https://wiki.haskell.org/How_to_work_on_lists>`_ from the Haskell Wiki

9.3 Pattern matching on lists
-----------------------------
Non-exhaustive matches are a failure mode that are easy to overlook when only
interested in grabbing stuff on either side of the cons operator. Take care to
anticipate what happens with ``[]`` when pattern matching on lists.

9.3.1 Using Maybe
^^^^^^^^^^^^^^^^^
You can use maybe to make the possibility of failure explicit in
the type signature. For example::

    safeTail :: [a] -> Maybe [a]
    safeTail [] = Nothing
    safeTail (_:[]) = Nothing
    safeTail (_:xs) = Just xs

    safeHead [] = Nothing
    safeHead (x:_) = Just x


9.4 List's syntactic sugar
--------------------------
As you know, something like ``[1,2,3] ++ [4]`` is really shorthand for ``(1 : 2
: 3 : []) ++ (4 : [])``. It's useful to keep this longhand notation in mind when
thinking about how various functions traverse a lists "spine" - the chain
of linked cons cells.


9.5 Using ranges to construct lists
-----------------------------------
* A range of elements ``[1..10]``.
* Range with a step value ``[1,2..10]``.
* ``[(-1)..(-10)]`` won't work, since Haskell assumes a forward
  step; Use ``[(-1),(-2)..(-10)]`` instead.

These list comprehensions are sugar for the functions from the ``Ord`` type
class such as ``enumFrom``, ``enumFromThen``, ``enumFromTo``, and
``enumFromThenTo``.

Be aware that ``enumFromTo`` must have its first argument be lower then the
second argument. Otherwise you'll get an empty list.

.. include:: exercises/9.5.1_-_enumfromto.rst


9.6 Extracting portions of lists
--------------------------------
::

  take :: Int -> [a] -> [a]
  drop :: Int -> [a] -> [a]
  splitAt :: Int -> [a] -> ([a],[a])
  -- Stops taking as soon as the condition is met.
  -- Maybe it should have been named takeUntil?
  takeWhile :: (a -> Bool) -> [a] -> [a]
  dropWhile :: (a -> Bool) -> [a] -> [a]

.. include:: exercises/9.6.1_-_thy_fearful_symmetry.rst


9.7 List comprehensions
-----------------------
List comprehensions are used to create a new list by taking the members of an
existing "source" list, and applying filters or transformations to it.

Syntactically, list comprehensions have the general form:

  **[** *expression* **|** *pattern* **<-** *genxpr* **,** *guard_or_let_binding* **]**

Where *pattern* **<-** *genxpr* and *guard_or_let_binding* may be repeated.

Here is a simple example::

  ·∾ [ x^2 | x <- [1..10], x `rem` 2 == 0]
  [4,16,36,64,100]

Which may be read as "x to the power of two such that x is drawn from the list
of one through ten, if the remainder of x divided by two equals zero". What a
mouthful.

A more complex example, to show that generators can reference each other::

  ·∾ [ x | xs <- [[(1,2),(3,4)],[(5,4),(3,2)]], (3,x) <- xs]
  [4,2]

If you try to draw from an empty list, you get an empty list::

  ·∾ [ x | x <- []]
  []

9.7.1 Adding predicates
^^^^^^^^^^^^^^^^^^^^^^^
As you can see, predicates (or guards) are conditions that can limit the
elements drawn from the generator list. These predicates must evaluate to Bool
values.

A note about evaluation: When writing a comprehension with multiple generators,
the rightmost generator will be exhausted first, then the second rightmost, and
so on.

The generators don't have to have the same length or even the same type.

For example::

  ·∾ [(x,y) | x <- [1,2,3], y <- ['a','b']]
  [(1,'a'),(1,'b'),(2,'a'),(2,'b'),(3,'a'),(3,'b')]

9.7.3 List comprehensions with strings
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
You can use the ``elem`` function to test for membership in a list.


9.8 Spines and nonstrict evaluation
-----------------------------------
In the case of a list, the spine is the connective structure of recursively
nested cons cells.

What the spine looks like::


  1 : 2 : 3 : []

      : <------+
     / \       |
    1   : <----+ This is the "spine"
       / \     |
      2   : <--+
         / \
        3  []

You can evaluate cons cells independently of the values they contain. It is
also possible to evaluate part of the spine and not the rest of it. Meaning
that it's possible to traverse a list without evaluating the elements in
it. Evaluation of the list proceeds down the spine. Constructing a list proceeds
up the spine.

9.8.1 Using GHCi's :sprint command
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
``:sprint`` prints a value without forcing its evaluation. Unevaluated subterms
are represented by ``_``. You can use this to visualize what a function is
strict about evaluating -- just the spine, or everything, or nothing, or
whatever.

::

  ·∾ blah = enumFromTo 'a' 'z'
  ·∾ :sprint blah
  blah = _
  ·∾ take 1 blah
  "a"
  ·∾ :sprint blah
  blah = 'a' : _
  ·∾ take 2 blah
  "ab"
  ·∾ :sprint blah
  blah = 'a' : 'b' : _

In GHCi code evaluates a bit differently than when compiling a file with GHC, so
``:sprint`` isn't always correct. For numeric values, use a concrete type, or
you'll encounter a situation like this::

  ·∾ nums = [1..10]
  ·∾ ints = [(1 :: Int)..10]
  ·∾ :sprint nums
  nums = _
  ·∾ :sprint ints
  ints = _
  ·∾ take 1 nums
  [1]
  ·∾ :sprint nums
  nums = _
  ·∾ take 1 ints
  [1]
  ·∾ :sprint ints
  ints = 1 : _

Here's an example where only the spine is evaluated::

  ·∾ uf = [undefined,undefined,undefined,undefined]
  ·∾ length uf
  4
  ·∾ :sprint uf
  uf = [_,_,_,_]

9.8.2 Spines are evaluated independently of values
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
* Values in Haskell get reduced to weak head normal (WHNF) form by default.
* WHNF means that the expression is only evaluated as far as in necessary to reach a data constructor.
* WHNF contains both the possibility that the expression is fully evaluated and the possibility that the
  expression has been evaluated to the point of arriving at a data constructor or lambda head.
* Normal form exceeds that by requiring that all sub-expression be fully evaluated.
* If no further inputs are possible, then it is still in WHNF but also in normal form (NF).
* ``(1, 2)`` WHNF and NF. ``(1, 1+1)`` is WHNF but not NF. ``\x -> x * 10`` is
  WHNF and NF. ``(1, "Papu" ++ "chon")`` is WHNF but not NF.

.. include:: exercises/9.8.3_-_bottom_madness.rst

9.9 Transforming lists of values
--------------------------------
* ``map`` can only be used with lists. ``fmap`` can be applied to things with
  the typeclass Functor, which includes data other than lists.
* Consider this example of lazy evaluation::

    Prelude> take 2 $ map (+1) [1, 2, undefined]
    [2,3]


9.10 Filtering lists of values
------------------------------
* ``filter`` has the following definition::

    filter :: (a -> Bool) -> [a] -> [a]
    filter _ [] = []
    filter pred (x:xs)
        | pred x = x : filter pred xs
        | otherwise = filter pred xs

* Filter takes a function that returns a Bool value, maps that function over
  a list, and returns a new lists of all the values that met the condition.


9.11 Zipping lists
------------------
* Zip takes two lists and returns a list of corresponding pairs.
* Source code `here <https://hackage.haskell.org/package/base-4.12.0.0/docs/src/GHC.List.html#zip>`_.
* It looks like this::

    Prelude> zip [1, 2, 3] [4, 5, 6]
    [(1,4),(2,5),(3,6)]

    Prelude> zip ['a'] [1..1000000000000000000]
    [('a',1)]

    Prelude> zip [1..100] ['a'..'c']
    [(1,'a'),(2,'b'),(3,'c')]

    Prelude> zip [] [1..1000000000000000000]
    []

* ``zip`` stops as soon as one of the lists runs out of values, and will return
  an empty list if either of the lists is empty
* ``unzip`` exists.
* ``zipWith`` is like ``zip``, but it map a function to both element extracted
  from each lists while zipping.
* Its type signature is ``zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]``
* Some examples of ``zipWith``::

    Prelude> zipWith (*) [1, 2, 3] [10, 11, 12]
    [10,22,36]

    Prelude> zipWith (==) ['a'..'f'] ['a'..'m']
    [True,True,True,True,True,True]
