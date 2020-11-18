******************
 Chapter 9: Lists
******************


9.1 Lists
---------
Although there are many composite data types in Haskell, the list datatype is
perhaps the most pervasively used. (Even sometimes where it doesn't make sense.)
If we consider the implications of immutability in pure functional languages
we may begin to understand why.

What if we need to modify an element in the middle of a singly-linked list?
Since the previous elements "next" link can't be mutated, and this restriction
cascades, we must deep copy the beginning of the list, and can tail-share the
rest.

In contrast, modifying an array would require a new deep copy of the entire
array for each change since mutation of substructures illegal, due to
referential transparency. (Recall chapter 1 of Introduction to Functional
Programming Through Lambda Calculus.)

Doubly-linked lists would also require a deep copy of the entire structure,
since the requirement of mutation to change links of cons cells on either side
of the target element would cascade through the entire structure.

From the platonic view of a pure functional language, it is easier to share
parts of a singly-linked list.

In actual Haskell code, things are probably more nuanced. I don't know, really,
I'm still only a beginner, as you can tell by these crummy notes :). Here are
some interesting links:

* https://stackoverflow.com/questions/9611904/haskell-lists-arrays-vectors-sequences/9613203#9613203
* https://stackoverflow.com/questions/30261346/list-manipulation-performance-in-haskell

So, what can we use Lists for? The most obvious use is as a representation of
some finite collection of values. Lists also may represent an infinite series of
values. Infinite series can be used to model streams of data; in which case they
are usually generated incrementally by some function.

Another use of lists is in combination with higher order functions that act as
control flow constructs. (Like ``map`` or ``fold``.) In this capacity they serve
roughly the same purpose as generators/iterators do with Pythons ``for`` and
``while`` loops.

In this chapter, we will:

* explain list's datatype and how to pattern match on lists;
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

Written differently this may look like... ::

  data List a = Nil | Cons a (List a)
  --                            ^
  --                            |
  --                can be replaced by Nil
  --               or Cons data constructor

Which I think makes the distinction between data and type constructor more
obvious.

In terms of implementation, Haskell lists have similar performance
characteristics as singly-linked lists, although average case performance in
some situations changes due to nonstrict evaluation.

Unlike many Wikipedia articles, the one on `linked-lists
<https://en.wikipedia.org/wiki/Linked_list>`_ is fairly easy to read, and I
recommend it to you.

.. topic:: Notes about speed

   Fast operations

   * Prepend an element, using ``(:)``
   * Get the first element, using ``head``
   * Remove first element, using ``tail``

   Slower operations

   Basically, the problem is that lists don't keep an index, so the spine must
   be traversed linearly every time you want to access an element. Another issue
   is that nodes are stored noncontiguously, increasing the time required to
   access individual elements, and reducing cache locality (contiguous elements
   in memory are often prefetched). Both of these combined mean that out-of-order
   execution is comparitively slower than arrays.

   Any function that does something with the :math:`n`'th element, or the first
   :math:`n` elements get slower as :math:`n` increases.

   * Taking a number of elements, starting from index :math:`n`, using ``take n xs``
   * Dropping a number of elements, ``drop n xs``
   * Retreiving an element at index :math:`n`, using ``xs !! n``
   * Splitting at an index, with ``splitAt n xs``

   Any function that needs to process the entire list gets slower as the list
   grows longer.

   * ``(++)``
   * ``elem``,
   * ``filter``
   * ``last``
   * ``length``
   * ``map``
   * ``maximum``
   * ``minimum``
   * ``sum``
   * ``zip``

   -- from `How to work on lists <https://wiki.haskell.org/How_to_work_on_lists>`_ on the Haskell Wiki


The docs for `GHC.List <https://hackage.haskell.org/package/base-4.14.0.0/docs/GHC-List.html>`_,
`Data.List <https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-List.html#v:genericIndex>`_,
and the section of `Prelude <https://hackage.haskell.org/package/base-4.12.0.0/docs/Prelude.html#g:13>`
on lists have notes about the time complexity of list processing functions in them.


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
Regular list syntax like ``[1,2,3]`` is somewhat unique, since unlike most
everything in Haskell it doesn't originate in a data constructor. This is an
example of syntactic sugar, and really stands for ``(1 : 2 : 3 : [])``.  It's
useful to keep this longhand notation in mind when thinking about how various
functions traverse a lists "spine" - the chain of linked cons cells.

The spine is the connective structure that holds the cons cells together and in
place. As we will soon see, this structure nests the cons cells rather than
ordering them in a right-to-left row. Because different functions may treat the
spine and the cons cells differently, it is important to understand this
underlying structure.

9.5 Using ranges to construct lists
-----------------------------------
Here are some examples of range syntax -- a convenient way to construct lists of
orderable elements::

  ·∾ [1..10]
  [1,2,3,4,5,6,7,8,9,10]

  ·∾ -- This won't work because a forward step is assumed.
  ·∾ [(-1)..(-10)]
  []

  ·∾ -- Step values are introduced with a comma.
  ·∾ -- Providing an explicit negative step will make the range work.
  ·∾ [(-1),(-2)..(-10)]
  [-1,-2,-3,-4,-5,-6,-7,-8,-9,-10]

These list ranges are sugar for the functions from the ``Ord`` type. Here
are those same expressions rephrased::

  ·∾ enumFromTo 1 10
  [1,2,3,4,5,6,7,8,9,10]

  ·∾ enumFromTo (-1) (-10)
  []

  ·∾ enumFromThenTo (-1) (-2) (-10)
  [-1,-2,-3,-4,-5,-6,-7,-8,-9,-10]

.. include:: exercises/9.5.1_-_enumfromto.rst


9.6 Extracting portions of lists
--------------------------------
::

  take :: Int -> [a] -> [a]
  drop :: Int -> [a] -> [a]
  splitAt :: Int -> [a] -> ([a],[a])
  takeWhile :: (a -> Bool) -> [a] -> [a]
  dropWhile :: (a -> Bool) -> [a] -> [a]

.. include:: exercises/9.6.1_-_thy_fearful_symmetry.rst


9.7 List comprehensions
-----------------------
List comprehensions are used to create a new list by taking the members of an
existing "source" list, and applying filters or transformations to it.

Here is a simple list comprehension of even numbers between 1 to 10 inclusive::

  ·∾ [ x^2 | x <- [1..10], x `rem` 2 == 0]
  [4,16,36,64,100]

This may be read as "x to the power of two such that x is drawn from the list
of one through ten, if the remainder of x divided by two equals zero". What a
mouthful. Here's a visual translation guide::

  -- expression
  --    |
  --    |  read as
  --    | "such that"
  --    |  |
  --    |  |  generator        guard
  --    V  v vvvvvvvvvvv   vvvvvvvvvvvvvv
  ·∾ [ x^2 | x <- [1..10], x `rem` 2 == 0]
  --           ^         ^
  --        read as       \
  --     "is drawn from"  read as "where"

Here is a more complex example, to show that generators can reference each
other, and act as filters with an appropriate pattern::

  ·∾ [ x | xs <- [[(1,2),(3,4)],[(5,4),(3,2)]], (3,x) <- xs]
  [4,2]

Comprehensions can have local bindings with ``let``::

  ·∾ [x^2 | let n = 3, x <- [1..n]]
  [1,4,9]

But they must be written to the left of the generator/guard that uses it, or
we'll get an error::

  ·∾ [x^2 | x <- [1..n], let n = 3]
  <interactive>:2:17: error: Variable not in scope: n

However, all let bindings are visible to the expression before the pipe. This
will be fine::

  ·∾ [x^y | x <- [1..3], let y = 3]
  [1,8,27]

I asked on IRC about why ``let`` bindings don't have an ``in`` keyword when used
as part of list comprehensions::

  <sshine>     justsomeguy, let-expressions in list comprehensions are much like
               let-expressions in do-notation
  <sshine>     justsomeguy, considering list comprehensions are monadic, it's
               not very special. but I guess you could ask why remove the "in"
               inside do notation?
  <justsomeguy> Yes, the missing “in” kinda threw me off.
  <davean>     justsomeguy: its not removed specificly there
  <davean>     justsomeguy: theres no 'in' in do notation.
  <sshine>     justsomeguy, you can think of list comprehensions as being
               syntax sugar for list monads. and you can extend list comprehension
               syntax to other monads than list using -XMonadComprehensions:
               https://gitlab.haskell.org/ghc/ghc/-/wikis/monad-comprehensions

  <sshine>     > do { x <- [1..10]; let {y = 3}; return (x * y) }
  <lambdabot>  [3,6,9,12,15,18,21,24,27,30]

  <sshine>     > [ x * y | x <- [1..10], let y = 3 ] -- vs. this
  <lambdabot>  [3,6,9,12,15,18,21,24,27,30]

  (sshine takes some pains to explain some other points to me about do notation,
   but I won't include that here. Those are in my logs. Thanks sshine :)

9.7.1 Adding predicates
^^^^^^^^^^^^^^^^^^^^^^^
As you can see, predicates (or guards) are conditions that can limit the
elements drawn from the generator list. These predicates must evaluate to Bool
values.

A note about evaluation: When writing a comprehension with multiple generators,
the generators will be exhausted in a depth-first manner. Also, every boolean
guard will be checked.::

  ·∾ [x | xs <- [[(1,2),(3,4)],[(5,4),(3,2)]], (3,x) <- xs]
  [4,2]

To me this resembles Cartesian products, or brace expansion in bash. ::

  ·∾ [(x,y) | x <- ['a','b','c'], y <- [1..10]]
  [('a',1),('a',2),('a',3),('a',4),('a',5),('a',6),('a',7),('a',8),('a',9),('a',10)
  ,('b',1),('b',2),('b',3),('b',4),('b',5),('b',6),('b',7),('b',8),('b',9),('b',10)
  ,('c',1),('c',2),('c',3),('c',4),('c',5),('c',6),('c',7),('c',8),('c',9),('c',10)]

  ∗  echo {a,b,c}{1..10}
  a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10

.. From the 2010 Haskell Language Report

.. 3.11 List Comprehensions

.. aexp → [ exp | qual1 , . . . , qualn ] (list comprehension, n ≥ 1 )
.. qual → pat <- exp                      (generator)
..      | let decls                       (local declaration)
..      | exp                             (boolean guard)

.. A list comprehension has the form [ e | q1 , . . . , qn ], n ≥ 1 , where the qi
.. qualifiers are either

.. * generators of the form p <- e, where p is a pattern (see Section 3.17) of type
..   t and e is an expression of type [t]
.. * local bindings that provide new definitions for use in the generated
..   expression e or subsequent boolean guards and generators
.. * boolean guards, which are arbitrary expressions of type Bool.

.. Such a list comprehension returns the list of elements produced by evaluating e
.. in the successive environments created by the nested, depth-first evaluation
.. of the generators in the qualifier list. Binding of variables occurs according
.. to the normal pattern matching rules (see Section 3.17), and if a match fails
.. then that element of the list is simply skipped over. Thus:

.. [ x | xs <- [ [(1,2),(3,4)], [(5,4),(3,2)] ], (3,x) <- xs ]

.. yields the list [4,2].

.. If a qualifier is a boolean guard, it must evaluate to True for the previous
.. pattern match to succeed. As usual, bindings in list comprehensions can shadow
.. those in outer scopes; for example:

.. [ x | x <- x, x <- x ] = [ z | y <- x, z <- y]

Generators don't have to have the same length. In this example, because of the
tuple constructor, the generators don't need to have same type, either::

  ·∾ [(x,y) | x <- [1,2,3], y <- ['a','b']]
  [(1,'a'),(1,'b'),(2,'a'),(2,'b'),(3,'a'),(3,'b')]

.. include:: exercises/9.7.2_-_comprehend_thy_lists.rst

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
