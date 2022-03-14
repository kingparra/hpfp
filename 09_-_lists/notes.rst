******************
 Chapter 9: Lists
******************


9.1 Lists
---------

.. 'Recursion has been described as the curious process of reaching ones goal by walking backward towards it.'

.. Programming is all about combining primitive operations into larger expressions to acheive the outcome you want.

   What primitive operations does the list type provide?

   I'll do a case analysis here based on the data constructors and pattern matching to try and puzzle it out...

   +----------+---------------------+----------------------------------------------------------------------+
   |  Name    |        Code         |  Description                                                         |
   +==========+=====================+======================================================================+
   |  cons    |  ``((:) x xs)``     |  Produce a new list by prepending an element to an existing list.    |
   +----------+---------------------+----------------------------------------------------------------------+
   |  visit   |  ``(_:_)``          |  Visit a cons cell, ignoring both the elements value and next cell.  |
   +----------+---------------------+----------------------------------------------------------------------+
   |  head    |  ``(x:_)``          |  Retrieve the left-most element.                                     |
   +----------+---------------------+----------------------------------------------------------------------+
   |  tail    |  ``(_:xs)``         |  Retrieve the list after the left-most element.                      |
   +----------+---------------------+----------------------------------------------------------------------+
   |  uncons  |  ``(x:xs)``         |  Retrieve the left-most element and the list the follows.            |
   +----------+---------------------+----------------------------------------------------------------------+
   |  nill    |  ``(stream : [])``  |  End a list.                                                         |
   +----------+---------------------+----------------------------------------------------------------------+
   |  empty   |  ``[]``             |  Create an empty list.                                               |
   +----------+---------------------+----------------------------------------------------------------------+

   But I really think of it more like this...

   Core operations: cons, head, tail.

   All three operations are provided by (:), or pattern matching on it.

   So, I guess that I should consider pattern matching when designing my own interfaces.

.. Which ideas are necessarily part of a list and which ideas are merely present in it?

   Necessary:

   * The data constructors cons and nil.
   * The order in which they are arranged.
   * Following these two things, the fact that there is no direct access to an element.

.. What are lists used for, and how are they different from other datastructures?

   Like arrays, vectors, and records, lists represent a collection of elements in a fixed linear order.

   Unlike Arrays,
   * lists do not have a fixed length. They can be added to or taken from at any point during program execution.
   * in Haskell particularly, lists may be infinite!
   * lists aren't layed out contiguously in memory, but may be spread out across different addresses.
   * lists don't have a static index. Operations that refer to elements in a list by index number have to
     traverse the spine every time and count the number of cons cells to find the index number.
   * you can evaluate the spine without forcing evaluation of the elements.
   * updates to elements change the element and every element that follows, sharing the preceeding
     elements. Arrays, in contrast, allow direct mutation of an element at a given index.

   Unlike Sets,
   * lists allow duplicate elements, and care about the order of elements

   Unlike Streams,
   * lists can end

   Unlike doubly-linked lists,
   * updates to elements can share the prefix, since referential transparency doesn't require
     rewriting of the previous-node pointer in a cons cell, which would cascade into a requirement
     for all of the structure to be rewritten.

   Unlike lists in Python,

   * lists can only contain elements of one type
   * lists can be infinite
   * lists have many advanced operations defined for them in the form of typeclasses
   * lists are referentially transparent

..
   Things you can't know about a list before you patern match on it:

   * the length
   * whether it's empty or not
   * whether is's infinite or finite
   * whether part of the spine is undefined or not
   * the index of an element

   Things you _can_ know about a list before you pattern match on it:

   * the type of the elements within it

   Things you can know about a (finite) list without evaluating the elements:

   * whether the spine has undefined in it
   * the length of the list (number of cons
     data constructors)

   Efficient operations:

   * in-order sequential access of elements
   * prepending an element using cons
   * poping an element from the head of the list

   Inefficient operations:

   * out-of-order access (or random access)
     to elements within the list
   * repeated access of elements (the spine
     has to be traversed every time)
   * modification of elements (this has CoW
     sementics, so elements are not mutated
     in-place, an entier new list is
     constructed instead. The head of the
     list up until the modified element can
     be reused, though.)

   * Use cases for lists as an abstract
     datatype.

     * Iterator
     * Infinite stream
     * Collection of values
     * Stack

   How does referential transparency restrict
   the way collections of values are updated?

In this chapter, we will:

* Explain the list datatype and how to pattern match on lists.
* Practice many standard library functions for operating on lists.
* Learn about the underlying representations of lists.
* See what that representation means for their evaluation.
* And do a whole bunch of exercises!


9.2 The list datatype
---------------------
::

  --#   The binary infix data constructor (:), used to
  --# construct nested cells of a list, pronounced "cons".
  --# Cons is right associative, and has a precedence of 5,
  --#  which is lower than the default of precedence of 9
  --#    for regular left associative prefix functions.
  --#              vvvvvvv
  data [] a = [] | a : [a]
  --#         ^^
  --#  The empty list data constructor, [], pronounced "nill".
  --#
  --# Since the [] type constructor only takes one type argument,
  --# a, lists must be homogenous in Haskell.
  --#
  --# Because of non-strict evaluation, lists can be infinite, and
  --# are often used similarly to iterators generated by range()
  --# in python.

  instance Eq   a => Eq          [a]
  instance Ord  a => Ord         [a]
  instance Show a => Show        [a]
  instance Read a => Read        [a]
  instance           Semigroup   [a]
  instance           Monoid      [a]  --# Depends on Semigroup.
  instance           Foldable    []   --# (foldMap) depends on Monoid.
  instance           Functor     []
  instance           Traversable []   --# Depends on Functor and Foldable.
  instance           Applicative []   --# Depends on Functor.
  instance           Monad       []   --# Depends on Applicative.
  instance           MonadFail   []   --# Depends on Monad.

.. from Will Kurts book: "If you get stuck on a topic
   in Haskell, it's almost always helpful to turn back
   to lists to see if they can give you some insight.


9.3 Pattern matching on lists
-----------------------------

9.3.1 Using Maybe
^^^^^^^^^^^^^^^^^


9.4 List's syntactic sugar
--------------------------
In both Lisp and Haskell, lists are implemented as linked records with two fields - one field
contains the elements value, and the other field contains a link to the next record or Nil
(end-of-list). These records are called *cons cells* in lisp.

More generally, for other data structures, linked records are instead called nodes. Nodes may have
many fields, including the possibility of multiple links to other nodes. Data may be arranged into
arbitrary structures this way.

The *spine* refers to the entire succession of nested cons cells that comprise a list. (Or linked
nodes that comprise some other data structure.) The field of a cons cell that contains the value is
not considered part of the spine.


9.5 Using ranges to construct lists
-----------------------------------

.. include:: exercises/9.5.1_-_enumfromto.rst


9.6 Extracting portions of lists
--------------------------------

.. include:: exercises/9.6.1_-_thy_fearful_symmetry.rst


9.7 List comprehensions
-----------------------

.. topic:: From the 2010 Haskell Language Report

   ::

     aexp -> [ exp | qual₁, ... qualₙ ] (list comprehension, n ≥ 1)
     qual -> pat <- exp                 (generator)
           | let decls                  (local declaration)
           | exp                        (boolean guard)

   A list comprehension has the form
   :math:`[ e | q_{1}, ... q_{n} ], n \geq 1`,
   where the :math:`q_{i}` qualifiers are either:

   * *generators* of the form :math:`p \leftarrow e`,
     where :math:`p` is a pattern of type :math:`t`
     and :math:`e` is an expression of type ``[t]``

   * *local bindings* that provide new definitions for
     us in the generated expression :math:`e` or
     subsequent boolean guards or generators.

   * *boolean guards*, where are arbitrary expressions
     of type ``Bool``.

   Such a list comprehension returns the list of
   elements produced by evaluating e in the successive
   environments created by the nested, depth-first
   evaluation of the generators in the qualifier list.
   Binding of variables occurs according to the normal
   pattern matching rules, and if a match fails then
   that element of the list is simply skipped over.

   ::

     ·∾ [ x |
      ⋮   xs    <- [[(1,2),(3,4)],[(5,4),(3,2)]]
      ⋮ , (3,x) <- xs
      ⋮ ]
     [4,2]

   If a qualifier is a boolean guard, it must evalute
   to ``True`` for the previous pattern match to
   succeed. As usual, in bindings in list comprehensions
   can shadow those in outer scopes; for example:

   ::

     [ x | x <- x, x <- x ] ≡ [ z | y <- x, z <- y]

   Variables bound by ``let`` have fully polymorphic
   types while those defined by ``<-`` are lambda
   bound and are thus monomorphic.

9.7.1 Adding predicates
^^^^^^^^^^^^^^^^^^^^^^^

.. include:: exercises/9.7.2_-_comprehend_thy_lists.rst

9.7.3 List comprehensions with strings
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. include:: exercises/9.7.4_-_square_cube.rst


9.8 Spines and nonstrict evaluation
-----------------------------------
The spine is the structure of a collection type that
arranges values in some order.

The spine is the connective structure that links a
composite data-structure together. In the case of a
list, it's the succession of nested cons data
constructors (and the final empty list data
constructor), like so: ``_ : _ : _ : []``.

Lists aren't constructed until they're
consumed. Until a value is consumed, there is
a series of placeholders as a blueprint of the
lists that can be constructed when it's needed.

Also, functions can traverse the spine of a list
without forcing evaluation of the values within
that list.

Another way of phrasing this is: You can evaluate
the cons data constructors in a spine without
forcing evaluation of the arguments to those
constructors.

Evaluation of the spine proceeds down the
spine. However, constructing the list (when
that is necessary) proceeds up the spine.

9.8.1 Using GHCi's :sprint command
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

9.8.2 Spines are evaluated independently of values
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Values in Haskell get reduced to weak head normal
form by default.

Weak head normal form means the expression is only
evaluated as far as is necessary to reach a data
constructor or a lambda awaiting an argument.

Also, since Haskell uses an outermost
reduction strategy, the subexpressions within
the lambda or data constructor don't need to
be evaluated.

**To determine if something is in WHNF we only have
to look at the outermost part of the expression.
If it's a data constructor or lambda, it's in
WHNF. If it's a function application, it's not.**

Evaluation of the list goes "down the spine" (left to
right). Construction of the lists goes "up the spine"
(right to left).

Functions that are spine strict can force complete
evaluation of the spine of a list even if they don't
force evaluation of each value.

.. include:: exercises/9.8.3_-_bottom_madness.rst


9.9 Transforming lists of values
--------------------------------
Map doesn't traverse the whole list and apply
the function immediately. The function is
applied to the values you force out of the
list one by one.

.. include:: exercises/9.9.1_-_more_bottoms.rst


9.10 Filtering lists of values
------------------------------
::

  filter :: (a -> Bool) -> [a] -> [a]
  filter _ []        = []
  filter pred (x:xs)
    | pred x         = x : filter pred xs
    | otherwise      = filter pred xs

.. include:: exercises/9.10.1_-_filtering.rst


9.11 Zipping lists
------------------

.. include:: exercises/9.11.1_-_zipping_exercises.rst


9.12 Chapter Exercises
----------------------

.. include:: exercises/9.12.1_-_Data.Char.rst

.. include:: exercises/9.12.2_-_ciphers.rst

.. include:: exercises/9.12.3_-_writing_your_own_standard_functions.rst
