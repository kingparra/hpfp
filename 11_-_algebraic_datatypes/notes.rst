*********************************
 Chapter 11: Algebraic datatypes
*********************************

.. This chapter roughly corresponds to Chapter 12 of Get
.. Programming with Haskell by Will Kurt and section 4.2.1 of the
.. 2010 Haskell Language Report. Stephen Diehls guide also has
.. a section, http://dev.stephendiehl.com/hask/#algebraic-datatypes
.. Also check out this nice article:
.. https://codewords.recurse.com/issues/three/algebra-and-calculus-of-algebraic-data-types


11.1 Algebraic datatypes
------------------------
This chapter introduces a few new ways to declare types, including record types,
type aliases using ``type``, and wrapper types using ``newtype``. Along the way,
a few hints on how to calculate the number of term-level inhabitants (or
cardinality) of a type are discussed.


11.2 Data declarations review
-----------------------------
::

  --    type         data
  -- constructor  constructor
  --      v            v
  data Trivial  =   Trivial'
  -- ^
  -- keyword
  --
  -- In this examples both constructors are constants,
  -- since they take no arguments.

  -- Constructors may also take arguments.
  --
  --          type variable   term-level placholder
  --                |         for a value of type a
  --                v                 v
  data UnaryTypeCon a = UnaryValueCon a
  --
  -- There is no requirement to use type constructor
  -- parameters as arguments to your data constructors;
  -- They can be phantom (lacking a term-level witness),
  -- instead.

.. In any realistic program, you'll need to define your own data
.. types to capture the specific requirements of the problem you're
.. solving and the specific forms of data you’re working with. Not
.. only that, but there's a significant payoff to thinking carefully
.. about the design of data types: the more precisely types capture
.. the requirements of a problem, the more closely type errors will
.. mirror errors in reasoning. (Paraphrased from Type-Driven
.. Development with Idris by Edwin Brady)


11.3 Data and type constructors
-------------------------------
Type constructors are used only at the type level. Data constructors construct
values at the term level. Type and data constructors that take no arguments
(like Bool, and True/False) are constants.

Sometimes constructors take arguments. In those cases, it's like a function in
at least one sense -- it must be applied to become a concrete type or value.

When writing a type it's possible to create parameters for the type constructor,
called type variables. These parameters are placeholders for potential types
that can be used to construct the overall type.

Type variable names may in turn be used by data constructors.  However, you
aren't required to use them -- they may be *phantom*, lacking a value-level
witnesses.


11.4 Type constructors and kinds
--------------------------------
Types constructors themselves have kind signatures to help us reason about their
arity and possible number of inhabitants. These kinds are essentially a simple
type system one level up for our type constructors. There are three possible
components of a kind signature: ``*`` (pronounced "type"), ``->`` which denotes
type constructor application, and ``#`` a GHC extension that represents unboxed
types, like those used to create primitive types, found in ``GHC.Prim``.

::

  import Data.Ratio (Ratio)

  ·∾ :kind Ratio
  Ratio :: * -> *

  ·∾ :kind Ratio Int
  Ratio Int :: *

One interesting use of kind signatures is to limit the arity of an input type
constructor, used to create the ``f a`` and ``f b`` types in ``fmap``'s type
signature::

  ·∾ :info fmap
  class Functor (f :: * -> *) where
    fmap :: (a -> b) -> f a -> f b

There is more nuance to kinds, but I don't have time to read about it right now,
so I'll just leave this link to the GHC wiki here.
https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/compiler/type-type


11.5 Data constructors and values
---------------------------------

.. include:: exercises/11.5.1_-_dog_types.rst
