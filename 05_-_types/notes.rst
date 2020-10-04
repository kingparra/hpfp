******************
 Chapter 5: Types
******************
`The Idea of Order at Key West
<https://www.poetryfoundation.org/poems/43431/the-idea-of-order-at-key-west>`_.

Here are some quotes about types from various external resources that I find
interesting.

.. epigraph::

   A type may be viewed as a set of clothes (or a suit of armor) that protects
   an underlying untyped representation from arbitrary or unintended use. It
   provides a protective covering that hides the underlying representation and
   constrains the way objects may interact with other objects.

   -- Luca Cardelli


.. epigraph::

   The great idea of constructive type theory is that there is no distinction
   between programs and proofs. Theorems are types (specifications), and proofs
   are programs.

   -- Robert Harper


.. epigraph::

   So the main idea behind modern verfication -- be it with static analysis,
   model checkers or type systems -- is reduction of the state space (and
   heuristic approaches -- like SAT solvers -- that allow us to tackle larger
   state spaces).

   -- Ron Pressler https://pron.github.io


.. epigraph::

   Make illegal states unrepresentable!

   -- Yaron Minsky https://blog.janestreet.com/effective-ml-revisited/


5.1 Types
---------
A deep understanding of types and how to read an interpret them is
fundamental to reading and writing Haskell.

In this chapter, we're going to take a deeper look at the type system and

* learn more about querying and reading type signatures;
* explore currying;
* take a closer look at different kinds of polymorphism;
* look at type inference and how to declare types for our functions.


5.2 What are types for?
-----------------------
"Type systems in logic and mathematics have been designed to impose constraints
that enforce correctness."

"For our purposes, we can say that well-designed type systems help eliminate some
classes or errors as well as concerns such as what the effect of a conditional
over a non-Boolean value might be."

These two sentences from the book seemed a bit out of place. Turns out they're
paraphrased from an awesome paper. Here is some more of it, to elaborate on the
idea.

.. topic:: Organizing Untyped Universes

   As soon as we start working in an untyped universe, we begin to organize
   it in different ways for different purposes. Types arise informally in
   any domain to categorize objects according to their usage and behavior.

                                   . . .

   Untyped universes of computational objects decompose naturally into subsets
   with uniform behavior. Sets of objects with uniform behavior may be named
   and are referred to as types.

                                   . . .

   After a valiant organization effort, then, we may start thinking of untyped
   universes as if they were typed. But this is just an illusion, because it is
   very easy to violate the type distinctions we have just created.

                                   . . .

   In computer memory, what is the bit-wise boolean or of a character and a
   machine operation? In Lisp, what is the effect of treating an arbitrary
   S-expression as a program? **In the λ-calculus, what is the effect of a
   conditional over a non-boolean value?** [Ed; One example of this was the
   definition of our isZero test, Z, from Rojas' paper, which has you treat
   0 as true, since they have the same representation.] In set theory, what
   is the set-union of the function successor and the function predecessor?

   Such questions are the unfortunate consequence of organizing untyped
   universes without going all the way to typed systems; it is then meaningful
   to ask about the (arbitrary) representations of higher-level concepts and
   their interactions.

                                   . . .

   **In mathematics as in programming, types impose constraints which help to
   enforce correctness**

                                   . . .

   A type may be viewed as a set of clothes (or a suit of armor) that protects
   an underlying untyped representation from arbitrary or unintended use. It
   provides a protective covering that hides the underlying representation and
   constrains the way objects may interact with other objects.

                                   . . .

   Breaking the type system allows a data representation to be manipulated in
   ways that were not intended, with potentially disastrous results. For
   example, use of an integer as a pointer can cause arbitrary modifications to
   programs and data.

   -- Luca Cardelli, from "On Understanding Types, Data Abstraction, and
      Polymorphism", from Section 1.1 "Organizing Untyped Universes". Also
      check out "Type Systems" by Luca Cardelli, published by Microsoft Research.

So types let us think at a higher level of abstraction, and prevent execution
errors.

Execution errors include things such as, unintended memory access, or
operations that don't make semantic sense, like adding numbers to strings.

In Haskell, type-checking occurs at compile time.

No type system can eliminate all possibility for error, so runtime errors and
exceptions still exist, and testing of program is necessary.

Some possible errors that well-typed programs can exhibit are listed `here
<https://ucsd-progsys.github.io/liquidhaskell-tutorial/Tutorial_01_Introduction.html>`_.

Good type systems can also enable compiler optimizations because the compiler
can know and predict certain things about the execution of a program based on
the types.

Furthermore, types can serve as documentation of your program.

Working with a good type system can eliminate those tests that only check that
you're passing the right sort of data around.


5.3 How to read type signatures
-------------------------------
The compiler doesn't know which specific numeric types a value is until the type
is either declared or the compiler is forced to infer a specific type based on
the function.

Instead, it gives the numeric literal type type with the broadest applicability
(most polymorphic) and says it's a constrained polymorphic ``:: Num a => a``
value.

5.3.1 Understanding the function type
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
The arrow, ``(->)``, is the type constructor for functions in Haskell.

The ``(->)`` type constructor takes two arguments and has no data
constructors.

The arrow in an infix operator that has two parameters and associated
to the right.

::

  ·∾ :info (->)
  data (->) (a :: TYPE q) (b :: TYPE r)      -- Defined in ‘GHC.Prim’
  infixr -1 ->

  instance  Applicative ((->) a)              -- Defined in ‘GHC.Base’
  instance  Functor ((->) r)                  -- Defined in ‘GHC.Base’
  instance  Monad ((->) r)                    -- Defined in ‘GHC.Base’
  instance  Monoid b => Monoid (a -> b)       -- Defined in ‘GHC.Base’
  instance  Semigroup b => Semigroup (a -> b) -- Defined in ‘GHC.Base’

The parameterization suggests that we will apply the function so some
argument that will be bound to the first parameter, with the second
parameter representing the return or result type.

In the expression ``fst :: (a, b) -> a`` we can know for certain that
the result ``a`` is the same ``a`` in ``(a, b)``.

How do we know it's the same ``a``? We can see that the type variable
have the same type, and that nothing happens in between the input and
output to transform the parameters value.

5.3.2 Type class constrained type variables
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
``Num a => a -> a`` reads "for the polymorphic type a which has an
instance of the ``Num`` type class, a returning a" or more simply
"``Num`` constrained ``a`` to ``a``". This is known as a type class
constraint, or constraint.

In this signature, ``a`` is still polymorphic, but constrained to types the
implement the type class ``Num``.

Type classes offer a standard set of functions that can be used across
several concrete types.

A type signature might have multiple type class constraints on one or
more of the variables.

Here's and example of what constrained type signatures can look like::

  -- multiple expressions can be annotated with a type at the same time
  (+), (-) :: Num a => a -> a -> a

  -- a and b have are constrained to types that implement Num
  :: (Num a, Num b) => a -> b -> b

  -- a is constrained to types that implement both Num and Ord
  :: (Ord a, Num a) => a -> a -> Ordering

  -- ...another way to write it...
  :: Ord a => Num a => a -> a -> Ordering

The syntax used to denote multiple required type classes that resembles a tuple
in the type class constraint (everything between the ``::`` and ``=>``) does not
show up on the term level. It does, however show up in the kind signature. For
example ``:kind Eq`` will return ``Eq :: * -> Constraint``.

.. include:: exercises/5.3.3_-_type_matching.rst


5.4 Currying
------------
First off, `here's a video <https://www.youtube.com/
watch?v=m12c99qgHBU&list=PLe7Ei6viL6jGp1Rfu0dil1JH1SHk9bgDV&index=7>`_.

All functions in Haskell take one argument, and return one result.  Currying
refers to the nesting of multiple functions, each accepting one argument and
returning one result, to allow the illusion of multiple parameter functions.

Each arrow in a type signature represents one argument and one result, with the
final type being the final result.

5.4.1 Partial application
^^^^^^^^^^^^^^^^^^^^^^^^^
The ability to apply only some of a functions arguments is called partial
application.

5.4.2 Manual currying and uncurrying
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
* Uncurried functions: One function, many arguments.
* Curried functions: Many functions, one argument apiece.

You can convert the two with the ``curry`` and ``uncurry`` functions, or by
using pattern matching, or breaking out parameters into multiple nested lambdas.

::

  ·∾ f x y = x+y
  ·∾ g (x,y) = x+y

  ·∾ :type uncurry f
  uncurry f :: Num c => (c, c) -> c
  ·∾ :type g
  g :: Num a => (a, a) -> a

  ·∾ :type curry g
  curry g :: Num t => t -> t -> t
  ·∾ :type f
  f :: Num a => a -> a -> a

  ·∾ g (1,2) == f 1 2
  True

5.4.3 Sectioning
^^^^^^^^^^^^^^^^
The term sectioning specifically refers to partial application of infix
operators. It looks something like this::

  ·∾  twoSupN = (2^)
  ·∾  twoSupN 5
  32

  ·∾  nSupTwo = (^2)
  ·∾  nSupTwo 5
  25

.. include:: exercises/5.4.5_-_type_arguments.rst


5.5 Polymorphism
----------------
Polymorphic

  poly- "many", morph "form", -ic "made of"

  "made of many forms"

Monomorphic

  mono- "one", morph "form", -ic "made of"

  "made of one form"

.. topic:: Geek words, greek roots

    You'll probably encounter more than a few greek words in functional
    programming. Concepts from category theory and logic regularly come
    up in conversation online.

    Here is a small sample of some of Greek word roots you may encounter
    to help you get a sense of their meaning intuitively before chasing
    down links on wikipedia.

    I stole this section from `John Chandler Burnhams detailed notes on HPFP
    <https://www.johnchandlerburnham.com/ projects/hpfp/05/>`_.

    +------------+---------------+------------+---------------+
    |   Root     |   Meaning     |   Root     |   Meaning     |
    +============+===============+============+===============+
    | hyle       |    matter     | isos       |    equal      |
    +------------+---------------+------------+---------------+
    | morphe     |     form      | ana        |       up      |
    +------------+---------------+------------+---------------+
    | polys      |     many      | kata       |     down      |
    +------------+---------------+------------+---------------+
    | monos      |      one      | epi        |     upon      |
    +------------+---------------+------------+---------------+
    | autos      |     self      | meta       |  beyond, with |
    +------------+---------------+------------+---------------+
    | endon      |       in      | para       |   beside      |
    +------------+---------------+------------+---------------+
    | ectos      |      out      | meter      |  measure      |
    +------------+---------------+------------+---------------+

In Haskell, polymorphism divides into two categories: *parametric polymorphism*
and *constrained polymorphism*.

Ad-hoc, or constrained, polymorphism in Haskell is implemented with type classes.

Parametric polymorphism refers to type variables, or parameters, that are fully
polymorphic. When unconstrained by a type class, their final, concrete type
could be anything.

Recall that when you see a lowercase name in a type signature, like ``a``, it is
a polymorphic type variable.

If a variable represents a set of possible values, then a type variable
represents a set of possible types.

Parametricity means that the behavior of a function with respect to the types of
its (parametrically polymorphic) arguments is uniform.

.. include:: exercises/5.5.1_-_parametricity.rst

5.5.2 Polymorphic constants
^^^^^^^^^^^^^^^^^^^^^^^^^^^
Numeric literals like ``(-10)`` and ``6.3`` are polymorphic and stay so until
given a more specific type.

5.5.3 Working around constraints
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
``fromIntegral`` takes an integral value and forces it to implement the
``Num`` type class, rendering it polymorphic.

This can be useful when working with functions that return a type which is too
concrete for further use

::

  ·∾ 6 / length [1,2,3]

  <interactive>:8:1: error:
      • No instance for (Fractional Int) arising from a use of ‘/’
      • In the expression: 6 / length [1, 2, 3]
        In an equation for ‘it’: it = 6 / length [1, 2, 3]

  ·∾ :type (/)
  (/) :: Fractional a => a -> a -> a

  ·∾ :type length
  length :: Foldable t => t a -> Int

  ·∾ -- length returns an Int, which is too concrete.

  ·∾ :type fromIntegral
  fromIntegral :: (Integral a, Num b) => a -> b

  ·∾ 6 / fromIntegral (length [1,2,3])
  2.0


5.6 Type inference
------------------
Type inference is a compiler feature that allows it to infer types without
requiring the programmer to write them explicitly every time.

Haskells type inference is built on an extended version of the
Damas-Hindly-Milner type system.

This particular system enables *global type inference* -- where no type
annotations are required at all.

In other languages, inference may be local to function bodies (*local type
inference*, such as in Rust), or even only inferred when you explicitly request
it with a keyword, like ``auto`` in C++ (weird, right?).

Haskell will infer the most generally applicable (polymorphic) type that is
still correct.

Well, most of the time. The monomorphism restriction is a counter-intuitive
rule in Haskells type inference implementation - If you forget to provide a
type signature, sometimes this rule will fill the free type variables with
specific types using "type defaulting" rules.

Thankfully, this is turned off by default since GHC 7.8.1. If not, you can
use ``:set -XNoMonomorphismRestriction``.

More on the `Haskell Wiki <https://wiki.haskell.org/Monomorphism_restriction>`_,
and in the `Haskell 2010 Language Report <https://www.haskell.org/onlinereport/
haskell2010/haskellch4.html#x10-930004.5.5>`_.

Also, here is a `wonderful video <https://www.youtube.com/
watch?v=bv7aenMgSkg&list=PLe7Ei6viL6jGp1Rfu0dil1JH1SHk9bgDV&index=16>`_
on type inference.

.. include:: exercises/5.6.1_-_apply_yourself.rst

5.7 Asserting types for declarations
------------------------------------
Adding type signatures to top level declarations in your code can provide
guidance about a functions purpose. It's generally a good idea to write them.

It's been mentioned that the types can express intent by giving them more
meaningful names. This is a good intuition, but some ways of naming types,
such as type aliases (defined with the ``type`` keyword) and wrapper types
(defined with ``newtype``), can lead to issues with tooling.

One of these is that searching API documentation by type signature with Hoogle
becomes a lot harder. Generally the tooling isn't smart enough to resolve type
aliases to their underlying types for searches, linting in editors, and other
static analysis. This means you have to know the name of the type aliases by
memory to search for them -- you can't just search for ``:: String -> String``
even though it's semantically just an alias for ``:: Name -> EmailAddr``.

So, just use your best judgement.

Lest you begin to think that type signatures are somehow exclusive to top-level
bindings, here's an example of assigning a type to a function within a where
clause::

  triple x = tripleItYo x
    where tripleItYo :: Integer -> Integer
          tripleItYo y = y * 3


5.8 Chapter Exercises
---------------------

.. include:: exercises/5.8.1_-_multiple_choice.rst

.. include:: exercises/5.8.2_-_determine_the_type.rst

.. include:: exercises/5.8.3_-_does_it_compile.rst

.. include:: exercises/5.8.4_-_type_variable_or_specific_type_constructor.rst

.. include:: exercises/5.8.5_-_write_a_type_signature.rst

.. include:: exercises/5.8.6_-_given_a_type_write_the_function.rst

.. include:: exercises/5.8.8_-_type_kwon_do.rst


5.10 Follow-up resources
------------------------
1. Luis Damas; Robin Milner. Principle type-schemes for functional programs
   (`pdf <http://web.cs.wpi.edu/~cs4536/c12/milner-damas_principal_types.pdf>`_)

2. Christopher Strachey. Fundamental Concepts in Programming Languages (`pdf
   <https://www.cs.cmu.edu/~crary/819-f09/Strachey67.pdf>`_)

   (Popular origin of the parametric/ad-hoc polymorphism distinction.)

Additionally, here are some things not from the book that I found relevant
and interesting.

4. A video by Chris Allen on how to query and interact with the type system. He
   discusses some techniques for using existing type signatures as guidance for
   implementing "stubbed-out" functions at the term level.

   .. raw:: html

      <iframe width="560" height="315"
      src="https://www.youtube.com/embed/veu5qj3J50I" frameborder="0"
      allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope;
      picture-in-picture" allowfullscreen></iframe>

5. Luca Cardelli. Type Systems. (`pdf
   <http://lucacardelli.name/Papers/TypeSystems.pdf>`_)

   This paper discusses what a type system is, some of the notation for
   describing them formally, and has a few examples of how to determine whether
   something is well-typed or not (using type judgements in the sequent
   calculus).

6. Luca Cardelli. On Understanding Types, Data Abstration, and Polymorphism
   (`pdf <http://lucacardelli.name/Papers/OnUnderstanding.A4.pdf>`_)

   This is really a paper on understanding subtyping in object-oriented languages
   formally, but the first few sections have a very readable exposition of how
   type systems arise from untyped "universes". I quoted heavily from it
   already, but I think the entire first sections are worth a read.

7. A short video on currying and partial application.

   .. raw:: html

      <iframe width="560" height="315"
      src="https://www.youtube.com/embed/m12c99qgHBU" frameborder="0"
      allow="accelerometer; autoplay; clipboard-write; encrypted-media;
      gyroscope; picture-in-picture" allowfullscreen></iframe>

8. A surprisingly detailed video (maybe brutally so) on how type inference works
   in Haskell. It goes over the algorithm for type inference detailed in the
   Haskell language report.

   .. raw:: html

      <iframe width="560" height="315"
      src="https://www.youtube.com/embed/bv7aenMgSkg" frameborder="0"
      allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope;
      picture-in-picture" allowfullscreen></iframe>
