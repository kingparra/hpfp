******************
 Chapter 5: Types
******************
The poem quoted on the chapter title page comes from `here
<https://www.poetryfoundation.org/poems/43431/the-idea-of-order-at-key-west>`_.

Now, here are some quotes about types from various external resources that I
found interesting.

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
   definition of our isZero test, Z, from Rojas' paper, which has you treat 0
   as true.] In set theory, what is the set-union of the function successor and
   the function predecessor?

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
errors (such as, but not limited to, unintended memory access).

"Constraining the way objects may interact with other objects" inspires the
possibility of enforcing protocols of permissible state changes by modelling the
domain using abstract types. `Here's <http://raganwald.com/2018/02/23/forde.html>`_
a blog post about just that. I'm probably overstating things, here; I've never
even tried. I mean, I've only been programming for two years, and I still kind
of suck at it. Can you tell I'm hyped, though?

In Haskell, type-checking occurs at compile time.

No type system can eliminate all possibility for error, so runtime errors and
exceptions still exist, and testing of program is necessary.

Good type systems can also enable compiler optimizations because the compiler
can know and predict certain things about the execution of a program based on
the types. 

Furthermore, types can serve as documentation of your program.

Working with a good type system can eliminate those tests that only check that
you're passing the right sort of data around.


5.3 How to read type signatures
-------------------------------
The compiler doesn't know which specific numeric types a value is
until the type is either declared or the compiler is forced to infer
a specific type based on the function. 

Instead, it gives the numeric literal type type with the broadest
applicability (most polymorphic) and says it's a constrained
polymorphic ``:: Num a => a`` value.

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
instance of the Num type class, a returning a" or more simply "Num
constrained a to a". This is known as a type class constraint, or
constraint.

Typeclasses offer a standard set of functions that can be used across
several concrete types.

A type signature might have multiple type class constraints on one or
more of the variables.

.. include:: exercises/5.3.3_-_type_matching.rst


5.4 Currying
------------
.. include:: exercises/5.4.5_-_type_arguments.rst
