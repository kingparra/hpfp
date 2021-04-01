********************
 Chapter 18 Outline
********************

* 18.1 Monad
* 18.2 Sorry - a monad is not a burrito

  * 18.2.1 Applicative m

    (It's possible to write class methods of
    applicative and functor only in terms of
    Monad.)

  * 18.2.2 Core operations

    (The main class methods provided by
    Monad: ``(>>=)``, ``(>>)``, ``pure``.)

  * 18.2.3 The novel part of Monad

    (How join reduces structure, like a
    generalized form of ``concat``. This is
    something that Functor and Applicative
    can't do.)

  * 18.2.4 What Monad is not

    (Monads are not: Impure, an eDSL, a
    value, or about strictness. Monad is
    only about structure manipulation.)

  * 18.2.5 Monad also lifts!

    (Monad has ``liftA`` functions, like
    applicative does.)

* 18.3 Do syntax and monads

  * 18.3.1 When ``fmap`` alone isn't enough

    (An example of using join to concatenate
    a two-dimensional monadic context.

    Shortly followed by a code snippet that
    desugars do blocks into their equivalents
    using ``(>>)`` and ``(>>=)``.

    I feel like this should be two
    subsections rather than one.)

* 18.4 Examples of Monad in use

  (Examples of various instances of Monad,
  and how they behave for their respective
  types.)

  * 18.4.1 List

    * 18.4.1.1 Specializing the types
    * 18.4.1.2 Example of the List Monad in use

      (The ``x <- xs`` within a do block
      binds individual elements from the
      input list ``xs``. The nested structure
      of monadic contexts before flattening
      with join is apparent.)

  * 18.4.2 Maybe Monad

    * 18.4.2.1 Specializing the types
    * 18.4.2.2 Using the Maybe Monad -- pages 754..758

      (Examples of sequencing monadic actions
      using Maybe. First with case expressions,
      then with a do block, and finally with
      bind and lambdas. If any action in the
      do block evaluates to ``Nothing``, the
      entire do block also becomes ``Nothing``.

      A key point is that successive lines in
      do blocks represent nested structure.)

    * 18.4.2.3 Exploding a spherical cow

      (This section details the evaluation
      process of a call to ``mkSphericalCow''``)

    * 18.4.2.4 Fail fast, like an overfunded startup

      (...and this section shows a somewhat
      elided version of the evaluation process
      for a case that produces ``Nothing``.)

  * 18.4.3 Either -- page 764

    * 18.4.3.1 Specializing the types
    * 18.4.3.2 Using the Either Monad

      (Notice that ``Either`` always
      short-circuits on the first failure
      condition.

      Also, whoa, what is ``Validation``
      again?  I definitely overlooked that
      in the last chapter.)

    * 18.4.3.3 Short Exercise: Either Monad -- page 768

* 18.5 Monad laws -- page 768

  * 18.5.1 Identity laws
  * 18.5.2 Associativity
  * 18.5.3 We're doing that thing again
  * 18.5.4 Bad Monads and their denizens

* 18.6 Application and composition
* 18.7 Chapter Exercises

  * 18.7.1 Write the instances -- page 780

    * 1
    * 2
    * 3
    * 4

  * 18.7.2 Write the functions -- page 781

    * 1
    * 2
    * 3
    * 4
    * 5
    * 6

* 18.8 Definition

  * Monad
  * Monadic function
  * bind

* 18.9 Follow-up resources

  * Tony Morris; Nick Partridge; Validation library
    http://hackage.haskell.org/package/validation

  * Conor McBride; Ross Paterson; Applicative
    Programming with Effects
    http://staff.city.ac.uk/~ross/papers/Applicative.html

  * Jeremy Gibbons; Bruno C. d. S. Oliveira; Essence
    of the Iterator Pattern

  * Ross Paterson; Constructing Applicative Functors
    http://staff.city.ac.uk/~ross/papers/Constructors.html

  * Sam Lindley; Philip Wadler; Jeremy Yallop;
    Idioms are oblivious, arrows are meticulous,
    monads are promiscuous.

    Note: Idiom means applicative functor and is a
    useful search term for published work on
    applicative functors.
