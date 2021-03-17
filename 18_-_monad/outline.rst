********************
 Chapter 18 Outline
********************

* 18.1 Monad
* 18.2 Sorry - a monad is not a burrito

  * 18.2.1 Applicative m
  * 18.2.2 Core operations
  * 18.2.3 The novel part of Monad
  * 18.2.4 What Monad is not
  * 18.2.5 Monad also lifts!

* 18.3 Do syntax and monads

  * 18.3.1 When fmap alone isn't enough

* 18.4 Examples of Monad in use

  * 18.4.1 List

    * Specializing the types
    * Example of the List Monad in use

  * 18.4.2 Maybe Monad

    * Specializing the types
    * Using the Maybe Monad
    * Exploding a spherical cow
    * Fail fast, like an overfunded startup

  * 18.4.3 Either

    * Specializing the types
    * Using the Either Monad
    * Short Exercise: Either Monad -- page 768

* 18.5 Monad laws -- page 768

  * 18.5.1 Identity laws
  * 18.5.2 Associativity
  * 18.5.3 We're doing that thing again
  * 18.5.4 Bad Monads and their denizens

* 18.6 Application and composition
* 18.7 Chapter Exercises

  * 18.7.1 Write Monad instances for the
    following types. Use QuickCheck to
    validate them. -- page 780

    * 1
    * 2
    * 3
    * 4

  * 18.7.2 Write the following functions
    using the methods provided by Monad
    and Functor -- page 781

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
