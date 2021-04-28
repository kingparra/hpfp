********************
 Chapter 22: Reader
********************


22.1 Reader
-----------
.. "
.. A program in a pure functional language is
.. written as a set of equations. Explicit data
.. flow ensures that the value of an expression
.. depends only on its free variables.
.. " ~ Monad for functional programming, Phillip Wadler


.. Some vocabulary...
..
.. Free variable
..
..   A variable that is not a parameter
..   or locally defined.
..
..   A variable that is used locally, but
..   defined in an enclosing scope.
..
.. Bound variable
..
..   A parameter.
..
..   A parameter name that has been saturated
..   with the value of its respective argument
..   for that function call.
..
.. Combinator
..
..   A function that serves only to combine
..   its parameters and does not have any
..   free variables within its definition.
..
..   In the lambda calculus, all names used in
..   the function body would also be present
..   in the head.
..
.. Closure
..
..   An expression (or function) that
..   introduces, or has, free variables.
..
..   In the lambda calculus, some names in the
..   function body would not be present in the
..   head.
..
..   "A lambda expression whose open bindings (free variables)
..   have been closed by (or bound in) the lexical environment,
..   resulting in a closed expression, or closure."
..
..   [5] Åke Wikström (1987). Functional Programming
..   using Standard ML. ISBN 0-13-331968-7. "The
..   reason it is called a "closure" is that an
..   expression containing free variables is
..   called an "open" expression, and by
..   associating to it the bindings of its free
..   variables, you close it."
..
..   Strictly, an anonymous function is a function
..   literal without a name, while a closure is
..   an instance of a function, a value, whose
..   non-local variables have been bound either
..   to values or to storage locations
..   (depending on the language; see the lexical
..   environment section below).
..
..   https://en.wikipedia.org/wiki/Closure_(computer_programming)
..
..   Peter J. Landin defined the term closure in
..   1964 as having an environment part and a
..   control part as used by his SECD machine for
..   evaluating expressions.[3] Joel Moses credits
..   Landin with introducing the term closure to
..   refer to **a lambda expression whose open
..   bindings (free variables) have been closed by
..   (or bound in) the lexical environment,
..   resulting in a closed expression, or
..   closure.[4][5]** This usage was subsequently
..   adopted by Sussman and Steele when they
..   defined Scheme in 1975,[6] a lexically scoped
..   variant of Lisp, and became widespread.
..
..   https://en.wikipedia.org/wiki/Closure_(computer_programming)
..
..  The scope of a variable describes where in a
..  program's text the variable may be used, while
..  the extent (or lifetime) describes when in a
..  program's execution a variable has a
..  (meaningful) value.
..
..  In languages with closures, variables must
..  continue to exist as long as any existing
..  closures have references to them. This is
..  most commonly implemented using some form
..  of garbage collection.
..
..
.. "
.. A program in a pure functional language is
.. written as a set of equations. Explicit data
.. flow ensures that the value of an expression
.. depends only on its free variables.
..
.. Hence substitution of equals for equals is
.. always valid, making such programs especially
.. easy to reason about. Explicit data flow also
.. ensures that the order of computation is
.. irrelevant, making such programs susceptible
.. to lazy evaluation.

.. It is with regard to modularity that explicit
.. data flow becomes both a blessing and a curse.
.. On the one hand, it is the ultimate in
.. modularity. All data in and all data out are
.. rendered manifest and accessible, providing a
.. maximum of flexibility. On the other hand, it
.. is the nadir of modularity. The essence of an
.. algorithm can become buried under the plumbing
.. required to carry data from its point of
.. creation to its point of use
.. " ~ Monad for functional programming, Phillip Wadler


..  |  justsomeguy What is the Reader monad?
..  |
..  |       Axman6 it's a way to pass around some
..  |              data that various parts of your
..  |              application needs, without needing to
..  |              explicitly pass it as function arguments
..  |
..  |  justsomeguy So, a named closure that shows up
..  |              in type signatures?
..  |
..  |      shachaf Well, a closure closes over some
..  |              existing value. "Reader" means a
..  |              thing is parameterized.
..  |
..  |       Axman6 so, it's commonly used to pass
..  |              around settings your application was
..  |              passed when it launched, from command
..  |              line arguments, config files, etc.
..  |
..  |      shachaf But "Reader r a" is just a function, "r -> a".


In this chapter, we will:

* examine the ``Functor``, ``Applicative``, and ``Monad``
  type class instances for *functions*;
* learn about the ``Reader`` newtype;
* and see some examples of using ``Reader``.


22.2 A new beginning
--------------------
.. todo Create an expect script that follows
.. the narrative in 22.2, loading in figures
.. in at the approriate times, and record it
.. with git and asciinema. Phew, that sounds
.. like a lot of work.
