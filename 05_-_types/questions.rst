*********************
 Chapter 5 Questions
*********************

* A maintainability (pipe|wet) dream

  So, I have this idea that it's impossible to plan for change, so the best I can
  do is make it easy to rewrite things. I should try to make components replaceable.

  Every time I write a function I try to ask myself

  * "Can I reason about this independently?",
  * "What context is necessary to understand this code?",
  * "Are all inputs and outputs accounted for?", and
  * "What happens if I cut-and-paste this into an external file? (Will it
    effect the execution flow, maybe because it uses a variable outside the
    function definition, or depends on a time-ordered event)?".

  Recently I encountered Idris, where you can encode so much in the type signature
  that the language can write the logic of function for you. Later, using Haskell,
  I changed a functions type signature, and all the places that used it threw type
  errors to inform me that I had to change the call site. This made me think...

  Is it possible to go one level further?

  Say you delete a functions business logic, type signature, and its documentation.
  Can you reimplement it given only its usage at call sites across the code base and
  the function name?

  I suspect that in some cases you can at least infer the type by trying to find
  one that agrees with its uses at all call sites. Maybe that's enough to figure
  out the logic, too. The name should help your puzzle out the intent of that
  function.

  Do functions like these exist?

* How can I inspect the types of expressions without using the repl?

  ...as a starting point
  https://hackage.haskell.org/package/base-4.14.0.0/docs/Type-Reflection.html is
  worth a look. Seems that is has some limitations, though.

* If a type class constraint gets specialized to a descendant type class, does
  the ancestor type class show up in the signature?

* Someone, somewhere, must have made a library of combinators based on type
  signatures alone. What does that look like?

* What are some errors to watch out for that still type-check?

* What are all of the primitive types available?

* Is it possible to define new primitive types?

* What are some examples of compiler optimizations that type type system allows
  us to do?

::

  romesrf> Can anyone point me to literature on the `newtype`. Is it zero cost?

  <ski> romesrf : given `newtype Foo = MkFoo Bar', `MkFoo myBar' is represented in the same way as `myBar', yes

  <romesrf> ski: truly amazing :)

  <ski> this means that `MkFoo' is strict (iow `MkFoo _|_ = _|_'), is a no-op, and matching on it is also a no-op

  <justsomeguy> What is the cost of creating a new datatype using the data keyword, anywys?
                Does it exist in memory, even if it isn't used?

  <geekosaur>   justsomeguy, just declaring it costs only some memory during compilation, and an
                info table at runtime which tells the garbage collector how to deal with any values
                of that type it finds

  <ski> (as opposed to `data Foo = MkFoo !Bar', where `MkFoo' is also strict, but matching on it is not a no-op)

  <geekosaur> justsomeguy, just declaring it costsonly some memory during compilation, and an
              info table at runtime which tells the garbage collector how to deal with any values of
              that type it finds. The info table is small and shared among all valuesof that type.
