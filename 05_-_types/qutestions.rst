*********************
 Chapter 5 Questions
*********************

A maintainability (pipe|wet) dream
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
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
