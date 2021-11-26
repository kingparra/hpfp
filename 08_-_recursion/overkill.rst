8.1 Recursion
-------------

Questions
^^^^^^^^^

3a **"But the lambda calculus does not appear on the surface to have any means of recursion, because
of the anonymity of expressions."**
3b **"How do you call something without a name?"**

(Someone wrote a good overview of this. It's much better then my rambling notes.
https://gist.github.com/lukechampine/a3956a840c603878fd9f. I ended up figuring out something similar
after reading a stack overflow article, and part of my reasoning process is what follows below.
Hopefully no one takes my notes to seriously -- these are just here to help me think -- they're much
messier than my normal written communication style.)

The basic idea is to duplicate a function definition, and then feed the duplicate definition to the
original as an argument. The original function expression takes that argument, binds it to a
parameter, and then calls that parameter with new arguments in the recursive case.

Here's how you might do that in Haskell, using an anonymous function literal.

( from here https://stackoverflow.com/questions/40099927/how-do-i-define-an-anonymous-recursive-function )

::

  > import Data.Function (fix)
  > fix (\f n -> if n == 0 then 1 else n * f (n-1)) 3
  6

So, ``fix`` is meant to behave like the Y combinator in LC. It duplicates the function.
How is it defined in Haskell, I wonder?

::

  fix f = let x = f x in x

Wow, that's pretty different from the untyped lambda calculus representation. Here is a
definition that more closely resembles the untyped LC representation.

::

  import Unsafe.Coerce (unsafeCoerce)

  y :: (a -> a) -> a
  y = \f -> (\x -> f (x' x)) (\x -> f (x' x))
      where x' = unsafeCoerce x

How are those two definitions equivalent? (Or are they?) I should try tracing the evaluation
steps to find out.


But also, what the heck is a fixedpoint?

"...a fixed point of a function is a value that is mapped to itself by the function."
~ https://www.wikiwand.com/en/Fixed-point_combinator

So, for example, the McCarthy 91 function has a fixed point for the input value 91. The preimage
is 91, and the image is also 91.

For the identity function, every input value is a fixed point.

Points that come back to the same value after a finite number of iterations of the function are
called periodic points. A fixed point is a periodic point with period equal to one.

4c **"But without understanding systematic behavior of recursion itself, it can be difficult to
reason about those HOFs."**

What are some examples of HOFs that are difficult to reason about without understanding recursion?

Thoughts about this section
^^^^^^^^^^^^^^^^^^^^^^^^^^^

It seems like each paragraph has two or three topics. Single topic paragraphs are easier to read.

The first sentence **"Recursion is defining a function in terms of itself via self-referential
expressions"** bugs me, too, because it gives the impression that recursion is something specific to
functions in a programming language, instead of a general pattern. Later, in paragraph 2, the
authors say **"Recursion is a natural property of many logical and mathematical systems..."** If you
take the first definition literally, the authors contradicts themselves. I think it would be better
to introduce the general concept of recursion, first, and then describing how it's used in
programming later.


8.2 Factorial!
--------------
