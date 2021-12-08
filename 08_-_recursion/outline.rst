*******************
 Chapter 8 Outline
*******************

.. Here's the general structure of this outline.
.. . . .
..  * <section number> <section title>
..    * p<paragraph number>. <One sentence summary of _what_ the main subject is. Not an
..      explanation of the subject, unless I feel like explaining it.>
..    * f<figure number>. <One sentence summary.>
.. . . .


* 8.1 Recursion

  * p1. What makes a function recursive. Indefinite repetitions.

  * p2. Recursion is a pattern that occurs in language and nature. Indefinite repetitions.

  * p3. Haskell and lambda calculus have similar evaluation strategies, and recursive anonymous
    function literals can be written using the Y-combinator in both languages.

  * p4. Recursion is the only method of expressing repetition in Haskell, so you need to understand it in
    order to read other peoples code. Something about leaky abstractions and HOFs.

* 8.2 Factorial!

  * p1. We will demonstrate a recursive function called factorial.

  * p2. Let's evaluate 4!:

    * f1. Shows the evaluation steps for 4!.

  * p3. Now let's look at a factorial definition, for only one input, the number 4.

    * f2. Shows a definition of a literal value equivalent to ``factorial 4``.

  * p4. The code above only covers one possible result for ``factorial``. We want to express the
    general idea of ``factorial``.

  * p5. A base case is non-recursive, and is what stops self-application. An example follows.

    * f3. Shows factorial definition generalized for any input value, but missing a base case, named
      ``brokenFact1``.

  * p6. Let's apply this to 4 and see what happens:

    * f4. Shows the evaluation steps of ``brokenFact1`` applied to 4.

  * p7. The base case stops recursive calls. Here's what it looks like for ``factorial``:

    * f5. Shows a complete definition of ``factorial``, as well as the evaluation steps of
      ``brokenFact1 4``.

  * p8. The base case for ``factorial``, defined as ``factorial 0 = 1`` provides a stopping point,
        so the reduction changes.

    * f6. Shows the evaluation steps of the fully defined ``factorial`` function, which has a base
          case.

  * p9. Making our base case an identity value for the recursive operation means that applying it doesn't
    change the value.

  * 8.2.1 Another way to look at recursion

    * p1. Composition chains function applications together.
    * p2. Composition has a definite number of repetitions, but recursive calls are indefinite.
    * p3. Function composition has the following type:

      * f1. Shows the type signature of the ``(.)`` operator.

    * p4. And when we use it like this:

      * f2. Shows an example of using ``(.)`` to compose functions that get the first five odd
        numbers after three.

    * p5.
    * p6. "Recursion is self-referential composition." Or, really, composition resembles the
      function call stack that is accumulated during recursive calls.
    * p7. "Now look again at how th ecompose function ``(.)`` is written:"

      * f3. Shows type signature and term-level definition of the ``(.)`` operator.

    * p8. Explanation of what function composition is -- a pipeline of function applications.

      * f4. Shows a term-level definition of ``(.)``, again.

    * p9. "...instead of a fixed number of applications, recursive functions rely on inputs to
      determine when to stop applying function to successive results."
    * p10. "Let's look at aomse code to see the similarity in patterns:"

      * f5. Shows a incrementing function and an expression named ``three`` with a value of ``3``,
        made by composing the increment function three times.

    * p11. "We don't presently have a means of changing how many times we want it to apply ince
      without writing a new function."
    * p12. Generalizing inc requires a new function.

      * f6. Shows the definition of ``incTimes``.

    * p13. Explains how ``incTimes`` lets you control the number of repetitions with the ``times``
      parameter.

      * f7. Shows ``incTimes`` applied to different values in GHCi.

    * p14. How the risk of infinite recursion is minimized in ``incTimes``.
    * p15. "We can absract the recursion out of ``incTimes``, too:"

      * f8. Shows ``incTimes`` defined in terms of ``applyTimes``, which adds a parameter for the
        function to be applied, instead of hard-coding the increment function in.

    * f16. "When we do, we can make the composition more obvious in ``applyTimes``:"

      * f9. Shows ``applyTimes`` rewritten to use the ``(.)`` operator, instead of parenthesis, so
        that reduction steps don't create nested parenthesis.

    * p17. "We're recursively composing our function ``f`` with ``applyTies (n-1) f`` however many
      subtractions it takes to get ``n`` to ``0``!"

  * 8.2.2 Intermission: Exercise

    * p1. "Write the evaluation of the following. It might be a little less noisy if you do so with
      the form that didn't use ``(.)``.

      * f1. Shows ``applyTimes 5 (+1) 5``.

* 8.3 Bottom

  * p1. Bottom represents computations that don't result in a value, like expressions that result in
    an error or infinite loops. Bottom corresponds to false in logic.

    * f1. Shows what happens when you evaluate an infinitely recursive expression in GHCi.

  * p2. Explains the GHCi output.

  * p3. "Next let's define a function that will return an exception:"

    * f2. Shows a function that explicitly throws an error for the value ``True``.

  * p4. "And let's try that out in GHCi:"

    * f3.

  * p5. Explanation of GHCi output.

  * p6. "Another example of a bottom would be a partial function. Let's consider a rewrite of the
    previous function:" Example of an infinite loop.

    * f4. Example of a partial function, the only input it's defined for is ``False``.

  * p7. This new function will give us a different exception.

    * f5. Shows the GHCi output of our new partial function definition. "Non-exhaustive patterns in
      function f."

  * p8. Haskell has made the fallback case for undefined inputs an error. The previous function was
    really:

    * f6. Shows ``f`` with an explicitly defined fallback case that throws an error.

  * p9. Partial vs total. How do we make our ``f`` into a total function?

    * f7. Shows a simplified definition of the ``Maybe`` datatype.

  * p10. Explanation of ``Maybe``. "Here's how we'd use it with ``f``:"

    * f8. Shows ``f`` adapted to return a result of type ``Maybe Int``.

  * p11. We'll get a type error if we try to load the code.

    * f9. Shows ``f``, using ``Maybe``, but missing a ``Just`` for one of the equations.
    * f10. Show what happens when you attempt to load f9 into GHCi. ``No instance for (Num (Maybe
      Int))``.

  * p12. We can get a better error message by making the result of ``0`` for our base case a
    concrete ``Int`` type.

    * f11. Basically f9 with ``f False = 0 :: Int``.

  * p13. "And then get a better type error in the bargain:"

    * f12. Shows loading f11 into GHCi. ``Couldn't match expected type ‘Maybe Int’ with actual type
      ‘Int’``.

  * p14. "We'll explain ``Maybe`` in more detail later."

.. TODO Update section 8.4 to reflect the new structure in v1 of the book. It currently is modeled
   after RC2.

* 8.4 Fibonacci numbers

  * p1. In order to demonstrate how to create recursive
    functions, we're going to walk through how to write a
    function that calculates the :math:`n`\th element of
    the Fibonacci sequence.

  * 8.4.1 Consider the types

    * p2. First consider what the input and output should be, and
      then encode that in a type signature. Consider any
      preconditions for valid input.

      * f1. Shows the type signature of ``fibonacci``.

  * 8.4.2 Consider the base case

    * p3. When can you solve the problem directly, without
      recursing? In this case, ``fibonacci`` should only operate
      on positive numbers, so if we get an argument value of 0,
      we'll return a 0 to stop the recursion. (It would probably
      make more sense to use a different type.)

    * p4. Fibonacci requires two base cases, since the sequence
      by definition starts with :math:`(0,1,…)`.

    * f2. Shows equations representing the two base cases of
      the ``fibonacci`` function and the function type signature
      in Haskell.

  * 8.4.3 Consider the arguments

    I don't understand this section.

    * p5. Each argument is a number that represents an index for
      the member of the Fibonacci sequence we want.

    * p6. In order to come up with the new element we must
      retrieve the two preceding elements.

      * f3. Same as the last figure, but also shows a stub for
        the recursive case that contains the arguments without
        any function calls.

  * 8.4.4 Consider the recursion

    * p7.

      * f4.

    * p8.

      * f5.

    * p9.

      * f6.

    * p10.

      * f7.

    * p11.

      * f8.

    * p12.

      * f9.

    * p13.

* 8.5 Integral division from scratch
* 8.6 Chapter exercises

  * 8.6.1 Review of types

    * 1
    * 2
    * 3
    * 4

  * 8.6.2 Reviewing currying

    * 1
    * 2
    * 3
    * 4
    * 5
    * 6

  * 8.6.3 Recursion

    * 1
    * 2
    * 3

  * 8.6.5 Fixing ``dividedBy``
  * 8.6.6 ``McCarthy91`` function
  * 8.6.7 Numbers into words

* 8.7 Definitions

  * Recursion

    This is a terrible definition of recursion.
