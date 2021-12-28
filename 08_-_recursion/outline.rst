*******************
 Chapter 8 Outline
*******************
Here's the general structure of this outline::

  . . .

   * <section number> <section title>

     * p<paragraph number>. <One sentence summary of _what_ the
       main subject is. Not an explanation of the subject,
       unless I feel like explaining it.>

     * f<figure number>. <One sentence summary.>

  . . .


* 8.1 Recursion

  * p1. Characteristics of recursive function definitions.

  * p2. An example of recursion as a pattern that occurs naturally in language.

  * p3. I'm not sure what the intention of this paragraph is. Why is the author bringing up LC,
    Turing Completeness, and the Y-combinator here? How is it relevant to writing recursive
    functions in Haskell?

  * p4. Recursion is the only method of expressing repetition in Haskell, so you need to understand
    it in order to read others code.

* 8.2 Factorial!

  * p1. We will demonstrate a recursive function called factorial.

  * p2. Let's evaluate 4!:

    * f1. Shows the evaluation steps for 4!.

  * p3. Now let's look at a factorial definition in Haskell, for only one input, the number 4.

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

  * p8. "The base case for ``factorial``, defined as ``factorial 0 = 1`` provides a stopping point,
        so the reduction changes:"

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

* 8.4 Fibonacci numbers

  * p1. In order to demonstrate how to create recursive
    functions, we're going to walk through how to write a
    function that calculates the :math:`n`\th element of
    the Fibonacci sequence.

  * 8.4.1 Consider the types

    * p2. First consider what the input and output should be, and
      then encode that in a type signature. The preconditions for
      valid input are hints about what type you should use.

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
      the element of the Fibonacci sequence we want to retrieve.

    * p6. In order to come up with the new element we must
      retrieve the two preceding elements.

      * f3. Same as the last figure, but also shows a stub for
        the recursive case that contains the arguments without
        any function calls.

  * 8.4.4 Consider the recursion

    * p7. How will the function call itself? What needs to happen next to produce a Fibonacci
      number?

      * f4. Shows a stub of the ``fibonacci`` function definition with the two base cases, but
        without a complete recursive case.

    * p8. "If you pass the value 6 to that function, what will happen?"

      * f5. Shows GHCi output of ``fibonacci 6``.

    * p9. We want to add the elements, not the index numbers of those elements. So we'll call
      ``fibonacci`` to retrieve them.

      * f6. Shows the complete definition of ``fibonacci``, with a working recursive case.

    * p10. "Now, if we apply this function to the value 6, we will get a different result:"

      * f7. Shows the GHCi output of ``fibonacci 6`` using the new definition.

    * p11. Why do we get this result? Because ``fibonacci`` evaluates its arguments recursively.

      * f8. Show each recursive function call that occurs when evaluating ``fibonacci 6``.

    * p12. "0 and 1 are defined as being equal to 0 and 1. So at this point, our recursion stops,
      and the function starts adding up the result:"

      * f9. Shows the process of adding together the reduced value of all the recursive function
        calls.

    * p13. Thinking about the evaluation process ahead of time can be intimidating. But you don't
      have to do everything at once.

* 8.5 Integral division from scratch

  * p1. Multiplication can be defined in terms of repeated addition. Likewise, division can be
    defined in terms of repeated subtraction.

  * p2. We will show how to define a function that performs multiplication in terms of addition
    using recursion, step by step.

    (Instead of explaining how to come up with a solution, this explains an existing solution step
    by step. Annoying!)

    * f1. Shows the type signature for ``dividedBy``.

  * p3. "Instead of having all the types labeled Integer we can instead do:"

    * f2. Shows type aliases ``Numerator``, ``Denominator``, and ``Quotient`` in the type signature
      for ``dividedBy``.

  * p5. ``type`` introduces a type alias.

  * p6. We aren't going to use those type synonyms after all. We also haven't written out a
    recursive implementation of ``dividedBy`` yet.

  * p7. The base case is when our result is lower than the divisor.

    * f3. Shows a psuedocode example of :math:`20/4` in terms of repeated subtraction steps. In
      comments, a stopping condition (result < divisor), and a count of the number of subtraction steps
      are mentioned.

  * p8. "Otherwise, we'll have a remainder. Let's look at a case where it doesn't divide evenly:"

    * f4. Shows :math:`24/5` in the same style as above.

  * p9. We can generalize the calculations in the figures above as a function. Also, now that the
    possibility of a remainder has been pointed out, we want to reflect it in the type signature by
    returning a tuple of ``(count, remainder)``.

    * f5. Shows a definition of ``dividedBy``.

  * p10. We changed the type signature to use ``Integral a =>`` and also to return a tuple ``(a,
    a)``.

  * p11. Explanation of ``go`` function idiom. Go functions are inner functions. This one keeps
    track of an extra argument, the count.

  * p12. Explains the two branches of the go function.

  * p13. The result is our base case.

  * p14. "Here’s an example of how dividedBy expands but with the code inlined:"

    * f6. ``dividedBy 10 2``

  * p15. First we'll show it in psuedocode, but keep track of how many times we'll subtract.

    * f7.

  * p16.

  * p17. "Now, we'll expand the code:"

    * f8. Shows a fragment of ``dividedBy``'s code during evaluation..

  * p18. "The otherwise above is literally the value True, so if the first branch
    fails, the otherwise branch always succeeds:"

    * f9. Continues the evaluation of ``dividedBy``'s recursive branch until the base case it hit.

  * f19. Explanation of final output.

* 8.6 Chapter exercises

  * 8.6.1 Review of types

    * 1

      * a
      * b
      * c
      * d

    * 2

      * a
      * b
      * c
      * d

    * 3

      * a
      * b
      * c
      * d

    * 4

      * a
      * b
      * c
      * d

  * 8.6.2 Reviewing currying

    * p1. Desk-check the evaluation steps of the following expressions.

      * f1. Shows the definitions of a few functions that concatenate strings and rearrange
        arguments.

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

  * 8.6.4 Fixing ``dividedBy``

    * p1. ``dividedby`` is a undefined for numbers 0 or less.
    * p2. Using ``div`` we can see how negative numbers should be handled:

      * f1. Shows GHCi output of div against different arguments, both positive and negative.

    * p3. The next issue is how to handle zero. Let's use a datatype to represent the possibility of
      a result or a division by zero.

      * f2. Shows the definition of ``DividedResult``.

  * 8.6.5 ``McCarthy91`` function

    * p1. We'll describe a function in English, math notation, and also show some test cases. Your
      task is to write it in Haskell.
    * p2. "The McCarthy 91 function yields x - 10 when x > 100 and 91 otherwise. The function is
      recursive:"

      * f1. Shows the McCarthy91 function in math notation.
      * f2. Show an name binding in haskell for the identifier ``mc91`` to ``undefined``.

    * p3. Map distributes a function over every element of a list.

      * f3. Shows the GHCi output of ``map mc91 [95..110]``

  * 8.6.6 Numbers into words

    * f1.
    * p1.
    * p2.
    * p3.
    * p4.

      * f2.

    * p5. "Also consider:"

      * f3.

    * p6. "Here is what your REPL output should look like when it's working:"

      * f4. Shows the GHCi output of ``wordNumber 123456``.

* 8.7 Definitions

  * 8.7.1 Recursion

    * p1.
    * p2. "This function is not recursive:"

      * f1.

    * p3. "This one is recursive:"

      * f2.
