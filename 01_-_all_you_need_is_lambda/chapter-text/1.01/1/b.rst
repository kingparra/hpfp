.. TODO Read part 1 of "Introduction to the Theory of
   Computation" by Sipser. It introduces most of the terminology
   needed to discuss models of computation like lambda calculus.

..
   Two big questions in computer science:

   * Computability - "What are the fundamental capabilities and limitations of computers?"
   * Complexity theory - "What makes some problems computationally hard and others easy?"


1b) **A calculus is a method of calculation or reasoning; the
lambda calculus is one process for formalizing a method.**

This sentence seems a bit circular. If we take the definition of
**calculus** as **"a method of calculation or reasoning"** and do
a textual substitution, then it becomes:

**The lambda "method of calculation or reasoning" is one process
for formalizing a method.**

So the lambda calculus is a method - for one process - of
formalizing a method?

Going by the common English usage, a I'm tempted to omit the
**"for one process"** like: **"The lambda calculus is a method
for formalizing a method"**, which reads more fluidly, but seems
even more circular.

But maybe some of these words have different usage in the context
of computer science?

Then,

  * What is a method?

    Maybe the word *method* here refers to the idea of an
    *effective method* in computability theory?

    The Stanford Encyclopedia of Philosophy defines an effective
    method on it's `page about the Church-Turing thesis`_ as:

    ..

      "A *method*, or *procedure*, M, for achieving some desired
      result is called *effective* (or *systematic* or
      *mechanical*) just in case:

      * M is set out in terms of a finite number of exact
        instructions (each instruction being expressed by
        means of a finite number of symbols);

      * M will, if carried out without error, produce the
        desired result in a finite number of steps;

      * M can (in practice or in principle) be carried out
        by a human being unaided by any machinery except
        paper and pencil;

      * M demands no insight, intuition, or ingenuity, on
        the part of the human being carrying out the method."

    ..

    So that means that:

    * systematic method,
    * systematic procedure,
    * mechanical method,
    * and mechanical procedure

    are all different words for the idea of an algorithm, right?


  * What is a process?

    I think Chris is using the `regular English definition of the word process
    <https://www.merriam-webster.com/dictionary/process>`_.

    **"1: a series of actions that produce something or that lead
    to a particular result"**

  * What is a formalism?

    https://www.wikiwand.com/en/Formalism_(philosophy_of_mathematics)

    ..

      "In the philosophy of mathematics, formalism is the view that
      holds that statements of mathematics and logic can be
      considered to be statements about the consequences of the
      manipulation of strings (alphanumeric sequences of symbols,
      usually as equations) using established manipulation rules."

      . . .

      "Rather, mathematical statements are syntactic forms whose
      shapes and locations have no meaning unless they are given
      an interpretation (or semantics)."

    ..

  * What does it mean to formalize a method?

    To put it into a calculus -- or, in other words, to represent
    it as a set of strings or expressions paired with rewrite
    rules that may (or may not) be ascribed meaning (semantics).

  * What does formalization entail, and how is it done?

  * What is an algorithm?

    From `merriam-websters definition of algorithm`_:

      **A procedure for solving a mathematical problem in a finite
      number of well-defined steps.**

      Well-defined has a specific meaning here.

      https://mathworld.wolfram.com/Well-Defined.html

      (An expression is well-defined if there is only
      one unique value that it can possibly stand for.

      A function is well-defined if it gives the same
      result regardless of the representation of the input.

      For the function f, if f(0.5) == f(1/2), then it's
      well-defined.

      The term "well-defined" is sometimes called "single-valued",
      instead, which I think is easier to understand.)

    From "Introduction to Computer Science using Python: A
    Computational Problem-Solving Approach."

      **An algorithm is a finite number of clearly described,
      unambiguous "doable" steps that can be systematically followed
      to produce a desired result for given input in a finite amount
      of time.**

  * What is the difference between an algorithm and a (mathematical) function?


..
  links

.. _merriam-websters definition of algorithm: https://www.merriam-webster.com/dictionary/algorithm
.. _page about the Church-Turing thesis: https://plato.stanford.edu/entries/church-turing/
