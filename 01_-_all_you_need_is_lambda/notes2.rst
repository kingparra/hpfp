***********************************
 Chapter 1: All You Need is Lambda
***********************************


1.1 All you need is lambda
--------------------------

Subjects
^^^^^^^^
* lambda calculus, model of computation, mathematical model, Alonzo Church, (logical)
  calculus, method, process, formalization, formalism, Turing machine, effective
  computability, problem, solvable problems, unsolvable problems, classes of problems,
  decidability, decision problem

Claims
^^^^^^
1b **"A calculus is method of calculation or reasoning; the lambda calculus is one process for
formalizing a method."**

* There *is* a definition of what a calculus is here, but I feel that it is too terse. I'm just
  not satisfied with it. What is a calculus, really?

  Logical calculus https://encyclopediaofmath.org/wiki/Logical_calculus

  Logico-mathematical calculus https://encyclopediaofmath.org/wiki/Logico-mathematical_calculus

  from a slide I found here https://www.cs.cmu.edu/~venkatg/teaching/15252-sp21/index.html

  "Calculus = just a bunch of rules for manipulating symbols.
  One can give meaning to the symbols (semantics), but that's not part of the calculus (pure syntax).
  One can associate meanings to expressions in a way that corresponds to computations (functional programs)."

  From 1.6 Multiple arguments, paragraph 8, sentence c.

  1.6 8c **"The lambda calculus is a process or method, like a game with a few simple rules for
  transforming lambdas but no specific meaning."**

* What is a process?
* What is a method?
* Is the word "method" here being used in the specialized sense that it appears as within
  discussions of the Church-Turing thesis, as in, an effective (mechanically calculable) method?

    https://plato.stanford.edu/entries/church-turing/

    "1. The Thesis and its History

    The Church-Turing thesis concerns the concept of an
    effective or systematic or mechanical method in logic,
    mathematics and computer science. "Effective" and its
    synonyms "systematic" and "mechanical" are terms of art
    in these disciplines: they do not carry their everyday
    meaning.

    **A method, or procedure, M, for achieving some desired
    result is called ‘effective’ (or ‘systematic’ or
    ‘mechanical’) just in case:

    * M is set out in terms of a finite number of exact
      instructions (each instruction being expressed by
      means of a finite number of symbols);

    * M will, if carried out without error, produce the
      desired result in a finite number of steps;

    * M can (in practice or in principle) be carried out by
      a human being unaided by any machinery except paper
      and pencil;

    * M demands no insight, intuition, or ingenuity, on the
      part of the human being carrying out the method.**

    . . .

    One of Alan Turing's achievements, in his famous paper
    of 1936, was to present a formally exact predicate with
    which the informal predicate “can be done by means of an
    effective method” may be replaced (Turing 1936). Alonzo
    Church, working independently, did the same (Church 1936a).

    . . .

    As explained by Turing (1936: 84), Hilbert's
    Entscheidungsproblem is this: Is there a general
    (effective) process for determining whether a given
    formula A of the first-order propositional calculus is
    provable?

    . . .

    Church's thesis: A function of positive integers is
    effectively calculable only if lambda-definable (or,
    equivalently, recursive)."

* What does it mean to formalize a method?
* How does someone formalize a method, in general?

  I'm not really sure, but I found this paper that discusses the subject:

  "How to Formalize It? Formalization Principles for Information Systems Development Methods", A.H.M.
  Hofstedeter and H.A. Proper, Information and Software Technology, 40(10), 519–540, 1998.

  Abstract. Although the need for formalisation of modelling techniques is generally recognised, not
  much literature is devoted to the actual process involved. This is comparable to the situation in
  mathematics where focus is on proofs but not on the process of proving. This paper tries to accommodate
  for this lacuna and provides essential principles for the process of formalisation in the context
  of modelling techniques as well as a number of small but realistic formalisation case studies.

  Keywords: Formalization, Methodologies, Information Systems

  https://www.semanticscholar.org/paper/
  How-to-formalize-it%3A-Formalization-principles-for-Hofstede-Proper/
  991cc9588026661e48effec5cb551304933b4795


  Also, `here is the definition of **formalization method**
  from the Encylopedia of Mathematics
  <https://encyclopediaofmath.org/wiki/Formalization_method>`_,

  Formalization method

  A way of expressing by a formal system a mathematical
  theory. It is one of the main methods in proof theory.

  An application of the formalization method involves
  carrying out the following stages.

  * Putting the original mathematical theory into symbols.
    In this all the propositions of the theory are written
    in a suitable logico-mathematical language L.

  * The deductive analysis of the theory and the choice of
    axioms, that is, of a collection of propositions of the
    theory from which all other propositions of the theory
    can be logically derived.

  * Adding the axioms in their symbolic notation to a
    suitable logical calculus based on L.

  The system obtained by this formalization is now itself
  the object of precise mathematical study (see Axiomatic
  method; Proof theory).

  References: [1] S.C. Kleene, "Introduction to
  metamathematics", North-Holland (1951)

* Are there other processes for formalizing a method?
* What is a formalism?
* What problem or turn of events motivated the creation of lambda calculus?
* Which papers and events were the lambda calculus introduced by?

4a **"We're starting from first principles here, so that when we get around to building
projects, you know what you're doing."**

* How will learning lambda calculus help me build projects?

  * It won't. But LC will be useful for several other things:

    * Communicating with other Haskellers.
    * Understanding how multiple arguments are treated during program evaluation.
    * Understanding how control flow works in functional languages.

      * Dependencies between function calls determine control flow, not a program counter.
      * Church-Rosser theorem: Regardless of the order reductions are performed in, the result will be the same.
      * Outermost reduction comes into play when determining which argument will be consumed first.

    * Illustrating the idea of equational reasoning.

      * Programs are like algebraic expressions.
      * Running a program corresponds to reducing those expressions to a simpler form.
      * At any point during program execution, a name may be replaced with its definition, as in math.
      * Thinking of programs this way means you can rearrange source code algebraically,
        too, in order to make it easier to read.
      * You can begin to think of a program as a graph. In this graph, each node is a
        function execution instance, represented as an equation. Within each equation,
        names don't change meaning. Connections between nodes represent input arguments to
        parameters of each nodes execution instance.

    * Reading type signatures, and deducing how different type signatures may be combined.

4c **"Lambda calculus is your foundation, because Haskell is a lambda calculus."**

* Is that really true? In what sense is Haskell a lambda calculus?

Remarks
^^^^^^^
The quote at the beginning of the chapter talks about great mathematicians. Then the first paragraph
name drops several concepts related to computability that someone without exposure to cs would
be completely unaware of. This makes me wonder: who is the real target audience, here?

Why is there no description of how learning lambda calculus will benefit your ability to write
Haskell code?

Where are the learning objectives?

*What* are the learning objectives?

What are the abilities you'll gain by completing the chapter?

I get that your asking me to trust you, but I think you've missed an opportunity to make your
writing more compelling.


1.2 What is functional programming?
-----------------------------------

Subjects
^^^^^^^^
* functional programming, programming paradigm, mathematical functions, expression,
  values, variables, functions, argument, input, application (of a function to its
  arguments), reduction, evaluation, first-class, argument passing, lambda expression,
  purity, referential transparency, abstraction, composability, (re)factoring, generic
  code

Claims
^^^^^^
2a **"Functional programming languages are all based on the lambda calculus."**

* What does it mean for a language to be based on LC?
* What about languages based on other calculi that allow equational reasoning, like closure
  calculus, or SKI combinator calculus? Are those not functional languages, too?
* LISP is one of the first functional languages and it was not initially based on lambda calculus,
  but on a formalism that McCarthy developed, instead.

  "The recursive functions mentioned in McCarthy's seminal paper, Recursive functions of Symbolic
  Expressions and Their Computation by Machine, Part I refer to the class of functions studied in
  computability theory."

  . . .

  "… one of the myths concerning LISP that people think up or invent for themselves becomes
  apparent, and that is that LISP is somehow a realization of the lambda calculus, or that
  was the intention. The truth is that I didn't understand the lambda calculus, really."
  ~ John McCarthy, Lisp session, History of Programming Languages

See the discussion here and linked article for details: https://news.ycombinator.com/item?id=20696931

2b **"Some languages in the general category incorporate features that aren't translatable
into lambda expressions."**

* What does it mean to translate a language feature into a lambda expression?
* By lambda expression, do you mean an expression in the lambda calculus, or the Haskell
  syntax for function literals?
* Assuming you mean an expression in LC; **How can a language feature not be translatable
  into lambda expressions? Isn't that like saying a language feature can't be translated
  to binary?** LC is just an encoding, after all.
* Also, in section 1.8 of "Functional Programming through Lambda Calculus" by Greg Michaelson, the
  author mentions that LC has been used to model imperative languages. How does that fit in?

  "1.9 Computing and theory of computing

  . . .

  **In the mid 1960s, Landin and Strachey both proposed the use of the λ-calculus to model
  imperative languages.** Landin's approach was based on an **operational** description of the
  λ-calculus defined in terms of an **abstract interpreter** for it - the SECD machine. **Having
  described the λ-calculus, Landin then used it to construct an abstract interpreter for ALGOL 60.**
  (McCarthy had also used an abstract interpreter to describe LISP). This approach formed the bases
  of the Vienna Definition Language (VDL) which was used to define IBM's PL/1. The SECD machine has
  been adapted to implement many functional languages on digital computers. Landin also developed
  the pure functional language ISWIM which influenced  later languages.

  **Strachey's approach was to construct descriptions of imperative languages using a notation based
  on λ-calculus so that every imperative language construct would have an equivalent function
  denotation.** This approach was strengthened by Scott's lattice theoretic description for
  λ-calculus. Currently, **denotational semantics** and its derivatives are used to give formal
  definitions of programming languages. Functional languages are closely related to λ-calculus based
  semantic languages.

  . . ."

2c **"Haskell is a pure functional language, because it does not."**

* What does the author mean by that?

  * Maybe he was trying to say that some imperative or effectful features don't map cleanly to the
    idea of program execution as substitution in a text rewriting system like LC?

3a **"The word purity is sometimes also used to mean what is more properly called referential
transparency."**

* Ok, I'll take your word for that. You said "sometimes". What about those other times? Is
  purity (as in purely functional) used to mean something else? If so, what?

Remarks
^^^^^^^
I think this section would be more clear if the phrase "return a value" is replaced with "reduces to
the value".


1.3 What is a function?
-----------------------

Subjects
^^^^^^^^
* function, relation, set, inputs, outputs, relationship, domain, codomain, range,
  preimage, image, surjective, bijective, injective, reflexive, symmetric, transitive,
  referential transparency, predictable, function body, return

General questions and comments
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
* What is the difference between the codomain, range, and image of a function? These ideas
  seem similar.


1.4 The structure of lambda expressions
---------------------------------------

Subjects
^^^^^^^^
* lambda terms, expression, variable, abstraction (this is what functions in LC are
  called), function, argument, input, output, head, body, parameter, name binding,
  application, anonymous function, alpha equivalence


1.5 Beta reduction
------------------

Subjects
^^^^^^^^
* application, substitution, head elimination, beta reduction, director string, identity
  function, non-capturing substitution [x := (\y.y)], function execution instance,
  associativity, left associative, grouping, free variable, bound variable, reducable
  expression, or redex, reduct


1.6 Multiple arguments
----------------------

Subjects
^^^^^^^^
* nested heads, currying, term, reducible expression, irreducible expression


1.7 Evaluation is simplification
--------------------------------

Subjects
^^^^^^^^
* normal form, beta normal form, fully evaluated expression, saturated function (all
  arguments applied), application vs simplification

Questions
^^^^^^^^^
1a **There are multiple normal forms in lambda calculus, but when we refer to normal form here, we
mean beta normal form.**

* Wait; This is the first sentence, and you haven't defined normal form. What is a normal form?

  From "Term Rewriting and All That" by Franz Baader and Tobias Nipkow,

  Chapter 1: Motivating Examples

  "**Termination: Is it always the case that after finitely many rule applications we reach
  an expression to which no more rules apply? Such an expression is then called a normal form.**

  . . .

  Confluence: If there are different ways of applying rules to a given term £, leading to
  different derived terms t\ and £2, can t\ and £2 be joined, i.e. can we always find a
  common term s that can be reached both from t\ and from £2 by rule application?

  . . .

  More generally, one can ask whether this is always possible, i.e. can we always make a
  non-confluent system confluent by adding implied rules (completion of term rewriting systems)."

  Chapter 2: Abstract Reduction systems

  "The term "reduction" has been chosen because in many applications something [Ed; such as the
  number of possible operations] decreases with each reduction step, but cannot decrease forever."

* What are the other normal forms?

1b **"Beta normal form is when you cannot beta reduce (apply lambdas to arguments) tht terms any
further."**

1.8 combinators
---------------

Subjects
^^^^^^^^
* combinator

Questions
^^^^^^^^^
* Are functions with no body, like ``(λxy.)`` also combinators?


1.9 Divergence
--------------

Subjects
^^^^^^^^
* divergence, non-termination, termination, convergence, meaningful result, or answer
