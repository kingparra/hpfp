********************
 Chapter 1 Overkill
********************


1.1 All you need is lambda
--------------------------

Subjects
^^^^^^^^
* lambda calculus, model of computation, mathematical model, Alonzo Church, (logical) calculus,
  method, process, formalization, formalism, Turing machine, effective computability, problem,
  solvable problems, unsolvable problems, classes of problems, decidability, decision problem

Claims
^^^^^^
1a **This chapter provides a very brief introduction to the lambda calculus, a model of computation
devised in the 1930s by Alonzo Church.**

* Very brief? How brief is that? What are some things that you aren't covering?

  * Pretty much any useful function definitions are not covered. Arithmetic, boolean operations,
    representation of basic data structures like pairs or lists, and recursion using the Y
    combinator aren't covered. Neither are different evaluation strategies. Neither are explicit
    substitution methods, for representing the process of beta-reduction precisely. Measuring the
    complexity of lambda expressions is also not mentioned. But that's OK, we only need the basics
    for understanding Haskell.

* What is a model of computation?

  * A model of computation is a model which describes how an output of a mathematical
    function is computed given an input. A model describes how units of computations,
    memories, and communications are organized. The computational complexity of an
    algorithm can be measured given a model of computation. Using a model allows
    studying the performance of algorithms independently of the variations that are
    specific to particular implementations and specific technology.

* What is a (mathematical) model?

  * from https://www.wikiwand.com/en/Mathematical_model

    A mathematical model is a description of a system using mathematical concepts and language. ...
    A model may help to explain a system and to study the effects of different components, and to
    make predictions about behavior.

* How does someone devise a model of computation?
* **What problem, idea, or event motivated Alonzo Church to create the lambda calculus?**

  This is a big subject that I can't hope to answer satisfactorily without a lot of reading, but
  here's my understanding of the issue right now.

  Since the late 1600s, mathematicians have been investigating the possibility that all of
  mathematics could be reduced to formal logic, which may then be calculated by machine. Leibniz
  and George Boole envisaged automating much of human thought, or at least the subset of thought
  that is based on logical reasoning.

  How can we determine which calculations can or cannot be computed? From the view of mathematics,
  when considering a particular function, the question may be rephrased to "can the mathematical
  function undergoing computation eventually result in an answer?", or "is the function effectively
  calculable", or in the case of logical statements (as in mathematical logic) "can the truth value
  eventually be decided?".

  This last question is known as the decision problem, or Entscheidungsproblem, and this is the
  problem that Church tries to make precise with LC. Church was interested in a few particular
  instances of the problem, articulated by David Hilbert -- Hilberts 10th problem, and Hilberts
  Program. He tries to address these examples, and assumes the reader will generalize his
  conclusions to the larger issue of what can be computed mechanically.

  Here is Church's account of the Entscheidungsproblem:

    "By the Entscheidungsproblem of a system of symbolic logic is here understood the problem to
    find an effective method by which, given any expression Q in the notation of the system, it
    can be determined whether or not Q is provable in the system." ~ Church 1936

  If you want to read more about the subject, I suggest "**Engines of Logic**" by Martin Davis and
  "**The Annotated Turing**" by Charles Petzold. Engines presents a timeline of several important
  thinkers, leading up to the idea of the decision problem, and the Church-Turing thesis. The
  Annotated Turing focuses on the Alan Turing's paper that describes his model of computation, the
  logical computing machine. Another interesting resource is "**Lambda Calculus: Then and Now**", a
  lecture by Dana Scott available through publications of the ACM. It links to a timeline of
  important academic papers leading up to the lambda calculus, and some of the lambda calculus'
  applications in programming language design.

  .. https://www.csc.kth.se/utbildning/kth/kurser/DD3001/drtmp12/DownloadMaterial/Lectures/Lecture1.pdf
  .. https://www.csc.kth.se/utbildning/kth/kurser/DD3001/drtmp12/DownloadMaterial/Lectures/Lecture3.pdf
  .. https://www.csc.kth.se/utbildning/kth/kurser/DD3001/drtmp12/DownloadMaterial/Lectures/Lecture4.pdf

  .. Leibniz, jul 1646 to nov 1716,
  ..
  .. * Belived human reasoning could be reduced to calculations. envisaged a calculus rationcinator
  ..   (resembing symbolic logic) to make such calculations feasible.

  .. Lebiniz's wonderful idea:
  .. * Seek an alphabet whose elements represent concepts,
  .. * this alphabet would form a language,
  .. * in this language by symbolic reasoning determine
  ..
  ..   * which sentences in the language were true and,
  ..   * what logical relationships existed among them.
  ..
  .. Leibniz held onto this vision throughtout his lifetime and made some progress towards it.

  .. hilberts 10th problem, hilberts program
  .. hilberts program: independence, consistency, completeness, decidability

  .. https://www.jstor.org/stable/2318447?origin=JSTOR-pdf "Hilbert's Tenth Problem is Unsolvable" by Martin Davis

* **When particularly was the lambda calculus introduced?
  In which papers, conferences, or historical events was LC introduced?**


  from Lambda Calculus: Then & Now ... http://fm.csl.sri.com/SSFT15/LambdaThenNow.pdf

  Alonzo Church, “An Unsolvable Problem in Elementary Number Theory,” American J. of Mathematics,
  vol. 5 (1936), pp. 345-363.

  Alonzo Church, “A Note on the Entscheidungsproblem,” J. of Symbolic Logic, vol. 1 (1936) pp.
  40-41. Correction: ibid, pp. 101-102.

  Alan Turing, “On Computable Numbers with an Application to the Entscheidungsproblem,” Proc. of the
  London Math. Soc., vol. 42 (1936), pp.  230-267. Correction: vol. 43 (1937), pp. 544-546.

  Alan Turing, “Computability and λ-definability,” J. Symbolic Logic, vol. 2 (1937), pp. 153-163.

  from https://math.stackexchange.com/questions/1165686/what-is-the-actual-significance-of-the-lambda-calculus-for-the-formalization-of

    * 1932 A. Church, "A set of postulates for the foundation of logic", Annals of Mathematics,
      Series 2, 33:346–366

      Church introduces the lambda calculus as part of his investigation into the foundations of
      mathematics.

    * 1934 Curry, Haskell B. "Functionality in combinatory logic." Proceedings of the National
      Academy of Sciences 20.11 (1934): 584-590

      Curry observes that types of the combinators could be seen as axiom-schemas for an
      intuitionistic implicational logic.

    * 1935 Kleene, S. C. & Rosser, J. B. "The inconsistency of certain formal logics". Annals of
      Mathematics 36 (3): 630–636

      The Kleene-Rosser paradox is established, showing the inconsistency of Curry's combinatory
      logic and Church's original lambda calculus.

    * 1936 A. Church, "An unsolvable problem of elementary number theory", American Journal of
      Mathematics, Volume 58, No. 2. (April 1936), pp. 345-363

      In response to Kleene and Rosser, Church introduces what would later be called the untyped
      lambda calculus. He did this by isolating the relevant portions of the original lambda
      calculus that pertained solely to computation.

    * 1940 Church, A. "A Formulation of the Simple Theory of Types". Journal of Symbolic Logic 5:
      1940.

      Church introduces the simply typed lambda calculus.

    * 1958 Curry, H. B., Feys, R., Craig, W., & Craig, W. (1958). Combinatory logic, vol. 1.
      North-Holland Publ.

      Curry, et al, observes a close correspondence between axioms of positive implicational
      propositional logic and "basic combinators".

    * 1969 Howard, W., 1980 [1969], “The formulae-as-types notion of construction,” in J. Seldin and
      J. Hindley (eds.), To H. B. Curry: Essays on Combinatory Logic, Lambda Calculus and Formalism,
      London, New York: Academic Press, pp. 480–490

      The Curry-Howard correspondence is circulated as notes but would not be officially published
      until 1980. It was based on the "formulas-as-types" or "propositions-as-sets" principle and
      linked with Church's simply typed lambda calculus.

      Howard did so by taking the untyped lambda calculus and creating what could be interpreted as a
      variant of the simply typed lambda calculus (in § "Type symbols, terms and constructors) that
      could be more readily expressed with the concepts he was explaining at the time.

      This correspondence would make intuitionistic natural deduction part of computer science proper
      [1], and would be instrumental to further developments in type theory.

    * 1972 Martin-Löf, P. "An intuitionistic theory of types." Omtryckt i (Sambin och Smith 1998)

      This is abridged, as there was a prior formalization in 1971 called "A theory of types" that was
      shown to be inconsistent as demonstrated by Girard's paradox, and his later refinements became
      predicative, along with adding many other seminal contributions to type theory. There would also
      be intensional and extensional variants.  The historical context is that it was based on an
      isomorphism between propositions and types, which is associated with the Curry-Howard
      correspondence, in which Howard directly mentions Martin-Löf during his communications. Thus,
      this links intuitionistic type theory to the simply typed lambda calculus, or, at the very
      minimum, to the entire family of the lambda calculi.  (2009-) Voevodsky, Vladimir. "Notes on
      type systems." Unpublished notes, (www.math.ias.edu/~ vladimir/Site3/Univalent_Foundations) HTML

      Voevodsky introduces the starting point of the homotopy type theory and the univalent
      foundations. All of this was based on Voevodsky's investigations into the foundations of
      mathematics, just as it was with Church, and those before him. Church's simply typed lambda
      calculus has played a not insignificant role in the history of these developments, despite its
      seeming invisibility in the most modern incarnation of these theories.

      Later refinements came in "The Simplicial Model of Univalent Foundations" (2012) and more can
      be read in the Homotopy Type Theory book. An overview was published in Quanta magazine that is
      highly approachable.

1b **"A calculus is a method of calculation or reasoning; the lambda calculus is one process for
formalizing a method."**

* There *is* a definition of what a calculus is here, but I feel that it is too terse. I'm just
  not satisfied with it. What is a calculus, really?

  Logical calculus https://encyclopediaofmath.org/wiki/Logical_calculus

  Logico-mathematical calculus https://encyclopediaofmath.org/wiki/Logico-mathematical_calculus

  from a slide I found here https://www.cs.cmu.edu/~venkatg/teaching/15252-sp21/index.html

  "Calculus = just a bunch of rules for manipulating symbols.
  One can give meaning to the symbols (semantics), but that's not part of the calculus (pure syntax).
  One can associate meanings to expressions in a way that corresponds to computations (functional programs)."

  The authors say something similar later on. From 1.6 Multiple arguments, paragraph 8, sentence c.

  1.6 8c **"The lambda calculus is a process or method, like a game with a few simple rules for
  transforming lambdas but no specific meaning."**

  * Does calling LC a "method of calculation or reasoning" make sense, given that it is purely
    syntactic? Calculation and reasoning require ascribing semantics to our symbol manipulation scheme.

* What is a process?
* What is a method?
* Are the **"method of calculuation or reasoning"** and the **"method"** that is to be formalized by
  LC different methods?
* Is LC a method for formalizing a method?
* Is the word "method" here being used in the specialized sense that it appears as within
  discussions of the Church-Turing thesis, as in, an effective (mechanically calculable) method?

    https://plato.stanford.edu/entries/church-turing/

    "1. The Thesis and its History

    The Church-Turing thesis concerns the concept of an effective or systematic or mechanical method
    in logic, mathematics and computer science. "Effective" and its synonyms "systematic" and
    "mechanical" are terms of art in these disciplines: they do not carry their everyday meaning.

    A method, or procedure, M, for achieving some desired result is called ‘effective’ (or
    ‘systematic’ or ‘mechanical’) just in case:

    * M is set out in terms of a finite number of exact instructions (each instruction being
      expressed by means of a finite number of symbols);

    * M will, if carried out without error, produce the desired result in a finite number of steps;

    * M can (in practice or in principle) be carried out by a human being unaided by any machinery
      except paper and pencil;

    * M demands no insight, intuition, or ingenuity, on the part of the human being carrying out the
      method.

    . . .

    One of Alan Turing's achievements, in his famous paper of 1936, was to present a formally exact
    predicate with which the informal predicate "can be done by means of an effective method" may be
    replaced (Turing 1936). Alonzo Church, working independently, did the same (Church 1936a).

    . . .

    As explained by Turing (1936: 84), Hilbert's Entscheidungsproblem is this: Is there a general
    (effective) process for determining whether a given formula A of the first-order propositional
    calculus is provable?

    . . .

    Church's thesis: A function of positive integers is effectively calculable only if
    lambda-definable (or, equivalently, recursive)."

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

  Also, `here is the definition of **formalization method** from the Encylopedia of Mathematics
  <https://encyclopediaofmath.org/wiki/Formalization_method>`_,

  **Formalization method**

  A way of expressing by a formal system a mathematical theory. It is one of the main methods in
  proof theory.

  An application of the formalization method involves carrying out the following stages.

  * Putting the original mathematical theory into symbols.  In this all the propositions of the
    theory are written in a suitable logico-mathematical language L.

  * The deductive analysis of the theory and the choice of axioms, that is, of a collection of
    propositions of the theory from which all other propositions of the theory can be logically
    derived.

  * Adding the axioms in their symbolic notation to a suitable logical calculus based on L.

  The system obtained by this formalization is now itself the object of precise mathematical study
  (see Axiomatic method; Proof theory).

  References: [1] S.C. Kleene, "Introduction to
  metamathematics", North-Holland (1951)

* Are there other processes for formalizing a method?
* What is a formalism?
* What problem or turn of events motivated the creation of lambda calculus?
* Which papers and events were the lambda calculus introduced by?

1c **"Like Turing machines, the lambda calculus formalizes the concept of effective computability,
thus determining which problems, or classes or problems, can be solved."**

* What is effective computability?
* Is effective computability a set of criteria for which problems can be solved mechanically?
* How does formalizing the concept of effective computability determine which problems can be solved?
* How does LC formalize the concept of effective computability?
* What is a class of problems? What are these classes categorized by? Complexity? Problem area?

2 **You may be wondering where the Haskell is. You may be contemplating skipping this chapter. You
may feel tempted to leap ahead to the fun stuff where we build a project.**

* Why do you think I would be contemplating skipping this chapter?

4a **"We're starting from first principles here, so that when we get around to building
projects, you know what you're doing."**

* How will learning lambda calculus help me build projects?

  * It won't. But LC will be useful for several other things:

    * Communicating with other Haskellers in public forums and chatrooms.
    * Being able to read papers from various functional programming conferences. This is important
      since Haskell language extensions (using GHC LANGUAGE pragmas) are often introduced and
      prototyped at conferences first, before gaining traction and getting a more robust
      implementation. Reading those papers is a quick way to get oriented on an extension because it
      explains the core idea in a self-contained way.
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
        too, in order to make it easier to read, or easier to modify in different ways.
      * You can begin to think of a program as a graph. In this graph, each node is a
        function execution instance, represented as an equation. Within each equation,
        names don't change meaning. Connections between nodes represent arguments, which act as
        inputs values bound to parameters names of each nodes execution instance.

    * Reading type signatures, and deducing how different type signatures may be combined.

    * Someone else asked this same question --
      https://teddit.net/r/haskell/comments/69wcm3/haskell_programming_from_first_principles_why_do/

      Here is the top comment:

        Blackheart

        63 points, 4 years ago

        There are many reasons why lambda-calculus is important.

        Untyped lambda-calculus (ULC), along with Turing machines, combinatorial logic, partial
        recursive functions and type-0 grammars, is one of the foundational models of computation,
        so we know that if ULC can be translated into a programming language then that language
        can express any computation.

        Compared with partial recursive functions, ULC is syntactic and easily axiomatized, so
        it's easy to list all the rules. You don't need a background in recursion theory or domain
        theory to grasp the definition.

        Compared with the other models, LC is notationally simple. To write down a program, you
        just need to write out a term; you don't need to define a machine or tape symbols; you
        don't need a separate disembodied list of definitions; scoping is extremely clear.
        Compared to combinatorial logic, it's more human-readable. To transform a program or show
        two programs are "the same", you can use essentially the same methods that you learned in
        high school to manipulate algebraic expressions. You can execute a program by hand.

        LC has both equational and rewriting models. An equational model says when two programs
        give the same result for the same inputs, but ignores the space/time complexity. Rewriting
        models are similar, except they also note the steps, so you can reason about complexity.
        In LC, the relationship between these two is usually pretty simple, so it's easy to start
        thinking about a problem in terms of correctness and then, later, once you've convinced
        yourself of that, think about rewrites and efficiency. This promotes separation of
        concerns.

        It's fairly easy to add types to ULC, and to compare the typed and untyped versions. When
        you add types in the most obvious way, types correspond to logical propositions and typed
        terms correspond to proofs of those propositions, so you get an additional way of thinking
        about programs, and writing total, correct programs becomes an exercise in proving theorems
        in constructive logic.

        These types "coordinatize" the space of computations so we can think about it in parts
        (e.g., sums, products) and not just as a big ball of mud.  LC is pretty amenable to
        extension with features we see in other programming languages, such as I/O, mutation and
        concurrency.

        There is a huge body of literature about lambda-calculi, so it's easy to benefit from the
        work of other people. LC is a lingua franca. It's conventions are well-established; it's
        concise; conceptually, it's robust enough to accommodate many sorts of extensions.

        You mentioned unnecessary jargon and complexity. Of course, I don't know specifically what you're
        referring to (and I haven't read the book you mention), but chances are it's probably not
        unnecessary. Because LC is concise, treatments of it can afford to give you the whole story.

        Most programming language definitions sweep a lot of things under the rug and/or punt it to
        a vague, assumed understanding of a von Neumann architecture. Practically none give you a
        complete, unambiguous list of ALL the rules which say how two programs are related.

        Think about the power of this as a tool. In pure ULC, you can prove that two programs do exactly the
        same thing on all inputs with 100% confidence, and it doesn't involve any testing or assumptions
        about the implementation or architecture.

  * When you say "know what you're doing", what do you imagine that I will be doing? What are the
    things I need to do, in order to build projects, that LC will help me to know?

4c **"Lambda calculus is your foundation, because Haskell is a lambda calculus."**

* Is that really true? In what sense is Haskell a lambda calculus?

  * Haskell's regular language syntax reduces to a subset of the language called the language kernel.
  * The language kernel then is reduced to the core type, which is an implementation of a typed
    lambda calculus called system fc. https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/compiler/fc
  * This eventually turns into a build artifact you can run on your computer.
  * Furthermore, the evaluation strategy used by Haskell resembles the lambda calculus.

Remarks
^^^^^^^
The quote at the beginning of the chapter talks about great mathematicians. Then the first paragraph
name drops several concepts related to computability that someone without exposure to cs would be
completely unaware of. This makes me wonder: who is the real target audience, here? Beginner
programmers, people who've never written a single line of code, don't know this stuff, and would
probably be put off by even mentioning it. Who then is the intended reader, and what things must
they know beforehand? Maybe it's CS dropouts like me?

Why is there no description of how learning lambda calculus will benefit your ability to write
Haskell code?

Where are the learning objectives?

*What* are the learning objectives?

What are the expected outcomes of completing the chapter?

What are the abilities you'll gain by completing the chapter that you did not have before?

I get that your asking me to trust you, but I think you've missed an opportunity to make your
writing more compelling by explaining the relevance of LC to writing Haskell.


1.2 What is functional programming?
-----------------------------------

Subjects
^^^^^^^^
* functional programming, programming paradigm, mathematical functions, expression,
  values, variables, functions, argument, input, application (of a function to its
  arguments), reduction, evaluation, first-class, argument passing, lambda expression,
  purity, referential transparency, abstraction, composability, (re)factoring, generic code

Claims
^^^^^^
1a **"Functional programming is a computer programming paradigm that relies on functions modeled on
mathematical functions."**

* What is a mathematical function?
* What is a programming paradigm?
* Why does it mean for functions in a PL to be modelled on mathematical functions?
* Do other programming languages not use functions that behave like mathematical functions?

2a **"Functional programming languages are all based on the lambda calculus."**

* What does it mean for a language to be based on LC?
* What about languages based on other calculi that allow equational reasoning, like closure
  calculus, or SKI combinator calculus? Are those not functional languages, too?
* LISP is one of the first functional languages, but it was not initially based on lambda calculus,
  but on a formalism that McCarthy developed, instead.

  "The recursive functions mentioned in McCarthy's seminal paper, Recursive functions of Symbolic
  Expressions and Their Computation by Machine, Part I refer to the class of functions studied in
  computability theory."

  . . .

  "… one of the myths concerning LISP that people think up or invent for themselves becomes
  apparent, and that is that LISP is somehow a realization of the lambda calculus, or that
  was the intention. The truth is that I didn't understand the lambda calculus, really."
  ~ John McCarthy, Lisp session, History of Programming Languages

Source here: https://dl.acm.org/doi/book/10.1145/800025#sec4

See the discussion here and linked article for details: https://news.ycombinator.com/item?id=20696931

  vga805 on Aug 14, 2019 [–]

  . . .

  So there are a two issues here,

    1) whether or not it was McCarthy's intention to realize the Lambda Calculus in LISP, and
    2) whether or not LISP is such a realization. Or at least some kind of close realization.

  The answer to 1 is clearly no. This doesn't imply an answer to 2 one way or another.

  If 2 isn't true, what explains the widespread belief? Is it really just that he, McCarthy,
  borrowed some notation?


  vilhelm_s on Aug 14, 2019 [–]

  Modern lisps do realize the lambda calculus, but this was not immediate. In particular, in order to
  exactly match the lambda-calculus beta-reduction rule, you need to use lexical rather than dynamic
  scope, which did not really become popular until Scheme in the 1970s.


1b **"The essence of functional programming is that programs are a combination of expressions."**

  .. etymology online
     expression (noun)

     early 15c., expressioun, "action of pressing out;" later "action of manifesting a feeling;" "a
     putting into words" (mid-15c.); from Late Latin expressionem (nominative expressio) "expression,
     vividness," in classical Latin "a pressing out, a projection," noun of action from past-participle
     stem of exprimere "represent, describe," literally "press out" (see express (v.)). Meaning "an
     action or creation that expresses feelings" is from 1620s. Of the face, from 1774. Occasionally the
     word also was used literally, for "the action of squeezing out." Related: Expressional.

     Merriam-Webster
     exprssion (noun)

     Definition of expression
     1a: an act, process, or instance of representing in a medium (such as words) : UTTERANCE
     b(1): something that manifests, embodies, or symbolizes something else this gift is an
           expression of my admiration for you
      (2): a significant word or phrase
      (3): a mathematical or logical symbol or a meaningful combination of symbols
      (4): the detectable effect of a gene
           also : EXPRESSIVITY sense 1
     2a: a mode, means, or use of significant representation or symbolism
         especially : felicitous or vivid indication or depiction of mood or sentiment
     b(1): the quality or fact of being expressive
      (2): facial aspect or vocal intonation as indicative of feeling
     3: an act or product of pressing out

     Synonyms: articulation, formulation, phrasing, statement, utterance, berbalism, voice, wording.

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
* Is there a formal definition of what referential transparency means?

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
  **referential transparency**, predictable, function body, return

General questions and comments
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
* What is the difference between the codomain, range, and image of a function? These ideas
  seem similar, and the distinctions between them are sometimes only evident if the function is
  partial, or has incomputable elements in the domain.
* Referential transparency is probably one of the most important ideas in functional programming, so
  I want to be able to explain it well. I should experiment with different ways of explaining the idea.

.. A name is referentially transparent if can be replaced by its definition at any point in the
.. program without changing its behaviour. So, referentially transparent names are indepent of
.. time ordering and don't have side-effects. They also don't mutate values outside of their
.. scope.

.. One definition of functional programming: No assignment statements, and no mutable objects. All data flow is explicit.
.. "proof of non-intereference"

.. FUNDAMENTAL CONCEPTS IN PROGRAMMING LANGUAGES page 19
.. 3.2.  Expressions and evaluation
.. 3.2.1. Values.
.. . . .
.. One of the most useful properties of expressions is that called by Quine referential
.. transparency. In essence this means that if we wish to find the value of an expression which
.. contains a sub-expression, the only thing we need to know about the sub-expression is its
.. value.
..
.. Also cites Quine 1 on this matter.
..
.. We tend to assume automatically that the symbol x in an expression such as 3x**2 + 2x + 17 stands
.. for the same thing (or has the same value) on each occasion it occurs. This is the most important
.. consequence of referential transparency
..
.. 3.3.1 Variables, page 22
..
.. If we consider L-values as well as R-Values, however, we can preserve referential transparency as
.. far as L-values are concerned. Thi is because L-values, being generalised addresses, are not
.. altered by assignment command
..
.. 3.3.1 Variables, page 22



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
