*********************
 Chapter 8: Overkill
*********************


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
parameter name, and then calls that parameter with new arguments in the recursive case.

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


.. topic:: Asking about it on IRC

   ::

    justsomeguy | Haskellbook says "Haskell has native recursion based on the same principle as the Y combinator". (Source here:
                | https://gist.github.com/kingparra/a0600fe64999c391c320058aa0072125) What does that mean?
    justsomeguy | In that gist, I added a comment with my stab at an explanation of that quote, but I feel like I'm missing something.
      monochrom | I think it is either false or requires a very postmodern definition of "same principles".
      monochrom | So let me start from the most anal and see if I can progress (regress?) to the most lax.
      monochrom | The Y combinator is untypable in Haskell. So it is irrelevant right there.
    justsomeguy | There are a lot of quotes like that in this book ... I think the intention is to relate how the evaluation strategy of LC
                | is a useful mental model for how Haskell evaluates, but sometimes it's frustrating because it seems like some of these are
                | factually incorrect.
      monochrom | But you can work around by introducing a few newtype wrappings and unwrappings. It is not too bad. Then you can have an
                | edited version of the Y combinator in Haskell.
    justsomeguy | I did find a definition of Y in haskell, but it requires unsafeCoerce: y = \f -> (\x -> f (x' x)) (\x -> f (x' x))
    justsomeguy |     where x' = unsafeCoerce x
    justsomeguy | y
     tomsmeding | I mean, the primary characteristic of the Y combinator is that Y f = f (Y f); and such a function does indeed exist: it's
                | Data.Function.fix, defined as, you guessed it, fix f = f (fix f)
      monochrom | But then, this still doesn't mean that Haskell's native recursion is actually defined in terms of that.
    justsomeguy | Right, that what I'm wondering about. ...
      monochrom | OK right, the next laxation is "we just mean that the fixed-point equation is solvable, and Y is one way to solve that
                | equation".
     tomsmeding | you need the weird structure of the standard Y combinator (which is untypeable in Haskell) because you don't have direct
                | recursion in the standard lambda calculs
      monochrom | But Y is by far not the only solution, not even in the untyped lambda calculus in the book's chapter 1.
      geekosaur | I have this feeling it's really talking about letrec
      geekosaur | but that's an odd way of putting it
     tomsmeding | geekosaur: yeah, then it's doing the reader a disservice by saying that it's the same principle as the Y combinator
      monochrom | This is a pretty broad disease. People cite Y as though it is the only solution. NO. There is an honest difference between
                | "fixed point combinator" and Y.
     tomsmeding | it's not, though it can be used to the same end
      monochrom | In the same sense as "white horse is not horse" (white horses don't stand for all horses), Y is not fixed-point combinator.
     tomsmeding | monochrom: are there any in the untyped lambda calculus that are typeable in Haskell?
      monochrom | No. They all rely on \x -> x x
     tomsmeding | makes sense
      monochrom | Alternatively, if you delete recursive bindings and recursive data type definitions from Haskell, you end up with something
                | less powerful than System F, and System F doesn't have \x -> x x, we know this because every program in System F
                | terminates.
     tomsmeding | (are there any in the simply-typed lambda calculus? No, because STLC is total)
     tomsmeding | (or, perhaps, _therefore_ STLC is total)
      monochrom | Yeah, like that kind of arguments.
     tomsmeding | (not sure you can sensibly put a causality relation there)
      monochrom | The most lax level is to first acknowledge that Haskell has syntactic recursion, which is by far totally not the point of
                | any fixed-point combinator (which spares you from syntactic recursion).
      monochrom | And then the semantics of Haskell goes on to map syntactic recursion to the use of a fixed-point combinator (and we don't
                | care which). As alluded in the Haskell Report.
      monochrom | So yeah, one point down for HFFP.
    [justsomeguy(Ziw)]
     monochrom | It uses a good pedogogical strategy, but it gets a couple of facts wrong.
      monochrom | Although, hiding behind the façade of "same" "principle" you can make any claim you like.
      monochrom | http://www.vex.net/~trebla/humour/tautologies.html #0
    justsomeguy | There are a few mroe like this. From ch1 "Functional programming languages are all based on the lambda calculus.".
                | Apparently the original lisp is based on McCarthys thesis.
    justsomeguy | *more
    justsomeguy | Thank you for clearing that up, monochrom
      monochrom | Oh, that one I have no objection.
      monochrom | Lisp's primary concern was cons cell. FP is only its secondary concern.
     tomsmeding | though it's fairly easy to be "based on" the lambda calculus :p
        Rembane | Maybe it's harder to not be based on the lambda calculus?
     tomsmeding | C isn't in any reasonable way, I guess
    justsomeguy | So your position is that Lisp isn't a functional lanauge, then? (I would say it isn't purely functional, but it's still
                | functional.)
      monochrom | I would pin Backus language "FP" as the 1st functional programming language. And it uses so many ideas from lambda calculus
                | that I would not object to "based on that".
           oats | you can write some really imperative code in some lisps
      monochrom | But if you don't accept that, I have a weaker stance.
           oats | lisp-family languages tend to be more expression-oriented, but idk if that can qualify it as functional
     tomsmeding | I mean, to be "based on" the lambda calculus, you need variables (only basically removes assembly and forth-likes from the
                | list of candidates), function application (same), and inline functions (removes a couple more, but leaves almost any
                | language that is still receiving updates today)
      geekosaur | you can write some really imperative code in haskell
           oats | new rule, lambda calculus is the only functional language :P
        Rembane | tomsmeding: So having a language that with some effort can be turned into lambda calculus doesn't count? :)
     tomsmeding | geekosaur: I wonder if our students, who are learning Haskell as a second language after an imperative one, would find that
                | a consolation :p
      monochrom | Landin taught us to explain programming languages by a lambda calculus on steroid. ("The Next 700 Programming Languages.")
                | So a revisionist would say that FPLs are based on that, retrospectively.
     tomsmeding | Rembane: such as?
      monochrom | But Landin in that paper used lambda calculus to explain Algol, not very functional. So there. >:)
    [justsomeguy(Ziw)]
      geekosaur | you can write some really imperative code in haskell
           oats | new rule, lambda calculus is the only functional language :P
        Rembane | tomsmeding: So having a language that with some effort can be turned into lambda calculus doesn't count? :)
     tomsmeding | geekosaur: I wonder if our students, who are learning Haskell as a second language after an imperative one, would find that
                | a consolation :p
      monochrom | Landin taught us to explain programming languages by a lambda calculus on steroid. ("The Next 700 Programming Languages.")
                | So a revisionist would say that FPLs are based on that, retrospectively.
     tomsmeding | Rembane: such as?
      monochrom | But Landin in that paper used lambda calculus to explain Algol, not very functional. So there. >:)
     tomsmeding | lol
      monochrom | Basically the paper covers everything except the Prolog camp...
        Rembane | tomsmeding: What monochrom said about Landin, Algol and lambda calculus. That should mean that C can be turned into lambda
                | calculus too. Or explained by it.

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
