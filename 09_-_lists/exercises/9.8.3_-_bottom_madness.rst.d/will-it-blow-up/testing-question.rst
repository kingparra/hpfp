**TL;DR** Check out http://hspec.github.io/expectations.html
and Control-f for "Expecting exceptions from pure code"

**justsomeguy** | I have this expression one = [x^y | x
<- [1..5], y <- [2, undefined]]. Evaluating it results
in an exception once it tries to create the second
element of the resulting list
[(1^2),(2^undefined)***exception. I want to write a
test using Hspec to prove that it throws this
particular exception. Can you point me in the right
direction to start solving this? I don't know where
exception handling stuff lives in Haskell.

**geekosaur** | Control.Exception but I would assume
Hspec has its own ways to prove something throws a
given exception

**Cale** | justsomeguy: Exceptions thrown by evaluation
of pure code (as opposed to by execution of IO) can
also be quite tricky to catch in general, since you
have to make sure that evaluation occurs inside the
catch.

**c_wraith** | in this particular case, you also need
to ensure the exception doesn't happen someplace else.

**segfaultfizzbuzz** | Cale: Ya'll need to adopt rust's
error handling techniques, except to be more
opinionated about it

**c_wraith** | so you can't just evaluate ``one !! 1``
and call yourself done. You need to pattern match much
more carefully

**justsomeguy** | Hspec has shouldThrow (
http://hspec.github.io/expectations.html ). I get some
type errors when I try to use it. I'll write an example
a link to it.

**monochrom** | In this case, "seq one ()" doesn't throw.

**Cale** | segfaultfizzbuzz: Well, this is a natural
consequence of having lazy evaluation. Really,
exceptions thrown from evalation probably ought not to
be caught, but it is sometimes nice to have as a last
resort.

**monochrom** | You will need many ingredients. One
ingredient is Control.DeepSeq for full evaluation.

**c_wraith** | you don't want DeepSeq here. You want to
be sure the bottom is in exactly the right place, not
just somewhere

**monochrom** | Another ingredient is
Control.Exception's evaluate to go back to IO to make
sure evaluation is triggered, and right at the moment
you want. And then undefined is a call to error, and
you find out which exception it corresponds to.

**c_wraith** | looks like hspec provides anyErrorCall
to handle that part

**Cale** | justsomeguy: You'll probably want to use
Control.Exception.evaluate. So as to turn your
expression into an IO action which evaluates the
expression and causes the exception to occur. Note
however, that since your expression is a list, it can
technically be evaluated without causing the exception,
so you may wish to also use Control.DeepSeq.rnf on it
to insist on evaluating everything deeply.
Control.Exception.evaluate will only evaluate something
as far as determining what its outermost data
constructor is, so in your case, it will probably
succeed in getting the thing as far as (1^2) : _

**monochrom** |
  _ : _

**monochrom** |
  % evaluate (undefined : undefined) >> putStrLn "I'm OK"

**yahb** | monochrom: I'm OK

**justsomeguy** | Alright, now I have a place to start
looking :^). I'll peruse the hackage docs
Control.Exception, deepseq's Control.DeepSeq, and
hspec's Test.Hspec for now.
