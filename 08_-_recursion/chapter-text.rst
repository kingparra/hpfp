**********************
 Chapter 8: Recursion
**********************

Imagine a portion of the territory of England has been
perfectly levelled, and a cartographer traces a map of
England. The work is perfect. There is no particular of
the territory of England, small as it can be, that has
not been recorded in the map. Everything has its own
correspondence. The map, then, must contain a map of
the map, that must contain a map of the map of the map,
and so on to infinity.

Jorge Luis Borges, citing Josiah Royce

275

CHAPTER 8. FUNCTIONS THAT CALL THEMSELVES 276

8.1 Recursion
-------------
Recursion is defining a function in terms of itself via self-referential
expressions. It means that the function will continue to call itself
and repeat its behavior until some condition is met to return a result.
It's an important concept in Haskell, and in mathematics, because it
gives us a means of expressing indefinite or incremental computation,
without forcing us to explicitly repeat ourselves, and allows the data
we are processing to decide when we are done computing.

Recursion is a natural property of many logical and mathematical
systems, including human language. That there is no limit on the
number of expressible, valid sentences in human language is due to
recursion. A sentence in English can have another sentence nested
within it. Sentences can be roughly described as structures that have
a noun phrase, a verb phrase, and optionally another sentence. This
possibility for unlimited, nested sentences is recursive and enables
the limitless expressibility therein. Recursion is a means of expressing
code that must take an indefinite number of steps to return a result.

But the lambda calculus does not appear on the surface to have
any means of recursion, because of the anonymity of expressions.
How do you call something without a name? Being able to write
recursive functions, though, is essential to Turing completeness.
We use a combinator—known as the Y combinator or fixed-point
combinator—to write recursive functions in the lambda calculus.
Haskell has native recursion based on the same principle as the Y
combinator.

It is important to have a solid understanding of the behavior of
recursive functions. In later chapters, we will see that, in fact, it is
not often necessary to write our own recursive functions, as many
standard higher-order functions have built-in recursion. But without
understanding the systematic behavior of recursion itself, it can be
difficult to reason about those HOFs. In this chapter, we will:

• Explore what recursion is and how recursive functions evaluate.
• Go step-by-step through the process of writing our own recur-
sive functions.
• Have fun with bottom.

CHAPTER 8. FUNCTIONS THAT CALL THEMSELVES 277

8.2 Factorial!
--------------
One of the classic introductory functions for demonstrating recursion
in functional languages is factorial. In arithmetic, you might have
seen expressions like 4!. The bang next to the number 4 is the notation
for the factorial function.

Let's evaluate 4!:

4! = 4 * 3 * 2 * 1
        12 * 2 * 1
            24 * 1
                24
4! = 24

Next, let's do it the silly way in Haskell:

fourFactorial :: Integer
fourFactorial = 4 * 3 * 2 * 1

This will return the correct result, but it only covers one possible
result for factorial. This is less than ideal. We want to express the
general idea of the function, not encode specific inputs and outputs
manually.

Let's look at some broken code to introduce the concept of a base
case:

-- This won't work. It never stops.
brokenFact1 :: Integer -> Integer
brokenFact1 n = n * brokenFact1 (n - 1)

Let's apply this to 4 and see what happens:

brokenFact1 4 =
  4 * (4 - 1)
  * ((4 - 1) - 1)
  * (((4 - 1) - 1) - 1)
  -- this series never stops

The way we can stop a recursive expression is by having a base case
that stops the self-application to further arguments. Understanding
this is critical for writing functions that are correct and terminate
properly. Here's what this looks like for factorial:

CHAPTER 8. FUNCTIONS THAT CALL THEMSELVES 278

module Factorial where
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)
brokenFact1 4 =
  4 * (4 - 1)
  * ((4 - 1) - 1)
  * (((4 - 1) - 1) - 1)
  * ((((4 - 1) - 1) - 1) - 1)
  * (((((4 - 1) - 1) - 1) - 1) - 1)
  -- never stops

But the base case factorial 0 = 1 in the fixed version gives our
function a stopping point, so the reduction changes:

-- Changes to
-- n = n * factorial (n - 1)
factorial 4 =
  4 * factorial (4 - 1)
  -- evaluate (-) applied to 4 and 1
  4 * factorial 3
  -- evaluate factorial applied to 3
  -- expands to 3 * factorial (3 - 1)
  4 * 3 * factorial (3 - 1)
  -- beta reduce (-) applied to 3 and 1
  4 * 3 * factorial 2
  -- evaluate factorial applied to 2
  4 * 3 * 2 * factorial (2 - 1)
  -- evaluate (-) applied to 2 and 1
  4 * 3 * 2 * factorial 1
  -- evaluate factorial applied to 1
  4 * 3 * 2 * 1 * factorial (1 - 1)

CHAPTER 8. FUNCTIONS THAT CALL THEMSELVES 279

  -- evaluate (-) applied to 1 and 1
  -- we know factorial 0 = 1
  -- so we evaluate that to 1
  4 * 3 * 2 * 1 * 1
  -- And when we evaluate
  -- our multiplications
  24

Making our base case an identity value for the function
(multiplication in this case) means that applying the
function to that case doesn't change the result of previous applications.

Another way to look at recursion
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
In the last chapter, we looked at the compose operator, (.), which
is a higher-order function. Function composition is a way of tying
two (or more) functions together such that the result of applying
the first function gets passed as an argument to the next function.
This is the same thing recursive functions are doing—taking the
result of the first application of a function and passing it to the next
function—except in the case of recursive functions, the first result
gets passed back to the same function rather than a different one,
until it reaches the base case and terminates.

Where function composition as we normally think of it is static
and definite, recursive compositions are indefinite. The number of
times the function may be applied depends on the arguments to the
function, and the applications can be infinite if a stopping point is
not clearly defined.

Let's recall that function composition has the following type:

(.) :: (b -> c) -> (a -> b) -> a -> c

And when we use it like this:

take 5 . filter odd . enumFrom $ 3

We know that the first result will be a list generated by enumFrom,
which will be passed to filter odd, giving us a list of only the odd
results, and that list will be passed to take 5, and our final result will

CHAPTER 8. FUNCTIONS THAT CALL THEMSELVES 280

be the first five members of that list. Thus, results get piped through
a series of functions.

Recursion is self-referential composition.1 We apply a function to
an argument, then pass that result on as an argument to a second
application of the same function, and so on.

Now look again at how (.) is defined:

(.) :: (b -> c) -> (a -> b) -> a -> c
(.) f g = \x -> f (g x)

A programming language, such as Haskell, that is built purely
on the lambda calculus has one verb for expressing computations
that can be evaluated: apply. We apply a function to an argument.
Applying a function to an argument and potentially doing something
with the result is all we can do, no matter what syntactic conveniences
we construct to make it seem like we are doing more than that. While
we give function composition a special name and operator to point
up the pattern and make it convenient to use, it's only a way of saying:

* Given two functions, f and g, as arguments to (.),
* when we get an argument x, apply g to x,
* then apply f to the result of (g x),
* or, to rephrase in code:

  (.) f g = \x -> f (g x)

With function recursion, you might notice that it is a form of
function application in the same way that composition is a form of
function application. The difference is that instead of a fixed number
of applications, recursive functions rely on inputs to determine when
to stop applying functions to successive results. Without a specified
stopping point, the result of (g x) will keep being passed back to g
indefinitely.

Let's look at some code to see the similarity in patterns:

1 Many thanks to George Makrydakis for discussing this with us.

CHAPTER 8. FUNCTIONS THAT CALL THEMSELVES 281

inc :: Num a => a -> a
inc = (+1)

three = inc . inc . inc $ 0
-- different syntax, same thing
three' = (inc . inc . inc) 0

Our composition of inc bakes the number of applications into
the source code. We don't presently have a means of changing how
many times we want it to apply inc without writing a new function.

So, we might want to make a general function that can apply inc
an indefinite number of times and allow us to specify as an argument
how many times it should be applied:

incTimes :: (Eq a, Num a) => a -> a -> a
incTimes 0 n =
  n

incTimes times n =
  1 + (incTimes (times - 1) n)

Here, times is a variable representing the number of
times the incrementing function (not called inc here
but written as 1 + in the function body) should be
applied to the argument n. If we want to apply it 0
times, it will return our n back to us. Otherwise, the
incrementing function will be applied as many times as
we've declared:

Prelude> incTimes 10 0
10
Prelude> incTimes 5 0
5
Prelude> incTimes 5 5
10

Does this look familiar? In a function such as incTimes, the looming
threat of unending recursion is minimized, because the number of
times to apply the function is an argument to the function itself,
and we've defined a stopping point: when (times - 1) is equal to 0, it
returns n, and that's all the applications we get.

We can abstract the recursion out of incTimes, too:

CHAPTER 8. FUNCTIONS THAT CALL THEMSELVES 282

applyTimes :: (Eq a, Num a) =>
               a -> (b -> b) -> b -> b
applyTimes 0 f b = b
applyTimes n f b = f (applyTimes (n-1) f b)

incTimes' :: (Eq a, Num a) => a -> a -> a
incTimes' times n = applyTimes times (+1) n

When we do, we can make the composition more obvious in applyTimes:

applyTimes :: (Eq a, Num a) =>
               a -> (b -> b) -> b -> b
applyTimes 0 f b =
  b
applyTimes n f b =
  f . applyTimes (n-1) f $ b

We're recursively composing our function f with applyTimes (n-1)
f however many subtractions it takes to get n to 0!

Intermission: Exercise
^^^^^^^^^^^^^^^^^^^^^^
Write out the evaluation of the following. It might be a little less
noisy if you do so with the form that doesn't use the composition
operator, (.):

applyTimes 5 (+1) 5


8.3 Bottom
----------
⊥, or bottom, is a term used in Haskell to refer to computations that do
not successfully result in a value. The two main varieties of bottom
are computations that fail with an error or those that fail to terminate.
In logic, ⊥ corresponds to false. Let us examine a few ways in which
we can have bottom in our programs:

Prelude> let x = x in x
*** Exception: <<loop>>

CHAPTER 8. FUNCTIONS THAT CALL THEMSELVES 283

Here, GHCi detects that let x = x in x is never going to return a
result and short-circuits the never-ending computation.2 This is an
example of bottom, because it is never going to return a result. Note
that if you're using a Windows computer, this example may freeze
GHCi instead of throwing an exception.

Next, let's define a function that will return an exception:

f :: Bool -> Int
f True = error "blah"
f False = 0

And let's try that out in GHCi:

Prelude> f False
0
Prelude> f True
*** Exception: blah

In the first case, when we evaluate f False and get 0, that doesn't
result in a bottom value. But, when we evaluate f True, we get an
exception, which is a means of expressing that a computation has
failed. We get an exception, because we specify that this value should
return an error. But this, too, is an example of bottom.

Another example of a bottom would be a partial function. Let's
consider a rewrite of the previous function:

f :: Bool -> Int
f False = 0

This has the same type and returns the same output. What we've
done is elided the f True = error "blah" case from the function definition.
This is not a solution to the problem with the previous function,
but it will give us a different exception. We can observe this for
ourselves in GHCi:

Prelude> f :: Bool -> Int; f False = 0
Prelude> f False
0

2 In GHCi 8.6.5, evaluating this expression may cause your REPL to hang. If you
enter it without the let, you will get a parse error instead.

CHAPTER 8. FUNCTIONS THAT CALL THEMSELVES 284

Prelude> f True
*** Exception: <interactive>:1:19-29:
  Non-exhaustive patterns in function f

The error value is still there, but our language implementation is
making it the fallback case, because we didn't write a total function,
that is, a function that handles all of its inputs. Because we failed to
define ways to handle all potential inputs, for example through an
"otherwise" case, the previous function was really:

f :: Bool -> Int
f False = 0
f _ = error $ "*** Exception: "
           ++ "Non-exhaustive"
           ++ "patterns in function f"

A partial function is one that does not handle all of its inputs. A
total function is one that does. How do we make our f into a total
function? One way is with the use of the datatype Maybe:

data Maybe a = Nothing | Just a

The Maybe datatype can take an argument or not. In the first case,
Nothing, there is no argument. This is our way to say that there is no
result or data from the function without hitting bottom. The second
case, Just a, takes an argument and allows us to return the data we
want. Maybe makes all uses of null values and most uses of bottom
unnecessary. Here's how we'd use it with f:

f :: Bool -> Maybe Int
f False = Just 0
f _ = Nothing

Note that the type and both cases all change. Not only do we
replace the error with the Nothing value from Maybe, but we also have
to wrap 0 in the Just constructor from Maybe. If we don't do so, we'll
get a type error when we try to load the code, as you can see:

f :: Bool -> Maybe Int
f False = 0
f _ = Nothing

CHAPTER 8. FUNCTIONS THAT CALL THEMSELVES 285

Prelude> :l code/brokenMaybe1.hs
[1 of 1] Compiling Main

brokenMaybe1.hs:2:11: error:
• No instance for (Num (Maybe Int)) arising from the literal ‘0'
• In the expression: 0
  In an equation for ‘f': f False = 0
  |
2 | f False = 0
  |
              ^
Failed, no modules loaded.

This type error is because, as before, 0 has the type Num a => a, so
it's trying to get an instance of Num for Maybe Int. We can clarify our
intent a bit:

f :: Bool -> Maybe Int
f False = 0 :: Int
f _ = Nothing

And then get a better type error in the bargain:

Prelude> :l code/brokenMaybe2.hs
[1 of 1] Compiling Main

brokenMaybe1.hs:2:11: error:
• Couldn't match expected type ‘Maybe Int'
  with actual type ‘Int'
• In the expression: 0 :: Int
  In an equation for ‘f':
    f False = 0 :: Int
  |
2 | f False = 0 :: Int
  |
              ^^^^^^^^
Failed, no modules loaded.

We'll explain Maybe in more detail a bit later.

CHAPTER 8. FUNCTIONS THAT CALL THEMSELVES 286

8.4 Fibonacci numbers
---------------------
Another classic demonstration of recursion in functional program-
ming is a function that, given some value 𝑛, calculates the nth number
in a Fibonacci sequence. The Fibonacci sequence is a sequence of
numbers in which each number is the sum of the previous two: 0, 1,
1, 2, 3, 5, 8, 13, 21, 34, and so on. It's an indefinite computation that
relies on adding two of its own members, so it's a perfect candidate
for a recursive function. We're going to walk through the steps of
how we would write such a function for ourselves to get a better
understanding of the reasoning process.

Consider the types
^^^^^^^^^^^^^^^^^^
The first thing we'll consider is the possible type signature for our
function. The Fibonacci sequence only involves positive whole num-
bers. The argument to our Fibonacci function is going to be a positive
whole number, because we're trying to return the nth member of the
Fibonacci sequence. Our result will also be a positive whole number,
since that's what Fibonacci numbers are. We would be looking, then,
for values that are of the Int or Integer types. We could use one of
those concrete types or use a type class for constrained polymor-
phism. Specifically, we want a type signature that takes one integral
argument and returns one integral result. So, our type signature will
look something like this:

fibonacci :: Integer -> Integer
-- or
fibonacci :: Integral a => a -> a

Consider the base case
^^^^^^^^^^^^^^^^^^^^^^
It may sometimes be difficult to determine your base case up front,
but it's worth thinking about. For one thing, you do want to ensure
that your function will terminate. For another thing, giving serious
consideration to your base case is a valuable part of understanding
how your function works. Fibonacci numbers are positive integers,
so a reasonable base case is 0. When the recursive process hits zero,
it should terminate.

CHAPTER 8. FUNCTIONS THAT CALL THEMSELVES 287

The Fibonacci sequence is a bit trickier than some, though, be-
cause it needs two base cases. The sequence has to start off with two
numbers, since two numbers are involved in computing the next.
The next number after 0 is 1, and we add 0 to 1 to start the sequence
so those will be our base cases:

fibonacci :: Integral a => a -> a
fibonacci 0 = 0
fibonacci 1 = 1

Consider the arguments
^^^^^^^^^^^^^^^^^^^^^^
We've already determined that the argument to our function, the
value to which the function is applied, is an integral number and
represents the member of the sequence we want to be evaluated.
That is, we want to pass a value such as 10 to this function and have it
calculate the 10th number in the Fibonacci sequence. We only need
to have one variable as a parameter to this function, then.

But that argument will also be used as an argument within the
function due to the recursive process. Every Fibonacci number is
the result of adding the preceding two numbers. So, in addition to a
variable x, we will need to use (x - 1) and (x - 2) to get both of the
numbers before our argument:

fibonacci :: Integral a => a -> a
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x = (x - 1) (x - 2)
-- note: this doesn't work yet.

Consider the recursion
^^^^^^^^^^^^^^^^^^^^^^
All right, now we come to the heart of the matter. In what way will this
function refer to itself and call itself? Look at what we've worked out
so far: what needs to happen next to produce a Fibonacci number?
One thing that needs to happen is that (x - 1) and (x - 2) need to
be added together to produce a result. Try simply adding those two
together and running the function that way:

CHAPTER 8. FUNCTIONS THAT CALL THEMSELVES 288

fibonacci :: Integral a => a -> a
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x = (x - 1) + (x - 2)

If you pass the value 6 to that function, what will happen?

Prelude> fibonacci 6
9

Why? Because ((6 - 1) + (6 - 2)) equals 9. But this isn't how we
calculate Fibonacci numbers! The sixth member of the Fibonacci
sequence is not ((6 - 1) + (6 - 2)). What we want is to add the fifth
member of the Fibonacci sequence to the fourth member. That result
will be the sixth member of the sequence. We do this by making the
function refer to itself. In this case, we have to specify that both (x -
1) and (x - 2) are themselves Fibonacci numbers, so we have to have
the function call itself twice:

fibonacci :: Integral a => a -> a
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x =
fibonacci (x - 1) + fibonacci (x - 2)

Now, if we apply this function to the value 6, we will get a different
result:

Prelude> fibonacci 6
8

Why? Because it evaluates its input recursively:

fibonacci 6 = fibonacci 5 + fibonacci 4
fibonacci 5 = fibonacci 4 + fibonacci 3
fibonacci 4 = fibonacci 3 + fibonacci 2
fibonacci 3 = fibonacci 2 + fibonacci 1
fibonacci 2 = fibonacci 1 + fibonacci 0

CHAPTER 8. FUNCTIONS THAT CALL THEMSELVES 289

0 and 1 are defined as being equal to 0 and 1. So at this point, our
recursion stops, and the function starts adding up the result:

fibonacci 0 +            0
fibonacci 1 +            1
fibonacci 2 +  (1 + 0 =) 1
fibonacci 3 +  (1 + 1 =) 2
fibonacci 4 +  (1 + 2 =) 3
fibonacci 5 =  (2 + 3 =) 5
fibonacci 6 =  (3 + 5 =) 8

It can be daunting at first to think how you would write a recursive
function and what it means for a function to call itself. But as you
can see, it's useful when a function makes reference to its own results
in a repeated fashion.


8.5 Integral division from scratch
----------------------------------
Many people learned multiplication by memorizing multiplication
tables, usually up to 10 × 10 or 12 × 12. In fact, one can perform
multiplication in terms of addition, repeated over and over. Similarly,
one can define integral division in terms of subtraction.

Let's think through our recursive division function one step at a
time. First, let's consider the types we would want to use for such
a function and see if we can construct a reasonable type signature.
When we divide numbers, we have a numerator and a denominator.
When we calculate 10 ÷ 2, or in code 10 / 5, to get the answer 2, 10 is
the numerator, 5 is the denominator, and 2 is the quotient. So we
have at least three numbers here. So, perhaps a type like Integer ->
Integer -> Integer would be suitable. You could even add some type
synonyms to make it more obvious, if you wish:

dividedBy :: Integer -> Integer -> Integer
dividedBy = div

Instead of having all the types labeled Integer we can instead do:

CHAPTER 8. FUNCTIONS THAT CALL THEMSELVES 290

type Numerator = Integer
type Denominator = Integer
type Quotient = Integer

dividedBy :: Numerator
          -> Denominator
          -> Quotient
dividedBy = div

The type keyword, instead of the more familiar data or newtype,
declares a type synonym, or type alias. Those are all Integer types,
but we can give them different names to make them easier for human
eyes to distinguish in type signatures.

For this example, we didn't write out the recursive implementation
of dividedBy we had in mind. As it turns out, when we write the
function, we will want to change the final type signature a bit, for
reasons we'll see in a moment. Sometimes, the use of type synonyms
can improve the clarity and purpose of your type signatures, so this
is something you'll see, especially in more complex code. For our
relatively simple function, it may not be necessary.

Next, let's think through our base case. The way we divide in terms
of subtraction is by stopping when our result of having repeatedly
subtracted is lower than the divisor. If it divides evenly, it'll stop at 0:

Solve 20 divided by 4
--    [1] [2]
-- [1]: Dividend or numerator
-- [2]: Divisor or denominator
-- Result is quotient

20 divided by 4 == 20 - 4, 16
                      - 4, 12
                      - 4, 8
                      - 4, 4
                      - 4, 0
-- 0 is less than 4, so we stop.
-- We subtract 5 times, so 20 / 4 == 5

CHAPTER 8. FUNCTIONS THAT CALL THEMSELVES 291

Otherwise, we'll have a remainder. Let's look at a case where it
doesn't divide evenly:

Solve 25 divided by 4

25 divided by 4 == 25 - 4, 21
                      - 4, 17
                      - 4, 13
                      - 4, 9
                      - 4, 5
                      - 4, 1
We stop at 1, because it's less than 4.

In the case of 25 divided by 4, we subtract 4 six times and 1 is
our remainder. We can generalize this process of dividing whole
numbers, returning the quotient and remainder, into a recursive
function that does the repeated subtraction and counting for us. Since
we'd like to return the quotient and the remainder, we're going to
return a 2-tuple—using our friend (,)—as the result of our recursive
function:

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n   d count
         | n < d = (count, n)
         | otherwise =
             go (n - d) d (count + 1)

We changed the type signature from the one we had originally
worked out, both to make it more polymorphic (Integral a => a ver-
sus Integer) and also to return a tuple instead of just an integer.

Here, we're using a common Haskell idiom called a go function.
This allows us to define a function via a where clause that can accept
more arguments than the top-level function dividedBy does. In this
case, the top-level function takes two arguments, num and denom, but
we need a third argument in order to keep track of how many times
we do the subtraction. That argument is called count and is defined
with a starting value of 0 and is incremented by 1 every time the
otherwise case is invoked.

CHAPTER 8. FUNCTIONS THAT CALL THEMSELVES 292

There are two branches in our go function. The first case is the
most specific: when the numerator n is less than the denominator d,
the recursion stops and returns a result. It is not significant that we
changed the argument names from num and denom to n and d. The go
function has already been applied to those arguments in
the definition of dividedBy so the num, denom, and 0
are bound to n, d, and count in the where clause.

The result is a tuple of count and the last value for n. This is our
base case that stops the recursion and gives a final result.

Here's an example of how dividedBy expands but with the code
inlined:

dividedBy 10 2

First, we'll do this the previous way, but we'll keep track of how
many times we subtract:

10 divided by 2 ==
  10 - 2, 8 (subtracted 1 time)
     - 2, 6 (subtracted 2 times)
     - 2, 4 (subtracted 3 times)
     - 2, 2 (subtracted 4 times)
     - 2, 0 (subtracted 5 times)

Since the final number is 0, there's no remainder. We subtracted
five times. So 10 / 2 == 5.

Now, we'll expand the code:

dividedBy 10 2 =
go 10 2 0
  | 10 < 2 = ...
  -- false, skip this branch
  | otherwise = go (10 - 2) 2 (0 + 1)

The otherwise above is literally the value True, so if the first branch
fails, the otherwise branch always succeeds:

CHAPTER 8. FUNCTIONS THAT CALL THEMSELVES 293

  go 8 2 1
  -- 8 isn't < 2, use the otherwise branch
  go (8 - 2) 2 (1 + 1)
  -- n == 6, d == 2, count == 2


  go 6 2 2
  go (6 - 2) 2 (2 + 1)
  -- 6 isn't < 2, use the otherwise branch
  -- n == 4, d == 2, count == 3


  go 4 2 3
  go (4 - 2) 2 (3 + 1)
  -- 4 isn't < 2, use the otherwise branch
  -- n == 2, d == 2, count == 4


  go 2 2 4
  go (2 - 2) 2 (4 + 1)
  -- 2 isn't < 2, use the otherwise branch
  -- n == 0, d == 2, count == 5


  go 0 2 5
  -- the n < d branch is finally evaluated
  -- because 0 < 2 is true
  -- n == 0, d == 2, count == 5
  | 0 < 2 = (5, 0)
(5, 0)

The result of count is the quotient, that is, how many times you
can subtract 2 from 10. In a case where there is a remainder, that
number would be the final value for your numerator and would be
returned as the remainder.

8.6 Chapter exercises
---------------------

Review of types
^^^^^^^^^^^^^^^
1. What is the type of [[True, False], [True, True], [False, True]]?

CHAPTER 8. FUNCTIONS THAT CALL THEMSELVES 294

   a) Bool
   b) mostly True
   c) [a]
   d) [[Bool]]

2. Which of the following has the same type as [[True, False],
   [True, True], [False, True]]?

   a) [(True, False), (True, True), (False, True)]
   b) [[3 == 3], [6 > 5], [3 < 4]]
   c) [3 == 3, 6 > 5, 3 < 4]
   d) ["Bool", "more Bool", "Booly Bool!"]

3. For the function below, which of the following statements are
   true?

   func :: [a] -> [a] -> [a]
   func x y = x ++ y

   a) x and y must be of the same type.
   b) x and y must both be lists.
   c) If x is a String, then y must be a String.
   d) All of the above.

4. For the func code above, which is a valid application of func to
   both of its arguments?

   a) func "Hello World"
   b) func "Hello" "World"
   c) func [1, 2, 3] "a, b, c"
   d) func ["Hello", "World"]

Reviewing currying
^^^^^^^^^^^^^^^^^^
Given the following definitions, tell us what value results from further
applications:

CHAPTER 8. FUNCTIONS THAT CALL THEMSELVES 295

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

-- fill in the types

flippy = flip cattyConny

appedCatty = cattyConny "woops"
frappe = flippy "haha"

  1. What is the value of appedCatty "woohoo!"? Try to determine the
     answer for yourself, then test it in the REPL.
  2. frappe "1"
  3. frappe (appedCatty "2")
  4. appedCatty (frappe "blue")
  5. cattyConny (frappe "pink")
  (cattyConny "green"
  (appedCatty "blue"))
  6. cattyConny (flippy "Pugs" "are") "awesome"

Recursion
^^^^^^^^^
1. Write out the steps for reducing dividedBy 15 2 to its final answer
   according to the Haskell code.
2. Write a function that recursively sums all numbers from 1 to n,
   n being the argument. So if n is 5, you'd add 1 + 2 + 3 + 4 + 5 to
   get 15. The type should be (Eq a, Num a) => a -> a.
3. Write a function that multiplies two integral numbers using
   recursive summation. The type should be (Integral a) => a ->
   a -> a.

Fixing dividedBy
^^^^^^^^^^^^^^^^
Our dividedBy function wasn't quite ideal. For one thing, it is a partial
function and doesn't return a result (bottom) when given a divisor
that is 0 or less.

CHAPTER 8. FUNCTIONS THAT CALL THEMSELVES 296

Using the pre-existing div function, we can see how negative
numbers should be handled:

Prelude> div 10 2
5
Prelude> div 10 (-2)
-5
Prelude> div (-10) (-2)
5
Prelude> div (-10) (2)
-5

The next issue is how to handle zero. Zero is undefined for division
in math, so we ought to use a datatype that lets us say there is no
sensible result when the user divides by zero. If you need inspiration,
consider using the following datatype to handle this:

data DividedResult =
    Result Integer
  | DividedByZero

McCarthy 91 function
^^^^^^^^^^^^^^^^^^^^
We're going to describe a function in English, then in math notation,
then show you what your function should return for some test inputs.
Your task is to write the function in Haskell.

The McCarthy 91 function yields x - 10 when x > 100 and 91
otherwise. The function is recursive:


         ⎧ 𝑛 − 10            if 𝑛 > 100
𝑀 𝐶(𝑛) = ⎨
         ⎩ 𝑀 𝐶(𝑀 𝐶(𝑛 + 11))  if 𝑛 ≤ 100

mc91 = undefined

You haven't seen map yet, but all you need to know right now is
that it applies a function to each member of a list and returns the
resulting list. We'll explain it in more detail in the next chapter:

Prelude> map mc91 [95..110]
[91,91,91,91,91,91,91,92,93,94,95,96,97,98,
99,100]

CHAPTER 8. FUNCTIONS THAT CALL THEMSELVES 297

Numbers into words
^^^^^^^^^^^^^^^^^^

module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord n = undefined

digits :: Int -> [Int]
digits n = undefined

wordNumber :: Int -> String
wordNumber n = undefined

Here, undefined is a placeholder to show you where you need to
fill in the functions. The n to the right of the function names is the
argument that will be an integer.

Fill in the implementations of the functions above so that wordNumber
returns the English word version of the Int value. You will first write
a function that turns integers from 0–9 into their corresponding
English words: "one," "two," and so on. Then, you will write a function
that takes the integer, separates the digits, and returns it as a list of
integers. Finally, you will need to apply the first function to the list
produced by the second function and turn it into a single string with
interspersed hyphens.

We've laid out multiple functions for you to consider as you tackle
the problem. You may not need all of them, depending on how you
solve it—these are just suggestions. Play with them, and look up their
documentation to understand them in greater detail.

You will probably find this difficult:

div:: Integral a => a -> a -> a
mod:: Integral a => a -> a -> a
map:: (a -> b) -> [a] -> [b]
concat:: [[a]] -> [a]
intersperse :: a -> [a] -> [a]
(++):: [a] -> [a] -> [a]
(:[]):: a -> [a]

CHAPTER 8. FUNCTIONS THAT CALL THEMSELVES 298

Also consider:

Prelude> div 135 10
13
Prelude> mod 135 10
5
Prelude> div 13 10
1
Prelude> mod 13 10
3

Here is what your REPL output should look like when it's working:

Prelude> wordNumber 12324546
"one-two-three-two-four-five-four-six"

8.7 Definitions
---------------
1. Recursion is a means of computing results that may require an
   indefinite amount of work to obtain through the use of repeated
   function application. Most recursive functions that terminate
   or otherwise do useful work will often have a case that calls itself
   and a base case that acts as a backstop of sorts for the recursion.

   This function is not recursive:

  lessOne :: Int -> Int
  lessOne n = n - 1

  This one is recursive:

  zero :: Int -> Int
  zero 0 = 0
  zero n = zero (n - 1)
