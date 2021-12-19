Stolen from here https://gist.github.com/lukechampine/a3956a840c603878fd9f

## The Y Combinator ##

The Y Combinator is a classic lambda calculus construct that many people find
baffling. Here's my attempt to explain it as clearly as possible (no
promises!). Familiarity with Haskell syntax is assumed.

The problem we're trying to solve is how to write an anonymous function (a
"lambda") that is recursive. Normally, if you want to write a recursive
function, it looks like this:

```haskell
fac n =
  if   n == 0
  then 1
  else n * fac (n-1)
```

Note that `fac` appears in the function body. Therein lies the problem: if
`fac` were a lambda, what name would you use to refer to it? This may seem
like a non-issue; why not just write the recursive function normally? Why use a
lambda? Well, in the lambda calculus, there are no "normal functions," there
are *only* lambdas. So the Y Combinator was invented as a means of writing
recursive functions in pure lambda calculus.

Getting back to the problem: we need to rewrite `fac` so that it doesn't
reference itself in its own function body. Here's the trick that forms the
basis of the Y Combinator: `fac` can't refer to `fac`, but it can refer to a
*generic function supplied as an argument*. In other words, we rewrite `fac` to
take an extra argument, `f`, which is a function. Then, instead of `fac`
recursively calling `fac`, it will just call `f`. It looks like this:

```haskell
fac f n =
  if n == 0
  then 1
  else n * f (n-1)
```

Now when we call `fac`, we'll call it as `fac fac 5`, so that when `fac` calls
`f`, it'll really be calling itself. Recursion!

There's an error in the above code snippet though. Since `f` is `fac`, it needs
to take another argument:

```haskell
fac f n =
  if n == 0
  then 1
  else n * f f (n-1)
```

Excellent. With all the references removed, we're now able to write `fac` as a
lambda:

```haskell
fac = (\f n ->
  if n == 0
  then 1
  else n * f f (n-1))
```

Note that `fac` itself takes no arguments. This is known as the "fixed-point"
version of `fac`, also known as "points-free" style (or "pointless" by its
detractors).

There's just one more problem. Recall that we are now calling `fac fac` instead
of just `fac`. That won't do! Think of how we would map our `fac` lambda to a
list:

```haskell
-- won't work! our lambda takes too many arguments!
map (\f n -> if n == 0 then 1 else n * f f (n-1)) [1..10]
```

To fix this, we need to supply the first argument to our lambda, `f`. Recall
that this is the lambda itself, so now we have:

```haskell
fac = (\f n -> if n == 0 then 1 else n * f f (n-1))
      (\f n -> if n == 0 then 1 else n * f f (n-1))

-- our mapped lambda now looks like this
map ((\f n -> if n == 0 then 1 else n * f f (n-1))
     (\f n -> if n == 0 then 1 else n * f f (n-1))) [1..10]
```

This will actually work! (Okay, not quite; see Addendum.) But it goes without
saying that this is rather ugly; any time we want to write a recursive lambda,
we have to write it out twice! Plus, wherever we recurse, we have to call `f f`
instead of just `f`. This is where the Y Combinator comes in: it abstracts away
these annoyances.

We'll start by rewriting `fac` like so:

```haskell
fac f = (\ff n -> if n == 0 then 1 else n * ff (n-1))
        (f f)

-- or as a lambda:
fac = \f -> (\ff n -> if n == 0 then 1 else n * ff (n-1))
            (f f)
```

Do you see what's different here? Instead of calling `f f` inside the factorial
logic, we pass it in as an argument; `ff = (f f)`.

What is the purpose of this? You'll see in just a second. First, we're going to
rewrite our doubly-applied version with this new style:

```haskell
fac = (\f -> (\ff n -> if n == 0 then 1 else n * ff (n-1)) (f f))
      (\f -> (\ff n -> if n == 0 then 1 else n * ff (n-1)) (f f))
```

Now we can pull out the factorial logic:

```haskell
-- looks just like our first lambda, but without the double f
fac' = \f n ->
    if n == 0 then 1
    else n * f (n-1)

fac = (\f -> fac' (f f))
      (\f -> fac' (f f))
```

Note that we couldn't do this before because of the double `f`.

Look at how general our `fac` function is! In fact, if we replace `fac'` with a
generic function, we get:

```haskell
-- the Y Combinator!
y fn = (\f -> fn (f f))
       (\f -> fn (f f))

-- as a lambda:
y = \fn -> (\f -> fn (f f))
           (\f -> fn (f f))
```

If you're brave, you can now write `fac 5` as it would appear in pure lambda
calculus:

```haskell
-- Y Combinator
(\fn -> (\f -> fn (f f))
        (\f -> fn (f f)))
-- applied to fac
(\f n -> if n == 0 then 1
         else n * f (n-1))
-- applied to 5
5
```

Just to make sure this works, we can try it with a Fibonnaci function as well:

```haskell
-- standard Fibonnaci implementation
fib n = if      n == 0 then 0
        else if n == 1 then 1
        else fib (n-1) + fib (n-2))

-- version that takes itself as an argument
fib' f n = if      n == 0 then 0
           else if n == 1 then 1
           else f f (n-1) + f f (n-2))

-- Y Combinator
(\fn -> (\f -> fn (f f))
        (\f -> fn (f f)))
-- applied to fib'
(\f n -> if      n == 0 then 0
         else if n == 1 then 1
         else f (n-1) + f (n-2))
-- applied to 5
5
```

These examples should return 120 and 5, respectively.

### Addendum ###

Haskell actually won't allow you to write `f f`, because it's strictly typed.
To get around this, import `Unsafe.Coerce` and preface every occurance of `f f`
with `unsafeCoerce f f`.

However, Haskell also has a much simpler way of writing a fixed-point
combinator (of which the Y Combinator is only one example):

```haskell
fix f = f (fix f)
```

This is possible because Haskell is "lazily evaluated," which means it only
evaluates expressions when their value is needed. Otherwise, when it tried to
evaluate the definition of `fix`, it would recurse forever and hang.

You can verify for yourself that `fix` works with the previous examples, no
`unsafeCoerce` necessary. But why does it work? Let's expand it a bit:

```haskell
fac' = \f n ->
    if n == 0 then 1
    else n * f (n-1)

-- fac is just the repeated application of fac' to itself
fac = fix fac'
    = fac' (fix fac')
    = fac' (fac' (fix fac'))
    = fac' (fac' (fac' (...)))

-- full expansion of fac 2:
-- first iteration (n == 2)
(\f n -> if n == 0 then 1 else n * f (n-1))
-- applied to second iteration (n == 1)
(\f n -> if n == 0 then 1 else n * f (n-1))
-- applied to second iteration (n == 0)
(\f n -> if n == 0 then 1 else n * f (n-1))
-- ...applied to infinitely more iterations...
(fix fac')
-- applied to 2
2

-- since we know the value of n, let's reduce the ifs:
(\f n -> n * f (n-1)) -- n == 2
(\f n -> n * f (n-1)) -- n == 1
(1)                   -- n == 0
2
```

What happened to the "infinitely more iterations?" Well, in the `n == 0` case,
`f` isn't used, so Haskell doesn't try to evaluate it! Pretty cool, huh? Let's
finish up the reduction:

```haskell
-- reduce the n == 1 case
-- note the disappearance of the f argument
(\f n -> n * f (n-1)) -- n == 2
(\n -> n * (1))       -- n == 1
2

-- reduce the n == 2 case
(\n -> n * (\n -> n * (1)) (n-1)) -- n == 2
2
-- further reduction
(\n -> n * (n-1) * (1))
2
-- and finally
2 * (2-1) * (1) = 2
```
