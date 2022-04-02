Chapter 10 Folding Lists
************************

.. TODO Proof read this document, number the paragraphs, and then transcribe all the figures - most of them are garbled.

.. 347

The explicit teaching of thinking is no trivial task,
but who said that the teaching of programming is? In
our terminology, the more explicitly thinking is
taught, the more of a scientist the programmer will
become. ~ Edsger Dijkstra

.. CHAPTER 10. DATA STRUCTURE ORIGAMI 348


10.1 Folds
----------
1a) Folding is a concept that extends in usefulness and importance beyond lists, but lists are often how they are introduced.
1b) Folds as a general concept are called catamorphisms.
1c) You're familiar with the root "morphism" from polymorphism.
1d) "Cata-" means "down" or "against," as in "catacombs."
1e) Catamorphisms are a means of deconstructing data.
1f) If the spine of a list is the structure of a list, then a fold is what can reduce that structure.1

2a) This chapter is a thorough look at the topic of folding lists in Haskell.
2b) We will:

2c) * Explain what folds are and how they work.
2d) * Detail the evaluation processes of folds.
2e) * Walk through writing folding functions.
2f) * Introduce scans, functions that are related to folds.


10.2 Bringing you into the fold
-------------------------------
1a) Let's start with a quick look at ``foldr``, short for "fold right."
1b) This is the fold you'll most often want to use with lists.
1c) The following type signature may look a little hairy, but let's compare it to what we know about mapping.
1d) Note that the type of ``foldr`` changed with GHC 7.10:

.. 10.2, Figure 1, page 348
::

  -- GHC 7.8 and older
  foldr :: (a -> b -> b) -> b -> [a] -> b

  -- GHC 7.10 and newer
  foldr :: Foldable t
        => (a -> b -> b)
        -> b
        -> t a
        -> b

2a) Lined up next to each other:

..
   [1] Note that a catamorphism can break down the structure but that structure might be rebuilt, so to speak, during evaluation.
   That is, folds can return lists as results.

.. CHAPTER 10. DATA STRUCTURE ORIGAMI 349

.. 10.2, Figure 1, page 349
::

  foldr :: Foldable t =>
           (a -> b -> b) -> b -> t  a -> b
  foldr :: (a -> b -> b) -> b -> [] a -> b

3a) For now, all you need to know is that GHC 7.10 abstracted out the list-specific part of folding into a type class that lets you reuse the same folding functions for any datatype that can be folded—not just lists.
3b) We can even recover the more concrete type, because we can always make a type more concrete, but never more generic:

.. Figure 3, page 349
::

  ·∾ :{
   ⋮ listFoldr :: (a -> b -> b)
   ⋮           -> b
   ⋮           -> [] a
   ⋮           -> b
   ⋮ listFoldr = foldr
   ⋮ :}
  ·∾

  ·∾ :type listFoldr
  listFoldr :: (a -> b -> b) -> b -> [a] -> b

4a) Now, let's notice a parallel between ``map`` and ``foldr``:

::

  foldr :: (a -> b -> b) -> b -> [a] -> b

  -- Remember how map works?
  map :: (a -> b) -> [a] -> [b]
  map (+1) 1 :      2 :     3 : []

      (+1) 1 : (+1) 2 : (+1) 3: []

  -- Given the list
  foldr (+) 0 (1 :  2 :  3 : [])

               1 + (2 + (3 + 0))

5a) Where map applies a function to each member of a list and returns a list, a fold replaces the cons constructors with the function and reduces the list.

..  CHAPTER 10. DATA STRUCTURE ORIGAMI 350


10.3 Recursive patterns
-----------------------
1a) Let's revisit sum:

.. 10.3, Figure 1, page 350
::

  ·∾ sum [1,5,10]
  16

2a) As we've seen, it takes a list, adds the elements together, and returns a single result.
2b) You might think of it as similar to the map functions we've looked at, except that it's mapping + over the list, replacing the cons operators themselves, and returning a single result, instead of mapping, for example, (+1) into each cons cell and returning a whole list of results back to us.
2c) This has the effect of both mapping an operator over a list and also reducing the list.
2d) In a previous section, we wrote sum in terms of recursion:

.. 10.3, Figure 2, page 350
::

  sum :: [Integer] -> Integer
  sum []     =  0
  sum (x:xs) =  x + sum xs

3a) And if we bring back our length function from earlier:

.. 10.3, Figure 3, page 350
::

  length :: [a] -> Integer
  length []     = 0
  length (_:xs) = 1 + length xs

4a) Do you see some structural similarity?
4b) What if you look at product and ``concat``, as well?

.. 10.3, Figure 4, page 350
::

  product :: [Integer] -> Integer
  product []     = 1
  product (x:xs) = x * product xs

  concat :: [[a]] -> [a]
  concat []     = []
  concat (x:xs) = x ++ concat xs

5a) In each case, the base case is the identity for that function.
5b) So the identity for sum, length, product, and ``concat``, respectively, are ``0``, ``0``, ``1``, and ``[]``.
5c) When we do addition, adding zero gives us the same result as our initial value: ``1 + 0 = 1``.
5d) But when we do multiplication,

.. CHAPTER 10. DATA STRUCTURE ORIGAMI 351

5d) it's multiplying by 1 that gives us the identity: ``2 × 1 = 2``.
5e) With list concatenation in Haskell, the identity is the empty list ``[]``, such that ``[1, 2, 3] ++ [] == [1, 2, 3]``.

6a) Also, each of them has a main function with a recursive pattern that associates to the right.
6b) The head of the list gets evaluated, set aside, and then the function moves to the right, evaluates the next head, and so on.


10.4 Fold right
---------------
1a) We call ``foldr`` the "right fold," because the fold is right associative, that is, it associates to the right.
1b) This is syntactically reflected in a straightforward definition of ``foldr``, as well:

.. 10.4, Figure 1, page 351
::

  foldr :: (a -> b -> b) -> b -> [a] -> b
  foldr f z []     = z
  foldr f z (x:xs) = f x (foldr f z xs)

2a) The similarities between this and the recursive patterns we saw above should be clear.
2b) The "rest of the fold," ``(foldr f z xs)``, is an argument to the function ``f`` we're folding with.
2c) The ``z`` is the zero of our fold.
2d) It provides a fallback value for the empty list case and a second argument to begin our fold with.
2e) The zero is often the identity for whatever function we're folding with, such as ``0`` for ``+`` and ``1`` for ``*``.

10.4.1 How ``foldr`` evaluates
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
1a) We're going to rejigger our definition of ``foldr`` a little bit.
1b) It won't change the semantics, but it'll make it easier to write out what's happening:

.. 10.4.1, Figure 1, page 351
::

  foldr :: (a -> b -> b) -> b -> [a] -> b
  foldr f z xs =
    case xs of
      []     -> z
      (x:xs) -> f x (foldr f z xs)

2a) Here, we see how the right fold associates to the right.
2b) This will reduce like the sum example from earlier:

.. CHAPTER 10. DATA STRUCTURE ORIGAMI 352

.. 10.4.1, Figure 2, page 352
::

  foldr (+) 0 [1, 2, 3]

3a) When we reduce that fold, the first step is substituting ``xs`` in our case expression:

.. 10.4.1, Figure 3, page 352
::

  foldr (+) 0 [1, 2, 3] =
    case [1, 2, 3] of
      ...

4a) Which case of the expression matches?

.. 10.4.1, Figure 4, page 352
::

  foldr (+) 0 [1, 2, 3] =
    case [1, 2, 3] of
      []     -> 0
      (x:xs) ->
        f x (foldr f z xs) -- <--- this one

5a) What are ``f``, ``x``, ``xs``, and ``z`` in that branch of the case?

.. 10.4.1, Figure 5, page 352
::

  foldr (+) 0 [1, 2, 3] =
    case [1, 2, 3] of
      []           -> 0
      (1 : [2, 3]) ->
        (+) 1 (foldr (+) 0 [2, 3])

6a) Critically, we're going to expand ``(foldr (+) 0 [2, 3])`` only because + is strict in both of its arguments, so it forces the next iteration.
6b) We could have a function that doesn't continually force the rest of the fold.
6c) If it were to stop on the first case here, then it would have returned the value ``1``.
6d) One such function is ``const``, which always returns the first argument.
6e) We'll show you how that behaves in a bit. Our next recursion is ``(foldr (+) 0 [2, 3])``:

.. 10.4.1, Figure 6, page 352
::

  foldr (+) 0 [2, 3] =
    case [2, 3] of
      []        ->
        0 -- this doesn't match again
      (2 : [3]) -> (+) 2 (foldr (+) 0 [3])

.. CHAPTER 10. DATA STRUCTURE ORIGAMI 353

7a)There is a ``(+)`` 1 implicitly wrapped around this continuation of the recursive fold.
7b)``+`` is not only strict in both of its arguments, but it's unconditionally so, so we're going to proceed to the next recursion of ``foldr``.
7c)Note that the function calls bounce between our folding function f and ``foldr``.
7d)This bouncing back and forth gives more control to the folding function.
7e)A hypothetical folding function, such as ``const``, which doesn't need the second argument, has the opportunity to do less work by not evaluating its second argument, which is "more of the fold."

8a) ``(+) 1 ((+) 2 ...)`` is implicitly wrapped around this next step of the recursive fold:

.. 10.4.1, Figure 7, page 353
::

  foldr (+) 0 [3] =
    case [3] of
      []       ->
        0 -- this doesn't match again
      (3 : []) -> (+) 3 (foldr (+) 0 [])

9a) We're going to ask for more ``foldr`` one last time.
9b) We have, again, ``(+) 1 ((+) 2 ((+) 3 ...))`` implicitly wrapped around this final step of the recursive fold.
9c) Finally, we hit our base case:

.. 10.4.1, Figure 8, page 353
::

  foldr (+) 0 [] =
    case [] of
      []       ->
        0 -- <-- This one finally matches
      -- ignore the other case,
      -- it doesn't happen

10a) So one way to think about the way Haskell evaluates is that it's like a text rewriting system.
10b) Our expression has thus far rewritten itself from:

.. 10.4.1, Figure 9, page 353
::

  foldr (+) 0 [1, 2, 3]

11a) Into:

.. 10.4.1, Figure 10, page 353
::

  (+) 1 ((+) 2 ((+) 3 0))

12a) If you wanted to clean it up a bit without changing how it evaluates, you could make it the following:

.. CHAPTER 10. DATA STRUCTURE ORIGAMI 354

.. 10.4.1, Figure 11, page 354
::

  1 + (2 + (3 + 0))

13a) As in arithmetic, we evaluate innermost parentheses first:

.. 10.4.1, Figure 12, page 354
::

  1 + (2 + (3 + 0))

  1 + (2 + 3)

  1 + 5

  6

14a) And now we're done, with the result of ``6``.

15a) We can also use a trick popularized by some helpful users in the Haskell IRC community to see how the fold associates:

.. 10.4.1, Figure 13, page 354
::

  xs = map show [1..5]
  y = foldr (\x y -> concat ["(",x,"+",y,")"]) "0" xs

16a) When we call y in the REPL, we can see how ``foldr`` evaluates:

.. 10.4.1, Figure 14, page 354
::

  Prelude> y
  "(1+(2+(3+(4+(5+0)))))"

17a) One initially non-obvious aspect of folding is that it happens in two stages, traversal and folding.
17b) Traversal is the stage in which the fold recurses over the spine.
17c) Folding refers to the evaluation or reduction of the folding function applied to the values.
17d) All folds recurse over the spine in the same direction; the difference between left folds and right folds is in the association, or parenthesization, of the folding function and, thus, in which direction the folding or reduction proceeds.

18a) With ``foldr``, the rest of our fold is an argument to the function we're folding with:

.. 10.4.1, Figure 15, page 354
::

  foldr f z (x:xs) = f x (foldr f z xs)
  --                     ^------------^
  --                    rest of the fold

.. 2 Idea borrowed from Cale Gibbard from the #haskell Freenode IRC channel and on the Haskell Wiki https://wiki.haskell.org/Fold#Examples.

.. CHAPTER 10. DATA STRUCTURE ORIGAMI 355

19a) Given this two-stage process and non-strict evaluation, if ``f`` doesn't evaluate its second argument (the rest of the fold), no more of the spine will be forced.
19b) One of the consequences of this is that ``foldr`` can avoid evaluating not only some or all of the values in the list, but some or all of the list's spine, as well!
19c) For this reason, ``foldr`` can be used with lists that are potentially infinite.
19d) For example, compare the following sets of results (recall that ``+`` will unconditionally evaluate the entire spine and all of the values):

.. 10.4.1, Figure 16, page 355
::

  Prelude> foldr (+) 0 [1..5]
  15

20a) While you cannot use ``foldr`` with addition on an infinite list, you can use functions that are not strict in both arguments and therefore do not require evaluation of every value in order to return a result.
20b) The function ``myAny``, for example, can return a ``True`` result as soon as it finds one ``True``:

.. 10.4.1, Figure 17, page 355
::

  myAny :: (a -> Bool) -> [a] -> Bool
  myAny f xs =
  foldr (\x b -> f x || b) False xs

21a) The following should work despite being an infinite list:

.. 10.4.1, Figure 18, page 355
::

  Prelude> myAny even [1..]
  True

22a) The following, however, will never finish evaluating, because it's always an odd number:

.. 10.4.1, Figure 19, page 355
::

  Prelude> myAny even (repeat 1)

23a) Another term we use - and that we've seen before — for this never ending evaluation is bottom or ``undefined``.
23b) There's no guarantee that a fold of an infinite list will finish evaluating even if you use ``foldr``, as it often depends on the input data and the fold function you supply to operate on it.
23c) Let us consider some more examples with a less inconvenient bottom:

.. CHAPTER 10. DATA STRUCTURE ORIGAMI 356

.. 10.4.1, Figure 20, page 356
::


  ·∾ -- Here we give an udefined value.

  ·∾ foldr (+) 0 [1,2,3,4,undefined]
  *** Exception: Prelude.undefined

  ·∾ xs = take 4 [1,2,3,4,undefined]
  ·∾ foldr (+) 0 xs
  10


  ·∾ -- Here, undefined is part of the spine.

  ·∾ xs = [1,2,3,4] ++ undefined

  ·∾ foldr (+) 0 xs
  *** Exception: Prelude.undefined

  ·∾ xs = take 4 ([1,2,3,4] ++ undefined)

  ·∾ foldr (+) 0 xs
  10

24a) By taking only the first four elements, we stop the recursive folding process after the fourth value, so our addition function does not run into bottom, and that works whether undefined is one of the values or part of the spine.

25a) The length function behaves differently; it evaluates the spine unconditionally but not the values:

.. 10.4.1, Figure 21, page 356
::

  Prelude> length [1, 2, 3, 4, undefined]
  5

  Prelude> length ([1, 2, 3, 4] ++ undefined)
  *** Exception: Prelude.undefined

26a) However, if we drop the part of the spine that includes the bottom before we use length, we can get an expression that works:

.. 10.4.1, Figure 22, page 356
::

  Prelude> xs = [1, 2, 3, 4] ++ undefined
  Prelude> length (take 4 xs)
  4

27a) The take function is non-strict like everything else you've seen so far, and in this case, it only returns as much list as you ask for.
27b) The difference in what it does is that it stops returning elements from a list when it hits the given length limit.
27c) Consider this:

.. CHAPTER 10. DATA STRUCTURE ORIGAMI 357

.. 10.4.1, Figure 23, page 357
::

  Prelude> xs = [1, 2] ++ undefined
  Prelude> length $ take 2 $ take 4 xs
  2

28a) It doesn't matter that take 4 could have hit the bottom!
28b) Nothing forced it to because of the take 2 between it and length.

29a) Now that we've seen how the recursive second argument to ``foldr``'s folding function works, let's consider the first argument:

.. 10.4.1, Figure 24, page 357
::

  foldr :: (a -> b -> b) -> b -> [a] -> b
  foldr f z []      = z
  foldr f z (x:xs)  = f x (foldr f z xs)
  --                    ^-- first argument

30a) The first argument, noted above, involves a pattern match that is strict by default—the ``f`` only applies to ``x`` if there is an ``x`` value and not just an empty list.
30b) This means that ``foldr`` must force an initial cons cell in order to discriminate between the ``[]`` and the ``(x:xs)`` cases, so the first cons cell *cannot* be undefined.

31a) Now, we're going to try something unusual to demonstrate that the first bit of the spine must be evaluated by ``foldr``.
31b) We have a somewhat silly, anonymous function that will ignore all of its arguments and return a value of ``9001``.
31c) We're using it with ``foldr``, because it will never force evaluation of any of its arguments, so we can have a bottom as a value or as part of the spine, and it will not force an evaluation:

.. 10.4.1, Figure 25, page 357
::

  ·∾ foldr (\_ _ -> 9001) 0 [1..5]
  9001

  ·∾ xs = [1,2,3,undefined]
  ·∾ foldr (\_ _ -> 9001) 0 xs
  9001

  ·∾ xs = [1,2,3] ++ undefined
  ·∾ foldr (\_ _ -> 9001) 0 xs
  9001

32a) Everything is fine unless the first cons cell of the spine is bottom:

.. 10.4.1, Figure 26, page 357
::

  Prelude> foldr (\_ _ -> 9001) 0 undefined
  *** Exception: Prelude.undefined

.. CHAPTER 10. DATA STRUCTURE ORIGAMI 358

.. 10.4.1, Figure 26, page 356
::

  Prelude> xs = [1,undefined]
  Prelude> foldr (\_ _ -> 9001) 0 xs
  9001

  Prelude> xs = [undefined, undefined]
  Prelude> foldr (\_ _ -> 9001) 0 xs
  9001

33a) The final two examples work, because it isn't the first cons cell that is bottom—the undefined values are inside the cons cells, not in the spine itself.
33b) Put differently, the cons cells contain bottom values but are not themselves bottom.
33c) We will experiment later with non-strictness and strictness to see how they affect the way our programs evaluate.

34a) Traversing the rest of the spine doesn't occur unless the function asks for the result of having folded the rest of the list.
34b) In the following examples, we don't force traversal of the spine, because ``const`` throws away its second argument, which is the rest of the fold:

.. 10.4.1, Figure 27, page 358
::

  ·∾ -- reminder:
  ·∾ -- const :: a -> b -> a
  ·∾ -- const x _ = x
  ·∾ const 1 2
  1
  ·∾ const 2 1
  2
  ·∾ foldr const 0 [1..5]
  1
  ·∾ foldr const 0 [1,undefined]
  1
  ·∾ foldr const 0 ([1,2] ++ undefined)
  1
  ·∾ foldr const 0 [undefined,2]
  *** Exception: Prelude.undefined

35a) Now that we've seen how ``foldr`` evaluates, we're going to look at ``foldl`` before we move on to learning how to write and use folds.

.. CHAPTER 10. DATA STRUCTURE ORIGAMI 359


10.5 Fold left
--------------
1a) Because of the way lists work, folds must first recurse over the spine of the list from beginning to end.
1b) Left folds traverse the spine in the same direction as right folds, but their folding process is left associative and proceeds in the opposite direction as that of ``foldr``.

2a) Here's a simple definition of ``foldl``.
2b) Note that to see the same type for ``foldl`` in your GHCi REPL, you will need to import Data.List for the same reason as for ``foldr``:


.. 10.5, Figure 1, page 359
.. NOTE This figure has been fixed.
::

  -- Again, different type in
  -- GHC 7.10 and newer.

  foldl :: (b -> a -> b) -> b -> [a] -> b
  foldl f acc [] = acc
  foldl f acc (x:xs) = foldl f (f acc x) xs

  foldl :: (b -> a -> b) -> b -> [a] -> b

  -- Given the list
  foldl (+) 0 (1 : 2 : 3 : [])

  -- foldl associates like this
          ((0 + 1) + 2) + 3

3a) We can also use the same trick we used to see the associativity of ``foldr`` to see the associativity of ``foldl``:

.. 10.5, Figure 2, page 359
.. NOTE I wrote this differently than the example in the book. It bothered me.
::

  ·∾ f x y = "(" ++ x ++ "+" ++ y ++ ")"

  ·∾ foldl f "0" ["1","2","3","4","5"]
  "(((((0+1)+2)+3)+4)+5)"

4a) We can see from this that ``foldl`` begins its reduction process by adding the ``acc`` (accumulator) value to the head of the list, whereas ``foldr`` adds it to the final element of the list, first.

5a) We can also use functions called scans to see how folds evaluate.
5b) Scans are similar to folds but return a list of all the intermediate stages

.. CHAPTER 10. DATA STRUCTURE ORIGAMI 360

5c) of the fold. We can compare ``scanr`` and ``scanl`` to their accompanying folds to see the difference in evaluation:


.. 10.5, Figure 3, page 360
::

  ·∾ foldr (+) 0 [1,2,3,4,5]
  15
  ·∾ scanr (+) 0 [1,2,3,4,5]
  [15,14,12,9,5,0]

  ·∾ foldl (+) 0 [1,2,3,4,5]
  15
  ·∾ scanl (+) 0 [1,2,3,4,5]
  [0,1,3,6,10,15]

6a) The relationship between scans and folds is as follows:

.. 10.5, Figure 4, page 360
::

  last (scanl f z xs)  ≡  foldl f z xs
  head (scanr f z xs)  ≡  foldr f z xs

7a) Each fold will return the same result for this operation, but we can see from the scans that they arrive at that result in a different order, due to the different associativity.
7b) We'll talk more about scans later.

10.5.1 Associativity and folding
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
1a) Next, we'll take a closer look at some of the effects of the associativity of ``foldl``.
1b) As we've said, both folds traverse the spine in the same direction.
1c) What's different is the associativity of the evaluation.

2a) The fundamental way to think about evaluation in Haskell is as substitution.
2b) When we use a right fold on a list with the function f and start value z, we're, in a sense, replacing the cons constructors with our folding function and the empty list constructor with our start value z:

.. 10.5.1, Figure 1, page 360
::

  [1..3] == 1 : 2 : 3 : []

  foldr f z [1, 2, 3]
  1 `f` (foldr f z [2, 3])
  1 `f` (2 `f` (foldr f z [3]))
  1 `f` (2 `f` (3 `f` (foldr f z [])))
  1 `f` (2 `f` (3 `f` z))

.. CHAPTER 10. DATA STRUCTURE ORIGAMI 361

3a) Furthermore, lazy evaluation lets our functions, rather than the ambient semantics of the language, dictate in which order things get evaluated.
3b) Because of this, the parentheses are real.
3c) In the above, the ``3 `f` z`` pairing gets evaluated first, because it's in the innermost parentheses.
3d) Right folds have to traverse the list outside-in, but the folding itself starts from the end of the list.

4a) It's hard to see this with arithmetic functions that are associative, such as addition, but it's an important point to understand, so we'll run through some different examples.
4b) Let's start by using an arithmetic operation that isn't associative:

.. 10.5.1, Figure 2, page 361
::

  ·∾ foldr (^) 2 [1..3]
  1
  ·∾ foldl (^) 2 [1..3]
  64

5a) This time we can see clearly that we get different results, and that difference results from the way the functions associate.
Here's a breakdown:

.. 10.5.1, Figure 3, page 361
::

  -- If you want to follow along,
  -- use paper and not the REPL.
  foldr (^) 2 [1..3]
  (1 ^ (2 ^ (3 ^ 2)))
  (1 ^ (2 ^ 9))
   1 ^ 512
   1

6a) Contrast that with this:

.. 10.5.1, Figure 4, page 361
::

  foldl (^) 2 [1..3]
  ((2 ^ 1) ^ 2) ^ 3
  (2 ^ 2) ^ 3
   4 ^ 3
   64

7a) In this next set of comparisons, we will demonstrate the effect of associativity on argument order by folding the same list into a new list, like this:

.. CHAPTER 10. DATA STRUCTURE ORIGAMI 362

.. 10.5.1, Figure 5, page 362
::

  ·∾ foldr (:) [] [1,2,3]
  [1,2,3]

  ·∾ foldl (flip (:)) [] [1,2,3]
  [3,2,1]

8a) We must use flip with foldl.
8b) Let's examine why.
8c) Like a right fold, a left fold cannot perform magic and go to the end of the list instantly; it must start from the beginning of the list.

9a) However, the parentheses dictate how our code evaluates.
9b) The type of the argument to the folding function changes in addition to the associativity:

.. 10.5.1, Figure 6, page 362
.. topic:: Figure 6

  ::

    foldr :: (a -> b -> b) -> b -> [a] -> b
    --       [1]  [2]  [3]
    foldl :: (b -> a -> b) -> b -> [a] -> b
    --       [4]  [5]  [6]

  1. The parameter of type a represents one of the list
     element arguments the folding function of ``foldr``
     is applied to.

  2. The parameter of type ``b`` will either be the
     start value or the result of the fold accumulated
     so far, depending on how far you are into the fold.

  3. The final result of having combined the list
     element and the start value or fold so far to
     compute the fold.

  4. The start value or fold accumulated so far is the
     first argument to ``foldl``'s folding function.

  5. The list element is the second argument to ``foldl``'s
     folding function.

  6. The final result of ``foldl``'s fold function is of
     type ``b``, like that of ``foldr``.

10a) The type of : requires that a value be the first argument and a list be the second argument:

.. 10.5.1, Figure 7, page 362
(:) :: a -> [a] -> [a]

.. CHAPTER 10. DATA STRUCTURE ORIGAMI 363

11a) So the value is prepended, or "cons'ed onto," the front of that list.

12a) In the following examples, the tilde means "is equivalent or equal to."
12b) If we write a right fold that has the cons constructor as our f and the empty list as our z, we get:

.. 10.5.1, Figure 7, page 362
::

  -- foldr f z [1, 2, 3]
  -- f ~ (:); z ~ []
  -- Run it in your REPL. It'll return True.
     foldr (:) [] (1 : 2 : 3 : [])
  == 1 : (2 : (3 : []))

13a) The cons'ing process for ``foldr`` matches the type signature for the ``:`` operator.
13b) It also reproduces the same list, because we're replacing the cons constructors with cons constructors and the null list with null list.
13c) However, for it to be identical, it also has to be right associative.

14a) Doing the same thing with ``foldl`` does not produce the same result.
14b) When using ``foldl``, the result we've accumulated so far is the first argument instead of the list element.
14c) This is the opposite of what : expects if we're accumulating a list.
14d) Trying to fold the identity of the list as above but with ``foldl`` would give us a type error, because the reconstructing process for ``foldl`` would look like this:

.. 10.5.1, Figure 8, page 362
::

  foldl f z [1, 2, 3]
  -- f ~ (:); z ~ []
  -- (((z `f` 1) `f` 2) `f` 3)
  ((([] : 1) : 2) : 3)

15a) That won't work, because the ``z`` is an empty list and the ``f`` is cons, so we have the order of arguments backwards for cons.
15b) Enter ``flip``, which takes backwards arguments and turns that frown upside-down.
15c) It will flip each set of arguments around for us, like this:

.. 10.5.1, Figure 9, page 363
::

  foldl f z [1, 2, 3]
  -- f ~ (flip (:)); z ~ []
  -- (((z `f` 1) `f` 2) `f` 3)
  f = flip (:)
  ((([] `f` 1) `f` 2) `f` 3)
   (([1] `f` 2) `f` 3)
    ([2, 1] `f` 3)
     [3, 2, 1]

.. CHAPTER 10. DATA STRUCTURE ORIGAMI 364

16a) Even when we've satisfied the types by flipping things around, the left-associating nature of ``foldl`` leads to a different result from that of ``foldr``.

17a) For the next set of comparisons, we're going to use a function called ``const`` that takes two arguments and always returns the first one.
17b) When we fold ``const`` over a list, it will take as its first pair of arguments the ``acc`` value and a value from the list—which value it takes first depends on which type of fold it is.
17c) We'll show you how it evaluates for the first example:

.. 10.5.1, Figure 10, page 364
::

  ·∾ foldr const 0 [1,2,3,4,5]
  (const 1 _)
  1

18a) Since ``const`` doesn't evaluate its second argument, the rest of the fold is never evaluated.
18b) The underscore represents the rest of the unevaluated fold.
18c) Now, let's look at the effect of flipping the arguments.
18d) The ``0`` result is because zero is our accumulator value here, so it's the first (or last) value of the list:

.. 10.5.1, Figure 11, page 364
::

  ·∾ foldr (flip const) 0 [1,2,3,4,5]
  0

19a) Next, let's look at what happens when we use the same functions but this time with ``foldl``.
19b) Take a few moments to understand the evaluation process that leads to these results:

.. 10.5.1, Figure 12, page 364
::

  Prelude> foldl (flip const) 0 [1..5]
  5
  Prelude> foldl const 0 [1..5]
  0

20a) This is the effect of left associativity.
20b) The spine traversal happens in the same order in a left or right fold—it must, because of the way lists are defined.
20c) Depending on your folding function, however, a left fold can lead to a different result than a right fold of the same list.


Exercises: Understanding folds
------------------------------
.. CHAPTER 10. DATA STRUCTURE ORIGAMI 365

1. ``foldr (*) 1 [1..5]`` Will return the same result as which of the following?

  a) ``flip (*) 1 [1..5]``
  b) ``foldl (flip (*)) 1 [1..5]``
  c) ``foldl (*) 1 [1..5]``

2. Write out the evaluation steps for::

     foldl (flip (*)) 1 [1..3]

3. One difference between ``foldr`` and ``foldl`` is:

   a) ``foldr``, but not ``foldl``, traverses the spine of a list from right to left.
   b) ``foldr``, but not ``foldl``, always forces the rest of the fold.
   c) ``foldr``, but not ``foldl``, associates to the right.
   d) ``foldr``, but not ``foldl``, is recursive.

4. Folds are catamorphisms, which means they are generally used to:

   a) Reduce structure.
   b) Expand structure.
   c) Render you catatonic.
   d) Generate infinite data structures.

5. The following are simple folds very similar to what you've already seen, but each has at least one error.
   Please fix and test them in your REPL:

   a) ``foldr (++) ["woot", "WOOT", "woot"]``
   b) ``foldr max [] "fear is the little death"``
   c) ``foldr and True [False, True]``
   d) This one is more subtle than the previous. Can it ever return a different answer?
      ::

        foldr (||) True [False, True]

   e) ``foldl ((++) . show) "" [1..5]``
   f) ``foldr const 'a' [1..5]``

.. CHAPTER 10. DATA STRUCTURE ORIGAMI 366

   g) foldr const 0 "tacos"
   h) foldl (flip const) 0 "burritos"
   i) foldl (flip const) 'z' [1..5]

Unconditional spine recursion
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
An important difference between ``foldr`` and ``foldl`` is that a left fold has the successive steps of the fold as its first argument.
The next recursion of the spine isn't intermediated by the folding function as it is in ``foldr``, which also means recursion of the spine is unconditional.
Having a function that doesn't force evaluation of either of its arguments won't change anything.
Let's review ``const``:


::

  Prelude> const
  1
  Prelude> (flip
  *** Exception:
  Prelude> (flip
  1
  1 undefined
  const) 1 undefined
  Prelude.undefined
  const) undefined 1
  Now compare:
  Prelude> xs = [1..5] ++ undefined
  Prelude> foldr const 0 xs
  1
  Prelude> foldr (flip const) 0 xs
  *** Exception: Prelude.undefined
  Prelude> foldl
  *** Exception:
  Prelude> foldl
  *** Exception:
  const 0 xs
  Prelude.undefined
  (flip const) 0 xs
  Prelude.undefined

However, while ``foldl`` unconditionally evaluates the spine, you can still selectively evaluate the values in the list.
This will throw an error, because the bottom is part of the spine, and ``foldl`` must evaluate the spine:

::

  Prelude> xs = [1..5] ++ undefined

.. CHAPTER 10. DATA STRUCTURE ORIGAMI 367

::

  Prelude> foldl (\_ _ -> 5) 0 xs
  *** Exception: Prelude.undefined
  But this is OK, because bottom is a value here:
  Prelude> xs = [1..5] ++ [undefined]
  Prelude> foldl (\_ _ -> 5) 0 xs
  5

This feature means that ``foldl`` is generally inappropriate with lists that are or could be infinite, but the combination of the forced spine evaluation with non-strictness means that it is also usually inappropriate even for long lists, as the forced evaluation of the spine affects performance negatively.
Because ``foldl`` must evaluate its whole spine before it starts evaluating values in each cell, it accumulates a pile of unevaluated values as it traverses the spine.

In most cases, when you need a left fold, you should use ``foldl'``.
This function, called "fold-l-prime," works the same way, except it is strict.
In other words, it forces evaluation of the values inside the cons cells as it traverses the spine, rather than accumulating unevaluated expressions for each element of a list.
The strict evaluation here means it has less negative effect on performance over long lists.


10.6 How to write fold functions
--------------------------------
When we write folds, we begin by thinking about what our start value for the fold is.
This is usually the identity value for the function.
When we sum the elements of a list, the identity of summation is ``0``.
When we multiply the elements of the list, the identity is ``1``.
This start value is also our fallback in case the list is empty.
Next, we consider our arguments.
A folding function takes two arguments, ``a`` and ``b``, where ``a`` is always going to be one of the elements of the list, and ``b`` is either the start value or the value accumulated as the list is being processed.

Let's say we want to write a function to take the first three letters of each String value in a list of strings and concatenate that result into a final String.
The type of the right fold for lists is:

::

  foldr :: (a -> b -> b) -> b -> [a] -> b

.. CHAPTER 10. DATA STRUCTURE ORIGAMI 368

First, we'll set up the beginnings of our expression:

::

  foldr (\a b -> undefined) []
  ["Pizza", "Apple", "Banana"]

We used an empty list as the start value, but since we plan to return a String as our result, we could be a little more explicit about our intent to build a String and make a small syntactic change:

::

  foldr (\a b -> undefined) ""
  ["Pizza", "Apple", "Banana"]
  Of course, because a String is a list, these are the same value:
  Prelude> "" == []
  True
  But "" signals intent with respect to the types involved:
  Prelude> :t ""
  "" :: [Char]
  Prelude> :t []
  [] :: [t]

Moving along, we next want to work on the function. We already know how to take the first three elements from a list, and we can reuse this for a String:

::

  foldr (\a b -> take 3 a) ""
  ["Pizza", "Apple", "Banana"]
  This will already type check and work, but it doesn't match the
  semantics we ask for:
  Prelude> :{
  *Main| let pab =
  *Main|
  ["Pizza", "Apple", "Banana"]
  *Main| :}
  Prelude> foldr (\a b -> take 3 a) "" pab
  "Piz"
  Prelude> foldl (\b a -> take 3 a) "" pab
  "Ban"

.. CHAPTER 10. DATA STRUCTURE ORIGAMI 369

We're only getting the first three letters of the first or the last string, depending on whether we do a right or left fold.
Note the argument naming order, due to the difference in the types of ``foldr`` and ``foldl``:

::

  foldr :: (a -> b -> b) -> b -> [a] -> b
  foldl :: (b -> a -> b) -> b -> [a] -> b

The problem here is that right now, we're not folding the list.
We're only mapping our take 3 over the list and selecting the first or last result:

::

  Prelude> map (take 3) pab
  ["Piz","App","Ban"]
  Prelude> head $ map (take 3) pab
  "Piz"
  Prelude> last $ map (take 3) pab
  "Ban"

So, let us make this a proper fold and accumulate the result by making use of the ``b`` argument.
Remember, the ``b`` is the start value.
Technically, we could use ``concat`` on the result of having mapped take ``3`` over the list (or its reverse, if we want to simulate ``foldl``):

::

  Prelude> concat $ map (take 3) pab
  "PizAppBan"
  Prelude> rpab = reverse pab
  Prelude> concat $ map (take 3) rpab
  "BanAppPiz"

But we need an excuse to play with ``foldr`` and ``foldl``, so we'll pretend none of this happened!

::

  Prelude> f = (\a b -> take 3 a ++ b)
  Prelude> foldr f "" pab
  "PizAppBan"
  Prelude> f' = (\b a -> take 3 a ++ b)
  Prelude> foldl f' "" pab
  "BanAppPiz"

.. CHAPTER 10. DATA STRUCTURE ORIGAMI 370

Here, we are concatenating the result of having taken three elements from the string value in our input list onto the front of the string we're accumulating.
If we want to be explicit, we can assert types for the values:

::

  Prelude> :{
  *Prelude| let f a b = take 3
  *Prelude|
  (a :: String) ++
  *Prelude|
  (b :: String)
  *Prelude| :}
  Prelude> foldr f "" pab
  "PizAppBan"

If we assert something that isn't true, the type checker catches us:

::

  Prelude> :{
  *Prelude| let f a b = take 3 (a :: String)
  *Prelude|
  ++ (b :: [String])
  *Prelude| :}
  • Couldn't match type ‘[Char]' with ‘Char'
  Expected type: [Char]
  Actual type: [String]
  • In the second argument of ‘(++)', namely
  ‘(b :: [String])'
  In the expression: take 3 (a :: String)
  ++ (b :: [String])
  In an equation for ‘f':
  f a b = take 3 (a :: String)
  ++ (b :: [String])

This can be useful for checking that your mental model of the code is accurate.

Exercises: Database processing
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Let's write some functions to process the following data:

.. CHAPTER 10. DATA STRUCTURE ORIGAMI 371

  ::

    import Data.Time
    data DatabaseItem = DbString String | DbNumber Integer | DbDate UTCTime deriving (Eq, Ord, Show)
    theDatabase :: [DatabaseItem]
    theDatabase = [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
                  , DbNumber 9001
                  , DbString "Hello, world!"
                  , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
                  ]

  1. Write a function that filters for DbDate values and returns a list of the UTCTime values inside them:

      filterDbDate :: [DatabaseItem] -> [UTCTime]
      filterDbDate = undefined

  2. Write a function that filters for DbNumber values and returns a list of the Integer values inside them:

       filterDbNumber :: [DatabaseItem] -> [Integer]
       filterDbNumber = undefined

  3. Write a function that gets the most recent date:

      mostRecent :: [DatabaseItem] -> UTCTime
      mostRecent = undefined

  4. Write a function that sums all of the DbNumber values:

.. CHAPTER 10. DATA STRUCTURE ORIGAMI 372

     sumDb :: [DatabaseItem] -> Integer
     sumDb = undefined

  5. Write a function that gets the average of the DbNumber values:

     -- You'll probably need to use fromIntegral
     -- to get from Integer to Double.
     avgDb :: [DatabaseItem] -> Double
     avgDb = undefined

10.7 Folding and evaluation
---------------------------
What differentiates ``foldr`` and ``foldl`` is associativity.
The right associativity of ``foldr`` means the folding function evaluates from the innermost cons cell to the outermost (the head).
On the other hand, ``foldl`` recurses unconditionally to the end of the list through self-calls, and then the folding function evaluates from the outermost cons cell to the innermost:

::

  Prelude> rcf = foldr (:) []
  Prelude> xs = [1, 2, 3] ++ undefined
  Prelude> take 3 $ rcf xs
  [1,2,3]
  Prelude> lcf = foldl (flip (:)) []
  Prelude> take 3 $ lcf xs
  *** Exception: Prelude.undefined

Let's dive into our const example a little more carefully:

::

  foldr const 0 [1..5]

With ``foldr``, you'll evaluate const 1 (...), but const ignores the rest of the fold that would have occurred from the end of the list up to the number 1, so this returns 1 without having evaluated any more of the values or the spine.
One way you could examine this for yourself would be:

.. CHAPTER 10. DATA STRUCTURE ORIGAMI 373

::

  Prelude> foldr const 0 ([1] ++ undefined)
  1
  Prelude> head ([1] ++ undefined)
  1
  Prelude> tail ([1] ++ undefined)
  *** Exception: Prelude.undefined

Similarly for foldl:

::

  foldl (flip const) 0 [1..5]

Here, ``foldl`` will recurse to the final cons cell, evaluate ``(flip const) (...) 5``, ignore the rest of the fold that would occur from the beginning up to the number ``5``, and return ``5``.

The relationship between foldr and foldl is such that:

::

  foldr f z xs =
  foldl (flip f) z (reverse xs)

But only for finite lists! Consider:

::

  Prelude> xs = repeat 0 ++ [1,2,3]
  Prelude> foldr const 0 xs
  0
  Prelude> xs' = repeat 1 ++ [1,2,3]
  Prelude> rxs = reverse xs'
  Prelude> foldl (flip const) 0 rxs
  ^CInterrupted.
  -- ^^ bottom.

If we flip our folding function f and reverse the list xs, foldr and foldl will return the same result:

::

  Prelude> xs = [1..5]
  Prelude> foldr (:) [] xs
  [1,2,3,4,5]
  Prelude> foldl (flip (:)) [] xs
  [5,4,3,2,1]
  Prelude> foldl (flip (:)) [] (reverse xs)
  [1,2,3,4,5]

.. CHAPTER 10. DATA STRUCTURE ORIGAMI 374

::

  Prelude> reverse $ foldl (flip (:)) [] xs
  [1,2,3,4,5]


10.8 Summary
------------
We presented a lot of material in this chapter. You might be feeling a
little weary of folds right now. So what's the executive summary?

``foldr``
^^^^^^^^^
1. The rest of the fold (recursive invocation of ``foldr``) is an argument to the folding function you pass to ``foldr``.
   It doesn't directly self-call as a tail-call like ``foldl``.
   You could think of it as alternating between applications of ``foldr`` and your folding function ``f``.
   The next invocation of ``foldr`` is conditional on ``f`` having asked for more of the results of having folded the list.

   That is:

   ::

      foldr :: (a -> b -> b) -> b -> [a] -> b
      -- ^

   That ``b`` we're pointing at in ``(a -> b -> b)`` is the rest of the fold.
   Evaluating that evaluates the next application of ``foldr``.

2. Associates to the right.

3. Works with infinite lists. We know this because:

   ::

     Prelude> foldr const 0 [1..]
     1

4. Is a good default choice whenever you want to transform data structures, be they finite or infinite.

``foldl``
^^^^^^^^^
1. Self-calls (using tail calls) through the list, only beginning to produce values after reaching the end of the list.

2. Associates to the left.

3. Cannot be used with infinite lists. Try the infinite list example earlier, and your REPL will hang.

.. CHAPTER 10. DATA STRUCTURE ORIGAMI 375

4. Is nearly useless and should almost always be replaced with foldl' for reasons we'll explain later when we talk about writing efficient Haskell programs.

10.9 Scans
----------
Scans, which we have mentioned above, work similarly to maps and also to folds. Like folds, they accumulate values instead of keeping a list's individual values separate. Like maps, they return a list of results. In this case, the list of results shows the intermediate stages of evaluation, that is, the values that accumulate as the function is doing its work.

Scans are not used as frequently as folds, and once you under- stand the basic mechanics of folding, there isn't a whole lot new to understand. Still, it is useful to know about them and get an idea of why you might need them.3

First, let's take a look at the types. We'll do a direct comparison of
the types of folds and scans, so the differences are clear:

::

  foldr :: (a -> b -> b) -> b -> [a] -> b
  scanr :: (a -> b -> b) -> b -> [a] -> [b]
  foldl :: (b -> a -> b) -> b -> [a] -> b
  scanl :: (b -> a -> b) -> b -> [a] -> [b]

The primary difference is that the final result is a list (a fold can return a list as a result, as well, but they don't always).
This means that they are not catamorphisms and, in an important sense, aren't folds at all.
But no matter!
The type signatures are similar, and the routes of spine traversal and evaluation are similar.
This does mean that you can use scans in places where you can't use a fold, precisely because you return a list of results rather than reducing the spine of the list.

The results that scans produce can be represented like this:

::

  scanr (+) 0 [1..3]

.. 3 The truth is that scans are not used often, but there are times when you want to fold a function over a list and return a list of the intermediate values that you can then use as input to some other function.
   For a particularly elegant use case, please see Chris Done's blog post: http://chrisdone.com/posts/twitter-problem-loeb.

.. CHAPTER 10. DATA STRUCTURE ORIGAMI 376

::

  [1 + (2 + (3 + 0)), 2 + (3 + 0), 3 + 0, 0]
  [6, 5, 3, 0]
  scanl (+) 0 [1..3]
  [0, 0 + 1,0 + 1 + 2, 0 + 1 + 2 + 3]
  [0, 1, 3, 6]
  scanl (+) 1 [1..3]
  -- unfolding the
  -- definition of scanl
  = [ 1, 1 + 1
  , (1 + 1) + 2
  , ((1 + 1) + 2) + 3
  ]
  -- evaluating addition
  = [1, 2, 4, 7]

Then, to make this more explicit and properly equational, we can follow along with how scanl expands for this expression based on the definition.
First, we must see how scanl is defined.
We're going to show you a version of it from a slightly older base library for GHC Haskell.
The differences don't change anything important for us here:

scanl :: (a -> b -> a) -> a -> [b] -> [a]
scanl f q ls =
q : (case ls of
[]
-> []
x:xs -> scanl f (f q x) xs)
In an earlier chapter, we wrote a recursive function that returns
the nth Fibonacci number. You can use a scan function to return a list
of Fibonacci numbers. We're going to do this in a source file, because
it will, in this state, return an infinite list (feel free to try loading it
into your REPL and running it, but be quick with the Ctrl-C):
fibs = 1 : scanl (+) 1 fibs

.. CHAPTER 10. DATA STRUCTURE ORIGAMI 377
We start with a value of 1 and cons that onto the front of the list
generated by our scan. The list itself has to be recursive, because, as
we saw previously, the idea of Fibonacci numbers is that each one is
the sum of the previous two in the sequence; scanning the results of
+ over a non-recursive list of numbers whose start value is 1 would
give us this:
scanl (+) 1 [1..3]
[1, 1 + 1, (1 + 1) + 2, ((1 + 1) + 2) + 3]
[1,2,4,7]
Instead of the [1, 1, 2, 3, 5, ...] that we're looking for.
Getting the Fibonacci number we want
But we don't really want an infinite list of Fibonacci numbers; that
isn't very useful. We need a method to either take some number
of elements from that list or find the nth element as we did before.
Fortunately, that's the easy part. We'll use the "bang bang" operator,
!!, to find the nth element. This operator is a way to index into a list,
and indexing in Haskell starts from 0. That is, the first value in your
list is indexed as 0. But, otherwise, the operator is straightforward:
(!!) :: [a] -> Int -> a
It needs a list as its first argument, an Int as its second argument,
and it returns one element from the list. Which item it returns is
the value that is in the nth spot, where n is our Int. Let's modify our
source file:
fibs
= 1 : scanl (+) 1 fibs
fibsN x = fibs !! x
Once we load the file into our REPL, we can use fibsN to return
the nth element of our scan:
Prelude> fibsN 0
1
Prelude> fibsN 2
2
Prelude> fibsN 6
13
.. CHAPTER 10. DATA STRUCTURE ORIGAMI 378
Now, you can modify your source code to use the take or takeWhile
functions or to filter it in any way you like. One note: filtering without
also taking won't work too well, because you're still getting an infinite
list. It's a filtered infinite list, sure, but still infinite.
Scans exercises
1. Modify your fibs function to only return the first 20 Fibonacci
numbers.
2. Modify fibs to return the Fibonacci numbers that are less than
100.
3. Try to write the factorial function from Chapter 8 as a scan.
You'll want scanl again, and your start value will be 1. Warning:
this will also generate an infinite list, so you may want to pass it
through a take function or similar.


.. NOTE Pay special attention to the paragraph layout in the chapter exercises.
10.10 Chapter exercises
-----------------------

10.10.1 Warm-up and review
^^^^^^^^^^^^^^^^^^^^^^^^^^
For the following set of exercises, you are not expected to use folds.
These are intended to review material from previous chapters. Feel
free to use any syntax or structure from previous chapters that seems
appropriate.

1. Given the following sets of consonants and vowels:

   ::

     stops = "pbtdkg"
     vowels = "aeiou"

a) Write a function that takes inputs from stops and vowels and makes 3-tuples of all possible stop-vowel-stop combinations. These will not all correspond to real words in English, although the stop-vowel-stop pattern is common enough that many of them will.

b) Modify that function so that it only returns the combinations that begin with a p.

.. CHAPTER 10. DATA STRUCTURE ORIGAMI 379

c) Now set up lists of nouns and verbs (instead of stops and vowels), and modify the function to make tuples representing possible noun-verb-noun sentences.

2. What does the following mystery function do? What is its type? Try to get a good sense of what it does before you test it in the REPL to verify it:

seekritFunc x =
div (sum (map length (words x)))
(length (words x))

3. We'd really like the answer to be more precise. Can you rewrite that using fractional division?

10.10.2 Rewriting functions using folds
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
In the previous chapter, you wrote these functions using direct recursion over lists. The goal now is to rewrite them using folds. Where possible, to gain a deeper understanding of folding, try rewriting the fold version so that it is point-free. Point-free versions of these functions written with a fold should look like this:
myFunc = foldr f z
So, for example, with the and function:
-- direct recursion, not using &&
myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs) =
if x == False
then False
else myAnd xs
-- direct recursion, using &&
myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs) = x && myAnd xs

.. CHAPTER 10. DATA STRUCTURE ORIGAMI 380

-- fold, not point-free
myAnd :: [Bool] -> Bool
myAnd = foldr
(\a b ->
if a == False
then False
else b) True
-- fold, both myAnd and the folding
-- function are point-free now
myAnd :: [Bool] -> Bool
myAnd = foldr (&&) True
The goal here is to converge on the final version where possible.
You don't need to write all variations for each example, but the
more variations you write, the deeper your understanding of these
functions will become.
1. myOr returns True if any Bool in the list is True:
myOr :: [Bool] -> Bool
myOr = undefined
2. myAny returns True if a -> Bool applied to any of the values in the
list returns True:
myAny :: (a -> Bool) -> [a] -> Bool
myAny = undefined
Example for validating myAny:
Prelude> myAny even [1, 3, 5]
False
Prelude> myAny odd [1, 3, 5]
True
3. Write two versions of myElem. One version should use folding
and the other should use any:
myElem :: Eq a => a -> [a] -> Bool

.. CHAPTER 10. DATA STRUCTURE ORIGAMI 381

Prelude> myElem 1 [1..10]
True
Prelude> myElem 1 [2..10]
False
4. Implement myReverse. Don't worry about trying to make it lazy:
myReverse :: [a] -> [a]
myReverse = undefined
Prelude> myReverse "blah"
"halb"
Prelude> myReverse [1..5]
[5,4,3,2,1]
5. Write myMap in terms of foldr. It should have the same behavior
as the built-in map:
myMap :: (a -> b) -> [a] -> [b]
myMap = undefined
6. Write myFilter in terms of foldr. It should have the same behav-
ior as the built-in filter:
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter = undefined
7. squish flattens a list of lists into a list:
squish :: [[a]] -> [a]
squish = undefined
8. squishMap maps a function over a list and concatenates the result:
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap = undefined
Prelude>
[1,2,3]
Prelude>
Prelude>
"WO b OT
squishMap (\x -> [1, x, 3]) [2]
f x = "WO " ++ [x] ++ " OT "
squishMap f "blah"
WO l OT WO a OT WO h OT "

.. CHAPTER 10. DATA STRUCTURE ORIGAMI 382

9. squishAgain flattens a list of lists into a list. This time, re-use the
squishMap function:
squishAgain :: [[a]] -> [a]
squishAgain = undefined
10. myMaximumBy takes a comparison function and a list and returns
the greatest element of the list based on the last value that the
comparison returns GT for:
myMaximumBy :: (a -> a -> Ordering)
-> [a]
-> a
myMaximumBy = undefined
Prelude> myMaximumBy (\_ _ -> GT) [1..10]
1
Prelude> myMaximumBy (\_ _ -> LT) [1..10]
10
Prelude> myMaximumBy compare [1..10]
10
11. myMinimumBy takes a comparison function and a list and returns
the least element of the list based on the last value that the
comparison returns LT for:
myMinimumBy :: (a -> a -> Ordering)
-> [a]
-> a
myMinimumBy = undefined
Prelude> myMinimumBy (\_ _ -> GT) [1..10]
10
Prelude> myMinimumBy (\_ _ -> LT) [1..10]
1
Prelude> myMinimumBy compare [1..10]
1
.. CHAPTER 10. DATA STRUCTURE ORIGAMI 383


10.11 Definitions
-----------------
1. A fold is a higher-order function which, given a function to accumulate the results and a
   recursive data structure, returns the built up value. Usually a "start value" for the
   accumulation is provided along with a function that can combine the type of values in the data
   structure with the accumulation. The term fold is typically used with reference to collections of
   values referenced by a recursive datatype. For a generalization of "breaking down structure," see
   catamorphism.

2. A catamorphism is a generalization of folds to arbitrary datatypes.  Where a fold allows you to
   break down a list into an arbitrary datatype, a catamorphism is a means of breaking down the
   structure of any datatype. The bool :: a -> a -> Bool -> a func- tion in Data.Bool is an example
   of a simple catamorphism for a simple, non-collection datatype. Similarly, maybe :: b -> (a -> b)
   -> Maybe a -> b is the catamorphism for Maybe. See if you can notice a pattern:

     data Bool = False | True
     bool :: a -> a -> Bool -> a
     data Maybe a = Nothing | Just a
     maybe :: b -> (a -> b) -> Maybe a -> b
     data Either a b = Left a | Right b
     either :: (a -> c) -> (b -> c) -> Either a b -> c

3. A tail call is the final result of a function. Some examples of tail
calls in Haskell functions:

::

f x y z = h (subFunction x y z)
where subFunction x y z = g x y z
-- the "tail call" is
-- h (subFunction x y z)
-- or, more precisely, h

.. CHAPTER 10. DATA STRUCTURE ORIGAMI 384

4. Tail recursion occurs in a function whose tail calls are recursive
invocations of itself. This is distinguished from functions that
call other functions in their tail call. For example:

f x y z = h (subFunction x y z)
where subFunction x y z = g x y z

The above is not tail recursive, since it calls h, not itself.

f x y z = h (f (x - 1) y z)

Still not tail recursive. f is invoked again but not in the tail call of f. It's an argument to the tail call, h:

f x y z = f (x - 1) y z

This is tail recursive. f is calling itself directly with no intermediaries.

foldr f z [] = z
foldr f z (x:xs) = f x (foldr f z xs)

Not tail recursive—we give up control to the combining function f before continuing through the list. foldr's recursive calls will bounce between foldr and f.

foldl f z [] = z
foldl f z (x:xs) = foldl f (f z x) xs

Tail recursive. foldl invokes itself recursively. The combining function is only an argument to the recursive fold.

10.12 Follow-up resources
-------------------------
1. Antoni Diller. Introduction to Haskell. Unit 6.  http://www.cantab.net/users/antoni.diller/haskell/haskell.html
2. Graham Hutton. A tutorial on the universality and expressiveness of fold.  http://www.cs.nott.ac.uk/~gmh/fold.pdf.
