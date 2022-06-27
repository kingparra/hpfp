**********************
 Chapter 10 Questions
**********************

Can I define ``foldr`` from memory?

Can I define ``foldl`` from memory?

How would I define ``scanl`` and ``scanr``?

::

  scanl f z []     = []
  scanl f z (x:xs) = z : scanl f (f z x) xs

  foldl f z []     = []
  foldl f z (x:xs) =     foldl f (f z x) xs

  scanr f z []     = [z]
  scanr f z (x:xs) = f x xs : (scanr f z xs)

Can I write out the evaluation steps for ``foldr``, ``foldl``,
``scanr``, and ``scanl``?

Why will ``foldr`` always evaluate the first cons cell?

When should I use ``foldr`` rather than ``foldl``?

Under what circumstances should I use ``foldl'`` rather than ``foldl``?

Which folding function forces evaluation of the lists entire spine?

Which folding function makes evaluation of the spine conditional on the input function?

What are some examples of folds on other data structures like trees or vectors or maps?

What do primary sources (from category theory) say a catamorphism is? I bet it's a different definition than what HPFP says it is.

Which learning objectives does each section correspond to?

Can I remember how the sections are arranged?

What prior material does each section depend on?

What abilities does the reader gain by learning the contents of this chapter?

What programming constructs are analogous to folds?

* How do I do a foldr/foldl/foldl' in javascript and python.
