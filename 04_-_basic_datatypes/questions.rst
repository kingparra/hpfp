*********************
 Chapter 4 Questions
*********************

Why does ``length (1,2)`` return ``1`` rather than ``2``?

  The instance of ``Foldable`` is written that way for ``(,)``. I should try
  writing my own version of this instance.

  http://haskell.1045720.n5.nabble.com/Foldable-for-td5856275.html

When should I use tuples rather than record types or other datatypes? Is there a good rule of
thumb?

  [10:26] <justsomeguy> What's a good rule of thumb for when to use tuples
  rather than record types or a data structure like Map?

  [10:27] <justsomeguy> (Tuples seem strange to me, so I'd like to only use them
  when appropriate. “length (1,2)” or “map (+3) (1,2)” don't do what I'd expect,
  for example.)

  [10:29] <Cale> justsomeguy: With respect to the Functor/Traversable/Foldable
  instances, you should think of pairs as being containers that hold a single
  element (the last component) with an additional "label" (the first component)

  [10:30] <Cale> justsomeguy: They're good as abstract things, but often you
  will want to transition to defining a proper data type once you have good
  names for the fields and especially if there are more than two of them

  [10:34] <justsomeguy> Cale: Would you say it's a good idea to use tuples
  mostly to facilitate passing around small collections of short-lived things
  using pattern matching?

  [10:35] <Cale> justsomeguy: I guess, though the same thing could be said of
  almost any user defined data type as well, so I don't know

  [10:38] <justsomeguy> I guess I'll just have to get a feel for it through
  experience. Your tip about Functor/Traversable/Foldable helps. Thanks Cale :)

  [10:38] <sshine> justsomeguy, I might use a record if I'm passing a value past
  a boundary that is somewhat a public API... when to name things is probably
  not specific to Haskell, or even FP.

  [10:39] <Cale> justsomeguy: Yeah, if you think of (k,v) as being a "key/value"
  pair, it might make more sense how e.g. fmap acts on the value, but not the
  key

  [10:39] <sshine> justsomeguy, if you can resolve your tupling in the same line
  as you introduce them, that's excellent. :)

  [10:39] <Cale> justsomeguy: But also, it works the way that it does because
  that is literally the only legal thing it could do -- if you try writing the
  instance, you'll see, the type system enforces it.

  So, I guess tuples are useful in the same way that lambdas are -- as anonymous
  short lived structures. Or, in other words, I should probably use them when it
  doesn't make sense to introduce a new name binding. I'm sure there's more to
  the subject.

How does floating-point arithmetic work?

How are negative numbers represented? What is twos complement?

Why can't I make a Ratio of Ratios?
