-- 16.3 There's a whole lot of fmap goin' round
-- page 625, after paragraph 2 on that page


-- "We can see how the type of fmap specializes to
-- different types here:"

-- Where do these types come from? They aren't defined
-- earlier in this chapter.
type E e = Either e
type C e = Constant e
type I   = Identity

{--  Functor f =>
fmap :: (a -> b) ->      f a ->      f b
     :: (a -> b) ->     [] a ->     [] b
     :: (a -> b) ->  Maybe a ->  Maybe b
     :: (a -> b) ->    E e a ->    E e b
     :: (a -> b) ->   (e,) a ->   (e,) b
     :: (a -> b) ->      I a ->      I b
     :: (a -> b) ->    C e a ->    C e b
-}


{-
-- "If you are using GHC 8 or newer, you can also see
-- this for yourself in your REPL by doing this:"

:set -XTypeApplications
:type fmap @Maybe
:type fmap @(Either _)

-- What is TypeApplications, what does it do, and how do
-- I use it?
-}
