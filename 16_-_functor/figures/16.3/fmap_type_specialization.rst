type E e = Either e
type C e = Constant e
type I = Identity

{-
-- Functor f =>
fmap :: (a -> b) ->      f a ->      f b
     :: (a -> b) ->     [] a ->     [] b
     :: (a -> b) ->  Maybe a ->  Maybe b
     :: (a -> b) ->    E e a ->    E e b
     :: (a -> b) ->   (e,) a ->   (e,) b
     :: (a -> b) ->      I a ->      I b
     :: (a -> b) ->    C e a ->    C e b

-- Try this
:set -XTypeApplications
:type fmap @Maybe
-}
