fmap  :: Functor f
      =>    (a -> b) -> f a -> f b

(<*>) :: Applicative f
      =>  f (a -> b) -> f a -> f b

(>>=) :: Monad f
      =>  f a -> (a -> f b) -> f b
