fmap         :: Functor f
             => (a -> b) -> f a -> f b

(flip (>>=)) :: Monad m
             => (a -> m b) -> m a -> m b

traverse     :: (Traversable t, Applicative f)
             => (a -> f b) -> t a -> f (t b)
