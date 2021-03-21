-- m ~ Maybe
(>>=) :: Monad m =>     m a -> (a ->     m b) -> m b
(>>=) ::            Maybe a -> (a -> Maybe b) -> Maybe b

-- same as pure
return :: Monad m => a ->     m a
return ::            a -> Maybe a
