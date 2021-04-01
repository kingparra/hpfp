-- m ~ Either e
(>>=) :: Monad m =>  m a -> (a -> m)          -> m b
(>>=) ::      Either e a -> (a -> Either e b) -> Either e b

return :: Monad m => a -> m a
return ::            a -> Either e a
