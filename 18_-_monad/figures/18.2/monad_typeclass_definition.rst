.. Type class definition of Monad, from page 739

|  class Applicative m => **Monad** (m :: \* -> \*) where
|    (**>>=**) :: m a -> (a -> m b) -> m b
|    (**>>**) :: m a -> m b -> m b
|    **return** :: a -> m a
|    {-# MINIMAL (>>=) #-}
|
|  instance Monad **(Either e)**
|  instance Monad **[]**
|  instance Monad **Maybe**
|  instance Monad **IO**
|  instance Monad **((->) r)**
|  instance Monoid a => **Monad ((,) a)**
