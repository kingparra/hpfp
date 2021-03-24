-- 16.2 What's a functor, page 628, fig 1
--
--
--              The input
--           type constructor
--         must  take  one type
--         argument  to  become
--           a concrete type.
--            vvvvvvvvvvvvv
class Functor (f :: * -> *) where
  --
  --
  --               This is the
  --               functorial
  --               type   that
  --               contains a.
  --               (By  taking    May be a differnt
  --                it  as  a       type than a.
  --                type arg.)       /
  --                    v           v
  fmap :: (a -> b)  ->  f a  ->  f b
  --      ^^^^^^^^               ^
  --     Function to             |
  --   perform  on  the    This is the same f!
  --    enclosed type.
  --
  --
  -- ⍘(<$)⍘ Has a default definition of ⍘fmap . const⍘.
  (<$) :: a -> f b -> f a
  --
  --
  -- We only need fmap for a complete definition of
  -- this type class (though we should be nice and
  -- follow the Functor laws).
  {-# MINIMAL fmap #-}
