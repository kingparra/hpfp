inc :: Num a => a -> a
inc = (+1)

three = inc . inc . inc $ 0
-- different syntax, same thing
three' = (inc . inc . inc) 0
