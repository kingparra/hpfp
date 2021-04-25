module SomethlingLikeFmap where


-- fmap :: Functor f
--      => (a -> b)   -> f a -> f b
-- traverse :: (Traversable t, Applicative f)
--      => (a -> f b) -> t a -> f (t b)


myData :: [String]
myFunc :: String -> IO Record


wrong :: [IO Record]
wrong = fmap myFunc myData


right :: IO [Record]
right = traverse myFunc myData
