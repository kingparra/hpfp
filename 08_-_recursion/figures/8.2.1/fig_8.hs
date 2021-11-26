applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b
applyTimes 0 f b = b
applyTimes n f b = f (applyTimes (n-1) f b)


incTimes' :: (Eq a, Num a) => a -> a -> a
incTimes' times n = applyTimes times (+1) n
