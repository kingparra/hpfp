zero :: Int -> Int
zero 0 = 0
zero n = zero (n - 1)
