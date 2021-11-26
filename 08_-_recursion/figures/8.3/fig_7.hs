f :: Bool -> Int
f False = 0
f _ = error $ "*** Exception: "
           ++ "Non-exhaustive"
           ++ "patters in function f"
