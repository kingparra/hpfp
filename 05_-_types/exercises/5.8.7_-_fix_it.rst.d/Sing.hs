module Sing where
-- 1
fstString :: String -> String
fstString x = x ++ " in the rain"

sndString :: String -> String
sndString x = x ++ " over the rainbow"

sing = if x > y then fstString x else sndString y
  where x = "Singin"
        y = "Somewhere"
-- 2
sing' = if x > y then sndString y else fstString x
  where x = "Singin"
        y = "Somewhere"
