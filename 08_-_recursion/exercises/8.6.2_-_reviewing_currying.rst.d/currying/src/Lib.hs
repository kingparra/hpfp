module Lib where


-- _(++" mrow "++)_
cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y


flippy :: String -> String -> String
flippy = flip cattyConny


-- ("woops mrow"++)_
appedCatty :: String -> String
appedCatty = cattyConny "woops"


-- _(++" mrow haha")
frappe :: String -> String
frappe = flippy "haha"
