-- 8.6.2 Reviewing currying, Paragraph 1, Figure 1, top of page 295.

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

-- fill in the types

flippy = flip cattyConny

appedCatty = cattyConny "woops"
frappe = flippy "haha"
