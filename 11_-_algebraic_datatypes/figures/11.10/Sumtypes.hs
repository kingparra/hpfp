-- 11.10 Sum types
-- page 411
-- The "|" in type signatures represents "+" in the algebra of types.
data Bool = False | True

-- page 412
{-
 - True | False = ??
 - True + False == ??
 -
 - -- False and True both == 1
 - 1 + 1 == ??
 -
 - -- We see that the cardinality of Bool is:
 - 1 + 1 == 2
 -
 - -- List of all possible values for Bool
 - [True, False] -- length is 2
 -
 - Prelude> length (enumFrom False)
 - 2
 -
 -}
