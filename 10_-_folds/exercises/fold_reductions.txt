*****************
 Fold Reductions
*****************



-- Right fold and scan
----------------------
foldr f z l =
  case l of
    [] -> z
    (x:xs) -> x `f` foldr f z xs

scanr f z l =
  case l of
    [] -> [z]
    (x:xs) -> foldr f z l : scanr f z xs



-- Left fold and scan
---------------------
foldl f z l =
  case l of
    [] -> z
    (x:xs) -> foldl f (f z x) xs

scanl f z l =
  z : (case l of
         [] -> []
         (x:xs) -> scanl f (f z x) xs)

-- seq is a primitive system function that when applied to x and y will first reduce x then return y.
-- The idea is that y references x so that when y is reduced x will not be a big unreduced chain anymore.
-- https://hackage.haskell.org/package/ghc-prim-0.10.0/docs/GHC-Prim.html#v:seq
foldl' f z l =
  case l of
    [] -> z
    (x:xs) -> seq (f z x) (foldl' f (f z x) xs)


-- Evaluation of foldr (+) 0 [1,2,3]
------------------------------------
foldr (+) 0 [1,2,3] =
  case [1,2,3] of
    [] -> 0
    (1:[2,3]) -> (+) 1 (foldr (+) 0 [2,3] =
                          case [2,3] of
                            [] -> 0
                            (2:[3]) -> (+) 2 (foldr (+) 0 [3] =
                                                case [3] of
                                                  [] -> 0
                                                  (3:[]) -> (+) 3 (foldr (+) 0 [] =
                                                                     case [] of
                                                                       [] -> 0)))
-- (+) 1 ((+) 2 ((+) 3 0))
-- (+) 1 ((+) 2 3)
-- (+) 1 5
-- 6



-- Evaluation of scanr (+) 0 [1,2,3]
------------------------------------
scanr (+) 0 [1,2,3] =
  case l of
    [] -> [z]
    (1:[2,3]) -> foldr (+) 0 [1,2,3] : (scanr (+) 0 [2,3] =
                                          case [2,3] of
                                            [] -> [0]
                                            (2:[3]) -> foldr (+) 0 [2,3] : (scanr (+) [3] =
                                                                              case [3] of
                                                                                [] -> [0]
                                                                                (3:[]) -> foldr (+) 0 [3] : (scanr (+) 0 [] =
                                                                                                               case [] of
                                                                                                                 [] -> [0])))
-- foldr (+) 0 [1,2,3] : foldr (+) 0 [2,3] : foldr (+) 0 [3] : [0]
-- [foldr (+) 0 [1,2,3], foldr (+) 0 [2,3], foldr (+) 0 [3], 0]
-- [6, 5, 3, 0]



-- Evaluation of foldl (+) 0 [1,2,3]
------------------------------------
foldl (+) 0 [1,2,3] =
  case [1,2,3] of
    [] -> 0
    (1:[2,3]) -> foldl  (+)  ((+) 0 1)  [2,3] =
                   case [2,3] of
                     [] -> ((+) 0 1)
                     (2:[3]) -> foldl  (+)  ((+) ((+) 0 1) 2)  [3] =
                                  case [3] of
                                    [] -> ((+) ((+) 0 1) 2)
                                    (3:[]) -> foldl  (+)  ((+) ((+) ((+) 0 1) 2) 3)  [] =
                                                 case [] of
                                                   [] -> ((+) ((+) ((+) 0 1) 2) 3)
-- ((+) ((+) ((+) 0 1) 2) 3)
-- ((+) ((+) 1 2) 3)
-- ((+) 3 3)
-- 6



-- Evaluation of scanl (+) 0 [1,2,3]
------------------------------------
scanl (+) 0 [1,2,3] =
  0 : (case [1,2,3] of
         [] -> []
         (1:[2,3]) -> scanl (+) ((+) 0 1) [2,3] =
                        ((+) 0 1) : (case [2,3] of
                                       [] -> []
                                       (2:[3]) -> scanl (+) ((+) ((+) 0 1) 2) [3] =
                                                    ((+) ((+) 0 1) 2) : (case [3] of
                                                                           [] -> []
                                                                           (3:[]) -> scanl (+) ((+) ((+) ((+) 0 1) 2) 3) [] =
                                                                                      ((+) ((+) ((+) 0 1) 2) 3) : (case [] of
                                                                                                                     [] -> [])))
-- 0 : ((+) 0 1) : ((+) ((+) 0 1) 2) : ((+) ((+) ((+) 0 1) 2) 3) : []
-- [0, ((+) 0 1), ((+) ((+) 0 1) 2), ((+) ((+) ((+) 0 1) 2) 3)]
-- [0, 1, 3, 6]



-- Evaluation of foldl' (+) 0 [1,2,3]
-------------------------------------
foldl' (+) 0 [1,2,3] =
  case [1,2,3] of
    [] -> 0
    --                   1
    --               vvvvvvvvv
    (1:[2,3]) -> seq ((+) 0 1) (foldl' (+) ((+) 0 1) [2,3] =
                                  case [2,3] of
                                    [] -> ((+) 0 1)
                                    --                     3
                                    --             vvvvvvvvvvvvvvvvv
                                    (2:[3]) -> seq ((+) ((+) 0 1) 2) (foldl' (+) ((+) ((+) 0 1) 2) [3] =
                                                                        case [3] of
                                                                          [] -> ((+) ((+) 0 1) 2)
                                                                          --                       6 
                                                                          --            vvvvvvvvvvvvvvvvvvvvvvvvv
                                                                          (3:[]) -> seq ((+) ((+) ((+) 0 1) 2) 3) (foldl' (+) ((+) ((+) ((+) 0 1) 2) 3) [] =
                                                                                                             case [] of
                                                                                                               -- thunk was already reduced at each seq
                                                                                                               --    vvvvvvvvvvvvvvvvvvvvvvvvvvvv
                                                                                                               [] -> ((+) ((+) ((+) 0 1) 2) 3))))
-- 1
-- 3
-- 6
