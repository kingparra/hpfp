module LookupTables where
import Control.Applicative
-- 17.5.2 What's the List applicative do?
-- page 694


f x = lookup x [ (3, "hello")
               , (4, "julie")
               , (5, "kbai")
               ]


g y = lookup y [ (7, "sup?")
               , (8, "chris")
               , (9, "aloha")
               ]


h z = lookup z [ (2, 3)
               , (5, 6)
               , (7, 8)
               ]


m x = lookup x [ (4, 10)
               , (8, 13)
               , (1, 9001)
               ]
