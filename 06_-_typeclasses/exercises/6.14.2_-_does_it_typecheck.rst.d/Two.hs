data Mood = Blah | Woot deriving Show

settleDown x = if x == Woot then Blah else x
