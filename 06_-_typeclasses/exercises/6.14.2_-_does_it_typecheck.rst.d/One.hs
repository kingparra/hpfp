data Person = Person Bool

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)
