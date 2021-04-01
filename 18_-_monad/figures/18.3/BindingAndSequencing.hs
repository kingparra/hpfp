module BindingAndSequencing where
import Control.Applicative ((*>))
-- page 751


bindingAndSequencing :: IO ()
bindingAndSequencing = do
  putStrLn "name pls:"
  name <- getLine
  putStrLn ("y helo thar: " ++ name)


bindingAndSequencing' :: IO ()
bindingAndSequencing' =
  putStrLn "name pls:" >>
  getLine >>=
  \name ->
    putStrLn ("y helo thar: " ++ name)


twoBinds :: IO ()
twoBinds = do
  putStrLn "name pls:"
  name <- getLine
  putStrLn "age pls:"
  age <- getLine
  putStrLn ("y helo thar: "
            ++ name ++ " who is: "
            ++ age ++ " years old.")


twoBinds' :: IO ()
twoBinds' =
  putStrLn "name pls:" >>
  getLine >>=
  \name ->
  putStrLn "age pls:" >>
  getLine >>=
  \age ->
  putStrLn ("y helo thar: "
            ++ name ++ " who is: "
            ++ age ++ " years old.")
