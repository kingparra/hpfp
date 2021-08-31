import Control.Monad.Reader
-- https://gist.github.com/egonSchiele/5752172

hello :: Reader String String
hello = do
    name <- ask
    return ("hello, " ++ name ++ "!")

bye :: Reader String String
bye = do
    name <- ask
    return ("bye, " ++ name ++ "!")

convo :: Reader String String
convo = do
    c1 <- hello
    c2 <- bye
    return $ c1 ++ " " ++ c2

main = do
  putStr "Enter your name: "
  name <- getLine
  (print . runReader convo) name
