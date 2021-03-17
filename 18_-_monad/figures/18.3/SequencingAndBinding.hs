module SequencingAndBinding where
import Control.Applicative ((*>))
-- 177


sequencing :: IO ()
sequencing = do
  putStrLn "blah"
  putStrLn "another thing"


sequencing' :: IO ()
sequencing' =
  putStrLn "blah" >>
  putStrLn "another thing"


sequencing'' :: IO ()
sequencing'' =
  putStrLn "blah" *>
  putStrLn "another thing"


binding :: IO ()
binding = do
  name <- getLine
  putStrLn name


binding' :: IO ()
binding' =
  getLine >>= putStrLn
