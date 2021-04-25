import Control.Monad ((<=<))
-- 21.7 Axing tedious code
-- This figure is not runnable
-- page 840



-- Thanks for the great example, Alex
data Query = Query
data SomeObj = SomeObj
data IoOnlyObj = IoOnlyObj
data Err = Err


-- There's a decoder function that makes some
-- object from String
decodeFn :: String -> Either Err SomeObj
decodeFn = undefined


-- There's a query, that runs agains the DB
-- and returns an array of strings
fetchFn :: Query -> IO [String]
fetchFn = undefined


-- an additional "context initializer", that also has IO
makeIoOnlyObj :: [SomeObj] -> IO [(SomeObj, IoOnlyObj)]
makeIoOnlyObj = undefined


-- before
pipelineFn :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn query = do
  a <- fetchFn query
  case sequence (map decodeFn a) of
    (Left err) -> return $ Left $ err
    (Right res) -> do
      a <- makeIoOnlyObj res
      return $ Right a

{-
 - The objective was to clean up this code. A
 - few things made them suspicious:
 -
 - 1. The use of sequence and map.
 - 2. Manually casing on the result of the
 -    sequence and the map.
 - 3. Binding monadically over the Either only
 -    to perform another monadic (IO) action
 -    inside of that.
 -
 - We pared the pipeline function down to
 - this:
 -}


pipelineFn' :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn' query = do
  a <- fetchFn query
  traverse makeIoOnlyObj (mapM decodeFn a)


-- We can make it pointfree if we want to.
pipelineFn'' :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn'' =
  ( traverse makeIoOnlyObj . mapM decodeFn =<< ) . fetchFn


-- ...and since mapM is just traverse with a
-- slightly different type:
pipelineFn''' :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn''' =
  ( traverse makeIoOnlyObj . traverse decodeFn =<< ) . fetchFn


-- Finally, since hlint won't stop yelling at me...
pipelineFn'''' :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn'''' =
  traverse makeIoOnlyObj . traverse decodeFn <=< fetchFn
