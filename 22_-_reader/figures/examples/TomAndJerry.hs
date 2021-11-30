import Control.Monad.Reader

tom = do
  -- ask allows you to ask for the environment
  -- ask :: MonadReader r m => m r
  env <- ask
  return (env ++ " This is Tom.")

jerry = do
  env <- ask
  return (env ++ " This is Jerry.")

tomAndJerry :: Reader String String
tomAndJerry = do
  t <- tom
  j <- jerry
  return (t++"\n"++j)

runJerryRun :: String
runJerryRun = (runReader tomAndJerry) "Who is this?"
