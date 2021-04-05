#!/usr/bin/env stack
{- stack script
   --install-ghc
   --resolver lts-17.7
   --package scotty
-}
{-# LANGUAGE OverloadedStrings #-}
-- https://hackage.haskell.org/package/scotty
import Web.Scotty
import Data.Monoid (mconcat)

-- scotty :: Port -> ScottyM () -> IO ()
-- Run a scotty application using the warp server.
main = scotty 3000 $ do
  -- get :: RoutePattern -> ActionM () -> ScottyM ()
  get "/:word" $ do
    -- param :: Parsable a => Text -> ActionM a
    beam <- param "word"
    -- html :: Text -> ActionM ()
    -- Set the body of the response to a given Text value.
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
