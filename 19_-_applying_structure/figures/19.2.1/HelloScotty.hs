#!/usr/bin/env stack
{- stack script
   --install-ghc
   --resolver lts-17.7
   --package scotty
-}
{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty

main = scotty 3001 $ do
  get "/" $ do
    html "Hello World!"
