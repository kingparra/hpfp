#!/usr/bin/env stack
{- stack script
   --resolver lts-16.27
   --install-ghc
   --package wreq
   --package bytestring
-}
module HttpStuff where
import Data.ByteString.Lazy hiding (map)
import Network.Wreq


-- replace with other websites if desired or needed
urls :: [String]
urls = [ "http://httpbin.org/ip"
       , "http://httpbin.org/bytes/5"
       ]


mappingGet :: [IO (Response ByteString)]
mappingGet = map get urls


traversedUrls :: IO [Response ByteString]
traversedUrls = traverse get urls
