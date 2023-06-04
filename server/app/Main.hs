{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Network.HTTP.Types (status200)
import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.Warp (run)

app :: Application
app _ respond = do
  putStrLn "minimum server running"
  respond $
    responseLBS
      status200
      [("Content-Type", "text/plain")]
      "Hello, Web!"

main :: IO ()
main = do
  putStrLn $ "http://localhost:8080/"
  run 8080 app
