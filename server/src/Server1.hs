{-# LANGUAGE OverloadedStrings #-}

module Server1
  ( runServer,
  )
where

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
      "Hello, Web from Server1"

runServer :: IO ()
runServer = do
  putStrLn "Server1 is running..."
  run 8080 app
