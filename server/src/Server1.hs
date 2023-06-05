{-# LANGUAGE OverloadedStrings #-}

module Server1
  ( runServer,
  )
where

import Network.HTTP.Types (status200)
import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setLogger, setPort)
import Network.Wai.Logger (withStdoutLogger)

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
  withStdoutLogger $
    \apilogger -> do
      let settings = setPort 8080 $ setLogger apilogger defaultSettings
      runSettings settings app
