{-# LANGUAGE OverloadedStrings #-}

module Server1
  ( runServer,
  )
where

import Network.HTTP.Types (status200, status404)
import Network.Wai (Application, Request (pathInfo), responseLBS)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setLogger, setPort)
import Network.Wai.Logger (withStdoutLogger)

router :: Application
router req = case pathInfo req of
  ["api"] -> indexApp req
  ["api", "hello"] -> halloApp req
  _ -> notFoundApp req

indexApp :: Application
indexApp _ respond = do
  respond $
    responseLBS
      status200
      [("Content-Type", "text/plain")]
      "index"

notFoundApp :: Application
notFoundApp _ respond = do
  respond $
    responseLBS
      status404
      [("Content-Type", "text/plain")]
      "404 Not found"

halloApp :: Application
halloApp _ respond = do
  respond $
    responseLBS
      status200
      [("Content-Type", "text/plain")]
      "Hello"

runServer :: IO ()
runServer = do
  putStrLn "Server1 is running..."
  withStdoutLogger $
    \apilogger -> do
      let settings = setPort 8080 $ setLogger apilogger defaultSettings
      runSettings settings router
