{-# LANGUAGE OverloadedStrings #-}

module Server
  ( run,
  )
where

import Db (insertMessage)
import Network.HTTP.Types (status200, status404)
import Network.Wai (Application, Request (pathInfo), responseLBS)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setLogger, setPort)
import Network.Wai.Logger (withStdoutLogger)

router :: Application
router req = case pathInfo req of
  ["api"] -> indexApp req
  ["api", "hello"] -> halloApp req
  ["api", "create-message"] -> createMessage req
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

createMessage :: Application
createMessage _ respond = do
  insertMessage "test by request"
  respond $
    responseLBS
      status200
      [("Content-Type", "text/plain")]
      "Succeeded"

run :: IO ()
run = do
  putStrLn "Server is running..."
  withStdoutLogger $
    \apilogger -> do
      let settings = setPort 8080 $ setLogger apilogger defaultSettings
      runSettings settings router
