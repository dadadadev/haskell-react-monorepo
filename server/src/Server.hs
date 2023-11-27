{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Server
  ( run,
  )
where

import Data.Aeson (FromJSON, decode, encode)
import qualified Data.ByteString.Lazy as BL
import Db (getPosts, insertPostOnlyMessage)
import GHC.Generics (Generic)
import Network.HTTP.Types (status200, status400, status404)
import Network.Wai (Application, Request (pathInfo), getRequestBodyChunk, responseLBS)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setLogger, setPort)
import Network.Wai.Logger (withStdoutLogger)

router :: Application
router req = case pathInfo req of
  ["api"] -> indexApp req
  ["api", "posts"] -> postsApp req
  ["api", "posts", "message"] -> postsMessageApp req
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

newtype Post = Post
  { message :: String
  }
  deriving (Show, Generic)

instance FromJSON Post

postsApp :: Application
postsApp _ respond = do
  posts <-
    getPosts ""
  respond $ responseLBS status200 [("Content-Type", "application/json")] (encode posts)

postsMessageApp :: Application
postsMessageApp req respond = do
  body <- getRequestBodyChunk req
  case decode (BL.fromChunks [body]) :: Maybe Post of
    Just post -> do
      insertPostOnlyMessage (message post)
      respond $ responseLBS status200 [("Content-Type", "text/plain")] "message added successfully"
    Nothing ->
      respond $ responseLBS status400 [("Content-Type", "text/plain")] "invalid JSON"

run :: IO ()
run = do
  putStrLn "Server is running..."
  withStdoutLogger $
    \apilogger -> do
      let settings = setPort 8080 $ setLogger apilogger defaultSettings
      runSettings settings router
