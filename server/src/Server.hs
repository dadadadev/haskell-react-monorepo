{-# LANGUAGE OverloadedStrings #-}

module Server
  ( run,
  )
where

import Control.Monad (join)
import Data.Aeson (decode, encode, (.:))
import Data.Aeson.Types (Object, parseMaybe)
import qualified Data.ByteString.Lazy as BL
import Db (deletePosts, getPosts, insertPostOnlyMessage)
import Models (DTO (toPresentation), OnlyMessage (message))
import Network.HTTP.Types (status200, status400, status404)
import Network.Wai (Application, Request (pathInfo), getRequestBodyChunk, responseLBS)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setLogger, setPort)
import Network.Wai.Logger (withStdoutLogger)

router :: Application
router req = case pathInfo req of
  ["api"] -> indexApp req
  ["api", "posts"] -> postsApp req
  ["api", "posts", "delete"] -> deletePostsApp req
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

postsApp :: Application
postsApp _ respond = do
  posts <- getPosts ""
  let aPIPosts = map toPresentation posts
  respond $ responseLBS status200 [("Content-Type", "application/json")] (encode aPIPosts)

deletePostsApp :: Application
deletePostsApp req respond = do
  body <- getRequestBodyChunk req
  case extractIds (BL.fromStrict body) of
    Just ids -> do
      deletePosts ids
      respond $ responseLBS status200 [("Content-Type", "text/plain")] "message added successfully"
    Nothing ->
      respond $ responseLBS status400 [("Content-Type", "text/plain")] "invalid JSON"

postsMessageApp :: Application
postsMessageApp req respond = do
  body <- getRequestBodyChunk req
  case decode (BL.fromChunks [body]) :: Maybe OnlyMessage of
    Just aPipost -> do
      insertPostOnlyMessage (message aPipost)
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

extractIds :: BL.ByteString -> Maybe [Int]
extractIds jsonData = do
  obj <- decode jsonData :: Maybe Object
  join $ parseMaybe (.: "ids") obj