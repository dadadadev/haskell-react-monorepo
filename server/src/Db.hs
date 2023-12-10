{-# LANGUAGE OverloadedStrings #-}

module Db
  ( insertPostOnlyMessage,
    getPosts,
    deletePosts,
  )
where

import Data.List (intercalate)
import Data.String (fromString)
import Database.SQLite.Simple (close, execute, open, query_)
import Database.SQLite.Simple.Types (Only (Only))
import Models (DTO (toData), Post)

insertPostOnlyMessage :: String -> IO ()
insertPostOnlyMessage postMessage = do
  conn <- open "db.sqlite3"
  execute conn "INSERT INTO post (str) VALUES (?)" (Only postMessage)
  _ <- query_ conn "SELECT * from post" :: IO [Post]
  close conn

getPosts :: String -> IO [Post] -- FIXME: parameter
getPosts _ = do
  conn <- open "db.sqlite3"
  dbPosts <- query_ conn "SELECT * from post" :: IO [Post]
  let posts = map toData dbPosts
  close conn
  return posts

deletePosts :: [Int] -> IO ()
deletePosts ids = do
  conn <- open "db.sqlite3"
  let placeholders = replicate (length ids) "?"
      query = "DELETE FROM post WHERE id IN (" ++ intercalate ", " placeholders ++ ")"
  execute conn (fromString query) ids
  close conn
