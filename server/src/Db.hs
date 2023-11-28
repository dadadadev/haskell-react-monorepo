{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Db
  ( insertPostOnlyMessage,
    getPosts,
  )
where

import Database.SQLite.Simple
  ( close,
    execute,
    open,
    query_,
  )
import Database.SQLite.Simple.Types (Only (Only))
import Models (DBPost, Post, dBPostToPost)

insertPostOnlyMessage :: String -> IO ()
insertPostOnlyMessage postMessage = do
  conn <- open "db.sqlite3"
  execute conn "INSERT INTO post (str) VALUES (?)" (Only postMessage)
  _ <- query_ conn "SELECT * from post" :: IO [DBPost]
  close conn

getPosts :: String -> IO [Post] -- FIXME: parameter
getPosts _ = do
  conn <- open "db.sqlite3"
  dbPosts <- query_ conn "SELECT * from post" :: IO [DBPost]
  let posts = map dBPostToPost dbPosts
  close conn
  return posts
