{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Db
  ( insertPostOnlyMessage,
    getPosts,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Database.SQLite.Simple
  ( FromRow,
    close,
    execute,
    open,
    query_,
  )
import Database.SQLite.Simple.FromRow (field, fromRow)
import Database.SQLite.Simple.Types (Only (Only))
import GHC.Generics (Generic)

data Post = Post
  { id :: Int,
    message :: String
  }
  deriving (Show, Generic)

instance FromRow Post where
  fromRow = Post <$> field <*> field

instance FromJSON Post

instance ToJSON Post

insertPostOnlyMessage :: String -> IO ()
insertPostOnlyMessage newMessage = do
  conn <- open "db.sqlite3"
  execute conn "INSERT INTO post (str) VALUES (?)" (Only (newMessage :: String))
  _ <- query_ conn "SELECT * from post" :: IO [Post]
  close conn

getPosts :: String -> IO [Post] -- FIXME: parameter
getPosts _ = do
  conn <- open "db.sqlite3"
  posts <- query_ conn "SELECT * from post" :: IO [Post]
  close conn
  return posts
