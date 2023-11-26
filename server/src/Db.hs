{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Db
  ( insertPostOnlyMessage,
  )
where

import Database.SQLite.Simple
  ( FromRow,
    close,
    execute,
    open,
    query_,
  )
import Database.SQLite.Simple.FromRow (field, fromRow)
import Database.SQLite.Simple.Types (Only (Only))

data PostRecord = PostRecord
  { id :: Int,
    message :: String
  }
  deriving (Show)

instance FromRow PostRecord where
  fromRow = PostRecord <$> field <*> field

insertPostOnlyMessage :: String -> IO ()
insertPostOnlyMessage newMessage = do
  conn <- open "db.sqlite3"
  execute conn "INSERT INTO post (str) VALUES (?)" (Only (newMessage :: String))
  r <- query_ conn "SELECT * from post" :: IO [PostRecord]
  mapM_ print r
  close conn
