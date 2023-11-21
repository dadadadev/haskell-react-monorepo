{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Db
  ( insertMessage,
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

data TestField = TestField Int String deriving (Show)

instance FromRow TestField where
  fromRow = TestField <$> field <*> field

insertMessage :: String -> IO ()
insertMessage message = do
  conn <- open "test.db"
  execute conn "INSERT INTO test (str) VALUES (?)" (Only (message :: String))
  r <- query_ conn "SELECT * from test" :: IO [TestField]
  mapM_ print r
  close conn
