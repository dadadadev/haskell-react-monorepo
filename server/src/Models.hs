{-# LANGUAGE DeriveGeneric #-}

module Models
  ( Post,
    DBPost (..),
    APIPost (..),
    postToDBPost,
    dBPostToPost,
    postToAPIPost,
  )
where

import Data.Aeson.Key (fromString)
import Data.Aeson.Types
  ( FromJSON (parseJSON),
    KeyValue ((.=)),
    ToJSON (toJSON),
    object,
    withObject,
    (.:),
    (.:?),
  )
import Database.SQLite.Simple (FromRow (fromRow))
import Database.SQLite.Simple.FromRow (field)
import GHC.Generics (Generic)

data Post = Post
  { id :: Int,
    message :: String
  }
  deriving (Show, Eq)

data DBPost = DBPost
  { dbPostId :: Int,
    dbPostMessage :: String
  }
  deriving (Show, Eq, Generic)

instance FromRow DBPost where
  fromRow = DBPost <$> field <*> field

data APIPost = APIPost
  { apiPostId :: Maybe Int,
    apiPostMessage :: String
  }
  deriving (Show, Eq, Generic)

instance FromJSON APIPost where
  parseJSON = withObject "APIPost" $ \v ->
    APIPost
      <$> v .:? fromString "id"
      <*> v .: fromString "message"

instance ToJSON APIPost where
  toJSON (APIPost oApiPostId oAapiPostMessage) =
    object
      [ fromString "id" .= oApiPostId,
        fromString "message" .= oAapiPostMessage
      ]

postToDBPost :: Post -> DBPost
postToDBPost (Post postId postMessage) = DBPost postId postMessage

dBPostToPost :: DBPost -> Post
dBPostToPost (DBPost postId postMessage) = Post postId postMessage

postToAPIPost :: Post -> APIPost
postToAPIPost (Post postId postMessage) =
  APIPost
    { apiPostId = Just postId,
      apiPostMessage = postMessage
    }
