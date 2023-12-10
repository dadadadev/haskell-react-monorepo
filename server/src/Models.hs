module Models
  ( Post (..),
    DTO (..),
    OnlyMessage (..),
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
  )
import Database.SQLite.Simple (FromRow (fromRow))
import Database.SQLite.Simple.FromRow (field)

class DTO a where
  toData :: a -> a
  toEntitiy :: a -> a
  toPresentation :: a -> a

type Message = String

data OnlyMessage = OnlyMessage
  { message :: Message
  }
  deriving (Show, Eq)

data Post
  = PostData
      { postId :: Int,
        postMessage :: Message
      }
  | PostEntity
      { postId :: Int,
        postMessage :: Message
      }
  | PostPresentacion
      { postId :: Int,
        postMessage :: Message
      }
  deriving (Show, Eq)

instance DTO Post where
  toData (PostData fromId fromMessage) = PostData fromId fromMessage
  toData (PostEntity fromId fromMessage) = PostData fromId fromMessage
  toData (PostPresentacion fromId fromMessage) = PostData fromId fromMessage
  toEntitiy (PostData fromId fromMessage) = PostEntity fromId fromMessage
  toEntitiy (PostEntity fromId fromMessage) = PostEntity fromId fromMessage
  toEntitiy (PostPresentacion fromId fromMessage) = PostEntity fromId fromMessage
  toPresentation (PostData fromId fromMessage) = PostPresentacion fromId fromMessage
  toPresentation (PostEntity fromId fromMessage) = PostPresentacion fromId fromMessage
  toPresentation (PostPresentacion fromId fromMessage) = PostPresentacion fromId fromMessage

instance FromRow Post where
  fromRow = PostEntity <$> field <*> field

instance FromJSON Post where
  parseJSON = withObject "Post" $ \v ->
    PostPresentacion
      <$> v .: fromString "id"
      <*> v .: fromString "message"

instance ToJSON Post where
  toJSON (PostPresentacion fromId fromMessage) =
    object
      [ fromString "id" .= fromId,
        fromString "message" .= fromMessage
      ]
  toJSON _ = error "Invalid Post type"

instance FromJSON OnlyMessage where
  parseJSON = withObject "OnlyMessage" $ \v ->
    OnlyMessage <$> (v .: fromString "message")
