module Data.User where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, encodeJson, jsonEmptyObject, (.?), (:=))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

-- | User identifier
newtype UserId = UserId Int

derive instance eqUserId :: Eq UserId
derive instance ordUserId :: Ord UserId
derive instance genericUserId :: Generic UserId _

instance showUserId :: Show UserId where
  show = genericShow

instance encodeJsonUserId :: EncodeJson UserId where
  encodeJson (UserId id) = encodeJson id

instance decodeJsonUserId :: DecodeJson UserId where
  decodeJson json = UserId <$> decodeJson json

-- | User domain model
type User =
  { id :: UserId
  , name :: String
  , email :: String
  }

-- | Request to create a new user
type CreateUserRequest =
  { name :: String
  , email :: String
  }

-- | User response from API
type UserResponse =
  { id :: UserId
  , name :: String
  , email :: String
  }

instance encodeJsonCreateUserRequest :: EncodeJson CreateUserRequest where
  encodeJson req = jsonEmptyObject
    # "name" := req.name
    # "email" := req.email

instance decodeJsonUserResponse :: DecodeJson UserResponse where
  decodeJson json = do
    obj <- decodeJson json
    id <- obj .? "id"
    name <- obj .? "name"
    email <- obj .? "email"
    pure { id, name, email }

