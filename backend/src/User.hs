{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module User
  ( User(..)
  , UserId(..)
  , CreateUserRequest(..)
  , UserResponse(..)
  ) where

import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)

-- | User identifier
newtype UserId = UserId { unUserId :: Int }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | User domain model
data User = User
  { userId :: UserId
  , userName :: String
  , userEmail :: String
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Request to create a new user
data CreateUserRequest = CreateUserRequest
  { createUserName :: String
  , createUserEmail :: String
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | User response for API
data UserResponse = UserResponse
  { userResponseId :: UserId
  , userResponseName :: String
  , userResponseEmail :: String
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Convert User to UserResponse
toUserResponse :: User -> UserResponse
toUserResponse user = UserResponse
  { userResponseId = userId user
  , userResponseName = userName user
  , userResponseEmail = userEmail user
  }
