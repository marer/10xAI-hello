{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}

module UserService
  ( UserService(..)
  , UserServiceC
  , createUser
  , getUserById
  , getAllUsers
  , updateUser
  , deleteUser
  ) where

import User (User(..), UserId, CreateUserRequest, UserResponse(..))
import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.IO.Class (MonadIO)

-- | Tagless final constraint for UserService
type UserServiceC m = (MonadReader (UserService m) m, MonadIO m)

-- | UserService effect interface
class Monad m => UserService m where
  -- | Create a new user
  createUser' :: CreateUserRequest -> m User

  -- | Get user by ID
  getUserById' :: UserId -> m (Maybe User)

  -- | Get all users
  getAllUsers' :: m [User]

  -- | Update an existing user
  updateUser' :: UserId -> CreateUserRequest -> m (Maybe User)

  -- | Delete a user
  deleteUser' :: UserId -> m Bool

-- | Create a new user (high-level operation)
createUser :: UserServiceC m => CreateUserRequest -> m UserResponse
createUser req = do
  user <- asks createUser' >>= ($ req)
  return $ UserResponse
    { userResponseId = userId user
    , userResponseName = userName user
    , userResponseEmail = userEmail user
    }

-- | Get user by ID (high-level operation)
getUserById :: UserServiceC m => UserId -> m (Maybe UserResponse)
getUserById uid = do
  maybeUser <- asks getUserById' >>= ($ uid)
  return $ fmap (\user -> UserResponse
    { userResponseId = userId user
    , userResponseName = userName user
    , userResponseEmail = userEmail user
    }) maybeUser

-- | Get all users (high-level operation)
getAllUsers :: UserServiceC m => m [UserResponse]
getAllUsers = do
  users <- asks getAllUsers'
  return $ map (\user -> UserResponse
    { userResponseId = userId user
    , userResponseName = userName user
    , userResponseEmail = userEmail user
    }) users

-- | Update an existing user (high-level operation)
updateUser :: UserServiceC m => UserId -> CreateUserRequest -> m (Maybe UserResponse)
updateUser uid req = do
  maybeUser <- asks updateUser' >>= ($ uid) >>= ($ req)
  return $ fmap (\user -> UserResponse
    { userResponseId = userId user
    , userResponseName = userName user
    , userResponseEmail = userEmail user
    }) maybeUser

-- | Delete a user (high-level operation)
deleteUser :: UserServiceC m => UserId -> m Bool
deleteUser uid = asks deleteUser' >>= ($ uid)
