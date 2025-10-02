{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}

module UserRepository
  ( UserRepository(..)
  , UserRepositoryC
  , InMemoryUserRepository(..)
  , createInMemoryUserRepository
  ) where

import User (User, UserId, CreateUserRequest)
import Control.Monad.Reader (MonadReader, asks, ReaderT, ask)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

-- | Tagless final constraint for UserRepository
type UserRepositoryC m = (MonadReader (UserRepository m) m, MonadIO m)

-- | UserRepository effect interface
class Monad m => UserRepository m where
  -- | Create a new user
  createUser :: CreateUserRequest -> m User

  -- | Get user by ID
  getUserById :: UserId -> m (Maybe User)

  -- | Get all users
  getAllUsers :: m [User]

  -- | Update an existing user
  updateUser :: UserId -> CreateUserRequest -> m (Maybe User)

  -- | Delete a user
  deleteUser :: UserId -> m Bool

-- | In-memory implementation of UserRepository
data InMemoryUserRepository = InMemoryUserRepository
  { userStore :: IORef (Map UserId User)
  , nextUserId :: IORef Int
  }

-- | Create a new in-memory user repository
createInMemoryUserRepository :: IO InMemoryUserRepository
createInMemoryUserRepository = do
  store <- newIORef Map.empty
  nextId <- newIORef 1
  return $ InMemoryUserRepository store nextId

-- | UserRepository instance for InMemoryUserRepository
instance MonadIO m => UserRepository (ReaderT InMemoryUserRepository m) where
  createUser req = do
    repo <- ask
    liftIO $ do
      nextId <- readIORef (nextUserId repo)
      let newUser = User
            { userId = UserId nextId
            , userName = createUserName req
            , userEmail = createUserEmail req
            }
      modifyIORef (userStore repo) (Map.insert (UserId nextId) newUser)
      writeIORef (nextUserId repo) (nextId + 1)
      return newUser

  getUserById uid = do
    repo <- ask
    liftIO $ do
      store <- readIORef (userStore repo)
      return $ Map.lookup uid store

  getAllUsers = do
    repo <- ask
    liftIO $ do
      store <- readIORef (userStore repo)
      return $ Map.elems store

  updateUser uid req = do
    repo <- ask
    liftIO $ do
      store <- readIORef (userStore repo)
      case Map.lookup uid store of
        Nothing -> return Nothing
        Just _ -> do
          let updatedUser = User
                { userId = uid
                , userName = createUserName req
                , userEmail = createUserEmail req
                }
          writeIORef (userStore repo) (Map.insert uid updatedUser store)
          return $ Just updatedUser

  deleteUser uid = do
    repo <- ask
    liftIO $ do
      store <- readIORef (userStore repo)
      if Map.member uid store
        then do
          writeIORef (userStore repo) (Map.delete uid store)
          return True
        else return False
