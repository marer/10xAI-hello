{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}

module API
  ( apiRoutes
  , AppM
  , AppEnv(..)
  ) where

import User (UserId(..), CreateUserRequest, UserResponse)
import UserService (UserServiceC, createUser, getUserById, getAllUsers, updateUser, deleteUser)
import UserRepository (UserRepositoryC, UserRepository(..), InMemoryUserRepository)
import Web.Scotty (ScottyM, get, post, put, delete, json, param, status, notFound, badRequest, body, liftIO)
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (decode)
import Data.ByteString.Lazy (ByteString)
import Network.HTTP.Types (status200, status201, status404, status400)

-- | Application monad combining all effects
type AppM = ReaderT AppEnv IO

-- | Application environment containing all dependencies
data AppEnv = AppEnv
  { userRepository :: InMemoryUserRepository
  }

-- | Constraint for our application effects
type AppC = (UserRepositoryC AppM, UserServiceC AppM, MonadIO AppM)

-- | UserService instance for AppM
instance UserRepository AppM where
  createUser req = do
    env <- ask
    liftIO $ runReaderT (createUser req) (userRepository env)

  getUserById uid = do
    env <- ask
    liftIO $ runReaderT (getUserById uid) (userRepository env)

  getAllUsers = do
    env <- ask
    liftIO $ runReaderT getAllUsers (userRepository env)

  updateUser uid req = do
    env <- ask
    liftIO $ runReaderT (updateUser uid req) (userRepository env)

  deleteUser uid = do
    env <- ask
    liftIO $ runReaderT (deleteUser uid) (userRepository env)

-- | UserService instance for AppM
instance UserService.UserService AppM where
  createUser' = createUser
  getUserById' = getUserById
  getAllUsers' = getAllUsers
  updateUser' = updateUser
  deleteUser' = deleteUser

-- | API routes
apiRoutes :: AppEnv -> ScottyM ()
apiRoutes env = do
  -- Health check
  get "/health" $ do
    status status200
    json ("OK" :: String)

  -- Get all users
  get "/api/users" $ do
    users <- liftIO $ runReaderT getAllUsers env
    json users

  -- Get user by ID
  get "/api/users/:id" $ do
    userIdStr <- param "id"
    let maybeUserId = fmap UserId (readMaybe userIdStr)
    case maybeUserId of
      Nothing -> do
        status status400
        json ("Invalid user ID" :: String)
      Just uid -> do
        maybeUser <- liftIO $ runReaderT (getUserById uid) env
        case maybeUser of
          Nothing -> do
            status status404
            notFound
          Just user -> json user

  -- Create new user
  post "/api/users" $ do
    body <- body
    case decode body of
      Nothing -> do
        status status400
        json ("Invalid JSON" :: String)
      Just req -> do
        user <- liftIO $ runReaderT (createUser req) env
        status status201
        json user

  -- Update user
  put "/api/users/:id" $ do
    userIdStr <- param "id"
    let maybeUserId = fmap UserId (readMaybe userIdStr)
    case maybeUserId of
      Nothing -> do
        status status400
        json ("Invalid user ID" :: String)
      Just uid -> do
        body <- body
        case decode body of
          Nothing -> do
            status status400
            json ("Invalid JSON" :: String)
          Just req -> do
            maybeUser <- liftIO $ runReaderT (updateUser uid req) env
            case maybeUser of
              Nothing -> do
                status status404
                notFound
              Just user -> json user

  -- Delete user
  delete "/api/users/:id" $ do
    userIdStr <- param "id"
    let maybeUserId = fmap UserId (readMaybe userIdStr)
    case maybeUserId of
      Nothing -> do
        status status400
        json ("Invalid user ID" :: String)
      Just uid -> do
        deleted <- liftIO $ runReaderT (deleteUser uid) env
        if deleted
          then do
            status status200
            json ("User deleted" :: String)
          else do
            status status404
            notFound

-- | Helper function to safely read integers
readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
  [(x, "")] -> Just x
  _ -> Nothing
