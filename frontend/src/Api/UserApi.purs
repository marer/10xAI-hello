module Api.UserApi where

import Prelude

import Affjax (defaultRequest, get, post, put, delete, printError)
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as ResponseFormat
import Data.Argonaut (Json, encodeJson, decodeJson)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.User (User, UserId, CreateUserRequest, UserResponse)
import Effect.Aff (Aff)
import Effect.Class.Console (log)

-- | Base URL for the API
baseUrl :: String
baseUrl = "http://localhost:3000"

-- | Get all users
getUsers :: Aff (Either String (Array UserResponse))
getUsers = do
  result <- get ResponseFormat.json $ baseUrl <> "/api/users"
  case result of
    Left err -> do
      log $ "Error getting users: " <> printError err
      pure $ Left $ "Failed to get users: " <> printError err
    Right response -> do
      case decodeJson response.body of
        Left err -> do
          log $ "Error decoding users: " <> show err
          pure $ Left $ "Failed to decode users: " <> show err
        Right users -> pure $ Right users

-- | Get user by ID
getUser :: UserId -> Aff (Either String (Maybe UserResponse))
getUser (UserId id) = do
  result <- get ResponseFormat.json $ baseUrl <> "/api/users/" <> show id
  case result of
    Left err -> do
      log $ "Error getting user: " <> printError err
      pure $ Left $ "Failed to get user: " <> printError err
    Right response -> do
      case decodeJson response.body of
        Left err -> do
          log $ "Error decoding user: " <> show err
          pure $ Left $ "Failed to decode user: " <> show err
        Right user -> pure $ Right $ Just user

-- | Create a new user
createUser :: CreateUserRequest -> Aff (Either String UserResponse)
createUser req = do
  let request = defaultRequest
        { url = baseUrl <> "/api/users"
        , method = Left POST
        , headers = [ ContentType "application/json" ]
        , content = Just $ encodeJson req
        }

  result <- post ResponseFormat.json request
  case result of
    Left err -> do
      log $ "Error creating user: " <> printError err
      pure $ Left $ "Failed to create user: " <> printError err
    Right response -> do
      case decodeJson response.body of
        Left err -> do
          log $ "Error decoding created user: " <> show err
          pure $ Left $ "Failed to decode created user: " <> show err
        Right user -> pure $ Right user

-- | Update a user
updateUser :: UserId -> CreateUserRequest -> Aff (Either String (Maybe UserResponse))
updateUser (UserId id) req = do
  let request = defaultRequest
        { url = baseUrl <> "/api/users/" <> show id
        , method = Left PUT
        , headers = [ ContentType "application/json" ]
        , content = Just $ encodeJson req
        }

  result <- put ResponseFormat.json request
  case result of
    Left err -> do
      log $ "Error updating user: " <> printError err
      pure $ Left $ "Failed to update user: " <> printError err
    Right response -> do
      case decodeJson response.body of
        Left err -> do
          log $ "Error decoding updated user: " <> show err
          pure $ Left $ "Failed to decode updated user: " <> show err
        Right user -> pure $ Right $ Just user

-- | Delete a user
deleteUser :: UserId -> Aff (Either String Boolean)
deleteUser (UserId id) = do
  result <- delete ResponseFormat.json $ baseUrl <> "/api/users/" <> show id
  case result of
    Left err -> do
      log $ "Error deleting user: " <> printError err
      pure $ Left $ "Failed to delete user: " <> printError err
    Right _ -> pure $ Right true

