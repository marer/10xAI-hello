{-# LANGUAGE OverloadedStrings #-}

module APITest (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (encode, decode)
import Data.ByteString.Lazy (ByteString, fromStrict)
import Data.Text (Text, pack, unpack)
import Network.HTTP.Types (status200, status201, status400, status404)
import Network.Wai (Application, Request, Response)
import Network.Wai.Test (SResponse(..), runSession, request, defaultRequest, setPath, setMethod, setRequestBody)
import Web.Scotty (scottyApp, get, post, put, delete, json, param, status, notFound, badRequest, body, liftIO)
import User (User(..), UserId(..), CreateUserRequest(..), UserResponse(..))
import UserService (UserService(..), UserServiceC, createUser, getUserById, getAllUsers, updateUser, deleteUser)
import UserRepository (InMemoryUserRepository, createInMemoryUserRepository, UserRepository(..))
import API (AppEnv(..), AppM, apiRoutes)
import TestUtils (genCreateUserRequest, genUser, createTestUserRepository, runTestAppM)

-- Mock UserService implementation for API testing
data MockUserService = MockUserService
  { mockCreateUser :: CreateUserRequest -> IO User
  , mockGetUserById :: UserId -> IO (Maybe User)
  , mockGetAllUsers :: IO [User]
  , mockUpdateUser :: UserId -> CreateUserRequest -> IO (Maybe User)
  , mockDeleteUser :: UserId -> IO Bool
  }

-- UserService instance for testing
instance UserService (ReaderT MockUserService IO) where
  createUser' req = do
    mockService <- ask
    liftIO $ mockCreateUser mockService req

  getUserById' uid = do
    mockService <- ask
    liftIO $ mockGetUserById mockService uid

  getAllUsers' = do
    mockService <- ask
    liftIO $ mockGetAllUsers mockService

  updateUser' uid req = do
    mockService <- ask
    liftIO $ mockUpdateUser mockService uid req

  deleteUser' uid = do
    mockService <- ask
    liftIO $ mockDeleteUser mockService uid

-- Create test application
createTestApp :: MockUserService -> IO Application
createTestApp mockService = do
  scottyApp $ do
    get "/health" $ do
      status status200
      json ("OK" :: String)

    get "/api/users" $ do
      users <- liftIO $ mockGetAllUsers mockService
      json $ map toUserResponse users

    get "/api/users/:id" $ do
      userIdStr <- param "id"
      let maybeUserId = fmap UserId (readMaybe userIdStr)
      case maybeUserId of
        Nothing -> do
          status status400
          json ("Invalid user ID" :: String)
        Just uid -> do
          maybeUser <- liftIO $ mockGetUserById mockService uid
          case maybeUser of
            Nothing -> do
              status status404
              notFound
            Just user -> json $ toUserResponse user

    post "/api/users" $ do
      body <- body
      case decode body of
        Nothing -> do
          status status400
          json ("Invalid JSON" :: String)
        Just req -> do
          user <- liftIO $ mockCreateUser mockService req
          status status201
          json $ toUserResponse user

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
              maybeUser <- liftIO $ mockUpdateUser mockService uid req
              case maybeUser of
                Nothing -> do
                  status status404
                  notFound
                Just user -> json $ toUserResponse user

    delete "/api/users/:id" $ do
      userIdStr <- param "id"
      let maybeUserId = fmap UserId (readMaybe userIdStr)
      case maybeUserId of
        Nothing -> do
          status status400
          json ("Invalid user ID" :: String)
        Just uid -> do
          deleted <- liftIO $ mockDeleteUser mockService uid
          if deleted
            then status status200
            else status status404

-- Helper to convert User to UserResponse
toUserResponse :: User -> UserResponse
toUserResponse user = UserResponse
  { userResponseId = userId user
  , userResponseName = userName user
  , userResponseEmail = userEmail user
  }

-- Helper for readMaybe
readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
  [(x, "")] -> Just x
  _ -> Nothing

spec :: Spec
spec = do
  describe "API Endpoints" $ do
    describe "Health Check" $ do
      it "should return OK for health check" $ do
        let mockService = MockUserService
              { mockCreateUser = \_ -> return $ User (UserId 1) "" ""
              , mockGetUserById = \_ -> return Nothing
              , mockGetAllUsers = return []
              , mockUpdateUser = \_ _ -> return Nothing
              , mockDeleteUser = \_ -> return False
              }

        app <- liftIO $ createTestApp mockService
        response <- runSession (request defaultRequest { requestMethod = "GET", rawPathInfo = "/health" }) app

        simpleStatus response `shouldBe` status200
        simpleBody response `shouldBe` "\"OK\""

    describe "GET /api/users" $ do
      it "should return empty list when no users exist" $ do
        let mockService = MockUserService
              { mockCreateUser = \_ -> return $ User (UserId 1) "" ""
              , mockGetUserById = \_ -> return Nothing
              , mockGetAllUsers = return []
              , mockUpdateUser = \_ _ -> return Nothing
              , mockDeleteUser = \_ -> return False
              }

        app <- liftIO $ createTestApp mockService
        response <- runSession (request defaultRequest { requestMethod = "GET", rawPathInfo = "/api/users" }) app

        simpleStatus response `shouldBe` status200
        simpleBody response `shouldBe` "[]"

      it "should return all users" $ do
        let users = [ User (UserId 1) "User 1" "user1@example.com"
                    , User (UserId 2) "User 2" "user2@example.com"
                    ]
        let mockService = MockUserService
              { mockCreateUser = \_ -> return $ head users
              , mockGetUserById = \_ -> return Nothing
              , mockGetAllUsers = return users
              , mockUpdateUser = \_ _ -> return Nothing
              , mockDeleteUser = \_ -> return False
              }

        app <- liftIO $ createTestApp mockService
        response <- runSession (request defaultRequest { requestMethod = "GET", rawPathInfo = "/api/users" }) app

        simpleStatus response `shouldBe` status200
        let body = simpleBody response
        body `shouldContain` "User 1"
        body `shouldContain` "User 2"
        body `shouldContain` "user1@example.com"
        body `shouldContain` "user2@example.com"

    describe "GET /api/users/:id" $ do
      it "should return 400 for invalid user ID" $ do
        let mockService = MockUserService
              { mockCreateUser = \_ -> return $ User (UserId 1) "" ""
              , mockGetUserById = \_ -> return Nothing
              , mockGetAllUsers = return []
              , mockUpdateUser = \_ _ -> return Nothing
              , mockDeleteUser = \_ -> return False
              }

        app <- liftIO $ createTestApp mockService
        response <- runSession (request defaultRequest { requestMethod = "GET", rawPathInfo = "/api/users/invalid" }) app

        simpleStatus response `shouldBe` status400
        simpleBody response `shouldContain` "Invalid user ID"

      it "should return 404 for non-existent user" $ do
        let mockService = MockUserService
              { mockCreateUser = \_ -> return $ User (UserId 1) "" ""
              , mockGetUserById = \_ -> return Nothing
              , mockGetAllUsers = return []
              , mockUpdateUser = \_ _ -> return Nothing
              , mockDeleteUser = \_ -> return False
              }

        app <- liftIO $ createTestApp mockService
        response <- runSession (request defaultRequest { requestMethod = "GET", rawPathInfo = "/api/users/999" }) app

        simpleStatus response `shouldBe` status404

      it "should return user for existing ID" $ do
        let testUser = User (UserId 42) "Test User" "test@example.com"
        let mockService = MockUserService
              { mockCreateUser = \_ -> return testUser
              , mockGetUserById = \uid -> return $ if uid == UserId 42 then Just testUser else Nothing
              , mockGetAllUsers = return [testUser]
              , mockUpdateUser = \_ _ -> return Nothing
              , mockDeleteUser = \_ -> return False
              }

        app <- liftIO $ createTestApp mockService
        response <- runSession (request defaultRequest { requestMethod = "GET", rawPathInfo = "/api/users/42" }) app

        simpleStatus response `shouldBe` status200
        let body = simpleBody response
        body `shouldContain` "Test User"
        body `shouldContain` "test@example.com"
        body `shouldContain` "42"

    describe "POST /api/users" $ do
      it "should return 400 for invalid JSON" $ do
        let mockService = MockUserService
              { mockCreateUser = \_ -> return $ User (UserId 1) "" ""
              , mockGetUserById = \_ -> return Nothing
              , mockGetAllUsers = return []
              , mockUpdateUser = \_ _ -> return Nothing
              , mockDeleteUser = \_ -> return False
              }

        app <- liftIO $ createTestApp mockService
        let req = defaultRequest
                { requestMethod = "POST"
                , rawPathInfo = "/api/users"
                , requestBody = fromStrict "invalid json"
                }
        response <- runSession (request req) app

        simpleStatus response `shouldBe` status400
        simpleBody response `shouldContain` "Invalid JSON"

      it "should create user with valid JSON" $ do
        let mockService = MockUserService
              { mockCreateUser = \req -> return $ User (UserId 1) (createUserName req) (createUserEmail req)
              , mockGetUserById = \_ -> return Nothing
              , mockGetAllUsers = return []
              , mockUpdateUser = \_ _ -> return Nothing
              , mockDeleteUser = \_ -> return False
              }

        app <- liftIO $ createTestApp mockService
        let userReq = CreateUserRequest "New User" "new@example.com"
        let req = defaultRequest
                { requestMethod = "POST"
                , rawPathInfo = "/api/users"
                , requestBody = encode userReq
                }
        response <- runSession (request req) app

        simpleStatus response `shouldBe` status201
        let body = simpleBody response
        body `shouldContain` "New User"
        body `shouldContain` "new@example.com"
        body `shouldContain` "1"

      it "should work with generated data" $ property $ \userReq ->
        let mockService = MockUserService
              { mockCreateUser = \req -> return $ User (UserId 1) (createUserName req) (createUserEmail req)
              , mockGetUserById = \_ -> return Nothing
              , mockGetAllUsers = return []
              , mockUpdateUser = \_ _ -> return Nothing
              , mockDeleteUser = \_ -> return False
              }

        let testAction = do
              app <- liftIO $ createTestApp mockService
              let req = defaultRequest
                      { requestMethod = "POST"
                      , rawPathInfo = "/api/users"
                      , requestBody = encode userReq
                      }
              response <- runSession (request req) app
              return $ simpleStatus response == status201 &&
                       simpleBody response `shouldContain` createUserName userReq &&
                       simpleBody response `shouldContain` createUserEmail userReq

        testAction `shouldReturn` True

    describe "PUT /api/users/:id" $ do
      it "should return 400 for invalid user ID" $ do
        let mockService = MockUserService
              { mockCreateUser = \_ -> return $ User (UserId 1) "" ""
              , mockGetUserById = \_ -> return Nothing
              , mockGetAllUsers = return []
              , mockUpdateUser = \_ _ -> return Nothing
              , mockDeleteUser = \_ -> return False
              }

        app <- liftIO $ createTestApp mockService
        let userReq = CreateUserRequest "Updated" "updated@example.com"
        let req = defaultRequest
                { requestMethod = "PUT"
                , rawPathInfo = "/api/users/invalid"
                , requestBody = encode userReq
                }
        response <- runSession (request req) app

        simpleStatus response `shouldBe` status400
        simpleBody response `shouldContain` "Invalid user ID"

      it "should return 400 for invalid JSON" $ do
        let mockService = MockUserService
              { mockCreateUser = \_ -> return $ User (UserId 1) "" ""
              , mockGetUserById = \_ -> return Nothing
              , mockGetAllUsers = return []
              , mockUpdateUser = \_ _ -> return Nothing
              , mockDeleteUser = \_ -> return False
              }

        app <- liftIO $ createTestApp mockService
        let req = defaultRequest
                { requestMethod = "PUT"
                , rawPathInfo = "/api/users/1"
                , requestBody = fromStrict "invalid json"
                }
        response <- runSession (request req) app

        simpleStatus response `shouldBe` status400
        simpleBody response `shouldContain` "Invalid JSON"

      it "should return 404 for non-existent user" $ do
        let mockService = MockUserService
              { mockCreateUser = \_ -> return $ User (UserId 1) "" ""
              , mockGetUserById = \_ -> return Nothing
              , mockGetAllUsers = return []
              , mockUpdateUser = \_ _ -> return Nothing
              , mockDeleteUser = \_ -> return False
              }

        app <- liftIO $ createTestApp mockService
        let userReq = CreateUserRequest "Updated" "updated@example.com"
        let req = defaultRequest
                { requestMethod = "PUT"
                , rawPathInfo = "/api/users/999"
                , requestBody = encode userReq
                }
        response <- runSession (request req) app

        simpleStatus response `shouldBe` status404

      it "should update existing user" $ do
        let testUser = User (UserId 42) "Original" "original@example.com"
        let mockService = MockUserService
              { mockCreateUser = \_ -> return testUser
              , mockGetUserById = \uid -> return $ if uid == UserId 42 then Just testUser else Nothing
              , mockGetAllUsers = return [testUser]
              , mockUpdateUser = \uid req ->
                  if uid == UserId 42
                    then return $ Just $ User uid (createUserName req) (createUserEmail req)
                    else return Nothing
              , mockDeleteUser = \_ -> return False
              }

        app <- liftIO $ createTestApp mockService
        let userReq = CreateUserRequest "Updated" "updated@example.com"
        let req = defaultRequest
                { requestMethod = "PUT"
                , rawPathInfo = "/api/users/42"
                , requestBody = encode userReq
                }
        response <- runSession (request req) app

        simpleStatus response `shouldBe` status200
        let body = simpleBody response
        body `shouldContain` "Updated"
        body `shouldContain` "updated@example.com"
        body `shouldContain` "42"

    describe "DELETE /api/users/:id" $ do
      it "should return 400 for invalid user ID" $ do
        let mockService = MockUserService
              { mockCreateUser = \_ -> return $ User (UserId 1) "" ""
              , mockGetUserById = \_ -> return Nothing
              , mockGetAllUsers = return []
              , mockUpdateUser = \_ _ -> return Nothing
              , mockDeleteUser = \_ -> return False
              }

        app <- liftIO $ createTestApp mockService
        response <- runSession (request defaultRequest { requestMethod = "DELETE", rawPathInfo = "/api/users/invalid" }) app

        simpleStatus response `shouldBe` status400
        simpleBody response `shouldContain` "Invalid user ID"

      it "should return 404 for non-existent user" $ do
        let mockService = MockUserService
              { mockCreateUser = \_ -> return $ User (UserId 1) "" ""
              , mockGetUserById = \_ -> return Nothing
              , mockGetAllUsers = return []
              , mockUpdateUser = \_ _ -> return Nothing
              , mockDeleteUser = \_ -> return False
              }

        app <- liftIO $ createTestApp mockService
        response <- runSession (request defaultRequest { requestMethod = "DELETE", rawPathInfo = "/api/users/999" }) app

        simpleStatus response `shouldBe` status404

      it "should delete existing user" $ do
        let testUser = User (UserId 42) "To Delete" "delete@example.com"
        let mockService = MockUserService
              { mockCreateUser = \_ -> return testUser
              , mockGetUserById = \uid -> return $ if uid == UserId 42 then Just testUser else Nothing
              , mockGetAllUsers = return [testUser]
              , mockUpdateUser = \_ _ -> return Nothing
              , mockDeleteUser = \uid -> return $ uid == UserId 42
              }

        app <- liftIO $ createTestApp mockService
        response <- runSession (request defaultRequest { requestMethod = "DELETE", rawPathInfo = "/api/users/42" }) app

        simpleStatus response `shouldBe` status200

    describe "API Integration Tests" $ do
      it "should handle complete user lifecycle" $ do
        let testUser = User (UserId 1) "Test User" "test@example.com"
        let updatedUser = User (UserId 1) "Updated User" "updated@example.com"

        let mockService = MockUserService
              { mockCreateUser = \req -> return $ User (UserId 1) (createUserName req) (createUserEmail req)
              , mockGetUserById = \uid ->
                  if uid == UserId 1
                    then return $ Just testUser
                    else return Nothing
              , mockGetAllUsers = return [testUser]
              , mockUpdateUser = \uid req ->
                  if uid == UserId 1
                    then return $ Just $ User uid (createUserName req) (createUserEmail req)
                    else return Nothing
              , mockDeleteUser = \uid -> return $ uid == UserId 1
              }

        app <- liftIO $ createTestApp mockService

        -- Create user
        let createReq = defaultRequest
                { requestMethod = "POST"
                , rawPathInfo = "/api/users"
                , requestBody = encode (CreateUserRequest "Test User" "test@example.com")
                }
        createResponse <- runSession (request createReq) app
        simpleStatus createResponse `shouldBe` status201

        -- Get user
        let getReq = defaultRequest { requestMethod = "GET", rawPathInfo = "/api/users/1" }
        getResponse <- runSession (request getReq) app
        simpleStatus getResponse `shouldBe` status200

        -- Update user
        let updateReq = defaultRequest
                { requestMethod = "PUT"
                , rawPathInfo = "/api/users/1"
                , requestBody = encode (CreateUserRequest "Updated User" "updated@example.com")
                }
        updateResponse <- runSession (request updateReq) app
        simpleStatus updateResponse `shouldBe` status200

        -- Delete user
        let deleteReq = defaultRequest { requestMethod = "DELETE", rawPathInfo = "/api/users/1" }
        deleteResponse <- runSession (request deleteReq) app
        simpleStatus deleteResponse `shouldBe` status200

    describe "Error handling" $ do
      it "should handle malformed requests gracefully" $ do
        let mockService = MockUserService
              { mockCreateUser = \_ -> return $ User (UserId 1) "" ""
              , mockGetUserById = \_ -> return Nothing
              , mockGetAllUsers = return []
              , mockUpdateUser = \_ _ -> return Nothing
              , mockDeleteUser = \_ -> return False
              }

        app <- liftIO $ createTestApp mockService

        -- Test various malformed requests
        let malformedRequests =
              [ defaultRequest { requestMethod = "GET", rawPathInfo = "/api/users/" }
              , defaultRequest { requestMethod = "GET", rawPathInfo = "/api/users/-1" }
              , defaultRequest { requestMethod = "GET", rawPathInfo = "/api/users/0" }
              ]

        forM_ malformedRequests $ \req -> do
          response <- runSession (request req) app
          simpleStatus response `shouldSatisfy` \status ->
            status == status400 || status == status404

