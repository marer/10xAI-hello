{-# LANGUAGE OverloadedStrings #-}

module IntegrationTest (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (encode, decode)
import Data.ByteString.Lazy (ByteString, fromStrict)
import Network.HTTP.Types (status200, status201, status400, status404)
import Network.Wai (Application)
import Network.Wai.Test (SResponse(..), runSession, request, defaultRequest)
import Web.Scotty (scottyApp, get, post, put, delete, json, param, status, notFound, badRequest, body, liftIO)
import User (User(..), UserId(..), CreateUserRequest(..), UserResponse(..), toUserResponse)
import UserService (UserService(..), UserServiceC, createUser, getUserById, getAllUsers, updateUser, deleteUser)
import UserRepository (InMemoryUserRepository, createInMemoryUserRepository, UserRepository(..))
import API (AppEnv(..), AppM)
import TestUtils (genCreateUserRequest, genUser, createTestUserRepository, runTestAppM)

-- Real integration test using actual UserRepository
createIntegrationApp :: IO Application
createIntegrationApp = do
  userRepo <- createInMemoryUserRepository
  let appEnv = AppEnv { userRepository = userRepo }

  scottyApp $ do
    get "/health" $ do
      status status200
      json ("OK" :: String)

    get "/api/users" $ do
      users <- liftIO $ runReaderT getAllUsers userRepo
      json users

    get "/api/users/:id" $ do
      userIdStr <- param "id"
      let maybeUserId = fmap UserId (readMaybe userIdStr)
      case maybeUserId of
        Nothing -> do
          status status400
          json ("Invalid user ID" :: String)
        Just uid -> do
          maybeUser <- liftIO $ runReaderT (getUserById uid) userRepo
          case maybeUser of
            Nothing -> do
              status status404
              notFound
            Just user -> json user

    post "/api/users" $ do
      body <- body
      case decode body of
        Nothing -> do
          status status400
          json ("Invalid JSON" :: String)
        Just req -> do
          user <- liftIO $ runReaderT (createUser req) userRepo
          status status201
          json user

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
              maybeUser <- liftIO $ runReaderT (updateUser uid req) userRepo
              case maybeUser of
                Nothing -> do
                  status status404
                  notFound
                Just user -> json user

    delete "/api/users/:id" $ do
      userIdStr <- param "id"
      let maybeUserId = fmap UserId (readMaybe userIdStr)
      case maybeUserId of
        Nothing -> do
          status status400
          json ("Invalid user ID" :: String)
        Just uid -> do
          deleted <- liftIO $ runReaderT (deleteUser uid) userRepo
          if deleted
            then status status200
            else status status404

-- Helper for readMaybe
readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
  [(x, "")] -> Just x
  _ -> Nothing

spec :: Spec
spec = do
  describe "Integration Tests" $ do
    describe "Complete User Lifecycle" $ do
      it "should handle full CRUD operations" $ do
        app <- liftIO createIntegrationApp

        -- 1. Create a user
        let createReq = defaultRequest
                { requestMethod = "POST"
                , rawPathInfo = "/api/users"
                , requestBody = encode (CreateUserRequest "Integration Test User" "integration@example.com")
                }
        createResponse <- runSession (request createReq) app
        simpleStatus createResponse `shouldBe` status201

        -- Extract user ID from response
        let createBody = simpleBody createResponse
        let decodedUser = decode createBody :: Maybe UserResponse
        case decodedUser of
          Nothing -> expectationFailure "Failed to decode created user"
          Just user -> do
            let userId = userResponseId user

            -- 2. Get the user by ID
            let getReq = defaultRequest
                    { requestMethod = "GET"
                    , rawPathInfo = "/api/users/" ++ show (unUserId userId)
                    }
            getResponse <- runSession (request getReq) app
            simpleStatus getResponse `shouldBe` status200

            -- 3. Get all users (should include our user)
            let getAllReq = defaultRequest { requestMethod = "GET", rawPathInfo = "/api/users" }
            getAllResponse <- runSession (request getAllReq) app
            simpleStatus getAllResponse `shouldBe` status200
            simpleBody getAllResponse `shouldContain` "Integration Test User"

            -- 4. Update the user
            let updateReq = defaultRequest
                    { requestMethod = "PUT"
                    , rawPathInfo = "/api/users/" ++ show (unUserId userId)
                    , requestBody = encode (CreateUserRequest "Updated Integration User" "updated@example.com")
                    }
            updateResponse <- runSession (request updateReq) app
            simpleStatus updateResponse `shouldBe` status200

            -- 5. Verify the update
            let getUpdatedReq = defaultRequest
                    { requestMethod = "GET"
                    , rawPathInfo = "/api/users/" ++ show (unUserId userId)
                    }
            getUpdatedResponse <- runSession (request getUpdatedReq) app
            simpleStatus getUpdatedResponse `shouldBe` status200
            simpleBody getUpdatedResponse `shouldContain` "Updated Integration User"
            simpleBody getUpdatedResponse `shouldContain` "updated@example.com"

            -- 6. Delete the user
            let deleteReq = defaultRequest
                    { requestMethod = "DELETE"
                    , rawPathInfo = "/api/users/" ++ show (unUserId userId)
                    }
            deleteResponse <- runSession (request deleteReq) app
            simpleStatus deleteResponse `shouldBe` status200

            -- 7. Verify the user is deleted
            let getDeletedReq = defaultRequest
                    { requestMethod = "GET"
                    , rawPathInfo = "/api/users/" ++ show (unUserId userId)
                    }
            getDeletedResponse <- runSession (request getDeletedReq) app
            simpleStatus getDeletedResponse `shouldBe` status404

    describe "Multiple Users Management" $ do
      it "should handle multiple users independently" $ do
        app <- liftIO createIntegrationApp

        -- Create multiple users
        let users = [ CreateUserRequest "User 1" "user1@example.com"
                    , CreateUserRequest "User 2" "user2@example.com"
                    , CreateUserRequest "User 3" "user3@example.com"
                    ]

        createdUsers <- forM users $ \userReq -> do
          let req = defaultRequest
                  { requestMethod = "POST"
                  , rawPathInfo = "/api/users"
                  , requestBody = encode userReq
                  }
          response <- runSession (request req) app
          simpleStatus response `shouldBe` status201

          let body = simpleBody response
          let decodedUser = decode body :: Maybe UserResponse
          case decodedUser of
            Nothing -> expectationFailure "Failed to decode created user"
            Just user -> return user

        -- Verify all users exist
        let getAllReq = defaultRequest { requestMethod = "GET", rawPathInfo = "/api/users" }
        getAllResponse <- runSession (request getAllReq) app
        simpleStatus getAllResponse `shouldBe` status200
        let allUsersBody = simpleBody getAllResponse
        allUsersBody `shouldContain` "User 1"
        allUsersBody `shouldContain` "User 2"
        allUsersBody `shouldContain` "User 3"

        -- Update one user
        let firstUser = head createdUsers
        let updateReq = defaultRequest
                { requestMethod = "PUT"
                , rawPathInfo = "/api/users/" ++ show (unUserId (userResponseId firstUser))
                , requestBody = encode (CreateUserRequest "Updated User 1" "updated1@example.com")
                }
        updateResponse <- runSession (request updateReq) app
        simpleStatus updateResponse `shouldBe` status200

        -- Delete one user
        let secondUser = createdUsers !! 1
        let deleteReq = defaultRequest
                { requestMethod = "DELETE"
                , rawPathInfo = "/api/users/" ++ show (unUserId (userResponseId secondUser))
                }
        deleteResponse <- runSession (request deleteReq) app
        simpleStatus deleteResponse `shouldBe` status200

        -- Verify final state
        let finalGetAllReq = defaultRequest { requestMethod = "GET", rawPathInfo = "/api/users" }
        finalGetAllResponse <- runSession (request finalGetAllReq) app
        simpleStatus finalGetAllResponse `shouldBe` status200
        let finalBody = simpleBody finalGetAllResponse
        finalBody `shouldContain` "Updated User 1"  -- Updated user
        finalBody `shouldNotContain` "User 2"       -- Deleted user
        finalBody `shouldContain` "User 3"          -- Unchanged user

    describe "Error Handling Integration" $ do
      it "should handle invalid requests gracefully" $ do
        app <- liftIO createIntegrationApp

        -- Test invalid user ID
        let invalidIdReq = defaultRequest { requestMethod = "GET", rawPathInfo = "/api/users/invalid" }
        invalidIdResponse <- runSession (request invalidIdReq) app
        simpleStatus invalidIdResponse `shouldBe` status400

        -- Test non-existent user
        let nonExistentReq = defaultRequest { requestMethod = "GET", rawPathInfo = "/api/users/999" }
        nonExistentResponse <- runSession (request nonExistentReq) app
        simpleStatus nonExistentResponse `shouldBe` status404

        -- Test invalid JSON
        let invalidJsonReq = defaultRequest
                { requestMethod = "POST"
                , rawPathInfo = "/api/users"
                , requestBody = fromStrict "invalid json"
                }
        invalidJsonResponse <- runSession (request invalidJsonReq) app
        simpleStatus invalidJsonResponse `shouldBe` status400

        -- Test update non-existent user
        let updateNonExistentReq = defaultRequest
                { requestMethod = "PUT"
                , rawPathInfo = "/api/users/999"
                , requestBody = encode (CreateUserRequest "Test" "test@example.com")
                }
        updateNonExistentResponse <- runSession (request updateNonExistentReq) app
        simpleStatus updateNonExistentResponse `shouldBe` status404

        -- Test delete non-existent user
        let deleteNonExistentReq = defaultRequest { requestMethod = "DELETE", rawPathInfo = "/api/users/999" }
        deleteNonExistentResponse <- runSession (request deleteNonExistentReq) app
        simpleStatus deleteNonExistentResponse `shouldBe` status404

    describe "Data Persistence" $ do
      it "should maintain data consistency across operations" $ do
        app <- liftIO createIntegrationApp

        -- Create a user
        let createReq = defaultRequest
                { requestMethod = "POST"
                , rawPathInfo = "/api/users"
                , requestBody = encode (CreateUserRequest "Persistence Test" "persistence@example.com")
                }
        createResponse <- runSession (request createReq) app
        simpleStatus createResponse `shouldBe` status201

        let createBody = simpleBody createResponse
        let decodedUser = decode createBody :: Maybe UserResponse
        case decodedUser of
          Nothing -> expectationFailure "Failed to decode created user"
          Just user -> do
            let userId = userResponseId user

            -- Perform multiple operations
            forM_ [1..5] $ \i -> do
              -- Update user
              let updateReq = defaultRequest
                      { requestMethod = "PUT"
                      , rawPathInfo = "/api/users/" ++ show (unUserId userId)
                      , requestBody = encode (CreateUserRequest ("Updated " ++ show i) ("updated" ++ show i ++ "@example.com"))
                      }
              updateResponse <- runSession (request updateReq) app
              simpleStatus updateResponse `shouldBe` status200

              -- Get user to verify
              let getReq = defaultRequest
                      { requestMethod = "GET"
                      , rawPathInfo = "/api/users/" ++ show (unUserId userId)
                      }
              getResponse <- runSession (request getReq) app
              simpleStatus getResponse `shouldBe` status200
              simpleBody getResponse `shouldContain` ("Updated " ++ show i)

            -- Final verification
            let finalGetReq = defaultRequest
                    { requestMethod = "GET"
                    , rawPathInfo = "/api/users/" ++ show (unUserId userId)
                    }
            finalGetResponse <- runSession (request finalGetReq) app
            simpleStatus finalGetResponse `shouldBe` status200
            simpleBody finalGetResponse `shouldContain` "Updated 5"

    describe "Concurrent-like Operations" $ do
      it "should handle rapid sequential operations" $ do
        app <- liftIO createIntegrationApp

        -- Create multiple users rapidly
        let userReqs = map (\i -> CreateUserRequest ("Rapid User " ++ show i) ("rapid" ++ show i ++ "@example.com")) [1..10]

        createdUsers <- forM userReqs $ \userReq -> do
          let req = defaultRequest
                  { requestMethod = "POST"
                  , rawPathInfo = "/api/users"
                  , requestBody = encode userReq
                  }
          response <- runSession (request req) app
          simpleStatus response `shouldBe` status201

          let body = simpleBody response
          let decodedUser = decode body :: Maybe UserResponse
          case decodedUser of
            Nothing -> expectationFailure "Failed to decode created user"
            Just user -> return user

        -- Verify all users were created with unique IDs
        let userIds = map (unUserId . userResponseId) createdUsers
        length (nub userIds) `shouldBe` length userIds  -- All IDs should be unique

        -- Perform mixed operations
        forM_ (zip createdUsers [1..]) $ \(user, i) -> do
          if i `mod` 2 == 0
            then do
              -- Update even-indexed users
              let updateReq = defaultRequest
                      { requestMethod = "PUT"
                      , rawPathInfo = "/api/users/" ++ show (unUserId (userResponseId user))
                      , requestBody = encode (CreateUserRequest ("Updated " ++ userResponseName user) ("updated" ++ show i ++ "@example.com"))
                      }
              updateResponse <- runSession (request updateReq) app
              simpleStatus updateResponse `shouldBe` status200
            else do
              -- Delete odd-indexed users
              let deleteReq = defaultRequest
                      { requestMethod = "DELETE"
                      , rawPathInfo = "/api/users/" ++ show (unUserId (userResponseId user))
                      }
              deleteResponse <- runSession (request deleteReq) app
              simpleStatus deleteResponse `shouldBe` status200

        -- Verify final state
        let finalGetAllReq = defaultRequest { requestMethod = "GET", rawPathInfo = "/api/users" }
        finalGetAllResponse <- runSession (request finalGetAllReq) app
        simpleStatus finalGetAllResponse `shouldBe` status200
        let finalBody = simpleBody finalGetAllResponse

        -- Should have 5 users left (even-indexed ones that were updated)
        let remainingUsers = filter (\(_, i) -> i `mod` 2 == 0) (zip createdUsers [1..])
        forM_ remainingUsers $ \(user, i) -> do
          finalBody `shouldContain` ("Updated " ++ userResponseName user)

-- Helper function for nub (remove duplicates)
nub :: Eq a => [a] -> [a]
nub [] = []
nub (x:xs) = x : nub (filter (/= x) xs)

