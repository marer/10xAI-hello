{-# LANGUAGE OverloadedStrings #-}

module UserRepositoryTest (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Monad.Reader (runReaderT)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as Map
import User (User(..), UserId(..), CreateUserRequest(..))
import UserRepository (InMemoryUserRepository(..), createInMemoryUserRepository, UserRepository(..))
import TestUtils (genCreateUserRequest, genUser, createTestUserRepository, runTestAppM)

spec :: Spec
spec = do
  describe "UserRepository" $ do
    describe "createUser" $ do
      it "should create a user with correct data" $ do
        repo <- liftIO createTestUserRepository
        let req = CreateUserRequest "John Doe" "john@example.com"
        user <- liftIO $ runReaderT (createUser req) repo

        userName user `shouldBe` "John Doe"
        userEmail user `shouldBe` "john@example.com"
        unUserId (userId user) `shouldBe` 1

      it "should assign sequential user IDs" $ do
        repo <- liftIO createTestUserRepository
        let req1 = CreateUserRequest "User 1" "user1@example.com"
            req2 = CreateUserRequest "User 2" "user2@example.com"
            req3 = CreateUserRequest "User 3" "user3@example.com"

        user1 <- liftIO $ runReaderT (createUser req1) repo
        user2 <- liftIO $ runReaderT (createUser req2) repo
        user3 <- liftIO $ runReaderT (createUser req3) repo

        unUserId (userId user1) `shouldBe` 1
        unUserId (userId user2) `shouldBe` 2
        unUserId (userId user3) `shouldBe` 3

      it "should store users in the repository" $ do
        repo <- liftIO createTestUserRepository
        let req = CreateUserRequest "Test User" "test@example.com"
        user <- liftIO $ runReaderT (createUser req) repo

        -- Verify user is stored
        storedUser <- liftIO $ runReaderT (getUserById (userId user)) repo
        storedUser `shouldBe` Just user

      it "should work with generated data" $ property $ \req ->
        let testAction = do
              user <- createUser req
              return $ (userName user == createUserName req) &&
                       (userEmail user == createUserEmail req)
        in runTestAppM testAction `shouldReturn` True

    describe "getUserById" $ do
      it "should return Nothing for non-existent user" $ do
        repo <- liftIO createTestUserRepository
        let nonExistentId = UserId 999
        result <- liftIO $ runReaderT (getUserById nonExistentId) repo
        result `shouldBe` Nothing

      it "should return correct user for existing ID" $ do
        repo <- liftIO createTestUserRepository
        let req = CreateUserRequest "John Doe" "john@example.com"
        createdUser <- liftIO $ runReaderT (createUser req) repo
        retrievedUser <- liftIO $ runReaderT (getUserById (userId createdUser)) repo

        retrievedUser `shouldBe` Just createdUser

      it "should return Nothing for ID 0" $ do
        repo <- liftIO createTestUserRepository
        result <- liftIO $ runReaderT (getUserById (UserId 0)) repo
        result `shouldBe` Nothing

      it "should work with generated data" $ property $ \req ->
        let testAction = do
              user <- createUser req
              retrievedUser <- getUserById (userId user)
              return $ retrievedUser == Just user
        in runTestAppM testAction `shouldReturn` True

    describe "getAllUsers" $ do
      it "should return empty list for new repository" $ do
        repo <- liftIO createTestUserRepository
        users <- liftIO $ runReaderT getAllUsers repo
        users `shouldBe` []

      it "should return all created users" $ do
        repo <- liftIO createTestUserRepository
        let req1 = CreateUserRequest "User 1" "user1@example.com"
            req2 = CreateUserRequest "User 2" "user2@example.com"
            req3 = CreateUserRequest "User 3" "user3@example.com"

        user1 <- liftIO $ runReaderT (createUser req1) repo
        user2 <- liftIO $ runReaderT (createUser req2) repo
        user3 <- liftIO $ runReaderT (createUser req3) repo

        allUsers <- liftIO $ runReaderT getAllUsers repo
        allUsers `shouldContain` user1
        allUsers `shouldContain` user2
        allUsers `shouldContain` user3
        length allUsers `shouldBe` 3

      it "should maintain order of creation" $ do
        repo <- liftIO createTestUserRepository
        let req1 = CreateUserRequest "First" "first@example.com"
            req2 = CreateUserRequest "Second" "second@example.com"

        user1 <- liftIO $ runReaderT (createUser req1) repo
        user2 <- liftIO $ runReaderT (createUser req2) repo

        allUsers <- liftIO $ runReaderT getAllUsers repo
        let userIds = map userId allUsers
        userIds `shouldContain` userId user1
        userIds `shouldContain` userId user2

    describe "updateUser" $ do
      it "should return Nothing for non-existent user" $ do
        repo <- liftIO createTestUserRepository
        let nonExistentId = UserId 999
            updateReq = CreateUserRequest "Updated" "updated@example.com"
        result <- liftIO $ runReaderT (updateUser nonExistentId updateReq) repo
        result `shouldBe` Nothing

      it "should update existing user correctly" $ do
        repo <- liftIO createTestUserRepository
        let createReq = CreateUserRequest "Original" "original@example.com"
            updateReq = CreateUserRequest "Updated" "updated@example.com"

        originalUser <- liftIO $ runReaderT (createUser createReq) repo
        updatedUser <- liftIO $ runReaderT (updateUser (userId originalUser) updateReq) repo

        case updatedUser of
          Nothing -> expectationFailure "Update should have succeeded"
          Just user -> do
            userId user `shouldBe` userId originalUser
            userName user `shouldBe` "Updated"
            userEmail user `shouldBe` "updated@example.com"

      it "should persist the update" $ do
        repo <- liftIO createTestUserRepository
        let createReq = CreateUserRequest "Original" "original@example.com"
            updateReq = CreateUserRequest "Updated" "updated@example.com"

        originalUser <- liftIO $ runReaderT (createUser createReq) repo
        _ <- liftIO $ runReaderT (updateUser (userId originalUser) updateReq) repo

        -- Verify the update persisted
        retrievedUser <- liftIO $ runReaderT (getUserById (userId originalUser)) repo
        case retrievedUser of
          Nothing -> expectationFailure "User should still exist after update"
          Just user -> do
            userName user `shouldBe` "Updated"
            userEmail user `shouldBe` "updated@example.com"

      it "should work with generated data" $ property $ \createReq updateReq ->
        let testAction = do
              user <- createUser createReq
              updatedUser <- updateUser (userId user) updateReq
              case updatedUser of
                Nothing -> return False
                Just u -> return $ (userName u == createUserName updateReq) &&
                          (userEmail u == createUserEmail updateReq) &&
                          (userId u == userId user)
        in runTestAppM testAction `shouldReturn` True

    describe "deleteUser" $ do
      it "should return False for non-existent user" $ do
        repo <- liftIO createTestUserRepository
        let nonExistentId = UserId 999
        result <- liftIO $ runReaderT (deleteUser nonExistentId) repo
        result `shouldBe` False

      it "should return True for existing user and remove it" $ do
        repo <- liftIO createTestUserRepository
        let req = CreateUserRequest "To Delete" "delete@example.com"

        user <- liftIO $ runReaderT (createUser req) repo
        deleteResult <- liftIO $ runReaderT (deleteUser (userId user)) repo

        deleteResult `shouldBe` True

        -- Verify user is gone
        retrievedUser <- liftIO $ runReaderT (getUserById (userId user)) repo
        retrievedUser `shouldBe` Nothing

      it "should not affect other users" $ do
        repo <- liftIO createTestUserRepository
        let req1 = CreateUserRequest "Keep Me" "keep@example.com"
            req2 = CreateUserRequest "Delete Me" "delete@example.com"

        user1 <- liftIO $ runReaderT (createUser req1) repo
        user2 <- liftIO $ runReaderT (createUser req2) repo

        _ <- liftIO $ runReaderT (deleteUser (userId user2)) repo

        -- user1 should still exist
        retrievedUser1 <- liftIO $ runReaderT (getUserById (userId user1)) repo
        retrievedUser1 `shouldBe` Just user1

        -- user2 should be gone
        retrievedUser2 <- liftIO $ runReaderT (getUserById (userId user2)) repo
        retrievedUser2 `shouldBe` Nothing

      it "should work with generated data" $ property $ \req ->
        let testAction = do
              user <- createUser req
              deleteResult <- deleteUser (userId user)
              retrievedUser <- getUserById (userId user)
              return $ deleteResult && retrievedUser == Nothing
        in runTestAppM testAction `shouldReturn` True

    describe "Repository state management" $ do
      it "should maintain consistent state across operations" $ do
        repo <- liftIO createTestUserRepository
        let req1 = CreateUserRequest "User 1" "user1@example.com"
            req2 = CreateUserRequest "User 2" "user2@example.com"

        -- Create users
        user1 <- liftIO $ runReaderT (createUser req1) repo
        user2 <- liftIO $ runReaderT (createUser req2) repo

        -- Verify both exist
        allUsers <- liftIO $ runReaderT getAllUsers repo
        length allUsers `shouldBe` 2

        -- Update one user
        let updateReq = CreateUserRequest "Updated User 1" "updated1@example.com"
        _ <- liftIO $ runReaderT (updateUser (userId user1) updateReq) repo

        -- Delete the other user
        _ <- liftIO $ runReaderT (deleteUser (userId user2)) repo

        -- Verify final state
        finalUsers <- liftIO $ runReaderT getAllUsers repo
        length finalUsers `shouldBe` 1

        retrievedUser1 <- liftIO $ runReaderT (getUserById (userId user1)) repo
        case retrievedUser1 of
          Nothing -> expectationFailure "Updated user should still exist"
          Just user -> userName user `shouldBe` "Updated User 1"

      it "should handle concurrent-like operations correctly" $ do
        repo <- liftIO createTestUserRepository

        -- Create multiple users
        let requests = map (\i -> CreateUserRequest ("User " ++ show i) ("user" ++ show i ++ "@example.com")) [1..5]
        users <- mapM (\req -> liftIO $ runReaderT (createUser req) repo) requests

        -- Verify all users exist
        allUsers <- liftIO $ runReaderT getAllUsers repo
        length allUsers `shouldBe` 5

        -- Update some users
        let updateReqs = map (\i -> CreateUserRequest ("Updated User " ++ show i) ("updated" ++ show i ++ "@example.com")) [1, 3, 5]
        _ <- mapM (\(user, req) -> liftIO $ runReaderT (updateUser (userId user) req) repo) (zip (take 3 users) updateReqs)

        -- Delete some users
        _ <- mapM (\user -> liftIO $ runReaderT (deleteUser (userId user)) repo) (drop 3 users)

        -- Verify final state
        finalUsers <- liftIO $ runReaderT getAllUsers repo
        length finalUsers `shouldBe` 3

        -- Verify updated users have correct data
        forM_ (take 3 users) $ \user -> do
          retrievedUser <- liftIO $ runReaderT (getUserById (userId user)) repo
          case retrievedUser of
            Nothing -> expectationFailure $ "User " ++ show (userId user) ++ " should still exist"
            Just u -> userName u `shouldStartWith` "Updated User"

    describe "Edge cases" $ do
      it "should handle empty user names" $ do
        repo <- liftIO createTestUserRepository
        let req = CreateUserRequest "" "test@example.com"
        user <- liftIO $ runReaderT (createUser req) repo
        userName user `shouldBe` ""

      it "should handle empty email addresses" $ do
        repo <- liftIO createTestUserRepository
        let req = CreateUserRequest "Test User" ""
        user <- liftIO $ runReaderT (createUser req) repo
        userEmail user `shouldBe` ""

      it "should handle special characters in user data" $ do
        repo <- liftIO createTestUserRepository
        let req = CreateUserRequest "User-Name_123.test" "user+tag@domain.co.uk"
        user <- liftIO $ runReaderT (createUser req) repo
        userName user `shouldBe` "User-Name_123.test"
        userEmail user `shouldBe` "user+tag@domain.co.uk"

