{-# LANGUAGE OverloadedStrings #-}

module UserServiceTest (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Monad.IO.Class (MonadIO, liftIO)
import User (User(..), UserId(..), CreateUserRequest(..), UserResponse(..), toUserResponse)
import UserService (UserService(..), UserServiceC, createUser, getUserById, getAllUsers, updateUser, deleteUser)
import UserRepository (InMemoryUserRepository, createInMemoryUserRepository, UserRepository(..))
import TestUtils (genCreateUserRequest, genUser, createTestUserRepository, runTestAppM)

-- Mock UserService implementation for testing
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

-- Helper to run tests with mock service
runMockTest :: MockUserService -> ReaderT MockUserService IO a -> IO a
runMockTest mockService action = runReaderT action mockService

spec :: Spec
spec = do
  describe "UserService" $ do
    describe "createUser" $ do
      it "should create a user and return UserResponse" $ do
        let mockService = MockUserService
              { mockCreateUser = \req -> return $ User (UserId 1) (createUserName req) (createUserEmail req)
              , mockGetUserById = \_ -> return Nothing
              , mockGetAllUsers = return []
              , mockUpdateUser = \_ _ -> return Nothing
              , mockDeleteUser = \_ -> return False
              }

        let req = CreateUserRequest "John Doe" "john@example.com"
        response <- runMockTest mockService $ createUser req

        userResponseId response `shouldBe` UserId 1
        userResponseName response `shouldBe` "John Doe"
        userResponseEmail response `shouldBe` "john@example.com"

      it "should work with generated data" $ property $ \req ->
        let mockService = MockUserService
              { mockCreateUser = \r -> return $ User (UserId 1) (createUserName r) (createUserEmail r)
              , mockGetUserById = \_ -> return Nothing
              , mockGetAllUsers = return []
              , mockUpdateUser = \_ _ -> return Nothing
              , mockDeleteUser = \_ -> return False
              }

        let testAction = do
              response <- createUser req
              return $ (userResponseName response == createUserName req) &&
                       (userResponseEmail response == createUserEmail req)

        runMockTest mockService testAction `shouldReturn` True

    describe "getUserById" $ do
      it "should return Nothing for non-existent user" $ do
        let mockService = MockUserService
              { mockCreateUser = \_ -> return $ User (UserId 1) "" ""
              , mockGetUserById = \_ -> return Nothing
              , mockGetAllUsers = return []
              , mockUpdateUser = \_ _ -> return Nothing
              , mockDeleteUser = \_ -> return False
              }

        let uid = UserId 999
        result <- runMockTest mockService $ getUserById uid
        result `shouldBe` Nothing

      it "should return UserResponse for existing user" $ do
        let testUser = User (UserId 42) "Alice Smith" "alice@example.com"
        let mockService = MockUserService
              { mockCreateUser = \_ -> return testUser
              , mockGetUserById = \uid -> return $ if uid == UserId 42 then Just testUser else Nothing
              , mockGetAllUsers = return [testUser]
              , mockUpdateUser = \_ _ -> return Nothing
              , mockDeleteUser = \_ -> return False
              }

        let uid = UserId 42
        result <- runMockTest mockService $ getUserById uid

        case result of
          Nothing -> expectationFailure "User should exist"
          Just response -> do
            userResponseId response `shouldBe` UserId 42
            userResponseName response `shouldBe` "Alice Smith"
            userResponseEmail response `shouldBe` "alice@example.com"

      it "should work with generated data" $ property $ \user ->
        let mockService = MockUserService
              { mockCreateUser = \_ -> return user
              , mockGetUserById = \uid -> return $ if uid == userId user then Just user else Nothing
              , mockGetAllUsers = return [user]
              , mockUpdateUser = \_ _ -> return Nothing
              , mockDeleteUser = \_ -> return False
              }

        let testAction = do
              result <- getUserById (userId user)
              case result of
                Nothing -> return False
                Just response -> return $ (userResponseId response == userId user) &&
                                 (userResponseName response == userName user) &&
                                 (userResponseEmail response == userEmail user)

        runMockTest mockService testAction `shouldReturn` True

    describe "getAllUsers" $ do
      it "should return empty list when no users exist" $ do
        let mockService = MockUserService
              { mockCreateUser = \_ -> return $ User (UserId 1) "" ""
              , mockGetUserById = \_ -> return Nothing
              , mockGetAllUsers = return []
              , mockUpdateUser = \_ _ -> return Nothing
              , mockDeleteUser = \_ -> return False
              }

        result <- runMockTest mockService getAllUsers
        result `shouldBe` []

      it "should return all users as UserResponse" $ do
        let users = [ User (UserId 1) "User 1" "user1@example.com"
                    , User (UserId 2) "User 2" "user2@example.com"
                    , User (UserId 3) "User 3" "user3@example.com"
                    ]
        let mockService = MockUserService
              { mockCreateUser = \_ -> return $ head users
              , mockGetUserById = \uid -> return $ find (\u -> userId u == uid) users
              , mockGetAllUsers = return users
              , mockUpdateUser = \_ _ -> return Nothing
              , mockDeleteUser = \_ -> return False
              }

        result <- runMockTest mockService getAllUsers
        length result `shouldBe` 3

        let userIds = map userResponseId result
        userIds `shouldContain` UserId 1
        userIds `shouldContain` UserId 2
        userIds `shouldContain` UserId 3

        let userNames = map userResponseName result
        userNames `shouldContain` "User 1"
        userNames `shouldContain` "User 2"
        userNames `shouldContain` "User 3"

      it "should work with generated data" $ property $ \users ->
        let mockService = MockUserService
              { mockCreateUser = \_ -> return $ head users
              , mockGetUserById = \uid -> return $ find (\u -> userId u == uid) users
              , mockGetAllUsers = return users
              , mockUpdateUser = \_ _ -> return Nothing
              , mockDeleteUser = \_ -> return False
              }

        let testAction = do
              responses <- getAllUsers
              return $ length responses == length users &&
                       all (\response -> any (\user -> userResponseId response == userId user &&
                                                   userResponseName response == userName user &&
                                                   userResponseEmail response == userEmail user) users) responses

        runMockTest mockService testAction `shouldReturn` True

    describe "updateUser" $ do
      it "should return Nothing for non-existent user" $ do
        let mockService = MockUserService
              { mockCreateUser = \_ -> return $ User (UserId 1) "" ""
              , mockGetUserById = \_ -> return Nothing
              , mockGetAllUsers = return []
              , mockUpdateUser = \_ _ -> return Nothing
              , mockDeleteUser = \_ -> return False
              }

        let uid = UserId 999
            req = CreateUserRequest "Updated" "updated@example.com"
        result <- runMockTest mockService $ updateUser uid req
        result `shouldBe` Nothing

      it "should return updated UserResponse for existing user" $ do
        let originalUser = User (UserId 42) "Original" "original@example.com"
        let mockService = MockUserService
              { mockCreateUser = \_ -> return originalUser
              , mockGetUserById = \uid -> return $ if uid == UserId 42 then Just originalUser else Nothing
              , mockGetAllUsers = return [originalUser]
              , mockUpdateUser = \uid req ->
                  if uid == UserId 42
                    then return $ Just $ User uid (createUserName req) (createUserEmail req)
                    else return Nothing
              , mockDeleteUser = \_ -> return False
              }

        let uid = UserId 42
            req = CreateUserRequest "Updated" "updated@example.com"
        result <- runMockTest mockService $ updateUser uid req

        case result of
          Nothing -> expectationFailure "Update should have succeeded"
          Just response -> do
            userResponseId response `shouldBe` UserId 42
            userResponseName response `shouldBe` "Updated"
            userResponseEmail response `shouldBe` "updated@example.com"

      it "should work with generated data" $ property $ \user updateReq ->
        let mockService = MockUserService
              { mockCreateUser = \_ -> return user
              , mockGetUserById = \uid -> return $ if uid == userId user then Just user else Nothing
              , mockGetAllUsers = return [user]
              , mockUpdateUser = \uid req ->
                  if uid == userId user
                    then return $ Just $ User uid (createUserName req) (createUserEmail req)
                    else return Nothing
              , mockDeleteUser = \_ -> return False
              }

        let testAction = do
              result <- updateUser (userId user) updateReq
              case result of
                Nothing -> return False
                Just response -> return $ (userResponseId response == userId user) &&
                                 (userResponseName response == createUserName updateReq) &&
                                 (userResponseEmail response == createUserEmail updateReq)

        runMockTest mockService testAction `shouldReturn` True

    describe "deleteUser" $ do
      it "should return False for non-existent user" $ do
        let mockService = MockUserService
              { mockCreateUser = \_ -> return $ User (UserId 1) "" ""
              , mockGetUserById = \_ -> return Nothing
              , mockGetAllUsers = return []
              , mockUpdateUser = \_ _ -> return Nothing
              , mockDeleteUser = \_ -> return False
              }

        let uid = UserId 999
        result <- runMockTest mockService $ deleteUser uid
        result `shouldBe` False

      it "should return True for existing user" $ do
        let testUser = User (UserId 42) "To Delete" "delete@example.com"
        let mockService = MockUserService
              { mockCreateUser = \_ -> return testUser
              , mockGetUserById = \uid -> return $ if uid == UserId 42 then Just testUser else Nothing
              , mockGetAllUsers = return [testUser]
              , mockUpdateUser = \_ _ -> return Nothing
              , mockDeleteUser = \uid -> return $ uid == UserId 42
              }

        let uid = UserId 42
        result <- runMockTest mockService $ deleteUser uid
        result `shouldBe` True

      it "should work with generated data" $ property $ \user ->
        let mockService = MockUserService
              { mockCreateUser = \_ -> return user
              , mockGetUserById = \uid -> return $ if uid == userId user then Just user else Nothing
              , mockGetAllUsers = return [user]
              , mockUpdateUser = \_ _ -> return Nothing
              , mockDeleteUser = \uid -> return $ uid == userId user
              }

        let testAction = do
              result <- deleteUser (userId user)
              return result

        runMockTest mockService testAction `shouldReturn` True

    describe "Service layer integration" $ do
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

        let testAction = do
              -- Create user
              createdResponse <- createUser (CreateUserRequest "Test User" "test@example.com")

              -- Get user
              retrievedResponse <- getUserById (userResponseId createdResponse)

              -- Get all users
              allResponses <- getAllUsers

              -- Update user
              updatedResponse <- updateUser (userResponseId createdResponse) (CreateUserRequest "Updated User" "updated@example.com")

              -- Delete user
              deleteResult <- deleteUser (userResponseId createdResponse)

              return (createdResponse, retrievedResponse, allResponses, updatedResponse, deleteResult)

        (created, retrieved, allUsers, updated, deleted) <- runMockTest mockService testAction

        -- Verify create
        userResponseName created `shouldBe` "Test User"
        userResponseEmail created `shouldBe` "test@example.com"

        -- Verify retrieve
        case retrieved of
          Nothing -> expectationFailure "User should exist"
          Just response -> userResponseName response `shouldBe` "Test User"

        -- Verify get all
        length allUsers `shouldBe` 1

        -- Verify update
        case updated of
          Nothing -> expectationFailure "Update should succeed"
          Just response -> do
            userResponseName response `shouldBe` "Updated User"
            userResponseEmail response `shouldBe` "updated@example.com"

        -- Verify delete
        deleted `shouldBe` True

    describe "Error handling" $ do
      it "should handle repository errors gracefully" $ do
        let mockService = MockUserService
              { mockCreateUser = \_ -> error "Repository error"
              , mockGetUserById = \_ -> error "Repository error"
              , mockGetAllUsers = error "Repository error"
              , mockUpdateUser = \_ _ -> error "Repository error"
              , mockDeleteUser = \_ -> error "Repository error"
              }

        let req = CreateUserRequest "Test" "test@example.com"

        -- These should throw exceptions, which is expected behavior
        runMockTest mockService (createUser req) `shouldThrow` anyException
        runMockTest mockService (getUserById (UserId 1)) `shouldThrow` anyException
        runMockTest mockService getAllUsers `shouldThrow` anyException
        runMockTest mockService (updateUser (UserId 1) req) `shouldThrow` anyException
        runMockTest mockService (deleteUser (UserId 1)) `shouldThrow` anyException

-- Helper function for finding elements
find :: (a -> Bool) -> [a] -> Maybe a
find _ [] = Nothing
find p (x:xs) = if p x then Just x else find p xs

