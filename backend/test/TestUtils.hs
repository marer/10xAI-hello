{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module TestUtils
  ( genUserId
  , genUserName
  , genUserEmail
  , genCreateUserRequest
  , genUser
  , genUserResponse
  , validUserNames
  , validUserEmails
  , invalidUserNames
  , invalidUserEmails
  , runTestAppM
  , createTestUserRepository
  ) where

import Test.QuickCheck (Arbitrary(..), Gen, elements, listOf1, choose, suchThat)
import Test.QuickCheck.Arbitrary.Generic (genericArbitrary)
import Data.Char (isAlpha, isAlphaNum, isAscii)
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import User (User(..), UserId(..), CreateUserRequest(..), UserResponse(..), toUserResponse)
import UserRepository (InMemoryUserRepository, createInMemoryUserRepository)

-- | Generate valid user IDs
genUserId :: Gen UserId
genUserId = UserId <$> choose (1, 10000)

-- | Generate valid user names
genUserName :: Gen String
genUserName = do
  length <- choose (2, 50)
  name <- listOf1 $ elements validNameChars
  return $ take length name
  where
    validNameChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ [' ', '-', '_', '.']

-- | Generate valid email addresses
genUserEmail :: Gen String
genUserEmail = do
  localPart <- genEmailLocalPart
  domain <- genEmailDomain
  return $ localPart ++ "@" ++ domain
  where
    genEmailLocalPart = do
      length <- choose (1, 30)
      chars <- listOf1 $ elements validEmailChars
      return $ take length chars
    genEmailDomain = do
      domainName <- listOf1 $ elements (['a'..'z'] ++ ['0'..'9'])
      tld <- elements ["com", "org", "net", "edu", "gov", "io", "co.uk"]
      return $ take 10 domainName ++ "." ++ tld
    validEmailChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['.', '+', '-', '_']

-- | Generate CreateUserRequest
genCreateUserRequest :: Gen CreateUserRequest
genCreateUserRequest = CreateUserRequest <$> genUserName <*> genUserEmail

-- | Generate User
genUser :: Gen User
genUser = User <$> genUserId <*> genUserName <*> genUserEmail

-- | Generate UserResponse
genUserResponse :: Gen UserResponse
genUserResponse = UserResponse <$> genUserId <*> genUserName <*> genUserEmail

-- | Valid user names for testing
validUserNames :: [String]
validUserNames =
  [ "John Doe"
  , "Alice Smith"
  , "Bob123"
  , "user_name"
  , "test.user"
  , "Mary-Jane"
  , "X"
  , "A" ++ replicate 49 'B'
  ]

-- | Valid email addresses for testing
validUserEmails :: [String]
validUserEmails =
  [ "user@example.com"
  , "test.email@domain.org"
  , "user+tag@example.net"
  , "a@b.co"
  , "user123@test-domain.edu"
  , "first.last@company.io"
  ]

-- | Invalid user names for testing
invalidUserNames :: [String]
invalidUserNames =
  [ ""
  , " "
  , "A"
  , replicate 51 'A'  -- Too long
  , "User@Invalid"
  , "User#Invalid"
  , "User$Invalid"
  ]

-- | Invalid email addresses for testing
invalidUserEmails :: [String]
invalidUserEmails =
  [ ""
  , "invalid-email"
  , "@domain.com"
  , "user@"
  , "user@domain"
  , "user..double@domain.com"
  , ".user@domain.com"
  , "user@.domain.com"
  , "user@domain..com"
  ]

-- | Run a test in the AppM monad
runTestAppM :: (MonadIO m) => ReaderT InMemoryUserRepository m a -> m a
runTestAppM action = do
  repo <- liftIO createInMemoryUserRepository
  runReaderT action repo

-- | Create a test user repository
createTestUserRepository :: IO InMemoryUserRepository
createTestUserRepository = createInMemoryUserRepository

-- | Arbitrary instances for QuickCheck
instance Arbitrary UserId where
  arbitrary = genUserId

instance Arbitrary User where
  arbitrary = genUser

instance Arbitrary CreateUserRequest where
  arbitrary = genCreateUserRequest

instance Arbitrary UserResponse where
  arbitrary = genUserResponse

