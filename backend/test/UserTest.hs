{-# LANGUAGE OverloadedStrings #-}

module UserTest (spec) where

import Test.Hspec
import Test.QuickCheck
import Data.Aeson (encode, decode, toJSON, fromJSON)
import Data.Aeson.Types (parseEither)
import User (User(..), UserId(..), CreateUserRequest(..), UserResponse(..), toUserResponse)
import TestUtils (genUser, genCreateUserRequest, genUserResponse, validUserNames, validUserEmails, invalidUserNames, invalidUserEmails)

spec :: Spec
spec = do
  describe "User Domain Model" $ do
    describe "UserId" $ do
      it "should have correct Show instance" $ do
        show (UserId 123) `shouldBe` "UserId {unUserId = 123}"

      it "should have correct Eq instance" $ do
        UserId 1 `shouldBe` UserId 1
        UserId 1 `shouldNotBe` UserId 2

      it "should serialize to JSON correctly" $ property $ \userId ->
        let json = encode userId
            decoded = decode json :: Maybe UserId
        in decoded `shouldBe` Just userId

      it "should deserialize from JSON correctly" $ property $ \userId ->
        let json = encode userId
            decoded = decode json :: Maybe UserId
        in decoded `shouldBe` Just userId

    describe "User" $ do
      it "should have correct Show instance" $ do
        let user = User (UserId 1) "John Doe" "john@example.com"
        show user `shouldContain` "UserId {unUserId = 1}"
        show user `shouldContain` "John Doe"
        show user `shouldContain` "john@example.com"

      it "should have correct Eq instance" $ do
        let user1 = User (UserId 1) "John Doe" "john@example.com"
            user2 = User (UserId 1) "John Doe" "john@example.com"
            user3 = User (UserId 2) "John Doe" "john@example.com"
        user1 `shouldBe` user2
        user1 `shouldNotBe` user3

      it "should serialize to JSON correctly" $ property $ \user ->
        let json = encode user
            decoded = decode json :: Maybe User
        in decoded `shouldBe` Just user

      it "should deserialize from JSON correctly" $ property $ \user ->
        let json = encode user
            decoded = decode json :: Maybe User
        in decoded `shouldBe` Just user

      it "should convert to UserResponse correctly" $ property $ \user ->
        let response = toUserResponse user
        in userResponseId response `shouldBe` userId user
        in userResponseName response `shouldBe` userName user
        in userResponseEmail response `shouldBe` userEmail user

    describe "CreateUserRequest" $ do
      it "should have correct Show instance" $ do
        let req = CreateUserRequest "John Doe" "john@example.com"
        show req `shouldContain` "John Doe"
        show req `shouldContain` "john@example.com"

      it "should have correct Eq instance" $ do
        let req1 = CreateUserRequest "John Doe" "john@example.com"
            req2 = CreateUserRequest "John Doe" "john@example.com"
            req3 = CreateUserRequest "Jane Doe" "john@example.com"
        req1 `shouldBe` req2
        req1 `shouldNotBe` req3

      it "should serialize to JSON correctly" $ property $ \req ->
        let json = encode req
            decoded = decode json :: Maybe CreateUserRequest
        in decoded `shouldBe` Just req

      it "should deserialize from JSON correctly" $ property $ \req ->
        let json = encode req
            decoded = decode json :: Maybe CreateUserRequest
        in decoded `shouldBe` Just req

    describe "UserResponse" $ do
      it "should have correct Show instance" $ do
        let response = UserResponse (UserId 1) "John Doe" "john@example.com"
        show response `shouldContain` "UserId {unUserId = 1}"
        show response `shouldContain` "John Doe"
        show response `shouldContain` "john@example.com"

      it "should have correct Eq instance" $ do
        let resp1 = UserResponse (UserId 1) "John Doe" "john@example.com"
            resp2 = UserResponse (UserId 1) "John Doe" "john@example.com"
            resp3 = UserResponse (UserId 2) "John Doe" "john@example.com"
        resp1 `shouldBe` resp2
        resp1 `shouldNotBe` resp3

      it "should serialize to JSON correctly" $ property $ \response ->
        let json = encode response
            decoded = decode json :: Maybe UserResponse
        in decoded `shouldBe` Just response

      it "should deserialize from JSON correctly" $ property $ \response ->
        let json = encode response
            decoded = decode json :: Maybe UserResponse
        in decoded `shouldBe` Just response

    describe "toUserResponse conversion" $ do
      it "should preserve all user data" $ property $ \user ->
        let response = toUserResponse user
        in userResponseId response `shouldBe` userId user
        in userResponseName response `shouldBe` userName user
        in userResponseEmail response `shouldBe` userEmail user

      it "should work with specific examples" $ do
        let user = User (UserId 42) "Alice Smith" "alice@example.com"
            response = toUserResponse user
        userResponseId response `shouldBe` UserId 42
        userResponseName response `shouldBe` "Alice Smith"
        userResponseEmail response `shouldBe` "alice@example.com"

    describe "JSON round-trip properties" $ do
      it "should preserve UserId through JSON serialization" $ property $ \userId ->
        let json = encode userId
            decoded = decode json :: Maybe UserId
        in decoded `shouldBe` Just userId

      it "should preserve User through JSON serialization" $ property $ \user ->
        let json = encode user
            decoded = decode json :: Maybe User
        in decoded `shouldBe` Just user

      it "should preserve CreateUserRequest through JSON serialization" $ property $ \req ->
        let json = encode req
            decoded = decode json :: Maybe CreateUserRequest
        in decoded `shouldBe` Just req

      it "should preserve UserResponse through JSON serialization" $ property $ \response ->
        let json = encode response
            decoded = decode json :: Maybe UserResponse
        in decoded `shouldBe` Just response

    describe "Edge cases and validation" $ do
      it "should handle empty strings in user names" $ do
        let user = User (UserId 1) "" "test@example.com"
        let response = toUserResponse user
        userResponseName response `shouldBe` ""

      it "should handle special characters in user names" $ do
        let specialName = "User-Name_123.test"
        let user = User (UserId 1) specialName "test@example.com"
        let response = toUserResponse user
        userResponseName response `shouldBe` specialName

      it "should handle long user names" $ do
        let longName = replicate 100 'A'
        let user = User (UserId 1) longName "test@example.com"
        let response = toUserResponse user
        userResponseName response `shouldBe` longName

      it "should handle various email formats" $ do
        let emails = ["user@domain.com", "user+tag@domain.co.uk", "first.last@sub.domain.org"]
        forM_ emails $ \email -> do
          let user = User (UserId 1) "Test User" email
          let response = toUserResponse user
          userResponseEmail response `shouldBe` email

