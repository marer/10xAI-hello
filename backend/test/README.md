# Backend Test Suite

This directory contains comprehensive generative unit tests for the Haskell backend application.

## Test Structure

The test suite is organized into the following modules:

### 1. **UserTest.hs** - Domain Model Tests
- Tests for `User`, `UserId`, `CreateUserRequest`, and `UserResponse` data types
- JSON serialization/deserialization tests
- Property-based tests using QuickCheck
- Edge case testing for data validation

### 2. **UserRepositoryTest.hs** - Repository Layer Tests
- Tests for the `InMemoryUserRepository` implementation
- CRUD operations testing (Create, Read, Update, Delete)
- State management and consistency tests
- Concurrent-like operation testing
- Property-based tests with generated data

### 3. **UserServiceTest.hs** - Service Layer Tests
- Tests for the `UserService` business logic layer
- Mock-based testing for service operations
- Integration between service and repository layers
- Error handling and edge case testing
- Property-based tests for service operations

### 4. **APITest.hs** - API Endpoint Tests
- HTTP endpoint testing using WAI Test
- Request/response validation
- Status code verification
- JSON payload testing
- Error handling for malformed requests

### 5. **IntegrationTest.hs** - End-to-End Tests
- Complete user lifecycle testing
- Multi-user management scenarios
- Data persistence verification
- Concurrent operation simulation
- Real repository integration testing

### 6. **TestUtils.hs** - Test Utilities
- QuickCheck generators for test data
- Helper functions for test setup
- Mock service implementations
- Test data validation utilities

## Test Features

### Generative Testing
- **QuickCheck Integration**: Property-based testing with automatically generated test data
- **Custom Generators**: Specialized generators for user names, emails, and other domain data
- **Edge Case Coverage**: Tests for boundary conditions and invalid inputs

### Comprehensive Coverage
- **Unit Tests**: Individual component testing
- **Integration Tests**: Component interaction testing
- **End-to-End Tests**: Complete workflow testing
- **Error Handling**: Exception and error condition testing

### Mock Testing
- **Service Layer Mocking**: Isolated testing of business logic
- **Repository Mocking**: Controlled testing of data operations
- **API Mocking**: HTTP endpoint testing without external dependencies

## Running Tests

### Prerequisites
Make sure you have the following dependencies installed:
- `hspec` - Testing framework
- `hspec-discover` - Automatic test discovery
- `QuickCheck` - Property-based testing
- `wai-test` - HTTP testing utilities

### Running All Tests
```bash
cd backend
stack test
```

### Running Specific Test Modules
```bash
# Run only domain model tests
stack test --test-arguments="--match UserTest"

# Run only repository tests
stack test --test-arguments="--match UserRepositoryTest"

# Run only service tests
stack test --test-arguments="--match UserServiceTest"

# Run only API tests
stack test --test-arguments="--match APITest"

# Run only integration tests
stack test --test-arguments="--match IntegrationTest"
```

### Running with Verbose Output
```bash
stack test --test-arguments="--format=specdoc"
```

### Running QuickCheck Properties
```bash
stack test --test-arguments="--match property"
```

## Test Data Generation

The test suite includes sophisticated data generators:

### User Data Generators
- `genUserId`: Generates valid user IDs (1-10000)
- `genUserName`: Generates valid user names (2-50 characters)
- `genUserEmail`: Generates valid email addresses
- `genCreateUserRequest`: Generates complete user creation requests
- `genUser`: Generates complete user objects
- `genUserResponse`: Generates user response objects

### Validation Data
- `validUserNames`: Predefined valid user name examples
- `validUserEmails`: Predefined valid email examples
- `invalidUserNames`: Predefined invalid user name examples
- `invalidUserEmails`: Predefined invalid email examples

## Test Patterns

### Property-Based Testing
```haskell
it "should preserve data through JSON serialization" $ property $ \user ->
  let json = encode user
      decoded = decode json :: Maybe User
  in decoded `shouldBe` Just user
```

### Mock Testing
```haskell
let mockService = MockUserService
      { mockCreateUser = \req -> return $ User (UserId 1) (createUserName req) (createUserEmail req)
      , mockGetUserById = \_ -> return Nothing
      , -- ... other mock implementations
      }
```

### Integration Testing
```haskell
it "should handle complete user lifecycle" $ do
  app <- liftIO createIntegrationApp
  -- Create, read, update, delete operations
  -- Verify state consistency
```

## Coverage Areas

### Domain Model
- ✅ Data type definitions
- ✅ JSON serialization/deserialization
- ✅ Type conversions
- ✅ Validation rules
- ✅ Edge cases and boundary conditions

### Repository Layer
- ✅ CRUD operations
- ✅ State management
- ✅ Data consistency
- ✅ Concurrent operations
- ✅ Error handling

### Service Layer
- ✅ Business logic
- ✅ Service-repository integration
- ✅ Data transformation
- ✅ Error propagation
- ✅ Mock testing

### API Layer
- ✅ HTTP endpoints
- ✅ Request/response handling
- ✅ Status codes
- ✅ JSON validation
- ✅ Error responses

### Integration
- ✅ End-to-end workflows
- ✅ Multi-user scenarios
- ✅ Data persistence
- ✅ System consistency
- ✅ Performance characteristics

## Continuous Integration

The test suite is designed to run in CI/CD environments:
- Fast execution for unit tests
- Comprehensive coverage reporting
- Deterministic test results
- Clear failure reporting
- Property-based test shrinking for debugging

## Contributing

When adding new tests:
1. Follow the existing test patterns
2. Include both unit and integration tests
3. Add property-based tests where appropriate
4. Update this README with new test categories
5. Ensure tests are deterministic and fast

