# Haskell + Halogen Web Application

A skeleton web application with Haskell backend using Scotty and tagless final style, and Halogen frontend.

## Architecture

### Backend (Haskell)
- **Framework**: Scotty for HTTP server
- **Architecture**: Tagless final style for dependency injection and testability
- **Domain**: User management with CRUD operations
- **Infrastructure**: In-memory repository implementation

### Frontend (PureScript + Halogen)
- **Framework**: Halogen for UI components
- **API**: Affjax for HTTP requests
- **Features**: User list, create/edit forms, delete functionality

## Project Structure

```
├── backend/
│   ├── src/
│   │   ├── User.hs                  # Domain models
│   │   ├── UserService.hs           # Tagless final service interface
│   │   ├── UserRepository.hs        # Repository implementation
│   │   ├── API.hs                   # API routes and handlers
│   │   ├── Server.hs                # Server configuration
│   │   └── Main.hs                  # Application entry point
│   └── backend.cabal                # Cabal configuration
├── frontend/
│   ├── src/
│   │   ├── Data/
│   │   │   └── User.purs            # Data types
│   │   ├── Api/
│   │   │   └── UserApi.purs         # API client
│   │   ├── Component/
│   │   │   ├── UserList.purs        # User list component
│   │   │   └── UserForm.purs        # User form component
│   │   └── Main.purs                # Application entry point
│   ├── index.html                   # HTML template
│   ├── package.json                 # NPM dependencies
│   └── spago.dhall                  # Spago configuration
└── README.md
```

## Getting Started

### Prerequisites
- Haskell Stack or Cabal
- Node.js and npm
- PureScript Spago

### Backend Setup

1. Navigate to the backend directory:
   ```bash
   cd backend
   ```

2. Install dependencies:
   ```bash
   cabal install --only-dependencies
   ```

3. Build the project:
   ```bash
   cabal build
   ```

4. Run the server:
   ```bash
   cabal run
   ```

The backend will start on `http://localhost:3000`

### Frontend Setup

1. Navigate to the frontend directory:
   ```bash
   cd frontend
   ```

2. Install dependencies:
   ```bash
   npm install
   spago install
   ```

3. Build the frontend:
   ```bash
   spago build
   spago bundle-app --main Main --to dist/app.js
   ```

4. Serve the frontend:
   ```bash
   npm run serve
   ```

The frontend will be available at `http://localhost:8080`

## API Endpoints

- `GET /health` - Health check
- `GET /api/users` - Get all users
- `GET /api/users/:id` - Get user by ID
- `POST /api/users` - Create new user
- `PUT /api/users/:id` - Update user
- `DELETE /api/users/:id` - Delete user

## Tagless Final Architecture

The backend uses tagless final style for:

1. **Dependency Injection**: Services are injected through the Reader monad
2. **Testability**: Easy to mock dependencies for testing
3. **Flexibility**: Can swap implementations without changing business logic

### Key Components:

- `UserService` - Tagless final interface for user operations
- `UserRepository` - Tagless final interface for data persistence
- `AppM` - Application monad combining all effects
- `AppEnv` - Environment containing all dependencies

## Features

- ✅ User CRUD operations
- ✅ RESTful API design
- ✅ CORS support
- ✅ Type-safe frontend-backend communication
- ✅ Component-based UI architecture
- ✅ Error handling
- ✅ Loading states

## Development

### Adding New Features

1. **Backend**: Add new domain models and services following the tagless final pattern
2. **Frontend**: Create new Halogen components and API clients
3. **Integration**: Update API endpoints and frontend API calls

### Testing

The tagless final architecture makes testing straightforward:

1. Create mock implementations of service interfaces
2. Inject mocks into the application monad
3. Test business logic without external dependencies

## License

This project is licensed under the BSD-3-Clause License.

