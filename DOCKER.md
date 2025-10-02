# Docker Setup for 10xAI-hello

This document explains how to use Docker with the 10xAI-hello application.

## Prerequisites

- Docker installed on your system
- Docker Compose (optional, for easier development)

## Quick Start

### Build the Docker Image

```bash
docker build -t 10xai-hello .
```

### Run Tests

```bash
# Run all tests
docker run --rm 10xai-hello /app/run-all-tests.sh

# Or using docker-compose
docker-compose run test
```

### Development Mode

```bash
# Start development servers
docker run -p 3000:3000 -p 8080:8080 -v $(pwd):/app 10xai-hello /app/start-dev.sh

# Or using docker-compose (recommended)
docker-compose up
```

## Docker Compose Commands

### Start Development Environment
```bash
docker-compose up
```
This will:
- Build the Docker image
- Start both backend (port 3000) and frontend (port 8080) servers
- Mount your source code for live development

### Run Tests
```bash
docker-compose run test
```

### Stop Services
```bash
docker-compose down
```

### Rebuild After Changes
```bash
docker-compose build --no-cache
```

## What's Included

The Docker image contains:

### System Dependencies
- Ubuntu 24.04 LTS (latest LTS)
- Build tools and utilities
- Git and development tools

### Backend (Haskell)
- Haskell Stack (latest)
- All Haskell dependencies from `backend.cabal`
- Test framework (HSpec, QuickCheck)

### Frontend (PureScript)
- Node.js LTS
- PureScript compiler (0.15.10)
- Spago package manager
- All PureScript dependencies from `spago.dhall`
- http-server for development

## Ports

- **3000**: Backend API server
- **8080**: Frontend development server

## Development Workflow

1. **Start the development environment**:
   ```bash
   docker-compose up
   ```

2. **Make changes** to your source code (mounted as volumes)

3. **Backend changes**: The Haskell backend will need to be rebuilt:
   ```bash
   docker-compose exec app bash
   cd backend
   stack build
   ```

4. **Frontend changes**: PureScript will auto-rebuild with:
   ```bash
   docker-compose exec app bash
   cd frontend
   spago build --watch
   ```

5. **Run tests**:
   ```bash
   docker-compose run test
   ```

## Troubleshooting

### Build Issues
- Clear Docker cache: `docker system prune -a`
- Rebuild without cache: `docker-compose build --no-cache`

### Port Conflicts
- Change ports in `docker-compose.yml` if 3000 or 8080 are in use

### Permission Issues (Linux/macOS)
- Ensure Docker has permission to access your project directory

### Windows Issues
- Use WSL2 for better performance
- Ensure line endings are set to LF in your editor

## Production Considerations

This Dockerfile is optimized for development. For production:

1. Use multi-stage builds to reduce image size
2. Remove development dependencies
3. Use specific version tags instead of `latest`
4. Add proper health checks and monitoring
5. Use non-root user for security
