# Makefile Usage Guide

This project uses a comprehensive Makefile to manage all build, test, and deployment operations. The Makefile replaces individual shell scripts with a unified command interface.

## Quick Start

```bash
# Install all dependencies
make install

# Build the entire project
make build

# Run all tests
make test

# Start development servers
make dev
```

## Available Commands

### Installation
- `make install` - Install all dependencies (backend + frontend)
- `make install-backend` - Install only backend dependencies
- `make install-frontend` - Install only frontend dependencies

### Building
- `make build` - Build both backend and frontend
- `make build-backend` - Build only backend
- `make build-frontend` - Build only frontend

### Testing
- `make test` - Run all tests with summary
- `make test-backend` - Run only backend tests
- `make test-frontend` - Run only frontend tests
- `make test-all` - Run all tests with detailed coverage info

### Development
- `make dev` - Start both backend and frontend development servers
- `make dev-backend` - Start only backend development server
- `make dev-frontend` - Start only frontend development server
- `make dev-watch` - Start development with file watching

### Docker
- `make docker-build` - Build Docker image
- `make docker-run` - Run application in Docker
- `make docker-test` - Run tests in Docker
- `make docker-dev` - Run development environment in Docker
- `make docker-clean` - Clean up Docker resources

### Deployment
- `make deploy` - Deploy to staging (default)
- `make deploy-staging` - Deploy to staging environment
- `make deploy-prod` - Deploy to production (with confirmation)

### Utilities
- `make clean` - Clean all build artifacts
- `make lint` - Run linting for both backend and frontend
- `make format` - Format code for both backend and frontend
- `make check` - Run type checking for both backend and frontend

### Information
- `make help` - Show all available commands
- `make status` - Show project status
- `make info` - Show project information
- `make version` - Show versions of all tools

## CI/CD Integration

The Makefile includes CI/CD targets for automated pipelines:

```bash
# Full CI pipeline
make ci

# Individual CI steps
make ci-install
make ci-build
make ci-test
make ci-deploy
```

## Color-Coded Output

The Makefile uses color-coded output for better readability:
- 🔵 Blue: Information and progress
- 🟢 Green: Success messages
- 🟡 Yellow: Warnings and prompts
- 🔴 Red: Errors and important notices
- 🔵 Cyan: Headers and summaries

## Examples

### Development Workflow
```bash
# Start fresh development session
make clean
make install
make build
make test
make dev
```

### Testing Workflow
```bash
# Run specific test suites
make test-backend
make test-frontend

# Run all tests with coverage
make test-all
```

### Docker Workflow
```bash
# Build and test in Docker
make docker-build
make docker-test

# Development with Docker
make docker-dev
```

### Production Deployment
```bash
# Deploy to staging
make deploy-staging

# Deploy to production (with confirmation)
make deploy-prod
```

## Troubleshooting

### Common Issues

1. **Dependencies not found**: Run `make install` first
2. **Build failures**: Check `make status` and `make version`
3. **Test failures**: Run individual test suites with `make test-backend` or `make test-frontend`
4. **Docker issues**: Use `make docker-clean` to reset Docker state

### Getting Help

- `make help` - Shows all available commands
- `make status` - Shows current project status
- `make version` - Shows tool versions

## Migration from Shell Scripts

The old shell scripts (`run-tests.sh`, `run-tests.ps1`) have been replaced by the Makefile. The equivalent commands are:

| Old Script | New Makefile Command |
|------------|---------------------|
| `./run-tests.sh` | `make test-backend` |
| `./run-tests.ps1` | `make test-backend` |
| Manual build | `make build` |
| Manual install | `make install` |

The Makefile provides much more functionality and better error handling than the original scripts.
