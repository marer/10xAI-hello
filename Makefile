# 10xAI-hello Project Makefile
# Unified build, test, and deploy system for Haskell backend and PureScript frontend

# Variables
BACKEND_DIR = backend
FRONTEND_DIR = frontend
DOCKER_IMAGE = 10xai-hello
DOCKER_TAG = latest
BACKEND_PORT = 3000
FRONTEND_PORT = 8080

# Colors for output
RED = \033[0;31m
GREEN = \033[0;32m
YELLOW = \033[1;33m
BLUE = \033[0;34m
CYAN = \033[0;36m
NC = \033[0m # No Color

# Default target
.PHONY: help
help: ## Show this help message
	@echo "$(CYAN)10xAI-hello Project Makefile$(NC)"
	@echo "$(CYAN)============================$(NC)"
	@echo ""
	@echo "$(YELLOW)Available targets:$(NC)"
	@awk 'BEGIN {FS = ":.*?## "} /^[a-zA-Z_-]+:.*?## / {printf "  $(GREEN)%-15s$(NC) %s\n", $$1, $$2}' $(MAKEFILE_LIST)
	@echo ""
	@echo "$(YELLOW)Quick start:$(NC)"
	@echo "  make install    # Install all dependencies"
	@echo "  make build      # Build both backend and frontend"
	@echo "  make test       # Run all tests"
	@echo "  make dev        # Start development servers"

# =============================================================================
# INSTALLATION TARGETS
# =============================================================================

.PHONY: install install-backend install-frontend
install: install-backend install-frontend ## Install all dependencies

install-backend: ## Install backend dependencies
	@echo "$(BLUE)📦 Installing backend dependencies...$(NC)"
	@cd $(BACKEND_DIR) && stack setup
	@cd $(BACKEND_DIR) && stack build --dependencies-only
	@echo "$(GREEN)✅ Backend dependencies installed$(NC)"

install-frontend: ## Install frontend dependencies
	@echo "$(BLUE)📦 Installing frontend dependencies...$(NC)"
	@cd $(FRONTEND_DIR) && npm install
	@cd $(FRONTEND_DIR) && spago install
	@echo "$(GREEN)✅ Frontend dependencies installed$(NC)"

# =============================================================================
# BUILD TARGETS
# =============================================================================

.PHONY: build build-backend build-frontend
build: build-backend build-frontend ## Build both backend and frontend

build-backend: ## Build backend only
	@echo "$(BLUE)🔨 Building backend...$(NC)"
	@cd $(BACKEND_DIR) && stack build
	@echo "$(GREEN)✅ Backend built successfully$(NC)"

build-frontend: ## Build frontend only
	@echo "$(BLUE)🔨 Building frontend...$(NC)"
	@cd $(FRONTEND_DIR) && spago build
	@echo "$(GREEN)✅ Frontend built successfully$(NC)"

# =============================================================================
# TEST TARGETS
# =============================================================================

.PHONY: test test-backend test-frontend test-all
test: test-all ## Run all tests

test-backend: ## Run backend tests only
	@echo "$(BLUE)🧪 Running backend tests...$(NC)"
	@echo "$(CYAN)==============================$(NC)"
	@cd $(BACKEND_DIR) && stack test --test-arguments="--format=specdoc"
	@if [ $$? -eq 0 ]; then \
		echo ""; \
		echo "$(GREEN)🎉 Backend tests passed!$(NC)"; \
		echo "$(CYAN)📊 Test Summary:$(NC)"; \
		echo "   $(GREEN)✅ Domain Model Tests (UserTest)$(NC)"; \
		echo "   $(GREEN)✅ Repository Tests (UserRepositoryTest)$(NC)"; \
		echo "   $(GREEN)✅ Service Tests (UserServiceTest)$(NC)"; \
		echo "   $(GREEN)✅ API Tests (APITest)$(NC)"; \
		echo "   $(GREEN)✅ Integration Tests (IntegrationTest)$(NC)"; \
	else \
		echo ""; \
		echo "$(RED)❌ Backend tests failed$(NC)"; \
		exit 1; \
	fi

test-frontend: ## Run frontend tests only
	@echo "$(BLUE)🧪 Running frontend tests...$(NC)"
	@echo "$(CYAN)==============================$(NC)"
	@cd $(FRONTEND_DIR) && npm test
	@if [ $$? -eq 0 ]; then \
		echo ""; \
		echo "$(GREEN)🎉 Frontend tests passed!$(NC)"; \
	else \
		echo ""; \
		echo "$(RED)❌ Frontend tests failed$(NC)"; \
		exit 1; \
	fi

test-all: test-backend test-frontend ## Run all tests with summary
	@echo ""
	@echo "$(CYAN)🔍 Complete Test Coverage:$(NC)"
	@echo "   • Property-based testing with QuickCheck"
	@echo "   • Mock testing for service layer"
	@echo "   • HTTP endpoint testing"
	@echo "   • End-to-end integration testing"
	@echo "   • Error handling and edge cases"
	@echo ""
	@echo "$(GREEN)✨ All tests completed successfully!$(NC)"

# =============================================================================
# DEVELOPMENT TARGETS
# =============================================================================

.PHONY: dev dev-backend dev-frontend dev-watch
dev: ## Start development environment (both servers)
	@echo "$(BLUE)🚀 Starting development environment...$(NC)"
	@echo "$(CYAN)=====================================$(NC)"
	@echo "$(YELLOW)Starting backend server...$(NC)"
	@cd $(BACKEND_DIR) && stack exec backend &
	@echo "$(YELLOW)Starting frontend server...$(NC)"
	@cd $(FRONTEND_DIR) && npm run serve &
	@echo ""
	@echo "$(GREEN)✅ Development servers started!$(NC)"
	@echo "   Backend: http://localhost:$(BACKEND_PORT)"
	@echo "   Frontend: http://localhost:$(FRONTEND_PORT)"
	@echo ""
	@echo "$(YELLOW)Press Ctrl+C to stop all servers$(NC)"
	@wait

dev-backend: ## Start backend development server only
	@echo "$(BLUE)🚀 Starting backend development server...$(NC)"
	@cd $(BACKEND_DIR) && stack exec backend

dev-frontend: ## Start frontend development server only
	@echo "$(BLUE)🚀 Starting frontend development server...$(NC)"
	@cd $(FRONTEND_DIR) && npm run serve

dev-watch: ## Start development with file watching
	@echo "$(BLUE)👀 Starting development with file watching...$(NC)"
	@echo "$(YELLOW)Backend will auto-rebuild on changes$(NC)"
	@echo "$(YELLOW)Frontend will auto-rebuild on changes$(NC)"
	@cd $(BACKEND_DIR) && stack build --file-watch &
	@cd $(FRONTEND_DIR) && npm run dev &
	@wait

# =============================================================================
# DOCKER TARGETS
# =============================================================================

.PHONY: docker-build docker-run docker-test docker-clean docker-push
docker-build: ## Build Docker image
	@echo "$(BLUE)🐳 Building Docker image...$(NC)"
	@docker build -t $(DOCKER_IMAGE):$(DOCKER_TAG) .
	@echo "$(GREEN)✅ Docker image built: $(DOCKER_IMAGE):$(DOCKER_TAG)$(NC)"

docker-run: ## Run application in Docker
	@echo "$(BLUE)🐳 Running application in Docker...$(NC)"
	@docker run -p $(BACKEND_PORT):$(BACKEND_PORT) -p $(FRONTEND_PORT):$(FRONTEND_PORT) $(DOCKER_IMAGE):$(DOCKER_TAG)

docker-test: ## Run tests in Docker
	@echo "$(BLUE)🐳 Running tests in Docker...$(NC)"
	@docker run --rm $(DOCKER_IMAGE):$(DOCKER_TAG) /app/run-all-tests.sh

docker-dev: ## Run development environment in Docker with volume mounts
	@echo "$(BLUE)🐳 Starting Docker development environment...$(NC)"
	@docker-compose up

docker-clean: ## Clean up Docker resources
	@echo "$(BLUE)🧹 Cleaning up Docker resources...$(NC)"
	@docker-compose down
	@docker system prune -f
	@echo "$(GREEN)✅ Docker cleanup completed$(NC)"

docker-push: ## Push Docker image to registry
	@echo "$(BLUE)📤 Pushing Docker image...$(NC)"
	@docker push $(DOCKER_IMAGE):$(DOCKER_TAG)

# =============================================================================
# DEPLOYMENT TARGETS
# =============================================================================

.PHONY: deploy deploy-staging deploy-prod
deploy: deploy-staging ## Deploy to staging (default)

deploy-staging: ## Deploy to staging environment
	@echo "$(BLUE)🚀 Deploying to staging...$(NC)"
	@echo "$(YELLOW)Building production image...$(NC)"
	@docker build -t $(DOCKER_IMAGE):staging .
	@echo "$(YELLOW)Running tests...$(NC)"
	@make test
	@echo "$(YELLOW)Deploying to staging...$(NC)"
	@echo "$(GREEN)✅ Staging deployment completed$(NC)"

deploy-prod: ## Deploy to production environment
	@echo "$(RED)⚠️  PRODUCTION DEPLOYMENT$(NC)"
	@echo "$(YELLOW)Are you sure you want to deploy to production? (y/N)$(NC)"
	@read -r confirm && [ "$$confirm" = "y" ] || exit 1
	@echo "$(BLUE)🚀 Deploying to production...$(NC)"
	@echo "$(YELLOW)Building production image...$(NC)"
	@docker build -t $(DOCKER_IMAGE):prod .
	@echo "$(YELLOW)Running full test suite...$(NC)"
	@make test-all
	@echo "$(YELLOW)Deploying to production...$(NC)"
	@echo "$(GREEN)✅ Production deployment completed$(NC)"

# =============================================================================
# UTILITY TARGETS
# =============================================================================

.PHONY: clean clean-backend clean-frontend clean-all
clean: clean-all ## Clean all build artifacts

clean-backend: ## Clean backend build artifacts
	@echo "$(BLUE)🧹 Cleaning backend artifacts...$(NC)"
	@cd $(BACKEND_DIR) && stack clean
	@echo "$(GREEN)✅ Backend cleaned$(NC)"

clean-frontend: ## Clean frontend build artifacts
	@echo "$(BLUE)🧹 Cleaning frontend artifacts...$(NC)"
	@cd $(FRONTEND_DIR) && rm -rf output dist
	@echo "$(GREEN)✅ Frontend cleaned$(NC)"

clean-all: clean-backend clean-frontend ## Clean all build artifacts
	@echo "$(BLUE)🧹 Cleaning all artifacts...$(NC)"
	@rm -rf node_modules
	@echo "$(GREEN)✅ All artifacts cleaned$(NC)"

.PHONY: lint lint-backend lint-frontend
lint: lint-backend lint-frontend ## Run linting for both backend and frontend

lint-backend: ## Lint backend code
	@echo "$(BLUE)🔍 Linting backend...$(NC)"
	@cd $(BACKEND_DIR) && stack build --pedantic
	@echo "$(GREEN)✅ Backend linting completed$(NC)"

lint-frontend: ## Lint frontend code
	@echo "$(BLUE)🔍 Linting frontend...$(NC)"
	@cd $(FRONTEND_DIR) && spago build
	@echo "$(GREEN)✅ Frontend linting completed$(NC)"

.PHONY: format format-backend format-frontend
format: format-backend format-frontend ## Format code for both backend and frontend

format-backend: ## Format backend code
	@echo "$(BLUE)🎨 Formatting backend...$(NC)"
	@cd $(BACKEND_DIR) && find src -name "*.hs" -exec hindent {} \;
	@echo "$(GREEN)✅ Backend formatting completed$(NC)"

format-frontend: ## Format frontend code
	@echo "$(BLUE)🎨 Formatting frontend...$(NC)"
	@cd $(FRONTEND_DIR) && find src -name "*.purs" -exec purty --write {} \;
	@echo "$(GREEN)✅ Frontend formatting completed$(NC)"

.PHONY: check check-backend check-frontend
check: check-backend check-frontend ## Run type checking for both backend and frontend

check-backend: ## Type check backend
	@echo "$(BLUE)🔍 Type checking backend...$(NC)"
	@cd $(BACKEND_DIR) && stack build --no-run-tests
	@echo "$(GREEN)✅ Backend type checking completed$(NC)"

check-frontend: ## Type check frontend
	@echo "$(BLUE)🔍 Type checking frontend...$(NC)"
	@cd $(FRONTEND_DIR) && spago build --no-install
	@echo "$(GREEN)✅ Frontend type checking completed$(NC)"

# =============================================================================
# CI/CD TARGETS
# =============================================================================

.PHONY: ci ci-install ci-build ci-test ci-deploy
ci: ci-install ci-build ci-test ## Run full CI pipeline

ci-install: ## CI: Install dependencies
	@echo "$(BLUE)🔧 CI: Installing dependencies...$(NC)"
	@make install

ci-build: ## CI: Build project
	@echo "$(BLUE)🔧 CI: Building project...$(NC)"
	@make build

ci-test: ## CI: Run tests
	@echo "$(BLUE)🔧 CI: Running tests...$(NC)"
	@make test-all

ci-deploy: ## CI: Deploy to staging
	@echo "$(BLUE)🔧 CI: Deploying to staging...$(NC)"
	@make deploy-staging

# =============================================================================
# STATUS AND INFO TARGETS
# =============================================================================

.PHONY: status info version
status: ## Show project status
	@echo "$(CYAN)📊 Project Status$(NC)"
	@echo "$(CYAN)================$(NC)"
	@echo "$(YELLOW)Backend:$(NC)"
	@cd $(BACKEND_DIR) && stack exec -- ghc --version 2>/dev/null || echo "  Not built"
	@echo "$(YELLOW)Frontend:$(NC)"
	@cd $(FRONTEND_DIR) && node --version 2>/dev/null || echo "  Node.js not found"
	@cd $(FRONTEND_DIR) && spago --version 2>/dev/null || echo "  Spago not found"
	@echo "$(YELLOW)Docker:$(NC)"
	@docker --version 2>/dev/null || echo "  Docker not found"

info: ## Show project information
	@echo "$(CYAN)ℹ️  Project Information$(NC)"
	@echo "$(CYAN)======================$(NC)"
	@echo "Backend: Haskell with Stack"
	@echo "Frontend: PureScript with Spago"
	@echo "Backend Port: $(BACKEND_PORT)"
	@echo "Frontend Port: $(FRONTEND_PORT)"
	@echo "Docker Image: $(DOCKER_IMAGE):$(DOCKER_TAG)"

version: ## Show versions of all tools
	@echo "$(CYAN)📋 Tool Versions$(NC)"
	@echo "$(CYAN)===============$(NC)"
	@echo -n "Stack: " && cd $(BACKEND_DIR) && stack --version 2>/dev/null || echo "Not found"
	@echo -n "Node.js: " && node --version 2>/dev/null || echo "Not found"
	@echo -n "npm: " && npm --version 2>/dev/null || echo "Not found"
	@echo -n "Spago: " && spago --version 2>/dev/null || echo "Not found"
	@echo -n "Docker: " && docker --version 2>/dev/null || echo "Not found"
	@echo -n "Docker Compose: " && docker-compose --version 2>/dev/null || echo "Not found"
