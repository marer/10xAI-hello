# Multi-stage Dockerfile for 10xAI-hello application
# Backend: Haskell with Stack
# Frontend: PureScript with Spago

FROM ubuntu:24.04

# Set environment variables
ENV DEBIAN_FRONTEND=noninteractive
ENV LANG=C.UTF-8
ENV LC_ALL=C.UTF-8

# Install system dependencies
RUN apt-get update && apt-get install -y \
  # Basic utilities
  curl \
  wget \
  git \
  build-essential \
  software-properties-common \
  ca-certificates \
  gnupg \
  lsb-release \
  # Development tools
  vim \
  nano \
  zsh \
  # Network tools
  net-tools \
  iputils-ping \
  # Clean up
  && rm -rf /var/lib/apt/lists/*

# Install Node.js (LTS version)
RUN curl -fsSL https://deb.nodesource.com/setup_lts.x | bash - \
  && apt-get install -y nodejs

# Install Haskell Stack
RUN curl -sSL https://get.haskellstack.org/ | sh

# Install PureScript compiler
RUN npm install -g purescript@0.15.10

# Install Spago (PureScript package manager)
RUN npm install -g spago

# Install http-server for frontend serving
RUN npm install -g http-server

# Set working directory
WORKDIR /app

# Copy project files
COPY . .

# Install backend dependencies
WORKDIR /app/backend
RUN stack setup
RUN stack build --dependencies-only

# Install frontend dependencies
WORKDIR /app/frontend
RUN npm install
RUN spago install

# Build the entire project
WORKDIR /app
RUN echo "Building backend..." && \
  cd backend && \
  stack build && \
  echo "Building frontend..." && \
  cd ../frontend && \
  spago build

# Create a comprehensive test script
RUN cat > /app/run-all-tests.sh << 'EOF'
#!/bin/bash

echo "🚀 Starting Complete Test Suite"
echo "================================"

# Test backend
echo "📦 Testing Backend..."
cd /app/backend
if ./run-tests.sh; then
echo "✅ Backend tests passed"
else
echo "❌ Backend tests failed"
exit 1
fi

# Test frontend
echo "📦 Testing Frontend..."
cd /app/frontend
if npm test; then
echo "✅ Frontend tests passed"
else
echo "❌ Frontend tests failed"
exit 1
fi

echo "🎉 All tests completed successfully!"
EOF

RUN chmod +x /app/run-all-tests.sh

# Create a development server script
RUN cat > /app/start-dev.sh << 'EOF'
#!/bin/bash

echo "🚀 Starting Development Environment"
echo "==================================="

# Start backend in background
echo "Starting backend server..."
cd /app/backend
stack exec backend &
BACKEND_PID=$!

# Wait a moment for backend to start
sleep 3

# Start frontend development server
echo "Starting frontend server..."
cd /app/frontend
npm run serve &
FRONTEND_PID=$!

echo "✅ Development servers started!"
echo "   Backend: http://localhost:3000"
echo "   Frontend: http://localhost:8080"
echo ""
echo "Press Ctrl+C to stop all servers"

# Wait for user interrupt
trap "kill $BACKEND_PID $FRONTEND_PID; exit" INT
wait
EOF

RUN chmod +x /app/start-dev.sh

# Expose ports
EXPOSE 3000 8080

# Set default command
CMD ["/bin/bash"]

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=5s --retries=3 \
  CMD curl -f http://localhost:3000/health || exit 1
