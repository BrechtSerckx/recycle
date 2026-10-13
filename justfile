# List available recipes
default:
    @just --list

# ── Backend ───────────────────────────────────────────────────────────────────

# Build the Haskell backend
build-backend:
    cabal build all

# Build the Haskell backend with hot-reload
dev-backend:
    cd recycle-ics && ghcid

# Build the Haskell backend tests with hot-reload
dev-backend-test:
    cd recycle-ics && ghcid -c 'cabal repl recycle-ics-test'

# Build the Haskell backend api tests with hot-reload
dev-backend-api-test:
    cd recycle-ics && ghcid -c 'cabal repl recycle-ics-api-test'

# Run backend tests
test-backend:
    cabal test recycle-ics-test

# Format Haskell sources with ormolu
fmt-backend:
    find recycle-ics -name "*.hs" -print0 | xargs -0 ormolu --mode inplace

# Lint Haskell sources with hlint
lint-backend:
    hlint recycle-ics

# Format cabal file with cabal-fmt
fmt-cabal:
    cabal-fmt --inplace recycle-ics/recycle-ics.cabal

# Check Haskell sources are formatted with ormolu
fmt-check-backend:
    find recycle-ics -name "*.hs" -print0 | xargs -0 ormolu --mode check

# Check cabal file is formatted with cabal-fmt
fmt-check-cabal:
    cabal-fmt --check recycle-ics/recycle-ics.cabal

# Start the server (requires a built frontend: just build-frontend)
serve:
    cabal run recycle-ics -- serve-ics

# ── Frontend ──────────────────────────────────────────────────────────────────

# Install frontend npm dependencies
install-frontend:
    cd recycle-ics-ui && npm install

# Build the frontend
build-frontend:
    cd recycle-ics-ui && npm run build

# Start the frontend dev server with hot reload
dev-frontend:
    cd recycle-ics-ui && npm start

# Format frontend sources with prettier
fmt-frontend:
    cd recycle-ics-ui && prettier --write 'src/**/*.{ts,tsx,css}'

# Check frontend sources are formatted with prettier
fmt-check-frontend:
    cd recycle-ics-ui && prettier --check 'src/**/*.{ts,tsx,css}'

# Type-check frontend without emitting output
typecheck-frontend:
    cd recycle-ics-ui && npx tsc --noEmit

# ── MCP (Hoogle) ──────────────────────────────────────────────────────────────

# Generate the Hoogle database from local GHC packages (for mcp-hoogle)
mcp-generate:
    mcp-hoogle generate

# Serve the Hoogle MCP server over stdio
mcp-serve:
    mcp-hoogle serve

# ── Combined ──────────────────────────────────────────────────────────────────

# Build everything (frontend first — its path is baked into the backend at compile time)
build: build-frontend build-backend

# Run all tests
test: test-backend

# Format all sources
fmt: fmt-backend fmt-cabal fmt-frontend

# Check all sources are formatted, without modifying them
fmt-check: fmt-check-backend fmt-check-cabal fmt-check-frontend

# Lint / type-check all sources
lint: lint-backend typecheck-frontend
