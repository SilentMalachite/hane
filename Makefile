.PHONY: build run test fmt fmt-check lint ci clean deps help setup

# Default target
.DEFAULT_GOAL := help

# Variables
CABAL_OPTS := --enable-tests
CABAL_BUILD_OPTS := -j$(shell nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 1)

help: ## Show this help message
	@echo "Hane - Haskell TUI Editor"
	@echo "Available targets:"
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "  %-15s %s\n", $$1, $$2}'

deps: ## Install dependencies
	@echo "Installing dependencies..."
	cabal update
	cabal build --dependencies-only $(CABAL_BUILD_OPTS)

tools: ## Install development tools
	@echo "Installing development tools..."
	cabal install fourmolu hlint
	@echo "Tools installed. Make sure ~/.cabal/bin is in your PATH."

setup: ## Initial project setup
	@echo "Setting up project..."
	cabal update
	@echo "Creating config directory..."
	mkdir -p ~/.config/hane
	@if [ ! -f ~/.config/hane/config.toml ]; then \
		echo "Creating default config file..."; \
		cp config/config.sample.toml ~/.config/hane/config.toml; \
	fi

build: ## Build the project
	@echo "Building project..."
	cabal build $(CABAL_BUILD_OPTS) $(CABAL_OPTS)

build-no-icu: ## Build without ICU support
	@echo "Building without ICU..."
	cabal build $(CABAL_BUILD_OPTS) $(CABAL_OPTS) -f -icu

run: ## Run the editor
	cabal run hane

run-no-icu: ## Run without ICU support
	cabal run -f -icu hane

test: ## Run all tests
	@echo "Running tests..."
	cabal test $(CABAL_BUILD_OPTS) $(CABAL_OPTS) --test-show-details=streaming

test-coverage: ## Run tests with coverage
	@echo "Running tests with coverage..."
	cabal test $(CABAL_BUILD_OPTS) $(CABAL_OPTS) --enable-coverage --test-show-details=streaming
	@echo "Coverage report generated in dist-newstyle/build/*/ghc-*/hane-*/test/hane-tests/build/hane-tests/hane-tests.tix"

fmt: ## Format source code
	@echo "Formatting source code..."
	@if command -v fourmolu >/dev/null 2>&1; then \
		fourmolu -i app src test; \
	else \
		echo "Warning: fourmolu not found. Install with: cabal install fourmolu"; \
		exit 1; \
	fi

fmt-check: ## Check formatting (CI)
	@echo "Checking formatting..."
	@if command -v fourmolu >/dev/null 2>&1; then \
		fourmolu -m check app src test; \
	else \
		echo "Warning: fourmolu not found. Install with: cabal install fourmolu"; \
		exit 1; \
	fi

lint: ## Run linter
	@echo "Running linter..."
	@if command -v hlint >/dev/null 2>&1; then \
		hlint app src test; \
	else \
		echo "Warning: hlint not found. Install with: cabal install hlint"; \
		exit 1; \
	fi

lint-details: ## Run linter with detailed output
	@echo "Running linter with detailed output..."
	hlint app src test --report=hlint-report.html
	@echo "Lint report generated: hlint-report.html"

freeze: ## Freeze dependencies
	cabal freeze

clean: ## Clean build artifacts
	@echo "Cleaning build artifacts..."
	cabal clean
	rm -rf dist dist-newstyle .ghc.environment.*
	find . -name '*.hi' -o -name '*.o' -o -name '*.hie' -delete

clean-all: clean ## Clean everything including config
	rm -rf ~/.config/hane/config.toml

# CI/CD targets
ci: fmt-check lint ## Run CI checks
	@echo "Running CI checks..."
	cabal update
	cabal build $(CABAL_BUILD_OPTS) -f -icu all $(CABAL_OPTS)
	cabal test $(CABAL_BUILD_OPTS) -f -icu all $(CABAL_OPTS) --test-show-details=streaming

ci-full: fmt-check lint test-coverage ## Run full CI pipeline
	@echo "Running full CI pipeline..."

# Development targets
dev: ## Start development mode (build and run)
	$(MAKE) build
	$(MAKE) run

check: fmt-check lint test ## Run all checks

# Documentation
docs: ## Generate documentation
	cabal haddock --haddock-html --haddock-quickjump --haddock-hyperlink-source $(CABAL_BUILD_OPTS)
