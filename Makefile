.DEFAULT_GOAL := help

.PHONY: all
all: setup build test lint

.PHONY: setup
setup: ## Pull down GHC and install libs
	stack setup
	stack build --dependencies-only --test --no-run-tests
	stack install happy

.PHONY: build
build: ## Compile binary
	stack build --pedantic --test --no-run-tests

.PHONY: clean
clean:
	stack clean

.PHONY: deploy
deploy: ## Deploy static assets
	stack exec filib-io build

help: ## Print available tasks
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
