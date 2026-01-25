.PHONY: build repl test release help static nix-build ghcid ghcid-test

.DEFAULT_GOAL = help

VERSION ?= $(shell grep "^version:" todo.cabal | cut -d " " -f9)

## Build project
build:
	@cabal build

## Build project with nix
nix-build:
	@nix build

## Run tests
test:
	@cabal test

## Build static binary with nix
static:
	@nix-build --no-link -A static_package static.nix

## Cut new release
release:
	@git tag ${VERSION} && git push --tags

## Run ghcid
ghcid:
	@ghcid --command "cabal repl lib:todo"

## Have ghcid run the test suite on successful recompile
ghcid-test:
	@ghcid --command "cabal repl test:todo-test" --test "main"

## Print current version
version:
	@echo ${VERSION}

## Show help screen.
help:
	@echo "Please use \`make <target>' where <target> is one of:"
	@echo
	@awk '/^[a-zA-Z\-0-9_]+:/ { \
		helpMessage = match(lastLine, /^## (.*)/); \
		if (helpMessage) { \
			helpCommand = substr($$1, 0, index($$1, ":")-1); \
			helpMessage = substr(lastLine, RSTART + 3, RLENGTH); \
			printf "%-30s %s\n", helpCommand, helpMessage; \
		} \
	} \
	{ lastLine = $$0 }' $(MAKEFILE_LIST)
