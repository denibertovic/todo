.PHONY: build repl test release help

.DEFAULT_GOAL = help

VERSION ?= $(shell grep "^version:" todo.cabal | cut -d " " -f9)

## Run build
build:
	@stack build

## Run repl
repl:
	@stack repl

## Run tests
test:
	@stack test

## Cut new release
release:
	@git tag ${VERSION} && git push --tags

## Print current version
version:
	@echo ${VERSION}

## Show help screen.
help:
	@echo "Please use \`make <target>' where <target> is one of\n\n"
	@awk '/^[a-zA-Z\-\_0-9]+:/ { \
		helpMessage = match(lastLine, /^## (.*)/); \
		if (helpMessage) { \
			helpCommand = substr($$1, 0, index($$1, ":")-1); \
			helpMessage = substr(lastLine, RSTART + 3, RLENGTH); \
			printf "%-30s %s\n", helpCommand, helpMessage; \
		} \
	} \
	{ lastLine = $$0 }' $(MAKEFILE_LIST)

