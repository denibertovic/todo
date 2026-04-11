# Convenience wrapper around `gradle` for the common build + test +
# install loop. Everything here assumes you are inside the repo's
# `devenv shell` (see ../devenv.nix) so that the JDK, Gradle, and
# Android SDK are on $PATH and $ANDROID_HOME is set.
#
# Usage (from inside `devenv shell`):
#   make test      — runs the :core JVM unit tests
#   make build     — builds the debug APK
#   make install   — installs the debug APK on the connected device via adb
#   make lint      — runs AGP lint
#   make clean     — removes all build outputs
#
# If you prefer not to enter the devenv shell interactively, prefix
# individual targets:
#
#   devenv shell -- make -C android test
#
# There is no Gradle wrapper in this repo on purpose — Gradle is
# pinned through devenv/nix, not through a project-local wrapper.
# See android/README.md for the rationale.

.PHONY: help test build install lint clean

.DEFAULT_GOAL := help

## Show this help screen
help:
	@echo "Todo Android — Makefile targets"
	@echo "  test        Run :core JVM unit tests"
	@echo "  build       Assemble the debug APK"
	@echo "  install     adb install -r the debug APK on an attached device"
	@echo "  lint        Run AGP lint on :app"
	@echo "  clean       Remove build outputs"

## Run :core unit tests
test:
	gradle :core:test

## Build a debug APK
build:
	gradle :app:assembleDebug

## adb install -r the freshly-built debug APK
install: build
	adb install -r app/build/outputs/apk/debug/app-debug.apk

## Run AGP lint on :app
lint:
	gradle :app:lintDebug

## Clean build outputs
clean:
	gradle clean
