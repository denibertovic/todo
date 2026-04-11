# todo-android

Android client for the [todo](../README.md) CRDT sync system. Speaks the same JSON-over-HTTP sync protocol as the Haskell CLI, so you can manage the same `todo.txt` from your laptop and your phone with operation-based merges handling offline edits.

## Layout

* `core/` — pure Kotlin/JVM module with the wire-format types, todo.txt parser, renderer, and CRDT. No Android SDK dependencies, so it's JVM-testable without an emulator or device.
* `app/` — Android application module. Room op log, Ktor HTTP client, WorkManager sync, and Compose UI.

## Prerequisites

Everything you need is already pinned in the repo's `devenv.nix`:

* JDK 17
* Gradle (pinned via nix, no project-local Gradle wrapper)
* Android SDK (platforms 26/34/35, build-tools 34.0.0/35.0.0)
* `adb` from `pkgs.android-tools`

Run commands inside the devenv shell so all of that is on `$PATH`:

```bash
devenv shell -- make -C android test
devenv shell -- make -C android build
```

Or drop into the shell interactively:

```bash
devenv shell
cd android
gradle :core:test
```

**No Gradle wrapper.** This project intentionally does not commit `gradlew` / `gradlew.bat` / `gradle/wrapper/*`. Gradle is pinned through devenv/nix — see `../devenv.nix`. The wrapper would just be a second, independent pin fighting with the nix one. If you want to build outside of devenv you'll need to install Gradle yourself and make sure its version matches what nix gives us (`gradle --version` inside `devenv shell`).

## Common tasks

```bash
make test      # run :core JVM tests
make build     # build the debug APK
make install   # adb install -r the debug APK
make lint      # run AGP lint on :app
make clean     # wipe build outputs
```

The debug APK lands at `app/build/outputs/apk/debug/app-debug.apk`.

## Onboarding a device

1. First-time bootstrap (SSH into the sync-server host, once):
   ```bash
   todo-sync-server generate-invite-code -d /path/to/todo-sync.db --server-url https://your.server
   ```
   Copy the invite code, then on your laptop:
   ```bash
   todo sync init https://your.server --invite-code <uuid>
   ```
   The laptop CLI now has its own auth token and doesn't need SSH access to the server for anything else.

2. Every subsequent device (phone, tablet, second laptop): on a device that's already registered, run
   ```bash
   todo sync invite
   ```
   The CLI calls the server's `/invite` endpoint with its stored auth token and prints a scannable QR code of the `todo-sync://register?...` deeplink directly to the terminal.

3. In the Android app's onboarding screen, tap **Scan QR**, point the camera at the terminal, confirm, done.

## Sync protocol notes

The Android client is bit-compatible with the Haskell CLI and server:

* `POST /sync` with an `Authorization: Bearer <token>` header, mandatory.
* `POST /invite` also requires a bearer token: any already-registered device can mint a fresh invite code for onboarding another device.
* Operation log semantics are LWW-on-fields, complete-wins, delete-wins, with device ID as the LWW tiebreaker.
* The full op log is kept locally in Room (`operations` table) and the visible items are materialized into `items_cache` after every sync. The cache is recomputed from scratch each time so it can never drift — at reasonable op counts this is cheaper than an incremental fold.
