{
  pkgs,
  lib,
  config,
  inputs,
  ...
}: let
  pkgs-unstable = import inputs.nixpkgs-unstable {system = pkgs.stdenv.system;};
  # Android SDK layer needs the EULA accepted before nix will build
  # it; re-instantiating nixpkgs with that option flipped is the
  # recommended way to do this inside a devenv. This keeps the
  # unfree-allow localized to the Android SDK only.
  pkgsAndroid = import inputs.nixpkgs {
    system = pkgs.stdenv.system;
    config = {
      allowUnfree = true;
      android_sdk.accept_license = true;
    };
  };
  # Pinned Android SDK layers. We take the bare minimum the `:app`
  # module needs (compileSdk 34 via platforms-android-34, build
  # tools 34.0.0, plus the android 26 platform for min-SDK
  # lowering). Updating these requires bumping the lists *and*
  # re-evaluating — the SDK is out-of-nix-store anyway so local
  # `sdkmanager update` still works for incremental changes.
  android = pkgsAndroid.androidenv.composeAndroidPackages {
    platformVersions = ["35" "34" "26"];
    buildToolsVersions = ["35.0.0" "34.0.0"];
    includeNDK = false;
    includeEmulator = false;
    includeSystemImages = false;
    cmdLineToolsVersion = "11.0";
    toolsVersion = null;
    platformToolsVersion = "35.0.2";
  };
in {
  # https://devenv.sh/basics/

  # https://devenv.sh/packages/
  packages = [
    pkgs.watchman
    pkgs.kubectl
    pkgs.aws-vault
    pkgs.kubernetes-helm
    pkgs.openssl
    pkgs.sqlite
    # Android toolchain (see android/README.md). We pull just JDK +
    # Gradle here — the Android SDK proper lives under
    # $ANDROID_HOME (set below) and is managed out-of-band with
    # sdkmanager because nix-packaged SDK layers lag the AGP
    # release cadence.
    pkgs.jdk17
    pkgs.gradle
    pkgs.android-tools # adb / fastboot
    android.androidsdk
  ];

  # https://devenv.sh/scripts/
  env = {
    # Point Android tooling at the nix-provided SDK root. This is a
    # read-only tree inside the nix store; nothing tries to mutate
    # it at build time. If you want a mutable SDK layer for
    # sdkmanager experiments, point ANDROID_HOME elsewhere by
    # exporting it in your shell before launching devenv.
    ANDROID_HOME = "${android.androidsdk}/libexec/android-sdk";
    ANDROID_SDK_ROOT = "${android.androidsdk}/libexec/android-sdk";
    # AGP 8.x wants a build-tools path for apksigner.
    ANDROID_BUILD_TOOLS = "${android.androidsdk}/libexec/android-sdk/build-tools/35.0.0";
    # The SDK layer under nix needs aapt2 from the build-tools dir.
    # Gradle resolves this automatically; exposing it here is just
    # for ad-hoc tooling.
    GRADLE_OPTS = "-Dorg.gradle.project.android.aapt2FromMavenOverride=${android.androidsdk}/libexec/android-sdk/build-tools/35.0.0/aapt2";
  };

  enterShell = ''
    echo "##############################################"
    echo "Welcome to your development shell...";
    echo "##############################################"
  '';

  # https://devenv.sh/tests/
  enterTest = ''
  '';

  # we use .envrc to source that
  dotenv.disableHint = true;

  # https://devenv.sh/services/

  # https://devenv.sh/languages/
  languages = {
    haskell = {
      enable = true;
      cabal.enable = true;
      lsp.enable = true;
    };
  };

  process.managers.process-compose = {
    # Uncomment this if you DON'T need the Terminal UI and just
    # want a log stream. Otherwise keep it commented and use
    # "make logs" instead. Both aproaches are more usable for copy pasting
    # logs than the TUI's Ctr+S approach ... especially if we disable json logging.
    # tui.enable = false;
    settings = {
      log_configuration = {
        fields_order = ["process" "level" "message"];
        no_metadata = false;
        no_color = true;
        flush_each_line = true;
        disable_json = true;
      };
    };
  };

  git-hooks.hooks = {
  };
  # See full reference at https://devenv.sh/reference/options/
}
