{
  pkgs,
  lib,
  config,
  inputs,
  ...
}: let
  pkgs-unstable = import inputs.nixpkgs-unstable {system = pkgs.stdenv.system;};
in {
  # https://devenv.sh/basics/

  # https://devenv.sh/packages/
  packages = [
    pkgs.watchman
    pkgs.kubectl
    pkgs.aws-vault
    pkgs.kubernetes-helm
    pkgs.openssl
  ];

  # https://devenv.sh/scripts/
  env = {
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
