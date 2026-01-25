# Build static binary with:
#
#     make static
#
# Or directly:
#
#     nix-build --no-link -A static_package static.nix
#
{
  compiler ? "ghc965",
  disableOptimization ? false,
}:
let
  # Pin static-haskell-nix version
  static-haskell-nix = fetchTarball {
    url = "https://github.com/nh2/static-haskell-nix/archive/481e7d73ca624278ef0f840a0a2ba09e3a583217.tar.gz";
    sha256 = "0y4hzk1jxp4fdjksg6p1q6g5i4xw7cmb50vg5np7z5ipk4y4gc2x";
  };

  # Fetch forge from git
  forge-src = fetchTarball {
    url = "https://github.com/denibertovic/forge/archive/3c7b2faceb99f81af8028bdc5b03c610666b8e0a.tar.gz";
    sha256 = "1k2fjq5wnv3avqwx3pl6yjn18vgfdd3802hw70j12syp0kcq4s4s";
  };

  # Use nixpkgs from static-haskell-nix
  normalPkgs = import "${static-haskell-nix}/nixpkgs.nix";

  # First pass: get the static pkgs from survey
  static-haskell-nix_pkgsMusl = (import "${static-haskell-nix}/survey" {
    inherit normalPkgs compiler disableOptimization;
  }).pkgs;

  # Add our packages to the haskell package set
  pkgs_with_todo = static-haskell-nix_pkgsMusl.extend (final: previous: {
    haskell = final.lib.recursiveUpdate previous.haskell {
      packages."${compiler}" = previous.haskell.packages."${compiler}".override (old: {
        overrides = final.lib.composeExtensions (old.overrides or (_: _: {})) (self: super: {
          forge = self.callCabal2nix "forge" forge-src {};
          todo = self.callCabal2nix "todo" ./. {};
        });
      });
    };
  });

  # Second pass: run survey again with our packages included
  # This applies all the static linking overlays to our package
  survey_output = import "${static-haskell-nix}/survey" {
    normalPkgs = pkgs_with_todo;
    inherit compiler disableOptimization;
  };

  static_package = survey_output.haskellPackages.todo;

  # Build script
  fullBuildScript = normalPkgs.writeShellScript "build-static-todo.sh" ''
    set -eu -o pipefail
    export NIX_PATH=nixpkgs=${normalPkgs.path}
    ${normalPkgs.nix}/bin/nix-build --no-link -A static_package "$@"
  '';

in
{
  inherit static_package;
  inherit fullBuildScript;
  inherit survey_output;
  shell = static_package.env;
}
