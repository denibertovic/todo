{
  description = "Todo - A CLI todo list manager";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    flake-utils.url = "github:numtide/flake-utils";
    forge = {
      url = "github:denibertovic/forge";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils, forge }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        haskellPackages = pkgs.haskellPackages.override {
          overrides = self: super: {
            forge = forge.packages.${system}.forge;
          };
        };

        todo = haskellPackages.callCabal2nix "todo" ./. {};

      in {
        packages = {
          default = todo;
          todo = todo;
        };

        apps.default = flake-utils.lib.mkApp {
          drv = todo;
          name = "todo";
        };

        devShells.default = haskellPackages.shellFor {
          packages = p: [ todo ];
          buildInputs = with pkgs; [
            haskellPackages.cabal-install
            haskellPackages.haskell-language-server
            haskellPackages.ghcid
          ];
        };
      }
    );
}
