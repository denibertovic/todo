{
  description = "Todo - A CLI todo list manager";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
    flake-utils.url = "github:numtide/flake-utils";
    forge = {
      url = "github:denibertovic/forge";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    forge,
  }:
    flake-utils.lib.eachDefaultSystem (
      system: let
        pkgs = nixpkgs.legacyPackages.${system};

        haskellPackages = pkgs.haskellPackages.override {
          overrides = self: super: {
            forge = forge.packages.${system}.forge;
            qrcode-core = pkgs.haskell.lib.markUnbroken super.qrcode-core;
          };
        };

        todo = haskellPackages.callCabal2nix "todo" ./. {};
        todo-sync-server = haskellPackages.callCabal2nix "todo-sync-server" ./todo-sync-server {};
      in {
        packages = {
          default = todo;
          todo = todo;
          todo-sync-server = todo-sync-server;
        };

        apps.default = flake-utils.lib.mkApp {
          drv = todo;
          name = "todo";
        };

        devShells.default = haskellPackages.shellFor {
          packages = p: [todo todo-sync-server];
          buildInputs = with pkgs; [
            haskellPackages.cabal-install
            haskellPackages.haskell-language-server
            haskellPackages.ghcid
          ];
        };
      }
    )
    //
    {
      nixosModules = {
        todo-sync-server = import ./nixos/module.nix { inherit self; };
        default = self.nixosModules.todo-sync-server;
      };

      homeManagerModules = {
        todo-sync = import ./nixos/home-manager.nix { inherit self; };
        default = self.homeManagerModules.todo-sync;
      };
    };
}
