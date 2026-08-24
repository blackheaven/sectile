{
  description = "sectile";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";

    safe-coloured-text = {
      url = "github:NorfairKing/safe-coloured-text";
      flake = false;
    };
  };

  outputs =
    inputs@{
      self,
      nixpkgs,
      flake-utils,
      ...
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            (import (inputs.safe-coloured-text + "/nix/overlay.nix"))
          ];
        };

        haskellPackages = pkgs.haskellPackages;
      in
      rec {
        packages.sectile =
          # activateBenchmark
          (
            haskellPackages.callCabal2nix "sectile" ./. {
              # Dependency overrides go here
            }
          );

        defaultPackage = packages.sectile;

        devShell =
          let
            scripts = pkgs.symlinkJoin {
              name = "scripts";
              paths = pkgs.lib.mapAttrsToList pkgs.writeShellScriptBin { };
            };
          in
          pkgs.mkShell {
            buildInputs = with haskellPackages; [
              haskell-language-server
              ghcid
              cabal-install
              scripts
            ];
            inputsFrom = [
              self.defaultPackage.${system}.env
            ];
          };
      }
    );
}
