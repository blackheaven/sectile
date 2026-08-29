{
  description = "sectile";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
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
        pkgs = import nixpkgs { inherit system; };

        jailbreakUnbreak =
          pkg: pkgs.haskell.lib.doJailbreak (pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.unmarkBroken pkg));

        haskellPackages = pkgs.haskellPackages.override {
          overrides = hself: hsuper: {
            ede = jailbreakUnbreak hsuper.ede;
          };
        };
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
