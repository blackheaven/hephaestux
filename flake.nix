{
  description = "hephaestux";

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

        haskellPackages = pkgs.haskellPackages.override {
          overrides = hself: hsuper: {
            # Dependency overrides go here
          };
        };

        nixpkgsOverlay = _final: _prev: {
          hephaestux = self.packages.${system}.hephaestux;
        };
      in
      rec {
        packages.hephaestux = haskellPackages.callCabal2nix "hephaestux" ./. { };

        defaultPackage = packages.hephaestux;

        overlays = nixpkgsOverlay;

        devShell = pkgs.mkShell {
          buildInputs = with haskellPackages; [
            haskell-language-server
            ghcid
            cabal-install
          ];
          inputsFrom = [
            self.defaultPackage.${system}.env
          ];
        };
      }
    );
}
