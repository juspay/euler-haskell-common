{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake/0.3.0";
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } ({ withSystem, ... }: {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        inputs.haskell-flake.flakeModule
      ];
      perSystem = { self', pkgs, lib, config, ... }: {
        packages.default = self'.packages.juspay-extra;
        haskellProjects.default = {
          overrides = self: super:
            with pkgs.haskell.lib.compose;
            lib.mapAttrs (k: v: lib.pipe super.${k} v) {
              juspay-extra = [ doJailbreak ];
            };
        };
      };
    });
}
