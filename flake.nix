{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    # haskell-flake 0.4.0 unreleased, points to latest master commit at the time
    haskell-flake.url = "github:srid/haskell-flake/908a59167f78035a123ab71ed77af79bed519771";
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
          settings = {
            juspay-extra = {
              jailbreak = true;
            };
          };
        };
      };
    });
}
