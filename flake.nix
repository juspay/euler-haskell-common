{
  inputs = {
    common.url = "github:juspay/nix-common";
  };

  outputs = inputs:
    inputs.common.lib.mkFlake { inherit inputs; } {
      perSystem = { self', pkgs, pkgs-latest, config, system, filter, ... }: {

        haskellProjects.default = let fs = pkgs-latest.lib.fileset; in {

          projectRoot = builtins.toString (fs.toSource {
            root = ./.;
            fileset = fs.unions [
              ./src
              ./test
              ./benchmark
              ./juspay-extra.cabal
            ];
          });

          autoWire = [ "packages" ];
          packages = {
            # Dependencies
          };
        };

        packages.default = self'.packages.juspay-extra;

        devShells.default = pkgs.mkShell {
          name = "euler-haskell-common-devshell";
          inputsFrom = [
            config.haskellProjects.default.outputs.devShell
            config.devShells.common
          ];
        };
      };
    };
}
