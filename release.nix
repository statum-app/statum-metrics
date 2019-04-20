let
  config = rec {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackgesOld: rec {
          statumMetrics =
            pkgs.haskell.lib.overrideCabal
              ( haskellPackagesNew.callPackage ./default.nix {}
              )
              ( oldDerivation: {
                  enableSharedExecutables = false;
                }
              );
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };
in
{ statumMetrics = pkgs.haskellPackages.statumMetrics;
}
