# Get a Nix shell with all the packages installed
# Also a good way of running the tests for all packages
with (import <nixpkgs> {}).pkgs;
let modifiedHaskellPackages = haskellngPackages.override {
      overrides = self: super: {
        servant = self.callPackage ../servant {};
        servant-server = self.callPackage ../servant-server {};
        servant-client = self.callPackage ../servant-client {};
        servant-jquery = self.callPackage ../servant-jquery {};
        servant-docs   = self.callPackage ../servant-docs {};
      };
    };
in modifiedHaskellPackages.ghcWithPackages ( p : with p ; [
    servant servant-server servant-client servant-jquery servant-docs
])
