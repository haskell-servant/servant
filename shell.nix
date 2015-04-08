with (import <nixpkgs> {}).pkgs;
let modifiedHaskellPackages = haskellngPackages.override {
      overrides = self: super: {
        servant = self.callPackage ../servant {};
        servant-server = self.callPackage ./servant-server {};
        servant-docs = self.callPackage ./. {};
      };
    };
in modifiedHaskellPackages.servant-docs.env
