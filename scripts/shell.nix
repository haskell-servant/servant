# Get a Nix shell with all the packages installed
# Also a good way of running the tests for all packages
with (import <nixpkgs> {}).pkgs;
let modifiedHaskellPackages = haskellngPackages.override {
      overrides = with haskell-ng.lib ; self: super: {
        servant = appendConfigureFlag ( self.callPackage ../servant {} )
            "--ghc-options=-Werror";
        servant-server   = appendConfigureFlag (self.callPackage
                ../servant-server {}) "--ghc-options=-Werror";
        servant-client   = appendConfigureFlag (self.callPackage
            ../servant-client {}) "--ghc-options=-Werror";
        servant-jquery   = appendConfigureFlag (self.callPackage
            ../servant-jquery {}) "--ghc-options=-Werror";
        servant-docs     = appendConfigureFlag (self.callPackage ../servant-docs
            {}) "--ghc-options=-Werror";
      };
    };
in modifiedHaskellPackages.ghcWithPackages ( p : with p ; [
    servant servant-server servant-client servant-jquery servant-docs
])
