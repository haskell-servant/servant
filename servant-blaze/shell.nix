let
  pkgs = import <nixpkgs> {};

  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: {
      servant = pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.appendConfigureFlag (self.callPackage ../servant {}) "--ghc-options=-Werror");
      servant-cassava = pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.appendConfigureFlag (self.callPackage ../servant-cassava {}) "--ghc-options=-Werror");
      servant-client = pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.appendConfigureFlag (self.callPackage ../servant-client {}) "--ghc-options=-Werror");
      servant-docs = pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.appendConfigureFlag (self.callPackage ../servant-docs {}) "--ghc-options=-Werror");
      servant-js = pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.appendConfigureFlag (self.callPackage ../servant-js {}) "--ghc-options=-Werror");
      servant-server = pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.appendConfigureFlag (self.callPackage ../servant-server {}) "--ghc-options=-Werror");
      servant-examples = pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.appendConfigureFlag (self.callPackage ../servant-examples {}) "--ghc-options=-Werror");
      servant-lucid = pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.appendConfigureFlag (self.callPackage ../servant-lucid {}) "--ghc-options=-Werror");
      servant-mock = pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.appendConfigureFlag (self.callPackage ../servant-mock {}) "--ghc-options=-Werror");
      servant-blaze = pkgs.haskell.lib.appendConfigureFlag (self.callPackage ./. {}) "--ghc-options=-Werror";
    };
  };

in haskellPackages.servant-blaze.env
