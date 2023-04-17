{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils/cfacdce06f30d2b68473a46042957675eebb3401";
    nixpkgs.url = "github:NixOS/nixpkgs/1fb781f4a148c19e9da1d35a4cbe15d0158afc4e";
    # symbols = {
    #   url = "github:deemp/symbols/add-fromlist";
    #   flake = false;
    # };
  };
  outputs = inputs: inputs.flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = inputs.nixpkgs.legacyPackages.${system};
      inherit (pkgs.haskell.lib) overrideCabal unmarkBroken dontCheck;

      override = {
        overrides = self: super: {
          # symbols = super.callCabal2nix "symbols" inputs.symbols.outPath { };
          servant-quickcheck = unmarkBroken super.servant-quickcheck;
          openapi3 = dontCheck (unmarkBroken super.openapi3);

          servant-doc-basic-auth = super.callCabal2nix "basic-auth" ./doc/cookbook/basic-auth { };
          servant-doc-basic-streaming = super.callCabal2nix "basic-streaming" ./doc/cookbook/basic-streaming { };
          servant-doc-curl-mock = super.callCabal2nix "curl-mock" ./doc/cookbook/curl-mock { };
          servant-doc-custom-errors = super.callCabal2nix "custom-errors" ./doc/cookbook/custom-errors { };
          servant-doc-db-mysql-basics = super.callCabal2nix "mysql-basics" ./doc/cookbook/db-mysql-basics { };
          servant-doc-db-postgres-pool = super.callCabal2nix "db-postgres-pool" ./doc/cookbook/db-postgres-pool { };
          servant-doc-db-sqlite-simple = super.callCabal2nix "db-sqlite-simple" ./doc/cookbook/db-sqlite-simple { };
          servant-doc-file-upload = super.callCabal2nix "file-upload" ./doc/cookbook/file-upload { };
          servant-doc-generic = super.callCabal2nix "generic" ./doc/cookbook/generic { };
          servant-doc-hoist-server-with-context = super.callCabal2nix "hoist-server" ./doc/cookbook/hoist-server-with-context { };
          servant-doc-https = super.callCabal2nix "https" ./doc/cookbook/https { };
          servant-doc-jwt-and-basic-auth = super.callCabal2nix "jwt-and" ./doc/cookbook/jwt-and-basic-auth { };
          servant-doc-managed-resource = super.callCabal2nix "managed-resource" ./doc/cookbook/managed-resource { };
          servant-doc-open-id-connect = super.callCabal2nix "open-id" ./doc/cookbook/open-id-connect { };
          servant-doc-pagination = super.callCabal2nix "pagination" ./doc/cookbook/pagination { };
          servant-doc-sentry = super.callCabal2nix "sentry" ./doc/cookbook/sentry { };
          servant-doc-structuring-apis = super.callCabal2nix "structuring-apis" ./doc/cookbook/structuring-apis { };
          servant-doc-testing = super.callCabal2nix "testing" ./doc/cookbook/testing { };
          servant-doc-tutorial = super.callCabal2nix "tutorial" ./doc/tutorial { };
          servant-doc-using-custom-monad = super.callCabal2nix "using-custom-monad" ./doc/cookbook/using-custom-monad { };
          servant-doc-using-free-client = super.callCabal2nix "using-free-client" ./doc/cookbook/using-free-client { };
          servant-doc-uverb = super.callCabal2nix "uverb" ./doc/cookbook/uverb { };

          servant = super.callCabal2nix "servant" ./servant { };
          servant-auth = super.callCabal2nix "servant-auth" ./servant-auth/servant-auth { };
          servant-auth-client = super.callCabal2nix "servant-auth-client" ./servant-auth/servant-auth-client { };
          servant-auth-docs = super.callCabal2nix "servant-auth-docs" ./servant-auth/servant-auth-docs { };
          servant-auth-server = super.callCabal2nix "servant-auth-server" ./servant-auth/servant-auth-server { };
          servant-auth-swagger = super.callCabal2nix "servant-auth-swagger" ./servant-auth/servant-auth-swagger { };
          servant-client = super.callCabal2nix "servant-client" ./servant-client { };
          servant-client-core = super.callCabal2nix "servant-client-core" ./servant-client-core { };
          servant-client-ghcjs = super.callCabal2nix "servant-client-ghcjs" ./servant-client-ghcjs { };
          servant-conduit = super.callCabal2nix "servant-conduit" ./servant-conduit { };
          servant-docs = super.callCabal2nix "servant-docs" ./servant-docs { };
          servant-foreign = super.callCabal2nix "servant-foreign" ./servant-foreign { };
          servant-http-streams = super.callCabal2nix "servant-http-streams" ./servant-http-streams { };
          servant-machines = super.callCabal2nix "servant-machines" ./servant-machines { };
          servant-named-core = super.callCabal2nix "servant-named-core" ./servant-named/servant-named-core { };
          servant-named-client = super.callCabal2nix "servant-named-client" ./servant-named/servant-named-client { };
          servant-named-server = super.callCabal2nix "servant-named-server" ./servant-named/servant-named-server { };
          servant-pipes = super.callCabal2nix "servant-pipes" ./servant-pipes { };
          servant-server = super.callCabal2nix "servant-server" ./servant-server { };
          servant-swagger = super.callCabal2nix "servant-swagger" ./servant-swagger { };
          servant-swagger-example = super.callCabal2nix "servant-swagger-example" ./servant-swagger/example { };
        };
      };
      ghcVersion = "ghc927";
      hpkgs = pkgs.haskell.packages.${ghcVersion};
      getHaskellPackagesDeps = someHaskellPackages: with pkgs.lib.lists; (subtractLists someHaskellPackages (concatLists (map (package: concatLists (__attrValues package.getCabalDeps)) someHaskellPackages)));
      ghcForPackages = hpkgs_: override_: localHaskellPackageNames: (hpkgs_.override override_).ghcWithPackages (ps: getHaskellPackagesDeps (map (x: ps.${x}) localHaskellPackageNames));
      ghc = ghcForPackages hpkgs override [
        "servant-doc-basic-auth"
        "servant-doc-basic-streaming"
        "servant-doc-curl-mock"
        "servant-doc-custom-errors"
        "servant-doc-db-mysql-basics"
        "servant-doc-db-postgres-pool"
        "servant-doc-db-sqlite-simple"
        "servant-doc-file-upload"
        "servant-doc-generic"
        "servant-doc-hoist-server-with-context"
        "servant-doc-https"
        "servant-doc-jwt-and-basic-auth"
        "servant-doc-managed-resource"
        "servant-doc-open-id-connect"
        "servant-doc-pagination"
        "servant-doc-sentry"
        "servant-doc-structuring-apis"
        "servant-doc-testing"
        "servant-doc-using-custom-monad"
        "servant-doc-using-free-client"
        "servant-doc-uverb"
        "servant-doc-tutorial"

        "servant-quickcheck"

        "servant"
        "servant-auth"
        "servant-auth-client"
        "servant-auth-docs"
        "servant-auth-server"
        "servant-auth-swagger"
        "servant-client"
        "servant-client-core"
        "servant-client-ghcjs"
        "servant-conduit"
        "servant-docs"
        "servant-foreign"
        "servant-http-streams"
        "servant-machines"
        "servant-named-core"
        "servant-named-client"
        "servant-named-server"
        "servant-pipes"
        "servant-server"
        "servant-swagger"
        "servant-doc-db-mysql-basics"
      ];

      tools = [
        pkgs.cabal-install
        # ghc should go before haskell-language-server - https://github.com/NixOS/nixpkgs/issues/225895
        ghc
        hpkgs.haskell-language-server
      ];

      devShells.default = pkgs.mkShell {
        buildInputs = tools;
      };
    in
    {
      inherit devShells;
    });
}

