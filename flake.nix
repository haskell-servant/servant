{
  description = "Servant development environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-25.05-darwin";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };

        # We use fourmolu compiled with GHC 9.12
        # as getting it to compile with lower GHC versions
        # is complicated and this works out of the box.
        haskellFormatter = pkgs.haskell.packages.ghc912.fourmolu_0_18_0_0;
        haskellLinter = pkgs.hlint;

        nixFormatter = pkgs.nixfmt-rfc-style;

        mkDevShell =
          {
            compiler ? "ghc92",
            tutorial ? false,
          }:
          let
            ghc = pkgs.haskell.packages.${compiler}.ghcWithPackages (ghcPkg: [
              # Some dependencies don't like being built with
              # as part of a project, so we pull them from nix.
              ghcPkg.zlib
              ghcPkg.lzma
            ]);
            docstuffs = pkgs.python3.withPackages (
              ps: with ps; [
                recommonmark
                sphinx
                sphinx_rtd_theme
              ]
            );
          in
          pkgs.mkShell {
            buildInputs =
              with pkgs;
              [
                ghc
                python3
                wget
                cabal-install
                postgresql
                openssl
                stack
                haskellPackages.hspec-discover
                haskellFormatter
                haskellLinter
                nixFormatter
              ]
              ++ (
                if tutorial then
                  [
                    docstuffs
                    postgresql
                  ]
                else
                  [ ]
              );
          };

        mkCiShell = tools: pkgs.mkShell { buildInputs = tools; };
      in
      {
        devShells = {
          default = mkDevShell { };
          tutorial = mkDevShell { tutorial = true; };

          # Development shells for different GHC versions.
          ghc92 = mkDevShell { compiler = "ghc92"; };
          ghc94 = mkDevShell { compiler = "ghc94"; };
          ghc96 = mkDevShell { compiler = "ghc96"; };
          ghc98 = mkDevShell { compiler = "ghc98"; };
          ghc910 = mkDevShell { compiler = "ghc910"; };
          ghc912 = mkDevShell { compiler = "ghc912"; };

          # Single-tool shells for CI checks.
          haskellFormatter = mkCiShell [ haskellFormatter ];
          haskellLinter = mkCiShell [ haskellLinter ];
        };
      }
    );
}
