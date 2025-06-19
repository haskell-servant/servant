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

        format-tools = [
          # We use fourmolu compiled with GHC 9.12
          # as getting it to compile with lower GHC versions
          # is complicated and this works out of the box.
          pkgs.haskell.packages.ghc912.fourmolu_0_18_0_0
          pkgs.nixfmt-rfc-style
        ];

        mkDevShell =
          {
            compiler ? "ghc92",
            tutorial ? false,
          }:
          let
            ghc = pkgs.haskell.packages.${compiler}.ghcWithPackages (_: [ ]);
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
                zlib
                python3
                wget
                cabal-install
                postgresql
                openssl
                stack
                haskellPackages.hspec-discover
              ]
              ++ format-tools
              ++ (
                if tutorial then
                  [
                    docstuffs
                    postgresql
                  ]
                else
                  [ ]
              );

            shellHook = ''
              eval $(grep export ${ghc}/bin/ghc)
              export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:"${pkgs.zlib}/lib";
            '';
          };
      in
      {
        devShells = {
          default = mkDevShell { };
          tutorial = mkDevShell { tutorial = true; };
          formatters = pkgs.mkShell {
            buildInputs = format-tools;
          };
        };
      }
    );
}
