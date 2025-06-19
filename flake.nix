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
                fourmolu
                nixfmt-rfc-style
                haskellPackages.hspec-discover
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
        };
      }
    );
}
