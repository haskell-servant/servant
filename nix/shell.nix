{ compiler ? "ghc901"
, tutorial ? false
, pkgs ? import ./nixpkgs.nix
}:
  
  with pkgs;

  let
    ghc = haskell.packages.${compiler}.ghcWithPackages (_: []);
    docstuffs = python3.withPackages (ps: with ps; [ recommonmark sphinx sphinx_rtd_theme ]);
  in
  stdenv.mkDerivation {
      name = "servant-dev";
      buildInputs = [ ghc zlib python3 wget cabal-install postgresql openssl stack haskellPackages.hspec-discover ]
        ++ (if tutorial then [docstuffs postgresql] else []);
      shellHook = ''
        eval $(grep export ${ghc}/bin/ghc)
        export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:"${zlib}/lib";
      '';
  }
