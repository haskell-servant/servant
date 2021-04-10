let nixos = fetchTarball { 
  url = "https://releases.nixos.org/nixos/20.09/nixos-20.09.3505.12d9950bf47/nixexprs.tar.xz";
  sha256 = "0fsl8bsdb8i536pfs4wrp0826h5l84xqlwx32sbz66jg4ykqp9lr";
}; in

{ compiler ? "ghc8102"
, tutorial ? false
, pkgs ? import nixos { config = {}; }
}:
  
  with pkgs;

  let
    ghc = haskell.packages.${compiler}.ghcWithPackages (_: []);
    docstuffs = python3.withPackages (ps: with ps; [ recommonmark sphinx sphinx_rtd_theme ]);
  in
  stdenv.mkDerivation {
      name = "servant-dev";
      buildInputs = [ ghc zlib python3 wget cabal-install postgresql openssl ]
        ++ (if tutorial then [docstuffs postgresql] else []);
      shellHook = ''
        eval $(grep export ${ghc}/bin/ghc)
        export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:"${zlib}/lib";
      '';
  }
