{ pkgs ? import <nixpkgs> {}
, compiler ? "ghc821"
, tutorial ? false
}:

with pkgs;

let
  ghc = haskell.packages.${compiler}.ghcWithPackages (_: []);
  docstuffs = python3.withPackages (ps: with ps; [ recommonmark sphinx sphinx_rtd_theme ]);
in

stdenv.mkDerivation {
    name = "servant-dev";
    buildInputs = [ ghc zlib python3 wget ]
      ++ (if tutorial then [docstuffs postgresql] else []);
    shellHook = ''
      eval $(grep export ${ghc}/bin/ghc)
      export LD_LIBRARY_PATH="${zlib}/lib";
    '';
}
