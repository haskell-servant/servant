let
  reflex-platform = import (builtins.fetchTarball {
    name = "reflex-platform";
    url = "https://github.com/reflex-frp/reflex-platform/archive/1aba6f367982bd6dd78ec2fda75ab246a62d32c5.tar.gz";
  }) {};

  pkgs = import ./nix/nixpkgs.nix;

in

pkgs.stdenv.mkDerivation {
  name = "ghcjs-shell";
  buildInputs = [
    (reflex-platform.ghcjs.ghcWithPackages (p: with p; [
      attoparsec
      hashable
    ]))
  ] ++ (with pkgs; [
    cabal-install
    gmp
    haskellPackages.cabal-plan
    haskellPackages.hspec-discover
    nodejs
    perl
    zlib
    openssl
    openssl.dev
    postgresql
  ]);
}
