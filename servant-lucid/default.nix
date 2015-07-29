{ mkDerivation, base, http-media, lucid, servant, stdenv }:
mkDerivation {
  pname = "servant-lucid";
  version = "0.5";
  src = ./.;
  buildDepends = [ base http-media lucid servant ];
  homepage = "http://haskell-servant.github.io/";
  description = "Servant support for lucid";
  license = stdenv.lib.licenses.bsd3;
}
