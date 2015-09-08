{ mkDerivation, base, cassava, http-media, servant, stdenv, vector
}:
mkDerivation {
  pname = "servant-cassava";
  version = "0.4.4.2";
  src = ./.;
  buildDepends = [ base cassava http-media servant vector ];
  homepage = "http://haskell-servant.github.io/";
  description = "Servant CSV content-type for cassava";
  license = stdenv.lib.licenses.bsd3;
}
