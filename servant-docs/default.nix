{ mkDerivation, aeson, base, bytestring, hashable, hspec
, http-media, lens, servant, stdenv, string-conversions, text
, unordered-containers
}:
mkDerivation {
  pname = "servant-docs";
  version = "0.3";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    aeson base bytestring hashable http-media lens servant
    string-conversions text unordered-containers
  ];
  testDepends = [ aeson base hspec lens servant ];
  homepage = "http://haskell-servant.github.io/";
  description = "generate API docs for your servant webservice";
  license = stdenv.lib.licenses.bsd3;
}
