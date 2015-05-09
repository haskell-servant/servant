{ mkDerivation, aeson, base, bytestring, hashable, hspec
, http-media, lens, servant, stdenv, string-conversions, text
, unordered-containers
}:
mkDerivation {
  pname = "servant-docs";
  version = "0.4";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    aeson base bytestring hashable http-media lens servant
    string-conversions text unordered-containers
  ];
  testDepends = [ aeson base hspec servant string-conversions ];
  homepage = "http://haskell-servant.github.io/";
  description = "generate API docs for your servant webservice";
  license = stdenv.lib.licenses.bsd3;
}
