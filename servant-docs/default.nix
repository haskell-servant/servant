{ mkDerivation, aeson, base, bytestring, bytestring-conversion
, case-insensitive, hashable, hspec, http-media, http-types, lens
, servant, stdenv, string-conversions, text, unordered-containers
}:
mkDerivation {
  pname = "servant-docs";
  version = "0.5";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    aeson base bytestring bytestring-conversion case-insensitive
    hashable http-media http-types lens servant string-conversions text
    unordered-containers
  ];
  testDepends = [ aeson base hspec lens servant string-conversions ];
  homepage = "http://haskell-servant.github.io/";
  description = "generate API docs for your servant webservice";
  license = stdenv.lib.licenses.bsd3;
}
