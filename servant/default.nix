{ mkDerivation, aeson, attoparsec, base, bytestring
, bytestring-conversion, case-insensitive, directory, doctest
, filemanip, filepath, hspec, http-media, http-types, network-uri
, parsec, QuickCheck, quickcheck-instances, stdenv
, string-conversions, text, url, vault
}:
mkDerivation {
  pname = "servant";
  version = "0.5";
  src = ./.;
  buildDepends = [
    aeson attoparsec base bytestring bytestring-conversion
    case-insensitive http-media http-types network-uri
    string-conversions text vault
  ];
  testDepends = [
    aeson attoparsec base bytestring directory doctest filemanip
    filepath hspec parsec QuickCheck quickcheck-instances
    string-conversions text url
  ];
  homepage = "http://haskell-servant.github.io/";
  description = "A family of combinators for defining webservices APIs";
  license = stdenv.lib.licenses.bsd3;
}
