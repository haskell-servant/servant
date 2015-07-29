{ mkDerivation, aeson, attoparsec, base, bytestring
, bytestring-conversion, containers, directory, doctest, either
, exceptions, filemanip, filepath, hspec, hspec-wai, http-types
, mmorph, mtl, network, network-uri, parsec, QuickCheck, safe
, servant, split, stdenv, string-conversions, system-filepath
, temporary, text, transformers, wai, wai-app-static, wai-extra
, warp
}:
mkDerivation {
  pname = "servant-server";
  version = "0.5";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    aeson attoparsec base bytestring containers either filepath
    http-types mmorph mtl network network-uri safe servant split
    string-conversions system-filepath text transformers wai
    wai-app-static warp
  ];
  testDepends = [
    aeson base bytestring bytestring-conversion directory doctest
    either exceptions filemanip filepath hspec hspec-wai http-types mtl
    network parsec QuickCheck servant string-conversions temporary text
    transformers wai wai-extra warp
  ];
  homepage = "http://haskell-servant.github.io/";
  description = "A family of combinators for defining webservices APIs and serving them";
  license = stdenv.lib.licenses.bsd3;
}
