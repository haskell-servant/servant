{ mkDerivation, aeson, attoparsec, base, bytestring
, bytestring-conversion, directory, either, exceptions, hspec
, hspec-wai, http-types, network, network-uri, parsec, QuickCheck
, safe, servant, split, stdenv, string-conversions, system-filepath
, temporary, text, transformers, wai, wai-app-static, wai-extra
, warp
}:
mkDerivation {
  pname = "servant-server";
  version = "0.2.4";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    aeson attoparsec base bytestring either http-types network-uri safe
    servant split string-conversions system-filepath text transformers
    wai wai-app-static warp
  ];
  testDepends = [
    aeson base bytestring bytestring-conversion directory either
    exceptions hspec hspec-wai http-types network parsec QuickCheck
    servant string-conversions temporary text transformers wai
    wai-extra warp
  ];
  homepage = "http://haskell-servant.github.io/";
  description = "A family of combinators for defining webservices APIs and serving them";
  license = stdenv.lib.licenses.bsd3;
}
