{ mkDerivation, aeson, base, bytestring, http-types, QuickCheck
, servant, servant-server, stdenv, transformers, wai, warp
}:
mkDerivation {
  pname = "servant-mock";
  version = "0.5";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    aeson base bytestring http-types QuickCheck servant servant-server
    transformers wai warp
  ];
  homepage = "http://github.com/haskell-servant/servant";
  description = "Derive a mock server for free from your servant API types";
  license = stdenv.lib.licenses.bsd3;
}
