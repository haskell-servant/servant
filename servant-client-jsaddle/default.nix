{ mkDerivation, aeson, base, bytestring, case-insensitive
, containers, exceptions, hspec, hspec-discover, http-media
, http-types, jsaddle, jsaddle-dom, jsaddle-warp, monad-control
, mtl, QuickCheck, semigroupoids, servant, servant-client-core
, stdenv, string-conversions, text, transformers
, transformers-base, wai, wai-extra, warp
}:
mkDerivation {
  pname = "servant-client-jsaddle";
  version = "0.16";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring case-insensitive containers exceptions http-media
    http-types jsaddle jsaddle-dom monad-control mtl semigroupoids
    servant servant-client-core string-conversions text transformers
    transformers-base
  ];
  executableHaskellDepends = [
    aeson base bytestring case-insensitive containers exceptions hspec
    http-media http-types jsaddle jsaddle-dom jsaddle-warp
    monad-control mtl QuickCheck semigroupoids servant
    servant-client-core string-conversions text
    transformers transformers-base wai wai-extra warp
  ];
  executableToolDepends = [ hspec-discover ];
  testHaskellDepends = [
    aeson base bytestring case-insensitive containers exceptions hspec
    http-media http-types jsaddle jsaddle-dom jsaddle-warp
    monad-control mtl QuickCheck semigroupoids servant
    servant-client-core string-conversions text
    transformers transformers-base wai wai-extra warp
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "http://haskell-servant.readthedocs.org/";
  description = "automatic derivation of querying functions for servant webservices for jsaddle (GHCJS, GHC + WebKit, GHC + websockets, etc)";
  license = stdenv.lib.licenses.bsd3;
}
