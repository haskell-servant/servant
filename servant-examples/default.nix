{ mkDerivation, aeson, base, bytestring, directory, either
, engine-io, engine-io-wai, http-types, js-jquery, lucid, mtl
, random, servant, servant-client, servant-docs, servant-js
, servant-lucid, servant-server, socket-io, stdenv, stm, text, time
, transformers, wai, wai-extra, warp
}:
mkDerivation {
  pname = "servant-examples";
  version = "0.5";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    aeson base bytestring directory either engine-io engine-io-wai
    http-types js-jquery lucid mtl random servant servant-client
    servant-docs servant-js servant-lucid servant-server socket-io stm
    text time transformers wai wai-extra warp
  ];
  homepage = "http://haskell-servant.github.io/";
  description = "Example programs for servant";
  license = stdenv.lib.licenses.bsd3;
}
