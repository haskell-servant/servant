{ mkDerivation, aeson, base, charset, filepath, hspec
, hspec-expectations, language-ecmascript, lens, servant
, servant-server, stdenv, stm, text, transformers, warp
}:
mkDerivation {
  pname = "servant-jquery";
  version = "0.2.2";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    aeson base charset filepath lens servant servant-server stm text
    transformers warp
  ];
  testDepends = [
    base hspec hspec-expectations language-ecmascript lens servant
  ];
  homepage = "http://haskell-servant.github.io/";
  description = "Automatically derive (jquery) javascript functions to query servant webservices";
  license = stdenv.lib.licenses.bsd3;
}
