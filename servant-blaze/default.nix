{ mkDerivation, base, blaze-html, http-media, servant, stdenv }:
mkDerivation {
  pname = "servant-blaze";
  version = "0.5";
  src = ./.;
  buildDepends = [ base blaze-html http-media servant ];
  homepage = "http://haskell-servant.github.io/";
  description = "Blaze-html support for servant";
  license = stdenv.lib.licenses.bsd3;
}
