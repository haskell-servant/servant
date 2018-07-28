# `servant-client-jsaddle`

This is a an implementation of the `servant-client-core` API on top of `jsaddle`, a framework that lets you write Haskell programs that compile to javascript to run in a browser or compile to native code that connects to a browser.

It is similar to `servant-client-ghcjs`, except it supports native compilation and native GHCi. It even reuses some of the logic from `servant-client-ghcjs`.

# Build

This package comes with a test suite that depends on `jsaddle-webkit2gtk`. You may want to skip that because of the heavy dependency footprint.

    cabal new-build --allow-newer=aeson,http-types --disable-tests

# Usage

TBD. Similar to `servant-client` and `servant-client-ghcjs`.
