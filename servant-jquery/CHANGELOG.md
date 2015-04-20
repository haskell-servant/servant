0.3
---
* Extend `HeaderArg` to support more advanced HTTP header handling (https://github.com/haskell-servant/servant-jquery/pull/6)
* Support content-type aware combinators (but require that endpoints support JSON)
* Add support for Matrix params (https://github.com/haskell-servant/servant-jquery/pull/11)
* Add functions that directly generate the Javascript code from the API type without having to manually pattern match on the result.

0.2.2
-----

* Fix an issue where toplevel Raw endpoints would generate a JS function with no name (https://github.com/haskell-servant/servant-jquery/issues/2)
* Replace dots by _ in paths (https://github.com/haskell-servant/servant-jquery/issues/1)
