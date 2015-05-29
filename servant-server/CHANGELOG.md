0.4.1
-----
* Bump attoparsec upper bound to < 0.14
* Bump wai-app-static upper bound to < 3.2
* Bump either upper bound to < 4.5

0.4
---
* `Delete` now is like `Get`, `Post`, `Put`, and `Patch` and returns a response body
* Add a `RouteMismatch` constructor for arbitrary HTTP response codes (https://github.com/haskell-servant/servant-server/pull/22)
* Add support for the `Patch` combinator
* Support for `Accept`/`Content-type` headers and for the content-type aware combinators in *servant-0.4*
* Export `toApplication` from `Servant.Server` (https://github.com/haskell-servant/servant-server/pull/29)
* Support other Monads than just `EitherT (Int, String) IO` (https://github.com/haskell-servant/servant-server/pull/21)
* Make methods return status code 204 if they return () (https://github.com/haskell-servant/servant-server/issues/28)
* Add server support for response headers
* Use `ServantErr` instead of `(Int,String)` in `EitherT` handlers
* Add `errXXX` functions for HTTP errors with sensible default reason strings
* Add `enter` function for applying natural transformations to handlers

0.2.4
-----
* Added support for matrix parameters, see e.g. http://www.w3.org/DesignIssues/MatrixURIs.html
* Add support for serializing based on Accept header
  (https://github.com/haskell-servant/servant-server/issues/9)
* Ignore trailing slashes
  (https://github.com/haskell-servant/servant-server/issues/5)


0.2.3
-----

* Fix consuming request body issue
  (https://github.com/haskell-servant/servant/issues/3)
* Make code sample in Servant.Server complete
