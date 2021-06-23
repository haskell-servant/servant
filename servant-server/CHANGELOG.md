[The latest version of this document is on GitHub.](https://github.com/haskell-servant/servant/blob/master/servant-server/CHANGELOG.md)
[Changelog for `servant` package contains significant entries for all core packages.](https://github.com/haskell-servant/servant/blob/master/servant/CHANGELOG.md)

0.18.3
------

### Significant changes

- Add response header support to UVerb (#1420)

### Other changes

- Support GHC-9.0.1.
- Bump `bytestring`, `hspec` and `base64-bytestring` dependencies.

0.18.2
------

### Significant changes

- Support `Fragment` combinator.

0.18.1
------

### Significant changes

- Union verbs

### Other changes

- Bump "tested-with" ghc versions
- Allow newer dependencies

0.18
----

### Significant changes

- Support for ghc8.8 (#1318, #1326, #1327)

- Configurable error messages for automatic errors thrown by servant,
  like "no route" or "could not parse json body" (#1312, #1326, #1327)


0.17
----

### Significant changes

- Add NoContentVerb [#1028](https://github.com/haskell-servant/servant/issues/1028) [#1219](https://github.com/haskell-servant/servant/pull/1219) [#1228](https://github.com/haskell-servant/servant/pull/1228)

  The `NoContent` API endpoints should now use `NoContentVerb` combinator.
  The API type changes are usually of the kind

  ```diff
  - :<|> PostNoContent '[JSON] NoContent
  + :<|> PostNoContent
  ```

  i.e. one doesn't need to specify the content-type anymore. There is no content.

- `Capture` can be `Lenient` [#1155](https://github.com/haskell-servant/servant/issues/1155) [#1156](https://github.com/haskell-servant/servant/pull/1156)

  You can specify a lenient capture as

  ```haskell
  :<|> "capture-lenient"  :> Capture' '[Lenient] "foo" Int :> GET
  ```

  which will make the capture always succeed. Handlers will be of the
  type `Either String CapturedType`, where `Left err` represents
  the possible parse failure.

- *servant-server* use queryString to parse QueryParam, QueryParams and QueryFlag [#1249](https://github.com/haskell-servant/servant/pull/1249) [#1262](https://github.com/haskell-servant/servant/pull/1262)

  Some APIs need query parameters rewriting, e.g. in order to support
   for multiple casing (camel, snake, etc) or something to that effect.

  This could be easily achieved by using WAI Middleware and modifying
  request's `Query`. But QueryParam, QueryParams and QueryFlag use
  `rawQueryString`. By using `queryString` rather then `rawQueryString`
  we can enable such rewritings.

- *servant* *servant-server* Make packages `build-type: Simple` [#1263](https://github.com/haskell-servant/servant/pull/1263)

  We used `build-type: Custom`, but it's problematic e.g.
  for cross-compiling. The benefit is small, as the doctests
  can be run other ways too (though not so conveniently).

0.16.2
------

* `singleton-bool-0.1.5` (`SBool` is re-exported)
    - Add `discreteBool :: Dec (a :~: b)` (GHC-7.8+)
    - Add `Show`, `Eq`, `Ord` `SBool b` instances.
* dependencies update

0.16.1
------

* Use `http-api-data-0.4.1` (a part of its API is re-exported)
  [#1181](https://github.com/haskell-servant/servant/pull/1181)

0.16
----

- Rename `ServantError` to `ClientError`, `ServantErr` to `ServerError`
  [#1131](https://github.com/haskell-servant/servant/pull/1131)
- *servant-server* Reorder HTTP failure code priorities
  [#1103](https://github.com/haskell-servant/servant/pull/1103)
- *servant-server* Re-organise internal modules
  [#1139](https://github.com/haskell-servant/servant/pull/1139)
- Allow `network-3.0`
  [#1107](https://github.com/haskell-servant/servant/pull/1107)

0.15
----

- Streaming refactoring.
  [#991](https://github.com/haskell-servant/servant/pull/991)
  [#1076](https://github.com/haskell-servant/servant/pull/1076)
  [#1077](https://github.com/haskell-servant/servant/pull/1077)

  The streaming functionality (`Servant.API.Stream`) is refactored to use
  `servant`'s own `SourceIO` type (see `Servant.Types.SourceT` documentation),
  which replaces both `StreamGenerator` and `ResultStream` types.

  New conversion type-classes are `ToSourceIO` and `FromSourceIO`
  (replacing `ToStreamGenerator` and `BuildFromStream`).
  There are instances for *conduit*, *pipes* and *machines* in new packages:
  [servant-conduit](https://hackage.haskell.org/package/servant-conduit)
  [servant-pipes](https://hackage.haskell.org/package/servant-pipes) and
  [servant-machines](https://hackage.haskell.org/package/servant-machines)
  respectively.

  Writing new framing strategies is simpler. Check existing strategies for examples.

  This change shouldn't affect you, if you don't use streaming endpoints.

- Drop support for GHC older than 8.0
  [#1008](https://github.com/haskell-servant/servant/pull/1008)
  [#1009](https://github.com/haskell-servant/servant/pull/1009)

- *servant* NewlineFraming encodes newline after each element (i.e last)
  [#1079](https://github.com/haskell-servant/servant/pull/1079)
  [#1011](https://github.com/haskell-servant/servant/issues/1011)

- *servant* Add `lookupResponseHeader :: ... => Headers headers r -> ResponseHeader h a`
  [#1064](https://github.com/haskell-servant/servant/pull/1064)

- *servant-server* Add `MonadMask Handler`
  [#1068](https://github.com/haskell-servant/servant/pull/1068)

- *servant* Export `GetHeaders'`
  [#1052](https://github.com/haskell-servant/servant/pull/1052)

- *servant* Add `Bitraversable` and other `Bi-` instances for `:<|>`
  [#1032](https://github.com/haskell-servant/servant/pull/1032)

- *servant* Add `PutCreated` method type alias
  [#1024](https://github.com/haskell-servant/servant/pull/1024)

- *servant* Add `ToSourceIO (NonEmpty a)` instance
  [#988](https://github.com/haskell-servant/servant/pull/988)

0.14.1
------

- Merge in `servant-generic` (by [Patrick Chilton](https://github.com/chpatrick))
  into `servant` (`Servant.API.Generic`),
  `servant-client-code` (`Servant.Client.Generic`)
  and `servant-server` (`Servant.Server.Generic`).

- *servant-server* Deprecate `Servant.Utils.StaticUtils`, use `Servant.Server.StaticUtils`.

0.14
----

- `Stream` takes a status code argument

  ```diff
  -Stream method        framing ctype a
  +Stream method status framing ctype a
  ```

  ([#966](https://github.com/haskell-servant/servant/pull/966)
   [#972](https://github.com/haskell-servant/servant/pull/972))

- `ToStreamGenerator` definition changed, so it's possible to write an instance
  for conduits.

  ```diff
  -class ToStreamGenerator f a where
  -   toStreamGenerator :: f a -> StreamGenerator a
  +class ToStreamGenerator a b | a -> b where
  +   toStreamGenerator :: a -> StreamGenerator b
  ```

  ([#959](https://github.com/haskell-servant/servant/pull/959))

- Added `NoFraming` streaming strategy
  ([#959](https://github.com/haskell-servant/servant/pull/959))

- *servant-server* File serving in polymorphic monad.
  i.e. Generalised types of `serveDirectoryFileServer` etc functions in
  `Servant.Utils.StaticFiles`
  ([#953](https://github.com/haskell-servant/servant/pull/953))

- *servant-server* `ReqBody` content type check is recoverable.
  This allows writing APIs like:

  ```haskell
        ReqBody '[JSON] Int      :> Post '[PlainText] Int
  :<|>  ReqBody '[PlainText] Int :> Post '[PlainText] Int
  ```

  which is useful when handlers are subtly different,
  for example may do less work.
  ([#937](https://github.com/haskell-servant/servant/pull/937))


0.13.0.1
--------

- Support `base-compat-0.10`

0.13
----

- Streaming endpoint support.
  ([#836](https://github.com/haskell-servant/servant/pull/836))
- *servant* Add `Servant.API.Modifiers`
  ([#873](https://github.com/haskell-servant/servant/pull/873))

0.12
----

### Breaking changes

* Added `hoistServer` member to the `HasServer` class, which is `HasServer`
  specific `enter`.
  ([#804](https://github.com/haskell-servant/servant/pull/804))

0.11
----

### Breaking changes

* Changed `HasServer` instances for `Header` to throw 400 when parsing fails
  ([#724](https://github.com/haskell-servant/servant/pull/724))
* Added `headersD` block to `Delayed`
  ([#724](https://github.com/haskell-servant/servant/pull/724))

### Other changes

* Add `err418`, `err422` error codes
  ([#739](https://github.com/haskell-servant/servant/pull/739))

0.10
----

### Breaking changes

* `Handler` is now an abstract datatype. Migration hint: change `throwE` to `throwError`.
  ([#641](https://github.com/haskell-servant/servant/issues/641))

* Changed `HasServer` instances for `QueryParam` and `QueryParam` to throw 400
  when parsing fails
  ([#649](https://github.com/haskell-servant/servant/pull/649))

### Other changes

* Added `paramsD` block to `Delayed`

* Add `err422` Unprocessable Entity
  ([#646](https://github.com/haskell-servant/servant/pull/646))

* Deprecate `serveDirectory` and introduce `serveDirectoryFileServer`,
  `serveDirectoryWebApp`, `serveDirectoryWebAppLookup`, `serveDirectoryEmbedded`
  and `serveDirectoryWith` which offer 4 default options and a more flexible
  one for serving static files.
  ([#658](https://github.com/haskell-servant/servant/pull/658))

* `DelayedIO` is an instance of `MonadResource`, allowing safe resource handling.
  ([#622](https://github.com/haskell-servant/servant/pull/622)
  , [#674](https://github.com/haskell-servant/servant/pull/674)
  , [#675](https://github.com/haskell-servant/servant/pull/675))

0.7.1
------

* Remove module `Servant.Server.Internal.Enter` (https://github.com/haskell-servant/servant/pull/478)
* Support GHC 8.0

0.7
---

* The `Router` type has been changed. Static router tables should now
  be properly shared between requests, drastically increasing the
  number of situations where servers will be able to route requests
  efficiently. Functions `layout` and `layoutWithContext` have been
  added to visualize the router layout for debugging purposes. Test
  cases for expected router layouts have been added.
* If an endpoint is discovered to have a non-matching "accept header",
  this is now a recoverable rather than a fatal failure, allowing
  different endpoints for the same route, but with different content
  types to be specified modularly.
* Export `throwError` from module `Servant`
* Add `Handler` type synonym

0.6.1
-----

* If servers use the `BasicAuth` combinator and receive requests with missing or
  invalid credentials, the resulting error responses (401 and 403) could be
  overwritten by subsequent alternative routes. Now `BasicAuth` uses `FailFatal`
  and the error responses can't be overwritten anymore.

0.6
---

* Query parameters that can't be parsed result in a `400` (was `404`).

0.5
---

* Add `Config` machinery (https://github.com/haskell-servant/servant/pull/327).
  This is a breaking change, as the signatures of both `route`, `serve` and the
  typeclass `HasServer` now take an additional parameter.
* Support for the `HttpVersion`, `IsSecure`, `RemoteHost` and `Vault` combinators
* Drop `EitherT` in favor of `ExceptT`
* Use `http-api-data` instead of `Servant.Common.Text`
* Remove matrix params.
* Remove `RouteMismatch`.
* Redefined constructors of `RouteResult`.
* Added `Delayed` and related functions (`addMethodCheck`, `addAcceptCheck`, `addBodyCheck`, `runDelayed`)
* Added support for Basic Authentication
* Add generalized authentication support via the `AuthServerData` type family and `AuthHandler` handler

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
