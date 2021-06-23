[The latest version of this document is on GitHub.](https://github.com/haskell-servant/servant/blob/master/servant-client/CHANGELOG.md)
[Changelog for `servant` package contains significant entries for all core packages.](https://github.com/haskell-servant/servant/blob/master/servant/CHANGELOG.md)

0.18.3
------

### Significant changes

- Add response header support to UVerb (#1420)

### Other changes

- Support GHC-9.0.1.
- Bump `bytestring`, `hspec`, `http-client` and `QuickCheck` dependencies.

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

0.18
----

### Significant changes

- Support for ghc8.8 (#1318, #1326, #1327)


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

- *servant-client* Added a function to create Client.Request in ClientEnv [#1213](https://github.com/haskell-servant/servant/pull/1213) [#1255](https://github.com/haskell-servant/servant/pull/1255)

  The new member `makeClientRequest` of `ClientEnv` is used to create
  `http-client` `Request` from `servant-client-core` `Request`.
  This functionality can be used for example to set
  dynamic timeouts for each request.

### Other changes

- *servant-client* *servant-client-core* *servant-http-streams* Fix Verb with headers checking content type differently [#1200](https://github.com/haskell-servant/servant/issues/1200) [#1204](https://github.com/haskell-servant/servant/pull/1204)

  For `Verb`s with response `Headers`, the implementation didn't check
  for the content-type of the response. Now it does.

- *servant-client* *servant-http-streams* `HasClient` instance for `Stream` with `Headers` [#1170](https://github.com/haskell-servant/servant/issues/1170) [#1197](https://github.com/haskell-servant/servant/pull/1197)
- *servant-client* Redact the authorization header in Show and exceptions [#1238](https://github.com/haskell-servant/servant/pull/1238)



0.16.0.1
--------

- Allow `base-compat-0.11`

0.16
----

- Rename `ServantError` to `ClientError`, `ServantErr` to `ServerError`
  [#1131](https://github.com/haskell-servant/servant/pull/1131)

- *servant-client-core* Rearrange modules. No more `Internal` modules, whole
  API is versioned.
  [#1130](https://github.com/haskell-servant/servant/pull/1130)

- *servant-client-core* `RequestBody` is now

    ```haskell
    = RequestBodyLBS LBS.ByteString
    | RequestBodyBS BS.ByteString
    | RequestBodySource (SourceIO LBS.ByteString)
    ```

  i.e. no more replicates `http-client`s API.
  [#1117](https://github.com/haskell-servant/servant/pull/1117)

- *servant-client-core* Keep structured exceptions in `ConnectionError`
  constructor of `ClientError`
  [#1115](https://github.com/haskell-servant/servant/pull/1115)

    ```diff
    -| ConnectionError Text
    +| ConnectionError SomeException
    ```

- *servant-client-core* Preserve failing request in `FailureResponse`
  constructor of `ClientError`
  [#1114](https://github.com/haskell-servant/servant/pull/1114)

    ```diff
    -FailureResponse Response
    +-- | The server returned an error response including the
    +-- failing request. 'requestPath' includes the 'BaseUrl' and the
    +-- path of the request.
    +FailureResponse (RequestF () (BaseUrl, BS.ByteString)) Response
    ```

- *servant-client* Fix (implement) `StreamBody` instance
  [#1110](https://github.com/haskell-servant/servant/pull/1110)

- *servant-client* Update CookieJar with intermediate request/responses (redirects)
  [#1104](https://github.com/haskell-servant/servant/pull/1104)

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

- *servant-client* Separate streaming client.
  [#1066](https://github.com/haskell-servant/servant/pull/1066)

  We now have two `http-client` based clients,
  in `Servant.Client` and `Servant.Client.Streaming`.

  Their API is the same, except for
  - `Servant.Client` **cannot** request `Stream` endpoints.
  - `Servant.Client` is *run* by direct
    `runClientM :: ClientM a -> ClientEnv -> IO (Either ServantError a)`
  - `Servant.Client.Streaming` **can** request `Stream` endpoints.
  - `Servant.Client.Streaming` is *used* by CPSised
    `withClientM :: ClientM a -> ClientEnv -> (Either ServantError a -> IO b) -> IO b`

  To access `Stream` endpoints use `Servant.Client.Streaming` with
  `withClientM`; otherwise you can continue using `Servant.Client` with `runClientM`.
  You can use both too, `ClientEnv` and `BaseUrl` types are same for both.

  **Note:** `Servant.Client.Streaming` doesn't *stream* non-`Stream` endpoints.
  Requesting ordinary `Verb` endpoints (e.g. `Get`) will block until
  the whole response is received.

  There is `Servant.Client.Streaming.runClientM` function, but it has
  restricted type. `NFData a` constraint prevents using it with
  `SourceT`, `Conduit` etc. response types.

  ```haskell
  runClientM :: NFData a => ClientM a -> ClientEnv -> IO (Either ServantError a)
  ```

  This change shouldn't affect you, if you don't use streaming endpoints.

- Drop support for GHC older than 8.0
  [#1008](https://github.com/haskell-servant/servant/pull/1008)
  [#1009](https://github.com/haskell-servant/servant/pull/1009)

- *servant-client-core* Add `NFData (GenResponse a)` and `NFData ServantError` instances.
  [#1076](https://github.com/haskell-servant/servant/pull/1076)

 *servant-client-core* Add `aeson` and `Lift BaseUrl` instances
  [#1037](https://github.com/haskell-servant/servant/pull/1037)

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

- *servant-client-core* Add `hoistClient` to `HasClient`.
  Just like `hoistServer` allows us to change the monad in which request handlers
  of a web application live, we also have `hoistClient` for changing the monad
  in which *client functions* live.
  Read [tutorial section for more information](https://docs.servant.dev/en/release-0.14/tutorial/Client.html#changing-the-monad-the-client-functions-live-in).
  ([#936](https://github.com/haskell-servant/servant/pull/936))

- *servant-client* Add more constructors to `RequestBody`, including
  `RequestBodyStream`.
  *Note:* we are looking for http-library agnostic API,
  so the might change again soon.
  Tell us which constructors are useful for you!
  ([#913](https://github.com/haskell-servant/servant/pull/913))

0.13.0.1
--------

- Support `base-compat-0.10`

0.13
----

- Streaming endpoint support.
  ([#836](https://github.com/haskell-servant/servant/pull/836))
- *servant* Add `Servant.API.Modifiers`
  ([#873](https://github.com/haskell-servant/servant/pull/873))
- *servant-client* Support `http-client`’s `CookieJar`
  ([#897](https://github.com/haskell-servant/servant/pull/897)
   [#883](https://github.com/haskell-servant/servant/pull/883))

0.12.0.1
--------

- Send `Accept` header.
  ([#858](https://github.com/haskell-servant/servant/issues/858))

0.12
----

- Factored out into `servant-client-core` all the functionality that was
  independent of the `http-client` backend.

0.11
----

### Other changes

- Path components are escaped
  ([#696](https://github.com/haskell-servant/servant/pull/696))
- `Req` `reqPath` field changed from `String` to `BS.Builder`
  ([#696](https://github.com/haskell-servant/servant/pull/696))
- Include `Req` in failure errors
  ([#740](https://github.com/haskell-servant/servant/pull/740))

0.10
-----

### Breaking changes

There shouldn't be breaking changes. Released as a part of `servant` suite.

### Other changes

* Add MonadBase and MonadBaseControl instances for ClientM
  ([#663](https://github.com/haskell-servant/servant/issues/663))

* client asks for any content-type in Accept contentTypes non-empty list
  ([#615](https://github.com/haskell-servant/servant/pull/615))

* Add `ClientLike` class that matches client functions generated using `client`
  with client data structure.
  ([#640](https://github.com/haskell-servant/servant/pull/640))

* Allow direct use of 'RequestBody'
  ([#661](https://github.com/haskell-servant/servant/pull/661))

0.9.1.1
-------

* Add MonadThrow and MonadCatch instances for ClientM

0.9
---

* BACKWARDS INCOMPATIBLE: `client` now returns a ClientM which is a Reader for
  BasicEnv. BasicEnv comprises the HttpManager and BaseUrl that have had to be
  passed to each method returned by `client`.

0.7.1
-----

* Support GHC 8.0
* `ServantError` has an `Eq` instance now.

0.6
---

* `client` no longer takes `BaseUrl` and `Manager` arguments. Instead, each function returned by `client` requires these two arguments.

0.5
---

* Use the `text` package instead of `String`.
* Support for the `HttpVersion`, `IsSecure`, `RemoteHost` and `Vault` combinators
* Added support for `path` on `BaseUrl`.
* `client` now takes an explicit `Manager` argument.
* Use `http-api-data` instead of `Servant.Common.Text`
* Client functions now consider any 2xx successful.
* Remove matrix params.
* Added support for Basic authentication
* Add generalized authentication support via the `AuthClientData` type family and
  `AuthenticateReq` data type

0.4.1
-----
* The `HasClient` instance for `Delete cts ()` now does not care at all about content types provided.

0.4
---
* `Delete` now is like `Get`, `Post`, `Put`, and `Patch` and returns a response body
* Support content-type aware combinators and `Accept`/`Content-type` headers
* Added a lot of tests
* Support multiple concurrent threads
* Use `ServantError` to report Errors instead of `String`
* Make the clients for `Raw` endpoints return the whole `Response` value (to be able to access response headers for example)
* Support for PATCH
* Make () instances expect No Content status code, and not try to decode body.
* Add support for response headers

0.2.2
-----
* Add TLS support
* Add matrix parameter support
