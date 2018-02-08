[The latest version of this document is on GitHub.](https://github.com/haskell-servant/servant/blob/master/servant-client/CHANGELOG.md)
[Changelog for `servant` package contains significant entries for all core packages.](https://github.com/haskell-servant/servant/blob/master/servant/CHANGELOG.md)

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
