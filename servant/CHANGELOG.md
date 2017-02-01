0.10
------

### Breaking changes

* Use `NT` from `natural-transformation` for `Enter`
  ([#616](https://github.com/haskell-servant/servant/issues/616))

* Change to `MkLink (Verb ...) = Link` (previously `URI`). To consume `Link`
  use its `ToHttpApiData` instance or `linkURI`.
  ([#527](https://github.com/haskell-servant/servant/issues/527))

### Other changes

* Add `Servant.API.TypeLevel` module with type families to work with API types.
  ([#345](https://github.com/haskell-servant/servant/pull/345)
  , [#305](https://github.com/haskell-servant/servant/issues/305))

* Default JSON content type change to `application/json;charset=utf-8`.
  ([#263](https://github.com/haskell-servant/servant/issues/263))
  Related browser bugs:
  [Chromium](https://bugs.chromium.org/p/chromium/issues/detail?id=438464) and
  [Firefox](https://bugzilla.mozilla.org/show_bug.cgi?id=918742)

* `Accept` class may accept multiple content-types. `MimeUnrender` adopted as well.
  ([#613](https://github.com/haskell-servant/servant/pull/614)
  , [#615](https://github.com/haskell-servant/servant/pull/615))

0.9.1
------

* Added 'noHeader' function for *not* adding response headers.

0.9
---

* Added Eq, Show, Read, Generic and Ord instances to IsSecure
* BACKWARDS INCOMPATIBLE: replace use of `ToFromByteString` with `To/FromHttpApiData` for `GetHeaders/BuildHeadersTo`
* BACKWARDS INCOMPATIBLE: Moved `From/ToFormUrlEncoded` classes, which were renamed to `From/ToForm` to `http-api-data`

0.8.1
----

* Add `CaptureAll` combinator. Captures all of the remaining segments in a URL.
* Add `Servant.API.TypeLevel` module, with frequently used type-level
functionaliy.

0.8
---

* Minor fixes, documentation changes and cabal tweaks

0.7.1
-----

* Add module `Servant.Utils.Enter` (https://github.com/haskell-servant/servant/pull/478)
* Allow to set the same header multiple times in responses.

0.5
---

* Add `WithNamedConfig` combinator.
* Add `HttpVersion`, `IsSecure`, `RemoteHost` and `Vault` combinators
* Fix safeLink, so Header is not in fact required.
* Add more instances for (:<|>)
* Use `http-api-data` instead of `Servant.Common.Text`
* Remove matrix params.
* Add PlainText String MimeRender and MimeUnrender instances.
* Add new `Verbs` combinator, and make all existing and new verb combinators
type synonyms of it.
* Add `BasicAuth` combinator to support Basic authentication
* Add generalized authentication support

0.4.2
-----
* Fix missing cases for `Patch` in `safeLink`

0.4.1
-----
* Allow whitespace after parsing JSON
* Stricter matching for `safeLink` for `Capture`

0.4
---
* `Delete` now is like `Get`, `Post`, `Put`, and `Patch` and returns a response body
* Multiple content-type/accept support for all the relevant combinators
* Provide *JSON*, *PlainText*, *OctetStream* and *FormUrlEncoded* content types out of the box
* Type-safe link generation to API endpoints
* Support for the PATCH HTTP method
* Removed the home-made QuasiQuote for writing API types in a more human-friendly format until we come up with a better design for it
* Make most if not all of the haddock code examples run through doctest
* Some general code cleanup
* Add response headers
