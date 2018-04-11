[The latest version of this document is on GitHub.](https://github.com/haskell-servant/servant/blob/master/servant/CHANGELOG.md)

0.13.0.1
--------

- Support `base-compat-0.10`

0.13
----

### Significant changes

- Streaming endpoint support.
  ([#836](https://github.com/haskell-servant/servant/pull/836))

  ```haskell
  type StreamApi f = "streamGetNewline" :> StreamGet NewlineFraming JSON (f Person)
  ```

  See tutorial for more details
  - [A web API as a type - StreamGet and StreamPost](http://haskell-servant.readthedocs.io/en/release-0.13/tutorial/ApiType.html#streamget-and-streampost)
  - [Serving an API - streaming endpoints](http://haskell-servant.readthedocs.io/en/release-0.13/tutorial/Server.html#streaming-endpoints)
  - [Querying an API - Querying Streaming APIs](http://haskell-servant.readthedocs.io/en/release-0.13/tutorial/Client.html#querying-streaming-apis)

- *servant* Add `Servant.API.Modifiers`
  ([#873](https://github.com/haskell-servant/servant/pull/873)
   [#903](https://github.com/haskell-servant/servant/pull/903))

  `QueryParam`, `Header` and `ReqBody` understand modifiers:
  - `Required` or `Optional` (resulting in `a` or `Maybe a` in handlers)
  - `Strict` or `Lenient` (resulting in `a` or `Either String a` in handlers)

  Also you can use `Description` as a modifier, but it doesn't yet work
  with `servant-docs`, only `servant-swagger`. [There is an issue.](https://github.com/haskell-servant/servant/issues/902)

- *servant-client* Support `http-client`’s `CookieJar`
  ([#897](https://github.com/haskell-servant/servant/pull/897)
   [#883](https://github.com/haskell-servant/servant/pull/883))

  `ClientM` preserves cookies between requests,
  if given initial `CookieJar`.
  To migrate from older code, change `ClientEnv` constructor
  to `mkClientEnv` which makes `ClientEnv` without `CookieJar`.

- *servant* Mono-kind-ise modifiers, resulting in better error messages.
  ([#887](https://github.com/haskell-servant/servant/issues/887)
   [#890](https://github.com/haskell-servant/servant/pull/890))

- *servant* Add `TypeError ... => HasServer`s instances in GHC-8.2 for
  not saturated modifiers (`Capture "foo" :> ...`) or `->` in place of `:>`.
  ([#893](https://github.com/haskell-servant/servant/pull/893))

- *Cookbook* example projects at
  http://haskell-servant.readthedocs.io/en/master/cookbook/index.html
  ([#867](https://github.com/haskell-servant/servant/pull/867)
   [#892](https://github.com/haskell-servant/servant/pull/882))

- *Experimental work* `servant-client-ghcjs`
  ([#818](https://github.com/haskell-servant/servant/pull/818)
   [#869](https://github.com/haskell-servant/servant/pull/869))

### Other changes

- *servant* Links aren't double escaped
  ([#878](https://github.com/haskell-servant/servant/pull/878))

- Dependency updates
  ([#900](https://github.com/haskell-servant/servant/pull/900)
   [#898](https://github.com/haskell-servant/servant/pull/898)
   [#895](https://github.com/haskell-servant/servant/pull/895)
   [#872](https://github.com/haskell-servant/servant/pull/872))

- Documentation updates
  ([#875](https://github.com/haskell-servant/servant/pull/875)
   [#861](https://github.com/haskell-servant/servant/pull/861))

- Refactorings
  ([#899](https://github.com/haskell-servant/servant/pull/899)
   [#896](https://github.com/haskell-servant/servant/pull/896)
   [#889](https://github.com/haskell-servant/servant/pull/889)
   [#891](https://github.com/haskell-servant/servant/pull/891)
   [#892](https://github.com/haskell-servant/servant/pull/892)
   [#885](https://github.com/haskell-servant/servant/pull/885))

0.12.1
------

### Bug fixes

- Prevent double-escaping in link segments
  ([#835](https://github.com/haskell-servant/servant/issues/835)
   [#878](https://github.com/haskell-servant/servant/pull/878))

0.12
---

### Significant changes

- *servant-client* *servant-client-core*
  Factored out of `servant-client` all the functionality that was
  independent of the `http-client` backend.
  ([#803](https://github.com/haskell-servant/servant/pull/803)
   [#821](https://github.com/haskell-servant/servant/issues/821))

  If you have own combinators, you'll need to add an additional `m` argument
  in `HasClient`, `Client` and `clientWithRoute`:

  ```diff
  -class HasClient api
  -  type Client (api :: *) :: *
  -  clientWithRoute :: Proxy api -> Req -> Client api
  +class HasClient m api
  +  type Client (m :: * -> *) (api :: *) :: *
  +  clientWithRoute :: Proxy m -> Proxy api -> Request -> Client m api
  ```

  See https://github.com/haskell-servant/servant-auth/pull/67/commits/f777818e3cc0fa3ed2346baff8328e96d62b1790 for a real world example.

- *servant-server* Added `hoistServer` member to the `HasServer` class, which is `HasServer`
  specific `enter`.
  ([#804](https://github.com/haskell-servant/servant/pull/804)
   [#824](https://github.com/haskell-servant/servant/pull/824))

  `enter` isn't exported from `Servant` module anymore. You can change
  `enter` to `hoistServer` in a straight forward way.
  Unwrap natural transformation and add a api type `Proxy`:

  ```diff
  -server = enter (NT nt) impl
  +server = hoistServer (Proxy :: Proxy MyApi) nt impl
  ```

  If you have own combinators, you'll need to define a new method of
  `HasServer` class, for example:

  ```haskell
  type ServerT (MyCombinator :> api) m = MyValue -> ServerT api m
  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s
  ```

  See https://github.com/haskell-servant/servant-auth/pull/67/commits/8ee3b6315247ac076516213fd7cfcdbfdb583ac9 for a real world example.

- Add `Description` and `Summary` combinators
  ([#767](https://github.com/haskell-servant/servant/pull/767))

  It's possible to annotate endpoints with free form text.
  This information is used by e.g. by `servant-swagger`, see screenshot in
  https://github.com/phadej/servant-swagger-ui

- Lower `:>` and `:<|>` infix precedence to 4 and 3 respectively
  ([#761](https://github.com/haskell-servant/servant/issues/761))

  This shouldn't affect you, except if you define your own infix operators
  for Servant type-level DSL.

### Other changes

- *servant-foreign* Derive `Data` for all types
  ([#809](https://github.com/haskell-servant/servant/pull/809))
- *servant-docs* Add authentication lenses
  ([#787](https://github.com/haskell-servant/servant/pull/787))
- *servant-docs* Generated markdown improvements
  ([#813](https://github.com/haskell-servant/servant/pull/787)
   [#767](https://github.com/haskell-servant/servant/pull/767)
   [#790](https://github.com/haskell-servant/servant/pull/790)
   [#788](https://github.com/haskell-servant/servant/pull/788))
- Add `addLinks` to generate all links for unnested APIs.
  ([#851](https://github.com/haskell-servant/servant/pull/851))
- Allow newest dependencies
 ([#772](https://github.com/haskell-servant/servant/pull/772)
  [#842](https://github.com/haskell-servant/servant/pull/842))
- Documentation improvements and typo fixes
 ([#757](https://github.com/haskell-servant/servant/pull/757)
  [#771](https://github.com/haskell-servant/servant/pull/771)
  [#775](https://github.com/haskell-servant/servant/pull/775)
  [#790](https://github.com/haskell-servant/servant/pull/790)
  [#791](https://github.com/haskell-servant/servant/pull/791)
  [#806](https://github.com/haskell-servant/servant/pull/806))
- Development process improvements
  ([#764](https://github.com/haskell-servant/servant/pull/764)
   [#839](https://github.com/haskell-servant/servant/pull/839))

0.11
----

### Breaking changes

- `Enter` refactored
  ([#734](https://github.com/haskell-servant/servant/issues/734)
  , [#736](https://github.com/haskell-servant/servant/pull/736))

### Other changes

- Add a type representing an empty API
  ([#753](https://github.com/haskell-servant/servant/pull/753))
- Add `linkURI'` and `Link` accessors
  ([#745](https://github.com/haskell-servant/servant/pull/745)
  , [#717](https://github.com/haskell-servant/servant/pull/717)
  , [#715](https://github.com/haskell-servant/servant/issues/715))
- Prepare for GHC-8.2
  ([#722](https://github.com/haskell-servant/servant/pull/722))
- Add `HasLink AuthProtect` instance
  ([#720](https://github.com/haskell-servant/servant/pull/720))
- `AllCTRender [] ()` `TypeError` (use `NoContent`)
  ([#671](https://github.com/haskell-servant/servant/pull/671))
- Documentation improvements and typo fixes
  ([#702](https://github.com/haskell-servant/servant/pull/702)
  , [#709](https://github.com/haskell-servant/servant/pull/709)
  , [#716](https://github.com/haskell-servant/servant/pull/716)
  , [#725](https://github.com/haskell-servant/servant/pull/725)
  , [#727](https://github.com/haskell-servant/servant/pull/727))

0.10
----

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
