[The latest version of this document is on GitHub.](https://github.com/haskell-servant/servant/blob/master/servant-docs/CHANGELOG.md)
[Changelog for `servant` package contains significant entries for all core packages.](https://github.com/haskell-servant/servant/blob/master/servant/CHANGELOG.md)

0.11.2
------

* Allow `servant-0.13`:
  - Doesn't have instances for streaming.
  - Servant.API.Modifiers extra information isn't used.

0.11.1
------

* Export `DocAuthentication` and related lenses.
* Make `defAction`'s documentation visible in Haddock documentation.
* Add a markdown header for the Headers an endpoint is sensitive to.
* Document the HTTP Method the parameters of an endpoint belong to
  (rather than assuming `GET` for all of them).
* Content type of sample response body is also displayed.
* Can now customise various aspects of how the document is produced
  using `markdownWith` and `RenderingOptions`:
    - How many content-types for each example are shown
    - Whether notes should be grouped together under their own header.

0.11
----

* changed the type of `rqbody`.

0.10
----

There are no changes. Released as a part of `servant` suite.

0.7.1
-----

* Support GHC 8.0

0.7
---

* Use `throwError` instead of `throwE` in documentation

0.5
----

* Support for the `HttpVersion`, `IsSecure`, `RemoteHost` and `Vault` combinators
* Support maximum samples setting with new `DocOptions` type (used by `docsWithOptions` and `docsWith`)
* Remove redundant second parameter of ToSample
* Add Generic-based default implementation for `ToSample` class
* Add more `ToSamples` instances: `Bool`, `Ordering`, tuples (up to 7), `[]`, `Maybe`, `Either`, `Const`, `ZipList` and some monoids
* Move `toSample` out of `ToSample` class
* Add a few helper functions to define `toSamples`
* Remove matrix params.
* Added support for Basic authentication

0.4
---
* `Delete` now is like `Get`, `Post`, `Put`, and `Patch` and returns a response body
* Allow for extra information to be added to the docs
* Support content-type aware combinators of *servant-0.4*
* Render endpoints in a canonical order (https://github.com/haskell-servant/servant-docs/pull/15)
* Remove ToJSON superclass from ToSample
* Split out Internal module
* Add support for response headers
* Allow `ToSample` to return a different type than it's arguments
* Add Proxy argument to `ToSample`

0.3
---

* Add the ability to display multiple responses, with some accompanying `Text` to describe the context in which we get the corresponding JSON.
* Expose the `headers` lens
* Represent an endpoint's path as `[String]` (previously `String`), fixing a corner case where the leading `/` would be missing.
