[The latest version of this document is on GitHub.](https://github.com/haskell-servant/servant/blob/master/servant-docs/CHANGELOG.md)
[Changelog for `servant` package contains significant entries for all core packages.](https://github.com/haskell-servant/servant/blob/master/servant/CHANGELOG.md)

0.11.9
------

### Significant changes

- Use Capture Description if available (#1423).

### Other changes

- Support GHC-9.0.1.
- Bump `bytestring` and `lens` dependencies.

0.11.8
------

### Significant changes

- Support `Fragment` combinator.

0.11.7
------

### Significant changes

- Add instance for ToSample NonEmpty

### Other changes

- Bump "tested-with" ghc versions
- Fix servant-docs code sample in README

0.11.5
----


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

- *servant-docs* Merge documentation from duplicate routes [#1240](https://github.com/haskell-servant/servant/issues/1240) [#1241](https://github.com/haskell-servant/servant/pull/1241)

  Servant supports defining the same route multiple times with different
  content-types and result-types, but servant-docs was only documenting
  the first of copy of such duplicated routes. It now combines the
  documentation from all the copies.

  Unfortunately, it is not yet possible for the documentation to specify
  multiple status codes.

- *servant-docs* Prevent race-conditions in testing [#1194](https://github.com/haskell-servant/servant/pull/1194)

0.11.4
------

- Drop dependency on `control-monad-omega` in favor of `Data.Universe.Helpers` from `universe-base`.

0.11.3
------

- Support `servant-0.15`
   - Instances for 'Stream' and 'StreamBody'

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
