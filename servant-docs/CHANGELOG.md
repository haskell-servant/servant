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
