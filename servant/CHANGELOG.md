HEAD
----
* Change Raw from `data Raw ...` to `newtype Raw (m :: * -> *) a = Raw ... a`
* Add `HttpVersion`, `IsSecure`, `RemoteHost` and `Vault` combinators
* Fix safeLink, so Header is not in fact required.
* Added more instances for (:<|>)

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
