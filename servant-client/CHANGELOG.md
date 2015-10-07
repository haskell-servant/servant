HEAD
----
* Update `instance HasClient Raw` for servant Raw changes
* Support for the `HttpVersion`, `IsSecure`, `RemoteHost` and `Vault` combinators
* Added support for `path` on `BaseUrl`.
* `client` now takes an explicit `Manager` argument.

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
