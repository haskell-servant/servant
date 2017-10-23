0.10.2
------

### Changes

* Add support for body/return content types in `Req`
  ([#832](https://github.com/haskell-servant/servant/pull/832/files)).

0.10.1
------

### Changes

* Don't drop samples in `HasDocs ReqBody` instance
  ([#755](https://github.com/haskell-servant/servant/pull/755/files)).
  *Breaking change in an `Internal` module*.

0.10
----

### Breaking changes

* Do not apply JavaScript specific mangling to the names.
  ([#191](https://github.com/haskell-servant/servant/issues/191))

0.7.1
-----

* Support GHC 8.0

0.5
-----
* Use the `text` package instead of `String`.
* Extract javascript-oblivious types and helpers to *servant-foreign*
* Typed-languages support
