[The latest version of this document is on GitHub.](https://github.com/haskell-servant/servant/blob/master/servant-foreign/CHANGELOG.md)
[Changelog for `servant` package contains significant entries for all core packages.](https://github.com/haskell-servant/servant/blob/master/servant/CHANGELOG.md)

0.15.4
------

### Significant changes

- Documentation improvements.

### Other changes

- Support GHC-9.0.1.
- Bump `lens` and `hspec` dependencies.

0.15.3
------

### Significant changes

- Support `Fragment` combinator.

0.15.2
------

* Support `servant-0.18`.

0.15.1
------

* Support `servant-0.17`

0.15
----

- *servant-foreign* Add support so `HasForeign` can be implemented for
  `MultipartForm` from [`servant-multipart`](http://hackage.haskell.org/package/servant-multipart)
  [#1035](https://github.com/haskell-servant/servant/pull/1035)

- Drop support for GHC older than 8.0
  [#1008](https://github.com/haskell-servant/servant/pull/1008)
  [#1009](https://github.com/haskell-servant/servant/pull/1009)


0.11.1
------

- Add missing `Semigroup` instances

0.11
----

### Breaking changes

- *servant* Add `Servant.API.Modifiers`
  ([#873](https://github.com/haskell-servant/servant/pull/873))
- Make foreign client Header arguments have the representation of 'Maybe' in those languages
  ([#843](https://github.com/haskell-servant/servant/pull/843))

0.10.2
------

### Changes

* Add instances for `Description` and `Summary` combinators
  ([#767](https://github.com/haskell-servant/servant/pull/767))
* Derive Data for all types
  ([#809](https://github.com/haskell-servant/servant/pull/809))

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
0.15
----

- *servant-foreign* Add support so `HasForeign` can be implemented for
  `MultipartForm` from [`servant-multipart`](http://hackage.haskell.org/package/servant-multipart)
  [#1035](https://github.com/haskell-servant/servant/pull/1035)

- Drop support for GHC older than 8.0
  [#1008](https://github.com/haskell-servant/servant/pull/1008)
  [#1009](https://github.com/haskell-servant/servant/pull/1009)


0.11.1
------

- Add missing `Semigroup` instances

0.11
----

### Breaking changes

- *servant* Add `Servant.API.Modifiers`
  ([#873](https://github.com/haskell-servant/servant/pull/873))
- Make foreign client Header arguments have the representation of 'Maybe' in those languages
  ([#843](https://github.com/haskell-servant/servant/pull/843))

0.10.2
------

### Changes

* Add instances for `Description` and `Summary` combinators
  ([#767](https://github.com/haskell-servant/servant/pull/767))
* Derive Data for all types
  ([#809](https://github.com/haskell-servant/servant/pull/809))

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
