1.1.9
-------

* Support `servant-0.18`

1.1.8
-------

* Support `servant-0.17`

1.1.7.1
-------

* Support `swagger2-2.4`

1.1.7
-----

* Support servant-0.15
   - support for 'Stream' and 'StreamBody' combinators
   - orphan 'ToSchema (SourceT m a)' instance
* Fix BodyTypes to work with generalized ReqBody' 
  [#88](https://github.com/haskell-servant/servant-swagger/pull/88)

1.1.6
-----

* Fixes:
  * `validateEveryToJSON` now prints validation errors

* Notes:
  * GHC-8.6 compatible release

1.1.5
-----

* Notes:
    * `servant-0.13` compatible release
    * Drops compatibility with previous `servant` versions.

1.1.4
-----

* Notes:
    * `servant-0.12` compatible release

1.1.3.1
---

* Notes:
   * GHC-8.2 compatible release

1.1.3
---

* Notes:
   * `servant-0.11` compatible release

1.1.2.1
---

* Notes:
   * `servant-0.10` compatible release

1.1.2
---

* Minor fixes:
  * Support for aeson-1, insert-ordered-containers-0.2
  * CaptureAll instance

1.1.1
---

* Minor fixes:
  * Fix `unused-imports` and `unused-foralls` warnings;
  * Fix tests to match `swagger2-2.1.1` (add `example` property for `UTCTime` schema).

1.1
---

* Breaking changes:
    * Requires `swagger2 >= 2.1`
    * Requires `servant >= 0.5`

* Notes:
    * GHC-8.0 compatible release

1.0.3
---

* Fixes:
    * Improve compile-time performance of `BodyTypes` even further (see [18e0d95](https://github.com/haskell-servant/servant-swagger/commit/18e0d95ef6fe9076dd9621cb515d8d1a189f71d3))!

1.0.2
---

* Minor changes:
    * Add GHC 7.8 support (see [#26](https://github.com/haskell-servant/servant-swagger/pull/26)).

* Fixes:
    * Improve compile-time performance of `BodyTypes` (see [#25](https://github.com/haskell-servant/servant-swagger/issues/25)).

1.0.1
---

* Fixes:
    * Stop using `Data.Swagger.Internal`;
    * Documentation fixes (links to examples).

1.0
---

* Major changes (see [#24](https://github.com/haskell-servant/servant-swagger/pull/24)):
    * Switch to `swagger2-2.*`;
    * Add automatic `ToJSON`/`ToSchema` validation tests;
    * Add great documentation;
    * Export some type-level functions for servant API.

* Minor changes:
    * Rework Todo API example;
    * Stop exporting `ToResponseHeader`, `AllAccept` and `AllToResponseHeader` (see [bd50db4](https://github.com/haskell-servant/servant-swagger/commit/bd50db48ca6a106e4366560ded70932d409de1e2));
    * Change maintainer, update authors/copyrights (see [1a62681](https://github.com/haskell-servant/servant-swagger/commit/1a6268101dc826a92c42e832e402e251c0d32147));
    * Include changelog and example files into `extra-source-files`.

0.1.2
---

* Fixes:
    * Fix default spec for `ReqBody` param to be required (see [#22](https://github.com/haskell-servant/servant-swagger/issues/22));
    * Set version bounds for `swagger2`.

0.1.1
---

* Fixes:
    * Fix `subOperations` to filter endpoints also by method (see [#18](https://github.com/haskell-servant/servant-swagger/issues/18));
    * Fix response schema in `ToSwagger` instance for `Header` (see [b59e557](https://github.com/haskell-servant/servant-swagger/commit/b59e557a05bc2669332c52b397879e7598747b82)).

0.1
---
* Major changes
    * Use `swagger2` for data model (see [#9](https://github.com/dmjio/servant-swagger/pull/9)); this changes almost everything.
