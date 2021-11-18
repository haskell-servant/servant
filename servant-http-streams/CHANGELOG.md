  [The latest version of this document is on GitHub.](https://github.com/haskell-servant/servant/blob/master/servant-http-streams/CHANGELOG.md)
[Changelog for `servant` package contains significant entries for all core packages.](https://github.com/haskell-servant/servant/blob/master/servant/CHANGELOG.md)

0.18.3
------

### Other changes

- Support GHC-9.0.1.
- Fix test suite running in CI.
- Bump `bytestring` and `hspec` dependencies.

0.18.2
------

### Significant changes

- Support `servant-client-core` 0.18.2.

0.18.1
------

### Significant changes

- Union verbs

### Other changes

- Bump "tested-with" ghc versions

0.18
----

### Significant changes

- Support for ghc8.8 (#1318, #1326, #1327)


0.17
----

### Significant changes

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

### Other changes

- *servant-client* *servant-client-core* *servant-http-streams* Fix Verb with headers checking content type differently [#1200](https://github.com/haskell-servant/servant/issues/1200) [#1204](https://github.com/haskell-servant/servant/pull/1204)

  For `Verb`s with response `Headers`, the implementation didn't check
  for the content-type of the response. Now it does.

- *servant-client* *servant-http-streams* `HasClient` instance for `Stream` with `Headers` [#1170](https://github.com/haskell-servant/servant/issues/1170) [#1197](https://github.com/haskell-servant/servant/pull/1197)
- *servant-client* Redact the authorization header in Show and exceptions [#1238](https://github.com/haskell-servant/servant/pull/1238)



0.16.0.1
--------

- Allow `base-compat-0.11`

0.16
----

- Initial release
