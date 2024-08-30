0.16.1
------

- Compatibility with newer dependencies, and newers GHCs.

0.16
----

- Compatibility with mtl-2.3, used with GHC 9.6.

0.15.3
------

### Other changes

- Support GHC-9.0.1.
- Bump `bytestring` dependency.

0.15.2
------

0.15.1
------

- `FromSourceIO Proxy` (`ListT`) instance `fail`s in `IO`, not in `m`.
  This works around `MonadFail` proposal, and makes `servant-pipes`
  behave like `servant-conduit` and `servant-machines`.

0.15
----

- First release with support for `servant-0.15` `Stream` refactoring.
