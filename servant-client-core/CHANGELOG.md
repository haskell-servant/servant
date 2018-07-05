[The latest version of this document is on GitHub.](https://github.com/haskell-servant/servant/blob/master/servant-client-core/CHANGELOG.md)
[Changelog for `servant` package contains significant entries for all core packages.](https://github.com/haskell-servant/servant/blob/master/servant/CHANGELOG.md)

0.14.1
------

- Merge in `servant-generic` (by [Patrick Chilton](https://github.com/chpatrick))
  into `servant` (`Servant.API.Generic`),
  `servant-client-code` (`Servant.Client.Generic`)
  and `servant-server` (`Servant.Server.Generic`).

0.14
----

- `Stream` takes a status code argument

  ```diff
  -Stream method        framing ctype a
  +Stream method status framing ctype a
  ```

  ([#966](https://github.com/haskell-servant/servant/pull/966)
   [#972](https://github.com/haskell-servant/servant/pull/972))

- `ToStreamGenerator` definition changed, so it's possible to write an instance
  for conduits.

  ```diff
  -class ToStreamGenerator f a where
  -   toStreamGenerator :: f a -> StreamGenerator a
  +class ToStreamGenerator a b | a -> b where
  +   toStreamGenerator :: a -> StreamGenerator b
  ```

  ([#959](https://github.com/haskell-servant/servant/pull/959))

- Added `NoFraming` streaming strategy
  ([#959](https://github.com/haskell-servant/servant/pull/959))

- *servant-client-core* Free `Client` implementation.
  Useful for testing `HasClient` instances.
  ([#920](https://github.com/haskell-servant/servant/pull/920))

- *servant-client-core* Add `hoistClient` to `HasClient`.
  Just like `hoistServer` allows us to change the monad in which request handlers
  of a web application live in, we also have `hoistClient` for changing the monad
  in which *client functions* live.
  Read [tutorial section for more information](https://haskell-servant.readthedocs.io/en/release-0.14/tutorial/Client.html#changing-the-monad-the-client-functions-live-in).
  ([#936](https://github.com/haskell-servant/servant/pull/936))

  iF you have own combinators, you'll need to define a new method of
  `HasClient` class, for example:

  ```haskell
  type Client m (MyCombinator :> api) = MyValue :> Client m api
  hoistClientMonad pm _ nt cl = hoistClientMonad pm (Proxy :: Proxy api) nt . cl
  ```

0.13.0.1
--------

- Support `base-compat-0.10`

0.13
----

- Streaming endpoint support.
  ([#836](https://github.com/haskell-servant/servant/pull/836))
- *servant* Add `Servant.API.Modifiers`
  ([#873](https://github.com/haskell-servant/servant/pull/873))

0.12
----

- First version. Factored out of `servant-client` all the functionality that was
  independent of the `http-client` backend.
