[The latest version of this document is on GitHub.](https://github.com/haskell-servant/servant/blob/master/servant-client-core/CHANGELOG.md)
[Changelog for `servant` package contains significant entries for all core packages.](https://github.com/haskell-servant/servant/blob/master/servant/CHANGELOG.md)

0.15.1
------

- `RequestF` is now parametrized on the request body type as well as the path
  type.

- `ServantError`'s `FailureResponse` constructor now carries the `Request` that
  caused the failure.


0.15
----

- Streaming refactoring.
  [#991](https://github.com/haskell-servant/servant/pull/991)
  [#1076](https://github.com/haskell-servant/servant/pull/1076)
  [#1077](https://github.com/haskell-servant/servant/pull/1077)

  The streaming functionality (`Servant.API.Stream`) is refactored to use
  `servant`'s own `SourceIO` type (see `Servant.Types.SourceT` documentation),
  which replaces both `StreamGenerator` and `ResultStream` types.

  New conversion type-classes are `ToSourceIO` and `FromSourceIO`
  (replacing `ToStreamGenerator` and `BuildFromStream`).
  There are instances for *conduit*, *pipes* and *machines* in new packages:
  [servant-conduit](https://hackage.haskell.org/package/servant-conduit)
  [servant-pipes](https://hackage.haskell.org/package/servant-pipes) and
  [servant-machines](https://hackage.haskell.org/package/servant-machines)
  respectively.

  Writing new framing strategies is simpler. Check existing strategies for examples.

  This change shouldn't affect you, if you don't use streaming endpoints.

- *servant-client* Separate streaming client.
  [#1066](https://github.com/haskell-servant/servant/pull/1066)

  We now have two `http-client` based clients,
  in `Servant.Client` and `Servant.Client.Streaming`.

  Their API is the same, except for
  - `Servant.Client` **cannot** request `Stream` endpoints.
  - `Servant.Client` is *run* by direct
    `runClientM :: ClientM a -> ClientEnv -> IO (Either ServantError a)`
  - `Servant.Client.Streaming` **can** request `Stream` endpoints.
  - `Servant.Client.Streaming` is *used* by CPSised
    `withClientM :: ClientM a -> ClientEnv -> (Either ServantError a -> IO b) -> IO b`

  To access `Stream` endpoints use `Servant.Client.Streaming` with
  `withClientM`; otherwise you can continue using `Servant.Client` with `runClientM`.
  You can use both too, `ClientEnv` and `BaseUrl` types are same for both.

  **Note:** `Servant.Client.Streaming` doesn't *stream* non-`Stream` endpoints.
  Requesting ordinary `Verb` endpoints (e.g. `Get`) will block until
  the whole response is received.

  There is `Servant.Client.Streaming.runClientM` function, but it has
  restricted type. `NFData a` constraint prevents using it with
  `SourceT`, `Conduit` etc. response types.

  ```haskell
  runClientM :: NFData a => ClientM a -> ClientEnv -> IO (Either ServantError a)
  ```

  This change shouldn't affect you, if you don't use streaming endpoints.

- *servant-client-core* Related to the previous:
  `streamingResponse` is removed from `RunClient`.
  We have a new type-class:

  ```haskell
  class RunClient m =>  RunStreamingClient m where
      withStreamingRequest :: Request -> (StreamingResponse -> IO a) ->  m a
  ```

- Drop support for GHC older than 8.0
  [#1008](https://github.com/haskell-servant/servant/pull/1008)
  [#1009](https://github.com/haskell-servant/servant/pull/1009)

- *servant-client-core* Add `NFData (GenResponse a)` and `NFData ServantError` instances.
  [#1076](https://github.com/haskell-servant/servant/pull/1076)

- *servant-client-core* Add `aeson` and `Lift BaseUrl` instances
  [#1037](https://github.com/haskell-servant/servant/pull/1037)

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
