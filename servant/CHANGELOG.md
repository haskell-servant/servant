[The latest version of this document is on GitHub.](https://github.com/haskell-servant/servant/blob/master/servant/CHANGELOG.md)

Package versions follow the [Package Versioning Policy](https://pvp.haskell.org/): in A.B.C, bumps to either A or B represent major versions.

0.20.3.0
----

### Significant changes

- Remove -XStrictData from servant{,-server}'s cabal files [#1780](https://github.com/haskell-servant/servant/issues/1780) [#1781](https://github.com/haskell-servant/servant/pull/1781)

  The addition of -XStrictData to servant.cabal and servant-server.cabal reduced the laziness
  of routing, which would trigger unimplemented endpoints using `error` or `undefined`,
  despite the fact that these endpoints themselves were not queried.

### Other changes

- Server-sent events (SSE) for client-side [#1811](https://github.com/haskell-servant/servant/issues/1811)

  Implement Server-sent events (SSE) for the Servant client using a new
  combinator "ServerSentEvents". The raw event messages, accumulated events and
  JSON-processed events can be exposed.

- Integrate MultiVerb [#1766](https://github.com/haskell-servant/servant/pull/1766) [#1804](https://github.com/haskell-servant/servant/pull/1804)

  Expose MultiVerb, a more ergonomic way of defining endpoints that return
  many kinds of responses. Read the cookbook https://docs.servant.dev/en/master/cookbook/multiverb/MultiVerb.html

- Exported addQueryParam [#1232](https://github.com/haskell-servant/servant/issues/1232) [#1785](https://github.com/haskell-servant/servant/pull/1785)

  `addQueryParams` is required to define custom `HasLink` instances which actually manipulate the
  generated query params. This function was not exported earlier and now it is.

- Add Host API combinator [#1800](https://github.com/haskell-servant/servant/pull/1800)

  Adding a Host combinator allows servant users to select APIs according
  to the Host header provided by clients.

- Use newtype deriving for ToHttpApiData in the type Range [#1813](https://github.com/haskell-servant/servant/pull/1813)

- Add public re-export of renderCurlBasePath lens [#1706](https://github.com/haskell-servant/servant/pull/1706)
- Remove GHC <= 8.10.7 from the support window [#1778](https://github.com/haskell-servant/servant/pull/1778)
- Add Servant.API.Range type [#1805](https://github.com/haskell-servant/servant/pull/1805)
- Add missing HasLink instance for DeepQuery [#1784](https://github.com/haskell-servant/servant/issues/1784) [#1814](https://github.com/haskell-servant/servant/pull/1814)

0.20.2
----
- Full query string helpers [#1604](https://github.com/haskell-servant/servant/pull/1604)

  This PR introduces `DeepQuery`, a route combinator that implements a pattern commonly known as deep objects.
  It builds upon the convention of using `[]` for a list of parameters: 
  `books?filter[search]=value&filter[author][name]=value`.
  The corresponding type would be `DeepQuery "filter" BookQuery :> Get '[JSON] [Book]`.
- Add IsIn instance for NamedRoutes [#1707](https://github.com/haskell-servant/servant/pull/1707)
- Renamed `AtLeastOneFragment` type class to `AtMostOneFragment` [#1727](https://github.com/haskell-servant/servant/pull/1727)

  The previously named `AtLeastOneFragment` type class defined in the
  `Servant.API.TypeLevel` module has been renamed to `AtMostOneFragment`,
  since the previous name was misleading.
- Use `Header'` in response headers. [#1697](https://github.com/haskell-servant/servant/pull/1697)

  Use `Header'` instead of `Header` in response, so it's possible to provide
  `Description`, for example:

  ```
  type PaginationTotalCountHeader =
    Header'
      '[ Description "Indicates to the client total count of items in collection"
       , Optional
       , Strict
       ]
      "Total-Count"
      Int
  ```

  Note: if you want to add header with description you should use `addHeader'`
  or `noHeader'` which accepts `Header'` with all modifiers.


0.20.1
----

- Support aeson-2.2 [#1695](https://github.com/haskell-servant/servant/pull/1695)

0.20
----

- Headers support in UVerb responses [#1570](https://github.com/haskell-servant/servant/issues/1570) [#1571](https://github.com/haskell-servant/servant/pull/1571)
- Generalize type of `Servant.Types.SourceT.source` to any foldable [#1593](https://github.com/haskell-servant/servant/pull/1593)
- Make `Mime(Un)Render PlainText String` instances encode/decode UTF-8 [#1645](https://github.com/haskell-servant/servant/issues/1645)
- Add HasStatus instance for Headers (that defers StatusOf to underlying value) [#1649](https://github.com/haskell-servant/servant/pull/1649)
- Make fromSourceIO run in IO [#1661](https://github.com/haskell-servant/servant/pull/1661)

  Some streaming abstractions, like io-streams, require stateful
  initialization. Since all actual call sites of `fromSourceIO`
  are in a context where `IO` actions can be executed, these
  streaming sources can be accomodated by having letting
  `fromSourceIO` run in `IO`.

  To migrate your existing `FromSourceIO` instance, simply put
  a `pure`/`return` in front of it.

- Fix the handling of multiple headers with the same name. [#1666](https://github.com/haskell-servant/servant/pull/1666)

0.19.1
------

Compatibility with GHC 9.4, see [PR #1592](https://github.com/haskell-servant/servant/pull/1592).

0.19
----

### Significant changes

- Drop support for GHC < 8.6.
- Support GHC 9.0 (GHC 9.2 should work as well, but isn't fully tested yet).
- Support Aeson 2 ([#1475](https://github.com/haskell-servant/servant/pull/1475)),
  which fixes a [DOS vulnerability](https://github.com/haskell/aeson/issues/864)
  related to hash collisions.
- Add `NamedRoutes` combinator, making support for records first-class in Servant
  ([#1388](https://github.com/haskell-servant/servant/pull/1388)).
 
  Users can now directly mark part as an API as defined by a record, instead of
  using `(:<|>)` to combine routes. Concretely, the anonymous:
  
  ```haskell
  type API = 
    "version" :> Get '[JSON] String :<|>
    "products" :> Get '[JSON] [Product]
  ```
  
  can be replaced with the explicitly-named:
  
  ```haskell
  type API = NamedRoutes NamedAPI
  data NamedAPI mode = NamedAPI
    { version :: mode :- "version" :> Get '[JSON] String
    , products :: mode :- "products" :> Get '[JSON] [Product]
    }
  ```
 
  `NamedRoutes` builds upon `servant-generic`, but improves usability by freeing
  users from the need to perform `toServant` / `fromServant` conversions
  manually. Serving `NamedRoutes NamedAPI` is now done directly by providing a
  record of handlers, and servant generates clients directly as records as well.
  In particular, it makes it much more practical to work with nested hierarchies
  of named routes.

  Two convenience functions, `(//)` and `(/:)`, have been added to make the
  usage of named route hierarchies more pleasant:
  
  ```haskell
  rootClient :: RootApi (AsClientT ClientM)
  rootClient = client (Proxy @API)

  helloClient :: String -> ClientM String
  helloClient name = rootClient // hello /: name

  endpointClient :: ClientM Person
  endpointClient = rootClient // subApi /: "foobar123" // endpoint

  type Api = NamedRoutes RootApi

  data RootApi mode = RootApi
    { subApi :: mode :- Capture "token" String :> NamedRoutes SubApi
    , hello :: mode :- Capture "name" String :> Get '[JSON] String
    , …
    } deriving Generic

  data SubApi mode = SubApi
    { endpoint :: mode :- Get '[JSON] Person
    , …
    } deriving Generic
  ```
  
- Add custom type errors for partially applied combinators
  ([#1289](https://github.com/haskell-servant/servant/pull/1289),
  [#1486](https://github.com/haskell-servant/servant/pull/1486)).
 
  For example, forgetting to document the expected type for a query parameter,
  as in:
 
  ``` haskell
  type API = QueryParam "param" :> Get '[JSON] NoContent
  ```
  
  will raise to the following error when trying to serve the API:

  ```
    • There is no instance for HasServer (QueryParam'
                                            '[Optional, Strict] "param" :> ...)
      QueryParam' '[Optional, Strict] "1" expects 1 more arguments
  ```
  
  As a consequence of this change, unsaturated types are now forbidden before `(:>)`.
  
- Add a `HeadNoContent` verb ([#1502](https://github.com/haskell-servant/servant/pull/1502)).

- *servant-client* / *servant-client-core* / *servant-http-streams*:
  Fix erroneous behavior, where only 2XX status codes would be considered
  successful, irrelevant of the status parameter specified by the verb
  combinator. ([#1469](https://github.com/haskell-servant/servant/pull/1469))

- *servant-client* / *servant-client-core*: Fix `Show` instance for
  `Servant.Client.Core.Request`.
 
 
- *servant-client* / *servant-client-core*: Allow passing arbitrary binary data
  in Query parameters.
  ([#1432](https://github.com/haskell-servant/servant/pull/1432)).

- *servant-docs*: Generate sample cURL requests
  ([#1401](https://github.com/haskell-servant/servant/pull/1401/files)).

  Breaking change: requires sample header values to be supplied with `headers`.
  
### Other changes

- Various bit rotten cookbooks have been updated and re-introduced on
  [docs.servant.dev](https://docs.servant.dev).

- Various version bumps.

0.18.3
------

### Significant changes

- Add response header support to UVerb (#1420).
- Use Capture Description if available (#1423).

### Other changes

- Support GHC-9.0.1.
- Bump `bytestring`, `attoparsec`, `hspec` and `singleton-bool` dependencies.

0.18.2
------

### Significant changes

- Introduce `Fragment` combinator.
- Fix `MimeRender` and `MimeUnrender` instances for `WithStatus`.

0.18.1
------

### Significant changes

- Union verbs

### Other changes

- Bump "tested-with" ghc versions
- Allow newer dependencies

0.18
----

### Significant changes

- Support for ghc8.8 (#1318, #1326, #1327)

- Configurable error messages for automatic errors thrown by servant,
  like "no route" or "could not parse json body" (#1312, #1326, #1327)

### Other changes

- Witness that a type-level natural number corresponds to a HTTP
  status code (#1310)

- Improve haddocs (#1279)

- Dependency management (#1269, #1293, #1286, #1287)


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

- *servant-client* Added a function to create Client.Request in ClientEnv [#1213](https://github.com/haskell-servant/servant/pull/1213) [#1255](https://github.com/haskell-servant/servant/pull/1255)

  The new member `makeClientRequest` of `ClientEnv` is used to create
  `http-client` `Request` from `servant-client-core` `Request`.
  This functionality can be used for example to set
  dynamic timeouts for each request.

- *servant-server* use queryString to parse QueryParam, QueryParams and QueryFlag [#1249](https://github.com/haskell-servant/servant/pull/1249) [#1262](https://github.com/haskell-servant/servant/pull/1262)

  Some APIs need query parameters rewriting, e.g. in order to support
   for multiple casing (camel, snake, etc) or something to that effect.

  This could be easily achieved by using WAI Middleware and modifying
  request's `Query`. But QueryParam, QueryParams and QueryFlag use
  `rawQueryString`. By using `queryString` rather then `rawQueryString`
  we can enable such rewritings.

- *servant* *servant-server* Make packages `build-type: Simple` [#1263](https://github.com/haskell-servant/servant/pull/1263)

  We used `build-type: Custom`, but it's problematic e.g.
  for cross-compiling. The benefit is small, as the doctests
  can be run other ways too (though not so conveniently).

- *servant* Remove deprecated modules [1268#](https://github.com/haskell-servant/servant/pull/1268)

  - `Servant.Utils.Links` is `Servant.Links`
  - `Servant.API.Internal.Test.ComprehensiveAPI` is `Servant.Test.ComprehensiveAPI`

### Other changes

- *servant-client* *servant-client-core* *servant-http-streams* Fix Verb with headers checking content type differently [#1200](https://github.com/haskell-servant/servant/issues/1200) [#1204](https://github.com/haskell-servant/servant/pull/1204)

  For `Verb`s with response `Headers`, the implementation didn't check
  for the content-type of the response. Now it does.

- *servant-docs* Merge documentation from duplicate routes [#1240](https://github.com/haskell-servant/servant/issues/1240) [#1241](https://github.com/haskell-servant/servant/pull/1241)

  Servant supports defining the same route multiple times with different
  content-types and result-types, but servant-docs was only documenting
  the first of copy of such duplicated routes. It now combines the
  documentation from all the copies.

  Unfortunately, it is not yet possible for the documentation to specify
  multiple status codes.

- Add sponsorship button [#1190](https://github.com/haskell-servant/servant/pull/1190)

  [Well-Typed](https://www.well-typed.com/) is a consultancy which could help you with `servant` issues
  (See consultancies section on https://www.servant.dev/).

- Try changelog-d for changelog management [#1230](https://github.com/haskell-servant/servant/pull/1230)

  Check the [CONTRIBUTING.md](https://github.com/haskell-servant/servant/blob/master/CONTRIBUTING.md) for details

- CI and testing tweaks. [#1154](https://github.com/haskell-servant/servant/pull/1154) [#1157](https://github.com/haskell-servant/servant/pull/1157) [#1182](https://github.com/haskell-servant/servant/pull/1182) [#1214](https://github.com/haskell-servant/servant/pull/1214) [#1229](https://github.com/haskell-servant/servant/pull/1229) [#1233](https://github.com/haskell-servant/servant/pull/1233) [#1242](https://github.com/haskell-servant/servant/pull/1242) [#1247](https://github.com/haskell-servant/servant/pull/1247) [#1250](https://github.com/haskell-servant/servant/pull/1250) [#1258](https://github.com/haskell-servant/servant/pull/1258)

  We are experiencing some bitrotting of cookbook recipe dependencies,
  therefore some of them aren't build as part of our CI anymore.

- New cookbook recipes [#1088](https://github.com/haskell-servant/servant/pull/1088) [#1171](https://github.com/haskell-servant/servant/pull/1171) [#1198](https://github.com/haskell-servant/servant/pull/1198)

  - [OIDC Recipe](#TODO)
  - [MySQL Recipe](#TODO)

- *servant-jsaddle* Progress on servant-jsaddle [#1216](https://github.com/haskell-servant/servant/pull/1216)
- *servant-docs* Prevent race-conditions in testing [#1194](https://github.com/haskell-servant/servant/pull/1194)
- *servant-client* *servant-http-streams* `HasClient` instance for `Stream` with `Headers` [#1170](https://github.com/haskell-servant/servant/issues/1170) [#1197](https://github.com/haskell-servant/servant/pull/1197)
- *servant* Remove unused extensions from cabal file [#1201](https://github.com/haskell-servant/servant/pull/1201)
- *servant-client* Redact the authorization header in Show and exceptions [#1238](https://github.com/haskell-servant/servant/pull/1238)
- Dependency upgrades [#1173](https://github.com/haskell-servant/servant/pull/1173) [#1181](https://github.com/haskell-servant/servant/pull/1181) [#1183](https://github.com/haskell-servant/servant/pull/1183) [#1188](https://github.com/haskell-servant/servant/pull/1188) [#1224](https://github.com/haskell-servant/servant/pull/1224) [#1245](https://github.com/haskell-servant/servant/pull/1245) [#1257](https://github.com/haskell-servant/servant/pull/1257)
- Documentation updates [#1162](https://github.com/haskell-servant/servant/pull/1162) [#1174](https://github.com/haskell-servant/servant/pull/1174) [#1175](https://github.com/haskell-servant/servant/pull/1175) [#1234](https://github.com/haskell-servant/servant/pull/1234) [#1244](https://github.com/haskell-servant/servant/pull/1244) [#1247](https://github.com/haskell-servant/servant/pull/1247)


0.16.2
------

* `singleton-bool-0.1.5` (`SBool` is re-exported)
    - Add `discreteBool :: Dec (a :~: b)` (GHC-7.8+)
    - Add `Show`, `Eq`, `Ord` `SBool b` instances.
* dependencies update

0.16.1
------

* Add `Semigroup` and `Monoid` `SourceT` instances
  [#1158](https://github.com/haskell-servant/servant/pull/1158)
  [#1159](https://github.com/haskell-servant/servant/pull/1159)
* Use `http-api-data-0.4.1`
  [#1181](https://github.com/haskell-servant/servant/pull/1181)
* Allow newer dependencies

0.16.0.1
--------

- Make tests work with `http-media-0.8`

0.16
----

### Significant changes

- Rename `ServantError` to `ClientError`, `ServantErr` to `ServerError`
  [#1131](https://github.com/haskell-servant/servant/pull/1131)
- *servant-client-core* Rearrange modules. No more `Internal` modules, whole
  API is versioned.
  [#1130](https://github.com/haskell-servant/servant/pull/1130)
- *servant-http-streams* New package
  [#1117](https://github.com/haskell-servant/servant/pull/1117)
- *servant-client-core* `RequestBody` is now

    ```haskell
    = RequestBodyLBS LBS.ByteString
    | RequestBodyBS BS.ByteString
    | RequestBodySource (SourceIO LBS.ByteString)
    ```

  i.e. no more replicates `http-client`s API.
  [#1117](https://github.com/haskell-servant/servant/pull/1117)

- *servant-client-core* Keep structured exceptions in `ConnectionError`
  constructor of `ClientError`
  [#1115](https://github.com/haskell-servant/servant/pull/1115)

    ```diff
    -| ConnectionError Text
    +| ConnectionError SomeException
    ```

- *servant-client-core* Preserve failing request in `FailureResponse`
  constructor of `ClientError`
  [#1114](https://github.com/haskell-servant/servant/pull/1114)

    ```diff
    -FailureResponse Response
    +-- | The server returned an error response including the
    +-- failing request. 'requestPath' includes the 'BaseUrl' and the
    +-- path of the request.
    +FailureResponse (RequestF () (BaseUrl, BS.ByteString)) Response
    ```

- *servant-client* Fix (implement) `StreamBody` instance
  [#1110](https://github.com/haskell-servant/servant/pull/1110)

### Other changes

- *servant-client* Update CookieJar with intermediate request/responses (redirects)
  [#1104](https://github.com/haskell-servant/servant/pull/1104)
- *servant-server* Reorder HTTP failure code priorities
  [#1103](https://github.com/haskell-servant/servant/pull/1103)
- *servant-server* Re-organise internal modules
  [#1139](https://github.com/haskell-servant/servant/pull/1139)
- Allow `network-3.0`
  [#1107](https://github.com/haskell-servant/servant/pull/1107)
- Add `NFData NoContent` instance
  [#1090](https://github.com/haskell-servant/servant/pull/1090)

- Documentation updates
  [#1127](https://github.com/haskell-servant/servant/pull/1127)
  [#1124](https://github.com/haskell-servant/servant/pull/1124)
  [#1098](https://github.com/haskell-servant/servant/pull/1098)

- CI updates
  [#1123](https://github.com/haskell-servant/servant/pull/1123)
  [#1121](https://github.com/haskell-servant/servant/pull/1121)
  [#1119](https://github.com/haskell-servant/servant/pull/1119)

0.15
----

### Significant changes

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

- *servant* `ComprehensiveAPI` is a part of public API in `Servant.Test.ComprehensiveAPI` module.
  This API type is used to verify that libraries implement all core combinators.
  Now we won't change this type between major versions.
  (This has been true for some time already).
  [#1070](https://github.com/haskell-servant/servant/pull/1070)

- *servant* Remove `Servant.Utils.Enter` module
  (deprecated in `servant-0.12` in favour of `hoistServer`)
  [#996](https://github.com/haskell-servant/servant/pull/996)

- *servant-foreign* Add support so `HasForeign` can be implemented for
  `MultipartForm` from [`servant-multipart`](http://hackage.haskell.org/package/servant-multipart)
  [#1035](https://github.com/haskell-servant/servant/pull/1035)

### Other changes

- *servant-client-core* Add `NFData (GenResponse a)` and `NFData ServantError` instances.
  [#1076](https://github.com/haskell-servant/servant/pull/1076)

- *servant* NewlineFraming encodes newline after each element (i.e last)
  [#1079](https://github.com/haskell-servant/servant/pull/1079)
  [#1011](https://github.com/haskell-servant/servant/issues/1011)

- *servant* Add `lookupResponseHeader :: ... => Headers headers r -> ResponseHeader h a`
  [#1064](https://github.com/haskell-servant/servant/pull/1064)

- *servant-server* Add `MonadMask Handler`
  [#1068](https://github.com/haskell-servant/servant/pull/1068)

- *servant-docs* Fix markdown indentation
  [#1043](https://github.com/haskell-servant/servant/pull/1043)

- *servant* Export `GetHeaders'`
  [#1052](https://github.com/haskell-servant/servant/pull/1052)

- *servant* Add `Bitraversable` and other `Bi-` instances for `:<|>`
  [#1032](https://github.com/haskell-servant/servant/pull/1032)

- *servant* Add `PutCreated` method type alias
  [#1024](https://github.com/haskell-servant/servant/pull/1024)

- *servant-client-core* Add `aeson` and `Lift BaseUrl` instances
  [#1037](https://github.com/haskell-servant/servant/pull/1037)

- *servant* Add `ToSourceIO (NonEmpty a)` instance
  [#988](https://github.com/haskell-servant/servant/pull/988)

- Development process improvements
    - Apply `stylish-haskell` to all modules
      [#1001](https://github.com/haskell-servant/servant/pull/1001)
    - Amend `CONTRIBUTING.md`
      [#1036](https://github.com/haskell-servant/servant/pull/1036)
    - `servant-docs` has golden tests for `ComprehensiveAPI`
      [#1071](https://github.com/haskell-servant/servant/pull/1071)
    - Other
      [#1039](https://github.com/haskell-servant/servant/pull/1039)
      [#1046](https://github.com/haskell-servant/servant/pull/1046)
      [#1062](https://github.com/haskell-servant/servant/pull/1062)
      [#1069](https://github.com/haskell-servant/servant/pull/1069)
      [#985](https://github.com/haskell-servant/servant/pull/985)

- *Documentation* Tutorial and new recipes
    - [Using free client](https://docs.servant.dev/en/latest/cookbook/using-free-client/UsingFreeClient.html)
      [#1005](https://github.com/haskell-servant/servant/pull/1005)
    - [Generating mock curl calls](https://docs.servant.dev/en/latest/cookbook/curl-mock/CurlMock.html)
      [#1033](https://github.com/haskell-servant/servant/pull/1033)
    - [Error logging with Sentry](https://docs.servant.dev/en/latest/cookbook/sentry/Sentry.html)
      [#987](https://github.com/haskell-servant/servant/pull/987)
    - [Hoist Server With Context for Custom Monads](https://docs.servant.dev/en/latest/cookbook/hoist-server-with-context/HoistServerWithContext.html)
      [#1044](https://github.com/haskell-servant/servant/pull/1044)
    - [How To Test Servant Applications](https://docs.servant.dev/en/latest/cookbook/testing/Testing.html)
      [#1050](https://github.com/haskell-servant/servant/pull/1050)
    - `genericServeT`: using custom monad with `Servant.API.Generic`
      in [Using generics](https://docs.servant.dev/en/latest/cookbook/generic/Generic.html)
      [#1058](https://github.com/haskell-servant/servant/pull/1058)
    - Tutorial
      [#974](https://github.com/haskell-servant/servant/pull/974)
      [#1007](https://github.com/haskell-servant/servant/pull/1007)
    - miscellanea: fixed typos etc.
      [#1030](https://github.com/haskell-servant/servant/pull/1030)
      [#1020](https://github.com/haskell-servant/servant/pull/1020)
      [#1059](https://github.com/haskell-servant/servant/pull/1059)

- *Documentation* README
  [#1010](https://github.com/haskell-servant/servant/pull/1010)

- *servant-client-ghcjs* updates. **note** package is not released on Hackage
  [#938](https://github.com/haskell-servant/servant/pull/938)

0.14.1
------

- Merge in (and slightly refactor) `servant-generic`
  (by [Patrick Chilton](https://github.com/chpatrick))
  into `servant` (`Servant.API.Generic`),
  `servant-client-code` (`Servant.Client.Generic`)
  and `servant-server` (`Servant.Server.Generic`).

- Deprecate `Servant.Utils.Links`, use `Servant.Links`.
  [#998](https://github.com/haskell-servant/servant/pull/998)

- *servant-server* Deprecate `Servant.Utils.StaticUtils`, use `Servant.Server.StaticUtils`.

0.14
----

### Significant changes

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
  of a web application live, we also have `hoistClient` for changing the monad
  in which *client functions* live.
  Read [tutorial section for more information](https://docs.servant.dev/en/release-0.14/tutorial/Client.html#changing-the-monad-the-client-functions-live-in).
  ([#936](https://github.com/haskell-servant/servant/pull/936))

  iF you have own combinators, you'll need to define a new method of
  `HasClient` class, for example:

  ```haskell
  type Client m (MyCombinator :> api) = MyValue :> Client m api
  hoistClientMonad pm _ nt cl = hoistClientMonad pm (Proxy :: Proxy api) nt . cl
  ```

- *servant* Add `safeLink' :: (Link -> a) -> ... -> MkLink endpoint a`,
  which allows to create helpers returning something else than `Link`.
  ([#968](https://github.com/haskell-servant/servant/pull/968))

- *servant-server* File serving in polymorphic monad.
  i.e. Generalised types of `serveDirectoryFileServer` etc functions in
  `Servant.Utils.StaticFiles`
  ([#953](https://github.com/haskell-servant/servant/pull/953))

- *servant-server* `ReqBody` content type check is recoverable.
  This allows writing APIs like:

  ```haskell
        ReqBody '[JSON] Int      :> Post '[PlainText] Int
  :<|>  ReqBody '[PlainText] Int :> Post '[PlainText] Int
  ```

  which is useful when handlers are subtly different,
  for example may do less work.
  ([#937](https://github.com/haskell-servant/servant/pull/937))

- *servant-client* Add more constructors to `RequestBody`, including
  `RequestBodyStream`.
  *Note:* we are looking for http-library agnostic API,
  so the might change again soon.
  Tell us which constructors are useful for you!
  ([#913](https://github.com/haskell-servant/servant/pull/913))

### Other changes

- `GetHeaders` instances implemented without `OverlappingInstances`
  ([#971](https://github.com/haskell-servant/servant/pull/971))

- Added tests or enabled tests
  ([#975](https://github.com/haskell-servant/servant/pull/975))

- Add [pagination cookbook recipe](https://docs.servant.dev/en/release-0.14/cookbook/pagination/Pagination.html)
  ([#946](https://github.com/haskell-servant/servant/pull/946))

- Add [`servant-flatten` "spice" to the structuring api recipe](https://docs.servant.dev/en/release-0.14/cookbook/structuring-apis/StructuringApis.html)
  ([#929](https://github.com/haskell-servant/servant/pull/929))

- Dependency updates
  ([#900](https://github.com/haskell-servant/servant/pull/900)
   [#919](https://github.com/haskell-servant/servant/pull/919)
   [#924](https://github.com/haskell-servant/servant/pull/924)
   [#943](https://github.com/haskell-servant/servant/pull/943)
   [#964](https://github.com/haskell-servant/servant/pull/964)
   [#967](https://github.com/haskell-servant/servant/pull/967)
   [#976](https://github.com/haskell-servant/servant/pull/976))

- Documentation updates
   [#963](https://github.com/haskell-servant/servant/pull/963)
   [#960](https://github.com/haskell-servant/servant/pull/960)
   [#908](https://github.com/haskell-servant/servant/pull/908)
   [#958](https://github.com/haskell-servant/servant/pull/958)
   [#948](https://github.com/haskell-servant/servant/pull/948)
   [#928](https://github.com/haskell-servant/servant/pull/928)
   [#921](https://github.com/haskell-servant/servant/pull/921))

- Development process improvements
  ([#680](https://github.com/haskell-servant/servant/pull/680)
   [#917](https://github.com/haskell-servant/servant/pull/917)
   [#923](https://github.com/haskell-servant/servant/pull/923)
   [#961](https://github.com/haskell-servant/servant/pull/961)
   [#973](https://github.com/haskell-servant/servant/pull/973))

### Note

(VIM) Regular-expression to link PR numbers: `s/\v#(\d+)/[#\1](https:\/\/github.com\/haskell-servant\/servant\/pull\/\1)/`

0.13.0.1
--------

- Support `base-compat-0.10`

0.13
----

### Significant changes

- Streaming endpoint support.
  ([#836](https://github.com/haskell-servant/servant/pull/836))

  ```haskell
  type StreamApi f = "streamGetNewline" :> StreamGet NewlineFraming JSON (f Person)
  ```

  See tutorial for more details
  - [A web API as a type - StreamGet and StreamPost](http://docs.servant.dev/en/release-0.13/tutorial/ApiType.html#streamget-and-streampost)
  - [Serving an API - streaming endpoints](http://docs.servant.dev/en/release-0.13/tutorial/Server.html#streaming-endpoints)
  - [Querying an API - Querying Streaming APIs](http://docs.servant.dev/en/release-0.13/tutorial/Client.html#querying-streaming-apis)

- *servant* Add `Servant.API.Modifiers`
  ([#873](https://github.com/haskell-servant/servant/pull/873)
   [#903](https://github.com/haskell-servant/servant/pull/903))

  `QueryParam`, `Header` and `ReqBody` understand modifiers:
  - `Required` or `Optional` (resulting in `a` or `Maybe a` in handlers)
  - `Strict` or `Lenient` (resulting in `a` or `Either String a` in handlers)

  Also you can use `Description` as a modifier, but it doesn't yet work
  with `servant-docs`, only `servant-swagger`. [There is an issue.](https://github.com/haskell-servant/servant/issues/902)

- *servant-client* Support `http-client`’s `CookieJar`
  ([#897](https://github.com/haskell-servant/servant/pull/897)
   [#883](https://github.com/haskell-servant/servant/pull/883))

  `ClientM` preserves cookies between requests,
  if given initial `CookieJar`.
  To migrate from older code, change `ClientEnv` constructor
  to `mkClientEnv` which makes `ClientEnv` without `CookieJar`.

- *servant* Mono-kind-ise modifiers, resulting in better error messages.
  ([#887](https://github.com/haskell-servant/servant/issues/887)
   [#890](https://github.com/haskell-servant/servant/pull/890))

- *servant* Add `TypeError ... => HasServer`s instances in GHC-8.2 for
  not saturated modifiers (`Capture "foo" :> ...`) or `->` in place of `:>`.
  ([#893](https://github.com/haskell-servant/servant/pull/893))

- *Cookbook* example projects at
  http://docs.servant.dev/en/master/cookbook/index.html
  ([#867](https://github.com/haskell-servant/servant/pull/867)
   [#892](https://github.com/haskell-servant/servant/pull/882))

- *Experimental work* `servant-client-ghcjs`
  ([#818](https://github.com/haskell-servant/servant/pull/818)
   [#869](https://github.com/haskell-servant/servant/pull/869))

### Other changes

- *servant* Links aren't double escaped
  ([#878](https://github.com/haskell-servant/servant/pull/878))

- Dependency updates
  ([#900](https://github.com/haskell-servant/servant/pull/900)
   [#898](https://github.com/haskell-servant/servant/pull/898)
   [#895](https://github.com/haskell-servant/servant/pull/895)
   [#872](https://github.com/haskell-servant/servant/pull/872))

- Documentation updates
  ([#875](https://github.com/haskell-servant/servant/pull/875)
   [#861](https://github.com/haskell-servant/servant/pull/861))

- Refactorings
  ([#899](https://github.com/haskell-servant/servant/pull/899)
   [#896](https://github.com/haskell-servant/servant/pull/896)
   [#889](https://github.com/haskell-servant/servant/pull/889)
   [#891](https://github.com/haskell-servant/servant/pull/891)
   [#892](https://github.com/haskell-servant/servant/pull/892)
   [#885](https://github.com/haskell-servant/servant/pull/885))

0.12.1
------

### Bug fixes

- Prevent double-escaping in link segments
  ([#835](https://github.com/haskell-servant/servant/issues/835)
   [#878](https://github.com/haskell-servant/servant/pull/878))

0.12
---

### Significant changes

- *servant-client* *servant-client-core*
  Factored out of `servant-client` all the functionality that was
  independent of the `http-client` backend.
  ([#803](https://github.com/haskell-servant/servant/pull/803)
   [#821](https://github.com/haskell-servant/servant/issues/821))

  If you have own combinators, you'll need to add an additional `m` argument
  in `HasClient`, `Client` and `clientWithRoute`:

  ```diff
  -class HasClient api
  -  type Client (api :: *) :: *
  -  clientWithRoute :: Proxy api -> Req -> Client api
  +class HasClient m api
  +  type Client (m :: * -> *) (api :: *) :: *
  +  clientWithRoute :: Proxy m -> Proxy api -> Request -> Client m api
  ```

  See https://github.com/haskell-servant/servant-auth/pull/67/commits/f777818e3cc0fa3ed2346baff8328e96d62b1790 for a real world example.

- *servant-server* Added `hoistServer` member to the `HasServer` class, which is `HasServer`
  specific `enter`.
  ([#804](https://github.com/haskell-servant/servant/pull/804)
   [#824](https://github.com/haskell-servant/servant/pull/824))

  `enter` isn't exported from `Servant` module anymore. You can change
  `enter` to `hoistServer` in a straight forward way.
  Unwrap natural transformation and add an api type `Proxy`:

  ```diff
  -server = enter (NT nt) impl
  +server = hoistServer (Proxy :: Proxy MyApi) nt impl
  ```

  If you have own combinators, you'll need to define a new method of
  `HasServer` class, for example:

  ```haskell
  type ServerT (MyCombinator :> api) m = MyValue -> ServerT api m
  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s
  ```

  See https://github.com/haskell-servant/servant-auth/pull/67/commits/8ee3b6315247ac076516213fd7cfcdbfdb583ac9 for a real world example.

- Add `Description` and `Summary` combinators
  ([#767](https://github.com/haskell-servant/servant/pull/767))

  It's possible to annotate endpoints with free form text.
  This information is used by e.g. by `servant-swagger`, see screenshot in
  https://github.com/phadej/servant-swagger-ui

- Lower `:>` and `:<|>` infix precedence to 4 and 3 respectively
  ([#761](https://github.com/haskell-servant/servant/issues/761))

  This shouldn't affect you, except if you define your own infix operators
  for Servant type-level DSL.

### Other changes

- *servant-foreign* Derive `Data` for all types
  ([#809](https://github.com/haskell-servant/servant/pull/809))
- *servant-docs* Add authentication lenses
  ([#787](https://github.com/haskell-servant/servant/pull/787))
- *servant-docs* Generated markdown improvements
  ([#813](https://github.com/haskell-servant/servant/pull/787)
   [#767](https://github.com/haskell-servant/servant/pull/767)
   [#790](https://github.com/haskell-servant/servant/pull/790)
   [#788](https://github.com/haskell-servant/servant/pull/788))
- Add `addLinks` to generate all links for unnested APIs.
  ([#851](https://github.com/haskell-servant/servant/pull/851))
- Allow newest dependencies
 ([#772](https://github.com/haskell-servant/servant/pull/772)
  [#842](https://github.com/haskell-servant/servant/pull/842))
- Documentation improvements and typo fixes
 ([#757](https://github.com/haskell-servant/servant/pull/757)
  [#771](https://github.com/haskell-servant/servant/pull/771)
  [#775](https://github.com/haskell-servant/servant/pull/775)
  [#790](https://github.com/haskell-servant/servant/pull/790)
  [#791](https://github.com/haskell-servant/servant/pull/791)
  [#806](https://github.com/haskell-servant/servant/pull/806))
- Development process improvements
  ([#764](https://github.com/haskell-servant/servant/pull/764)
   [#839](https://github.com/haskell-servant/servant/pull/839))

0.11
----

### Breaking changes

- `Enter` refactored
  ([#734](https://github.com/haskell-servant/servant/issues/734)
  , [#736](https://github.com/haskell-servant/servant/pull/736))

### Other changes

- Add a type representing an empty API
  ([#753](https://github.com/haskell-servant/servant/pull/753))
- Add `linkURI'` and `Link` accessors
  ([#745](https://github.com/haskell-servant/servant/pull/745)
  , [#717](https://github.com/haskell-servant/servant/pull/717)
  , [#715](https://github.com/haskell-servant/servant/issues/715))
- Prepare for GHC-8.2
  ([#722](https://github.com/haskell-servant/servant/pull/722))
- Add `HasLink AuthProtect` instance
  ([#720](https://github.com/haskell-servant/servant/pull/720))
- `AllCTRender [] ()` `TypeError` (use `NoContent`)
  ([#671](https://github.com/haskell-servant/servant/pull/671))
- Documentation improvements and typo fixes
  ([#702](https://github.com/haskell-servant/servant/pull/702)
  , [#709](https://github.com/haskell-servant/servant/pull/709)
  , [#716](https://github.com/haskell-servant/servant/pull/716)
  , [#725](https://github.com/haskell-servant/servant/pull/725)
  , [#727](https://github.com/haskell-servant/servant/pull/727))

0.10
----

### Breaking changes

* Use `NT` from `natural-transformation` for `Enter`
  ([#616](https://github.com/haskell-servant/servant/issues/616))

* Change to `MkLink (Verb ...) = Link` (previously `URI`). To consume `Link`
  use its `ToHttpApiData` instance or `linkURI`.
  ([#527](https://github.com/haskell-servant/servant/issues/527))

### Other changes

* Add `Servant.API.TypeLevel` module with type families to work with API types.
  ([#345](https://github.com/haskell-servant/servant/pull/345)
  , [#305](https://github.com/haskell-servant/servant/issues/305))

* Default JSON content type change to `application/json;charset=utf-8`.
  ([#263](https://github.com/haskell-servant/servant/issues/263))
  Related browser bugs:
  [Chromium](https://bugs.chromium.org/p/chromium/issues/detail?id=438464) and
  [Firefox](https://bugzilla.mozilla.org/show_bug.cgi?id=918742)

* `Accept` class may accept multiple content-types. `MimeUnrender` adopted as well.
  ([#613](https://github.com/haskell-servant/servant/pull/614)
  , [#615](https://github.com/haskell-servant/servant/pull/615))

0.9.1
------

* Added 'noHeader' function for *not* adding response headers.

0.9
---

* Added Eq, Show, Read, Generic and Ord instances to IsSecure
* BACKWARDS INCOMPATIBLE: replace use of `ToFromByteString` with `To/FromHttpApiData` for `GetHeaders/BuildHeadersTo`
* BACKWARDS INCOMPATIBLE: Moved `From/ToFormUrlEncoded` classes, which were renamed to `From/ToForm` to `http-api-data`

0.8.1
----

* Add `CaptureAll` combinator. Captures all of the remaining segments in a URL.
* Add `Servant.API.TypeLevel` module, with frequently used type-level
functionaliy.

0.8
---

* Minor fixes, documentation changes and cabal tweaks

0.7.1
-----

* Add module `Servant.Utils.Enter` (https://github.com/haskell-servant/servant/pull/478)
* Allow to set the same header multiple times in responses.

0.5
---

* Add `WithNamedConfig` combinator.
* Add `HttpVersion`, `IsSecure`, `RemoteHost` and `Vault` combinators
* Fix safeLink, so Header is not in fact required.
* Add more instances for (:<|>)
* Use `http-api-data` instead of `Servant.Common.Text`
* Remove matrix params.
* Add PlainText String MimeRender and MimeUnrender instances.
* Add new `Verbs` combinator, and make all existing and new verb combinators
type synonyms of it.
* Add `BasicAuth` combinator to support Basic authentication
* Add generalized authentication support

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
