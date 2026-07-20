# Migrating to explicit client query-string encoding

Servant client requests now represent query strings with
[`PartialEscapeQuery`](https://hackage.haskell.org/package/http-types/docs/Network-HTTP-Types-URI.html#t:PartialEscapeQuery)
instead of `Query`. Each section of a parameter value therefore says whether it
still needs URL encoding. This removes the previous ambiguity where a
`ByteString` value could be interpreted as either decoded data or wire-ready,
percent-encoded data.

This is a breaking API change for custom client combinators, client middleware
that edits `Request`, custom `RunClient` backends, and clients of the
`QueryString` combinator. Ordinary clients generated from `QueryParam`,
`QueryParams`, `QueryFlag`, and `DeepQuery` API types do not need call-site
changes and now handle reserved characters consistently.

## Choosing `QE` or `QN`

Import the query types from `http-types`:

```haskell
import Network.HTTP.Types
  ( EscapeItem (QE, QN)
  , PartialEscapeQuery
  , renderQueryPartialEscape
  )
```

Add `http-types >= 0.12.4 && < 0.13` as a direct dependency when your package
uses these names directly.

Use the constructors as follows:

| Value representation | New value | Result on the wire |
| --- | --- | --- |
| Decoded bytes that still need URL encoding | `[QE "a + b"]` | `a%20%2B%20b` |
| Already encoded or intentionally literal syntax | `[QN "a%20%2B%20b"]` | `a%20%2B%20b` |
| A value-less parameter | `[]` | `flag` |
| An empty value | `[QE ""]` | `empty=` |
| Mixed data and literal query syntax | `[QE "+", QN "+language:haskell"]` | `%2B+language:haskell` |

`QN` bypasses all escaping. Only use it for trusted, validated syntax or bytes
that are already correctly percent-encoded. Passing untrusted decoded input to
`QN` can allow characters such as `&` and `=` to change the query structure.

## Migrating `Request` and custom combinators

The relevant `servant-client-core` types changed conceptually as follows:

```haskell
-- Before
requestQueryString :: Seq QueryItem
appendToQueryString :: Text -> Maybe ByteString -> Request -> Request
setQueryString :: Query -> Request -> Request

-- After
requestQueryString :: Seq PartialEscapeQueryItem
appendToQueryString :: Text -> [EscapeItem] -> Request -> Request
setQueryString :: PartialEscapeQuery -> Request -> Request
```

For decoded values, replace `Just value` with `[QE value]`. For values that your
code already encoded, replace it with `[QN value]`. Replace `Nothing` with `[]`.
For example:

```haskell
-- Before
appendToQueryString "search" (Just rawSearch) request
appendToQueryString "encoded" (Just encodedValue) request
appendToQueryString "verbose" Nothing request

-- After
appendToQueryString "search" [QE rawSearch] request
appendToQueryString "encoded" [QN encodedValue] request
appendToQueryString "verbose" [] request
```

`encodeQueryParamValue` returns wire-ready bytes using
`ToHttpApiData.toEncodedQueryParam`, so tag its result with `QN`:

```haskell
appendToQueryString
  "limit"
  [QN (encodeQueryParamValue limit)]
  request
```

This is the pattern used by Servant's `QueryParam` and `QueryParams` client
instances. Unlike earlier releases, a custom `toEncodedQueryParam`
implementation is now honored; query-specific encodings should no longer be
placed in `toEncodedUrlPiece`.

For example, an API-specific search language can deliberately preserve trusted
`+` and `:` syntax in a normal `QueryParam`:

```haskell
import Data.ByteString.Builder (byteString)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Web.HttpApiData (ToHttpApiData (..))

newtype SearchExpression = SearchExpression Text

instance ToHttpApiData SearchExpression where
  toQueryParam (SearchExpression value) = value
  toEncodedQueryParam (SearchExpression value) =
    byteString (encodeUtf8 value)
```

Servant treats the result of `toEncodedQueryParam` as `QN`, so the custom wire
syntax is not encoded again. Validate values before using this technique; use
the default `toEncodedQueryParam` behavior for ordinary user input.

## Migrating `QueryString` clients

The generated client argument for `QueryString` is now `PartialEscapeQuery`:

```haskell
type SearchAPI = "search" :> QueryString :> Get '[JSON] Results

search :: PartialEscapeQuery -> ClientM Results
search = client (Proxy @SearchAPI)
```

Convert an old `Query` by deciding how each old value was represented:

```haskell
-- Before: values were ambiguous
oldQuery =
  [ ("q", Just "haskell + servant")
  , ("verbose", Nothing)
  ]

-- After: these are decoded values
newQuery =
  [ ("q", [QE "haskell + servant"])
  , ("verbose", [])
  ]
```

If an old value was already percent-encoded, use `QN` instead of `QE` to avoid
double encoding.

## Migrating custom client backends

Render the new request field with `renderQueryPartialEscape`:

```haskell
import Data.Foldable (toList)
import Network.HTTP.Types (renderQueryPartialEscape)

renderRequestQuery :: Request -> ByteString
renderRequestQuery =
  renderQueryPartialEscape True . toList . requestQueryString
```

The `True` argument includes the leading `?` only when the query is non-empty.
Do not apply a second `renderQuery`, `urlEncode`, or blanket decoding pass to the
result.

## `DeepQuery` and server handlers

`ToDeepQuery` continues to return decoded `Text`. Servant now marks those values
for encoding before rendering, so spaces, literal `+`, `&`, `=`, Unicode, and
other reserved data round-trip correctly.

On the server, `QueryString` handlers still receive the `http-types` `Query`
shape, now documented through the `DecodedQuery` alias:

```haskell
handler :: DecodedQuery -> Handler Result
```

WAI has already URL-decoded both parameter names and values before the handler
runs, including converting a wire-level `+` to a space. Likewise,
`FromDeepQuery` receives decoded field names and values. Remove any workaround
that calls `urlDecode` inside a `QueryString` handler or `FromDeepQuery`
instance, as it can corrupt literal percent sequences or plus signs.

## Behavior checklist

After migrating, verify these cases in downstream tests:

- a literal `+` in decoded input arrives at the server as `+`, not a space;
- spaces, `&`, `=`, `%`, Unicode, and repeated parameters round-trip;
- value-less `flag` remains distinct from `empty=` if your server cares;
- already encoded values use `QN` exactly once;
- custom backends include a leading `?` only for non-empty queries.

This change implements the direction proposed in
[servant#1779](https://github.com/haskell-servant/servant/issues/1779) and also
resolves the representation ambiguity behind
[servant#1626](https://github.com/haskell-servant/servant/issues/1626) and the
literal-query-syntax use case in
[servant#1100](https://github.com/haskell-servant/servant/issues/1100).
