{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
module Servant.LinksSpec where

import           GHC.Generics
                 (Generic)
import           Data.Proxy
                 (Proxy (..))
import           Data.String
                 (fromString)
import qualified Data.Text as T
import           Network.URI
                 (escapeURIString)
import           Test.Hspec
                 (Expectation, Spec, describe, it, shouldBe)

import           Servant.API
import           Servant.API.QueryString 
                 (ToDeepQuery (toDeepQuery))
import           Servant.Links
import           Servant.Test.ComprehensiveAPI
                 (comprehensiveAPIWithoutRaw)

type TestApi =
  -- Capture and query params
       "hello" :> Capture "name" String :> QueryParam "capital" Bool :> Delete '[JSON] NoContent
  :<|> "hi"    :> Capture "name" String :> QueryParam' '[Required] "capital" Bool :> Delete '[JSON] NoContent
  :<|> "all" :> CaptureAll "names" String :> Get '[JSON] NoContent

  -- Flags
  :<|> "balls" :> QueryFlag "bouncy" :> QueryFlag "fast" :> Delete '[JSON] NoContent

  -- Fragment
  :<|> "say" :> Fragment String :> Get '[JSON] NoContent

  -- UVerb
  :<|> "uverb-example" :> UVerb 'GET '[JSON] '[WithStatus 200 NoContent]

  -- DeepQuery
  :<|> "books" :> DeepQuery "filter" BookQuery :> Get '[JSON] [Book]

  -- All of the verbs
  :<|> "get" :> Get '[JSON] NoContent
  :<|> "put" :> Put '[JSON] NoContent
  :<|> "post" :> ReqBody '[JSON] Bool :> Post '[JSON] NoContent
  :<|> "delete" :> Header "ponies" String :> Delete '[JSON] NoContent
  :<|> "raw" :> Raw
  :<|> NoEndpoint

type LinkableApi =
       "all" :> CaptureAll "names" String :> Get '[JSON] NoContent
  :<|> "get" :> Get '[JSON] NoContent

apiLink :: (IsElem endpoint TestApi, HasLink endpoint)
         => Proxy endpoint -> MkLink endpoint Link
apiLink = safeLink (Proxy :: Proxy TestApi)

data Book
data BookQuery = BookQuery 
  { author :: String
  , year   :: Int
  } deriving (Generic, Show, Eq)

instance ToDeepQuery BookQuery where
  toDeepQuery (BookQuery author year) =
    [ ([T.pack "author"], Just $ toQueryParam author)
    , ([T.pack "year"], Just $ toQueryParam year)
    ]


newtype QuuxRoutes mode = QuuxRoutes
  { corge :: mode :- "corge" :> Post '[PlainText] NoContent
  } deriving Generic

newtype WaldoRoutes mode = WaldoRoutes
  { waldo :: mode :- "waldo" :> Get '[JSON] NoContent
  } deriving Generic

data FooRoutes mode = FooRoutes
  { baz :: mode :- "baz" :> Get '[JSON] NoContent
  , qux :: mode :- "qux" :> NamedRoutes QuuxRoutes
  , quux :: mode :- "quux" :> QueryParam "grault" String :> Get '[JSON] NoContent
  , garply :: mode :- "garply" :> Capture "garply" String
           :> Capture "garplyNum" Int :> NamedRoutes WaldoRoutes
  } deriving Generic

data BaseRoutes mode = BaseRoutes
  { foo :: mode :- "foo" :> NamedRoutes FooRoutes
  , bar :: mode :- "bar" :> Get '[JSON] NoContent
  } deriving Generic

recordApiLink
  :: (IsElem endpoint (NamedRoutes BaseRoutes), HasLink endpoint)
  => Proxy endpoint -> MkLink endpoint Link
recordApiLink = safeLink (Proxy :: Proxy (NamedRoutes BaseRoutes))

-- | Convert a link to a URI and ensure that this maps to the given string
-- given string
shouldBeLink :: Link -> String -> Expectation
shouldBeLink link expected =
    toUrlPiece link `shouldBe` fromString expected

isNotBracket :: Char -> Bool
isNotBracket c = c /= '[' && c /= ']'

(//) :: a -> (a -> b) -> b
x // f = f x
infixl 1 //

(/:) :: (a -> b -> c) -> b -> a -> c
(/:) = flip
infixl 2 /:

spec :: Spec
spec = describe "Servant.Links" $ do
    it "generates correct links for capture query params" $ do
        let l1 = Proxy :: Proxy ("hello" :> Capture "name" String :> Delete '[JSON] NoContent)
        apiLink l1 "hi" `shouldBeLink` "hello/hi"

        let l2 = Proxy :: Proxy ("hello" :> Capture "name" String
                                         :> QueryParam "capital" Bool
                                         :> Delete '[JSON] NoContent)
        apiLink l2 "bye" (Just True) `shouldBeLink` "hello/bye?capital=true"

        let l4 = Proxy :: Proxy ("hi" :> Capture "name" String
                                      :> QueryParam' '[Required] "capital" Bool
                                      :> Delete '[JSON] NoContent)
        apiLink l4 "privet" False `shouldBeLink` "hi/privet?capital=false"

    it "generates correct links for CaptureAll" $ do
        apiLink (Proxy :: Proxy ("all" :> CaptureAll "names" String :> Get '[JSON] NoContent))
          ["roads", "lead", "to", "rome"]
          `shouldBeLink` "all/roads/lead/to/rome"

    it "generated correct links for UVerbs" $ do
      apiLink (Proxy :: Proxy ("uverb-example" :> UVerb 'GET '[JSON] '[WithStatus 200 NoContent]))
        `shouldBeLink` "uverb-example"

    it "generates correct links for query flags" $ do
        let l1 = Proxy :: Proxy ("balls" :> QueryFlag "bouncy"
                                         :> QueryFlag "fast" :> Delete '[JSON] NoContent)
        apiLink l1 True True `shouldBeLink` "balls?bouncy&fast"
        apiLink l1 False True `shouldBeLink` "balls?fast"

    it "generates correct link for fragment" $ do
        let l1 = Proxy :: Proxy ("say" :> Fragment String :> Get '[JSON] NoContent)
        apiLink l1 "something" `shouldBeLink` "say#something"

    it "generates correct links for all of the verbs" $ do
        apiLink (Proxy :: Proxy ("get" :> Get '[JSON] NoContent)) `shouldBeLink` "get"
        apiLink (Proxy :: Proxy ("put" :> Put '[JSON] NoContent)) `shouldBeLink` "put"
        apiLink (Proxy :: Proxy ("post" :> Post '[JSON] NoContent)) `shouldBeLink` "post"
        apiLink (Proxy :: Proxy ("delete" :> Delete '[JSON] NoContent)) `shouldBeLink` "delete"
        apiLink (Proxy :: Proxy ("raw" :> Raw)) `shouldBeLink` "raw"

    it "can generate all links for an API that has only linkable endpoints" $ do
        let (allNames :<|> simple) = allLinks (Proxy :: Proxy LinkableApi)
        simple `shouldBeLink` "get"
        allNames ["Seneca", "Aurelius"] `shouldBeLink` "all/Seneca/Aurelius"

    it "can generate all links for ComprehensiveAPIWithoutRaw" $ do
        let firstLink :<|> _ = allLinks comprehensiveAPIWithoutRaw
        firstLink `shouldBeLink` ""

    it "Generate links from record fields accessors" $ do
      fieldLink bar `shouldBeLink` "bar"
      (fieldLink foo // baz)  `shouldBeLink` "foo/baz"
      (fieldLink foo // qux // corge) `shouldBeLink` "foo/qux/corge"
      (fieldLink foo // quux /: Nothing) `shouldBeLink` "foo/quux"
      (fieldLink foo // quux /: Just "floop") `shouldBeLink` "foo/quux?grault=floop"
      (fieldLink foo // garply /: "captureme" /: 42 // waldo)
        `shouldBeLink` "foo/garply/captureme/42/waldo"

    it "generated correct links for DeepQuery" $ do
      let bFilter = Proxy :: Proxy ("books" :> DeepQuery "filter" BookQuery :> Get '[JSON] [Book])
      let exampleQuery = BookQuery { author = "Herbert", year = 1965 }
      let expected = escapeURIString isNotBracket "books?filter[author]=Herbert&filter[year]=1965"
      apiLink bFilter exampleQuery `shouldBeLink` expected

    it "Check links from record fields" $ do
      let sub1 = Proxy :: Proxy ("bar" :> Get '[JSON] NoContent)
      recordApiLink sub1 `shouldBeLink` "bar"

      let sub2 = Proxy :: Proxy ("foo" :> "baz" :> Get '[JSON] NoContent)
      recordApiLink sub2 `shouldBeLink` "foo/baz"

      let sub3 = Proxy :: Proxy ("foo" :> "quux" :> QueryParam "grault" String
                                       :> Get '[JSON] NoContent)
      recordApiLink sub3 (Just "floop") `shouldBeLink` "foo/quux?grault=floop"

      let sub4 :: Proxy ("foo" :> "garply" :> Capture "garplyText" String
                       :> Capture "garplyInt" Int :> "waldo"
                       :> Get '[JSON] NoContent)
          sub4 = Proxy
      recordApiLink sub4 "captureme" 42
        `shouldBeLink` "foo/garply/captureme/42/waldo"

-- The doctests below aren't run on CI, setting that up is tricky.
-- They are run by makefile rule, however.

-- |
-- Before https://github.com/CRogers/should-not-typecheck/issues/5 is fixed,
-- we'll just use doctest
--
-- with TypeError comparing for errors is difficult.
--
-- >>> apiLink (Proxy :: Proxy WrongPath)
-- ...
-- ......:...:...
-- ...
--
-- >>> apiLink (Proxy :: Proxy WrongReturnType)
-- ...
-- ...Could not deduce...
-- ...
--
-- >>> apiLink (Proxy :: Proxy WrongContentType)
-- ...
-- ......:...:...
-- ...
--
-- >>> apiLink (Proxy :: Proxy WrongMethod)
-- ...
-- ...Could not deduce...
-- ...
--
-- >>> apiLink (Proxy :: Proxy NotALink)
-- ...
-- ...Could not deduce...
-- ...
--
-- >>> linkURI $ apiLink (Proxy :: Proxy NoEndpoint)
-- ...
-- <interactive>...
-- ...
--
-- sanity check
-- >>> toUrlPiece $ apiLink (Proxy :: Proxy AllGood)
-- "get"
type WrongPath = "getTypo" :> Get '[JSON] NoContent
type WrongReturnType = "get" :> Get '[JSON] Bool
type WrongContentType = "get" :> Get '[OctetStream] NoContent
type WrongMethod = "get" :> Post '[JSON] NoContent
type NotALink = "hello" :> ReqBody '[JSON] Bool :> Get '[JSON] Bool
type AllGood = "get" :> Get '[JSON] NoContent
type NoEndpoint = "empty" :> EmptyAPI
