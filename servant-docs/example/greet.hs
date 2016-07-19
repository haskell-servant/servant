{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
import Control.Lens
import Data.Aeson
import Data.Proxy
import Data.String.Conversions
import Data.Text (Text)
import GHC.Generics
import Servant.API
import Servant.Docs

-- * Example

-- | A greet message data type
newtype Greet = Greet Text
  deriving (Generic, Show)

-- | We can get JSON support automatically. This will be used to parse
-- and encode a Greeting as 'JSON'.
instance FromJSON Greet
instance ToJSON Greet

-- | We can also implement 'MimeRender' for additional formats like 'PlainText'.
instance MimeRender PlainText Greet where
    mimeRender Proxy (Greet s) = "\"" <> cs s <> "\""

-- We add some useful annotations to our captures,
-- query parameters and request body to make the docs
-- really helpful.
instance ToCapture (Capture "name" Text) where
  toCapture _ = DocCapture "name" "name of the person to greet"

instance ToCapture (Capture "greetid" Text) where
  toCapture _ = DocCapture "greetid" "identifier of the greet msg to remove"

instance ToParam (QueryParam "capital" Bool) where
  toParam _ =
    DocQueryParam "capital"
                  ["true", "false"]
                  "Get the greeting message in uppercase (true) or not (false).\
                  \Default is false."
                  Normal

instance ToSample Greet where
  toSamples _ =
    [ ("If you use ?capital=true", Greet "HELLO, HASKELLER")
    , ("If you use ?capital=false", Greet "Hello, haskeller")
    ]

instance ToSample Int where
  toSamples _ = singleSample 1729

-- We define some introductory sections, these will appear at the top of the
-- documentation.
--
-- We pass them in with 'docsWith', below. If you only want to add
-- introductions, you may use 'docsWithIntros'
intro1 :: DocIntro
intro1 = DocIntro "On proper introductions." -- The title
    [ "Hello there."
    , "As documentation is usually written for humans, it's often useful \
      \to introduce concepts with a few words." ] -- Elements are paragraphs

intro2 :: DocIntro
intro2 = DocIntro "This title is below the last"
    [ "You'll also note that multiple intros are possible." ]


-- API specification
type TestApi =
       -- GET /hello/:name?capital={true, false}  returns a Greet as JSON or PlainText
       "hello" :> Capture "name" Text :> QueryParam "capital" Bool :> Get '[JSON, PlainText] Greet

       -- POST /greet with a Greet as JSON in the request body,
       --             returns a Greet as JSON
  :<|> "greet" :> ReqBody '[JSON] Greet :> Post '[JSON] (Headers '[Header "X-Example" Int] Greet)

       -- DELETE /greet/:greetid
  :<|> "greet" :> Capture "greetid" Text :> Delete '[JSON] NoContent

testApi :: Proxy TestApi
testApi = Proxy

-- Build some extra information for the DELETE /greet/:greetid endpoint. We
-- want to add documentation about a secret unicorn header and some extra
-- notes.
extra :: ExtraInfo TestApi
extra =
    extraInfo (Proxy :: Proxy ("greet" :> Capture "greetid" Text :> Delete '[JSON] NoContent)) $
             defAction & headers <>~ ["unicorns"]
                       & notes   <>~ [ DocNote "Title" ["This is some text"]
                                     , DocNote "Second secton" ["And some more"]
                                     ]

-- Generate the data that lets us have API docs. This
-- is derived from the type as well as from
-- the 'ToCapture', 'ToParam' and 'ToSample' instances from above.
--
-- If you didn't want intros and extra information, you could just call:
--
-- > docs testAPI :: API
docsGreet :: API
docsGreet = docsWith defaultDocOptions [intro1, intro2] extra testApi

main :: IO ()
main = putStrLn $ markdown docsGreet
