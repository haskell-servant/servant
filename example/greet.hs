{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
import Data.Aeson
import Data.Proxy
import Data.Text(Text)
import GHC.Generics
import Servant.API
import Servant.Docs

-- * Example

-- | A greet message data type
newtype Greet = Greet { msg :: Text }
  deriving (Generic, Show)

instance FromJSON Greet
instance ToJSON Greet

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
                  "Get the greeting message in uppercase (true) or not (false). Default is false."
                  Normal

instance ToSample Greet where
  toSample = Just $ Greet "Hello, haskeller!"

  toSamples =
    [ ("If you use ?capital=true", Greet "HELLO, HASKELLER")
    , ("If you use ?capital=false", Greet "Hello, haskeller")
    ]

instance ToIntro "on proper introductions" where
  toIntro _ = DocIntro "On proper introductions." -- The title
    [ "Hello there."
    , "As documentation is usually written for humans, it's often useful \
      \to introduce concepts with a few words." ] -- Elements are paragraphs

instance ToIntro "on zebras" where
  toIntro _ = DocIntro "This title is below the last"
    [ "You'll also note that multiple intros are possible." ]


-- API specification
type TestApi =
       -- GET /hello/:name?capital={true, false}  returns a Greet as JSON
       "hello" :> Capture "name" Text :> QueryParam "capital" Bool :> Get Greet

       -- POST /greet with a Greet as JSON in the request body,
       --             returns a Greet as JSON
  :<|> "greet" :> ReqBody Greet :> Post Greet

       -- DELETE /greet/:greetid
  :<|> "greet" :> Capture "greetid" Text :> Delete

type IntroducedApi =
    Intro "on proper introductions" :> Intro "on zebras" :> TestApi

introducedApi :: Proxy IntroducedApi
introducedApi = Proxy

-- Generate the data that lets us have API docs. This
-- is derived from the type as well as from
-- the 'ToCapture', 'ToParam' and 'ToSample' instances from above.
docsGreet :: API
docsGreet = docs introducedApi

main :: IO ()
main = putStrLn $ markdown docsGreet
