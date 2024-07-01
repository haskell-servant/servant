# MultiVerb: Powerful endpoint types

`MultiVerb` allows you to represent an API endpoint with multiple response types, status codes and headers.

## Preliminaries

```haskell
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}

import GHC.Generics
import Generics.SOP qualified as GSOP
import Network.Wai.Handler.Warp as Warp

import Servant.API
import Servant.API.MultiVerb
import Servant.Server
import Servant.Server.Generic
```

## Writing an endpoint

Let us create an endpoint that captures an 'Int' and has the following logic:

* If the number is negative, we return status code 400 and an empty body;
* If the number is even, we return a 'Bool' in the response body;
* If the number is odd, we return another 'Int' in the response body.

Let us list all possible HTTP responses:
```haskell

type Responses =
  '[ RespondEmpty 400 "Negative"
   , Respond 200 "Odd number" Int
   , Respond 200 "Even number" Bool
   ]
```

Let us create the return type. We will create a sum type that lists the values on the Haskell side that correspond to our HTTP responses.
In order to tie the two types together, we will use a mechanism called `AsUnion` to create a correspondance between the two:

```haskell
data Result
  = NegativeNumber
  | Odd Int
  | Even Bool
  deriving stock (Generic)
  deriving (AsUnion Responses)
    via GenericAsUnion Responses Result

instance GSOP.Generic Result
```

These deriving statements above tie together the responses and the return values, and the order in which they are defined matters. For instance, if `Even` and `Odd` had switched places in the definition of `Result`, this would provoke an error:

```
• No instance for ‘AsConstructor
    ((:) @Type Int ('[] @Type)) (Respond 200 "Even number" Bool)’
        arising from the 'deriving' clause of a data type declaration
```

(_If you would prefer to write an intance of 'AsUnion' by yourself, read more in Annex 1 “Implementing AsUnion manually” section._)

Finally, let us write our endpoint description:

```haskell
type MultipleChoicesInt =
  Capture "int" Int
  :> MultiVerb
    'GET
    '[JSON]
    Responses
    Result
```

This piece of code is to be read as "Create an endpoint that captures an integer, and accepts a GET request with the `application/json` MIME type,
and can send one of the responses and associated result value."

### Implementing AsUnion manually

In the above example, the `AsUnion` typeclass is derived through the help of the `DerivingVia` mechanism,
and the `GenericAsUnion` wrapper.

If you would prefer implementing it yourself, you need to encode your responses as [Peano numbers](https://wiki.haskell.org/Peano_numbers),
augmented with the `I`(identity) combinator.

See how three options can be encoded as the Z (zero), S Z (successor to zero, so one),
and S (S Z) (the sucessor to the successor to zero, so two). This encoding is static, so we know in advance how to decode them to
Haskell datatypes. See the instance below for the encoding/decoding process:

```
instance AsUnion MultipleChoicesIntResponses MultipleChoicesIntResult where
  toUnion NegativeNumber =       Z (I ())
  toUnion (Even b)       =    S (Z (I b))
  toUnion (Odd i)        = S (S (Z (I i)))

  fromUnion       (Z (I ())) = NegativeNumber
  fromUnion    (S (Z (I b))) = Even b
  fromUnion (S (S (Z (I i)))) = Odd i
  fromUnion (S (S (S x))) = case x of {}
```

## Integration in a routing table

We want to integrate our endpoint into a wider routing table with another
endpoint: `version`, which returns the version of the API

```haskell
data Routes mode = Routes
  { choicesRoutes :: mode :- "choices" :> Choices
  , version :: mode :- "version" :> Get '[JSON] Int
  }
  deriving stock (Generic)
```

```haskell
type Choices = NamedRoutes Choices'
data Choices' mode = Choices'
  { choices :: mode :- MultipleChoicesInt
  }
  deriving stock (Generic)

choicesServer :: Choices' AsServer
choicesServer =
  Choices'
    { choices = choicesHandler
    }

routesServer :: Routes AsServer
routesServer =
  Routes
    { choicesRoutes = choicesServer
    , version = versionHandler
    }

choicesHandler :: Int -> Handler Result
choicesHandler parameter =
  if parameter < 0
  then pure NegativeNumber
  else
    if even parameter
    then pure $ Odd 3
    else pure $ Even True

versionHandler :: Handler Int
versionHandler = pure 1
```

We can now plug everything together:


```haskell
main :: IO ()
main = do
  putStrLn "Starting server on http://localhost:5000"
  let server = genericServe routesServer
  Warp.run 5000 server
```

Now let us run the server and observe how it behaves:

```
$ http http://localhost:5000/version
HTTP/1.1 200 OK
Content-Type: application/json;charset=utf-8
Date: Thu, 29 Aug 2024 14:22:20 GMT
Server: Warp/3.4.1
Transfer-Encoding: chunked

1
```


```
$ http http://localhost:5000/choices/3
HTTP/1.1 200 OK
Content-Type: application/json;charset=utf-8
Date: Thu, 29 Aug 2024 14:22:30 GMT
Server: Warp/3.4.1
Transfer-Encoding: chunked

true
```

```
$ http http://localhost:5000/choices/2
HTTP/1.1 200 OK
Content-Type: application/json;charset=utf-8
Date: Thu, 29 Aug 2024 14:22:33 GMT
Server: Warp/3.4.1
Transfer-Encoding: chunked

3
```

```
$ http http://localhost:5000/choices/-432
HTTP/1.1 400 Bad Request
Date: Thu, 29 Aug 2024 14:22:41 GMT
Server: Warp/3.4.1
Transfer-Encoding: chunked
```

You have now learned how to use the MultiVerb feature of Servant.

## Annex 1: Implementing AsUnion manually

Should you need to implement `AsUnion` manually, here is how to do it. `AsUnion` relies on 
two methods, `toUnion` and  `fromUnion`. They respectively encode your response type to, and decode it from, an inductive type that resembles a [Peano number](https://wiki.haskell.org/Peano_numbers).

Let's see it in action, with explanations below:

```haskell
instance => AsUnion MultipleChoicesIntResponses MultipleChoicesIntResult where
  toUnion NegativeNumber = Z (I ())
  toUnion (Even b)    = S (Z (I b))
  toUnion (Odd i)  = S (S (Z (I i)))

  fromUnion       (Z (I ())) = NegativeNumber
  fromUnion    (S (Z (I b))) = Even b
  fromUnion (S (S (Z (I i)))) = Odd i
  fromUnion (S (S (S x))) = case x of {}
```

### Encoding our data to a Union

Let's see how the implementation of `toUnion` works:

In the first equation for `toUnion`, `NegativeNumber` gets translated by `toUnion` into `Z (I ())`.
`I` is the constructor that holds a value. Here it is holds no meaningful value, because `NegativeNumber` does not have any argument.
In the tradition of Peano numbers, we start with the `Z`, for Zero.

Then `Even`, which holds a value, `b`, must then be encoded. Following Zero is its Successor, so we wrap the `Z` within a `S` constructor.
Since it has one argument, we can store it in the `I` constructor.

The pattern repeats with `Odd`, which hole a value (`i`) too. We add a `S`uccessor constructor to the previous encoding, 
and we store the value inside `I`.

### Decoding the Union

Since every member of our sum type was encoded to a unique form as an inductive data structure, we can decode them quite easily:

* `Z (I ())` is our `NegativeNumber` constructor;
* `(S (Z (I b)))` is `Even` with `b`;
* `(S (S (Z (I i))))` is `Odd` with `i`.

Finally, the last equation of `fromUnion` is here to satisfy GHC's pattern checker. It does not serve any functional purpose.
