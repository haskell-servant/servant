{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE OverloadedStrings   #-}
module Servant.Swagger.Internal.Test where

import           Data.Aeson                         (ToJSON (..))
import           Data.Aeson.Encode.Pretty           (encodePretty', defConfig,
                                                     confCompare)
import           Data.Swagger                       (Pattern, ToSchema,
                                                     toSchema)
import           Data.Swagger.Schema.Validation
import           Data.Text                          (Text)
import qualified Data.Text.Lazy                     as TL
import qualified Data.Text.Lazy.Encoding            as TL
import           Data.Typeable
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck                    (Arbitrary, Property,
                                                     counterexample, property)

import           Servant.API
import           Servant.Swagger.Internal.TypeLevel

-- $setup
-- >>> import Control.Applicative
-- >>> import GHC.Generics
-- >>> import Test.QuickCheck
-- >>> :set -XDeriveGeneric
-- >>> :set -XGeneralizedNewtypeDeriving
-- >>> :set -XDataKinds
-- >>> :set -XTypeOperators

-- | Verify that every type used with @'JSON'@ content type in a servant API
-- has compatible @'ToJSON'@ and @'ToSchema'@ instances using @'validateToJSON'@.
--
-- /NOTE:/ @'validateEveryToJSON'@ does not perform string pattern validation.
-- See @'validateEveryToJSONWithPatternChecker'@.
--
-- @'validateEveryToJSON'@ will produce one @'prop'@ specification for every type in the API.
-- Each type only gets one test, even if it occurs multiple times in the API.
--
-- >>> data User = User { name :: String, age :: Maybe Int } deriving (Show, Generic, Typeable)
-- >>> newtype UserId = UserId String deriving (Show, Generic, Typeable, ToJSON, Arbitrary)
-- >>> instance ToJSON User
-- >>> instance ToSchema User
-- >>> instance ToSchema UserId
-- >>> instance Arbitrary User where arbitrary = User <$> arbitrary <*> arbitrary
-- >>> type UserAPI = (Capture "user_id" UserId :> Get '[JSON] User) :<|> (ReqBody '[JSON] User :> Post '[JSON] UserId)
--
-- >>> hspec $ context "ToJSON matches ToSchema" $ validateEveryToJSON (Proxy :: Proxy UserAPI)
-- <BLANKLINE>
-- ToJSON matches ToSchema
--   User
-- ...
--   UserId
-- ...
-- Finished in ... seconds
-- 2 examples, 0 failures
--
-- For the test to compile all body types should have the following instances:
--
--    * @'ToJSON'@ and @'ToSchema'@ are used to perform the validation;
--    * @'Typeable'@ is used to name the test for each type;
--    * @'Show'@ is used to display value for which @'ToJSON'@ does not satisfy @'ToSchema'@.
--    * @'Arbitrary'@ is used to arbitrarily generate values.
--
-- If any of the instances is missing, you'll get a descriptive type error:
--
-- >>> data Contact = Contact { fullname :: String, phone :: Integer } deriving (Show, Generic)
-- >>> instance ToJSON Contact
-- >>> instance ToSchema Contact
-- >>> type ContactAPI = Get '[JSON] Contact
-- >>> hspec $ validateEveryToJSON (Proxy :: Proxy ContactAPI)
-- ...
-- ...No instance for (Arbitrary Contact)
-- ...  arising from a use of ‘validateEveryToJSON’
-- ...
validateEveryToJSON
  :: forall proxy api .
     TMap (Every [Typeable, Show, Arbitrary, ToJSON, ToSchema])
          (BodyTypes JSON api)
  => proxy api   -- ^ Servant API.
  -> Spec
validateEveryToJSON _ = props
  (Proxy :: Proxy [ToJSON, ToSchema])
  (maybeCounterExample . prettyValidateWith validateToJSON)
  (Proxy :: Proxy (BodyTypes JSON api))

-- | Verify that every type used with @'JSON'@ content type in a servant API
-- has compatible @'ToJSON'@ and @'ToSchema'@ instances using @'validateToJSONWithPatternChecker'@.
--
-- For validation without patterns see @'validateEveryToJSON'@.
validateEveryToJSONWithPatternChecker :: forall proxy api. TMap (Every [Typeable, Show, Arbitrary, ToJSON, ToSchema]) (BodyTypes JSON api) =>
  (Pattern -> Text -> Bool)   -- ^ @'Pattern'@ checker.
  -> proxy api                -- ^ Servant API.
  -> Spec
validateEveryToJSONWithPatternChecker checker _ = props
  (Proxy :: Proxy [ToJSON, ToSchema])
  (maybeCounterExample . prettyValidateWith (validateToJSONWithPatternChecker checker))
  (Proxy :: Proxy (BodyTypes JSON api))

-- * QuickCheck-related stuff

-- | Construct property tests for each type in a list.
-- The name for each property is the name of the corresponding type.
--
-- >>> :{
--  hspec $
--    context "read . show == id" $
--      props
--        (Proxy :: Proxy [Eq, Show, Read])
--        (\x -> read (show x) === x)
--        (Proxy :: Proxy [Bool, Int, String])
-- :}
-- <BLANKLINE>
-- read . show == id
--   Bool
-- ...
--   Int
-- ...
--   [Char]
-- ...
-- Finished in ... seconds
-- 3 examples, 0 failures
props :: forall p p'' cs xs. TMap (Every (Typeable ': Show ': Arbitrary ': cs)) xs =>
  p cs                                          -- ^ A list of constraints.
  -> (forall x. EveryTF cs x => x -> Property)  -- ^ Property predicate.
  -> p'' xs                                     -- ^ A list of types.
  -> Spec
props _ f px = sequence_ specs
  where
    specs :: [Spec]
    specs = tmapEvery (Proxy :: Proxy (Typeable ': Show ': Arbitrary ': cs)) aprop px

    aprop :: forall p' a. (EveryTF cs a, Typeable a, Show a, Arbitrary a) => p' a -> Spec
    aprop _ = prop (show (typeOf (undefined :: a))) (f :: a -> Property)

-- | Pretty print validation errors
-- together with actual JSON and Swagger Schema
-- (using 'encodePretty').
--
-- >>> import Data.Aeson
-- >>> import Data.Foldable (traverse_)
-- >>> data Person = Person { name :: String, phone :: Integer } deriving (Generic)
-- >>> instance ToJSON Person where toJSON p = object [ "name" .= name p ]
-- >>> instance ToSchema Person
-- >>> let person = Person { name = "John", phone = 123456 }
-- >>> traverse_ putStrLn $ prettyValidateWith validateToJSON person
-- Validation against the schema fails:
--   * property "phone" is required, but not found in "{\"name\":\"John\"}"
-- <BLANKLINE>
-- JSON value:
-- {
--     "name": "John"
-- }
-- <BLANKLINE>
-- Swagger Schema:
-- {
--     "properties": {
--         "name": {
--             "type": "string"
--         },
--         "phone": {
--             "type": "integer"
--         }
--     },
--     "required": [
--         "name",
--         "phone"
--     ],
--     "type": "object"
-- }
-- <BLANKLINE>
--
-- FIXME: this belongs in "Data.Swagger.Schema.Validation" (in @swagger2@).
prettyValidateWith
  :: forall a. (ToJSON a, ToSchema a)
  => (a -> [ValidationError]) -> a -> Maybe String
prettyValidateWith f x =
  case f x of
    []      -> Nothing
    errors  -> Just $ unlines
      [ "Validation against the schema fails:"
      , unlines (map ("  * " ++) errors)
      , "JSON value:"
      , ppJSONString json
      , ""
      , "Swagger Schema:"
      , ppJSONString (toJSON schema)
      ]
  where
    ppJSONString = TL.unpack . TL.decodeUtf8 . encodePretty' ppCfg
    ppCfg = defConfig { confCompare = compare }

    json   = toJSON x
    schema = toSchema (Proxy :: Proxy a)

-- | Provide a counterexample if there is any.
maybeCounterExample :: Maybe String -> Property
maybeCounterExample Nothing  = property True
maybeCounterExample (Just s) = counterexample s (property False)
