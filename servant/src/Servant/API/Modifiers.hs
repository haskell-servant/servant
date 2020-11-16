{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
module Servant.API.Modifiers (
    -- * Required / optional argument
    Required, Optional,
    FoldRequired, FoldRequired',
    -- * Lenient / strict parsing
    Lenient, Strict,
    FoldLenient, FoldLenient',
    -- * Utilities
    RequiredArgument,
    foldRequiredArgument,
    unfoldRequiredArgument,
    RequestArgument,
    unfoldRequestArgument,
    ) where

import           Data.Proxy
                 (Proxy (..))
import           Data.Singletons.Bool
                 (SBool (..), SBoolI (..))
import           Data.Text
                 (Text)
import           Data.Type.Bool
                 (If)

-- | Required argument. Not wrapped.
data Required

-- | Optional argument. Wrapped in 'Maybe'.
data Optional

-- | Fold modifier list to decide whether argument is required.
--
-- >>> :kind! FoldRequired '[Required, Description "something"]
-- FoldRequired '[Required, Description "something"] :: Bool
-- = 'True
--
-- >>> :kind! FoldRequired '[Required, Optional]
-- FoldRequired '[Required, Optional] :: Bool
-- = 'False
--
-- >>> :kind! FoldRequired '[]
-- FoldRequired '[] :: Bool
-- = 'False
--
type FoldRequired mods = FoldRequired' 'False mods

-- | Implementation of 'FoldRequired'.
type family FoldRequired' (acc :: Bool) (mods :: [*]) :: Bool where
    FoldRequired' acc '[]                = acc
    FoldRequired' acc (Required ': mods) = FoldRequired' 'True mods
    FoldRequired' acc (Optional ': mods) = FoldRequired' 'False mods
    FoldRequired' acc (mod      ': mods) = FoldRequired' acc mods

-- | Leniently parsed argument, i.e. parsing never fail. Wrapped in @'Either' 'Text'@.
data Lenient

-- | Strictly parsed argument. Not wrapped.
data Strict

-- | Fold modifier list to decide whether argument should be parsed strictly or leniently.
--
-- >>> :kind! FoldLenient '[]
-- FoldLenient '[] :: Bool
-- = 'False
--
type FoldLenient mods = FoldLenient' 'False mods

-- | Implementation of 'FoldLenient'.
type family FoldLenient' (acc :: Bool) (mods ::  [*]) :: Bool where
    FoldLenient' acc '[]               = acc
    FoldLenient' acc (Lenient ': mods) = FoldLenient' 'True mods
    FoldLenient' acc (Strict  ': mods) = FoldLenient' 'False mods
    FoldLenient' acc (mod     ': mods) = FoldLenient' acc mods

-- | Helper type alias.
--
-- * 'Required' ↦ @a@
--
-- * 'Optional' ↦ @'Maybe' a@
--
type RequiredArgument mods a = If (FoldRequired mods) a (Maybe a)

-- | Fold a 'RequiredAgument' into a value
foldRequiredArgument
    :: forall mods a r. (SBoolI (FoldRequired mods))
    => Proxy mods
    -> (a -> r)        -- ^ 'Required'
    -> (Maybe a -> r)  -- ^ 'Optional'
    -> RequiredArgument mods a
    -> r
foldRequiredArgument _ f g mx =
    case (sbool :: SBool (FoldRequired mods), mx) of
        (STrue, x)  -> f x
        (SFalse, x) -> g x

-- | Unfold a value into a 'RequiredArgument'.
unfoldRequiredArgument
    :: forall mods m a. (Monad m, SBoolI (FoldRequired mods), SBoolI (FoldLenient mods))
    => Proxy mods
    -> m (RequiredArgument mods a)            -- ^ error when argument is required
    -> (Text -> m (RequiredArgument mods a))  -- ^ error when argument is strictly parsed
    -> Maybe (Either Text a)                  -- ^ value
    -> m (RequiredArgument mods a)
unfoldRequiredArgument _ errReq errSt mex =
    case (sbool :: SBool (FoldRequired mods), mex) of
        (STrue, Nothing)  -> errReq
        (SFalse, Nothing) -> return Nothing
        (STrue, Just ex)  -> either errSt return ex
        (SFalse, Just ex) -> either errSt (return . Just) ex

-- | Helper type alias.
--
-- By default argument is 'Optional' and 'Strict'.
--
-- * 'Required', 'Strict' ↦ @a@
--
-- * 'Required', 'Lenient' ↦ @'Either' 'Text' a@
--
-- * 'Optional', 'Strict' ↦ @'Maybe' a@
--
-- * 'Optional', 'Lenient' ↦ @'Maybe' ('Either' 'Text' a)@
--
type RequestArgument mods a =
    If (FoldRequired mods)
       (If (FoldLenient mods) (Either Text a) a)
       (Maybe (If (FoldLenient mods) (Either Text a) a))

-- | Unfold a value into a 'RequestArgument'.
unfoldRequestArgument
    :: forall mods m a. (Monad m, SBoolI (FoldRequired mods), SBoolI (FoldLenient mods))
    => Proxy mods
    -> m (RequestArgument mods a)            -- ^ error when argument is required
    -> (Text -> m (RequestArgument mods a))  -- ^ error when argument is strictly parsed
    -> Maybe (Either Text a)                 -- ^ value
    -> m (RequestArgument mods a)
unfoldRequestArgument _ errReq errSt mex =
    case (sbool :: SBool (FoldRequired mods), mex, sbool :: SBool (FoldLenient mods)) of
        (STrue,  Nothing, _)      -> errReq
        (SFalse, Nothing, _)      -> return Nothing
        (STrue,  Just ex, STrue)  -> return ex
        (STrue,  Just ex, SFalse) -> either errSt return ex
        (SFalse, Just ex, STrue)  -> return (Just ex)
        (SFalse, Just ex, SFalse) -> either errSt (return . Just) ex

-- $setup
-- >>> import Servant.API
