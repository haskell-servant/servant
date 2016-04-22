-- | This module contains QuickCheck-related logic.
module Servant.QuickCheck.Internal.Testable where

import           Control.Concurrent.MVar  (MVar, modifyMVar_, newMVar, readMVar)
import           Control.Monad.Except     (runExceptT, ExceptT)
import           GHC.Generics             (Generic)
import           Network.HTTP.Client      (Request, RequestBody (..),
                                           requestBody, Manager)
import           Servant.API              ((:<|>)(..))
import           Servant.Client           (ServantError (..), BaseUrl)
import           System.IO.Unsafe         (unsafePerformIO)
import           Test.QuickCheck          (Arbitrary (..), discard)
import           Test.QuickCheck.Property (Testable (..), forAllShrink,
                                           ioProperty, (.&.))

import Servant.QuickCheck.Internal.Predicates


type FinalClient a = Manager -> BaseUrl -> ExceptT ServantError IO a

-- * ShouldMatch

-- | Two corresponding client functions. Used for checking that APIs match.
data ShouldMatch a = ShouldMatch
  { smClient :: a
  , smManager :: Manager
  , smBaseUrls :: (BaseUrl, BaseUrl)
  } deriving (Functor, Generic)

instance {-# OVERLAPPING #-} (Show a, Eq a)
    => Testable (ShouldMatch (FinalClient a)) where
    property sm = ioProperty $ do
        let (burl1, burl2) = smBaseUrls sm
        e1' <- runExceptT $ smClient sm (smManager sm) burl1
        e2' <- runExceptT $ smClient sm (smManager sm) burl2
        modifyMVar_ currentReq $ \x -> case x of
          Nothing      -> error "impossible"
          Just (x', _) -> return $ Just (x', "LHS:\n" ++ show e1'
                                        ++ "\nRHS:\n" ++ show e2')
        case (e1', e2') of
          (Right v1, Right v2) -> return $ v1 == v2
          (Left (FailureResponse a1 b1 c1), Left (FailureResponse a2 b2 c2)) ->
             return $ a1 == a2 && b1 == b2 && c1 == c2
          (err1, err2) -> error $ "Exception response:"
                               ++ "\nLHS:\n" ++ show err1
                               ++ "\nRHS:\n" ++ show err2

instance {-# OVERLAPPABLE #-} (Arbitrary a, Show a, Testable (ShouldMatch b))
      => Testable (ShouldMatch (a -> b)) where
    property sm = forAllShrink arbitrary shrink go
      where go x = ($ x) <$> sm

instance (Testable (ShouldMatch a), Testable (ShouldMatch b))
      => Testable (ShouldMatch (a :<|> b)) where
    property sm = property (fstAlt <$> sm) .&. property (sndAlt <$> sm)

-- * ShouldSatisfy

data ShouldSatisfy filter expect a = ShouldSatisfy
  { ssVal :: a
  , ssFilter :: Predicates filter
  , ssExpect :: Predicates expect
  , ssManager :: Manager
  , ssBaseUrl :: BaseUrl
  } deriving (Functor)

instance {-# OVERLAPPING #-}
     (Show a, Eq a, HasPredicate expect (Either ServantError a))
      => Testable (ShouldSatisfy filter expect (FinalClient a)) where
    property ss = ioProperty $ do
        a' <- runExceptT $ ssVal ss (ssManager ss) (ssBaseUrl ss)
        modifyMVar_ currentReq $ \x -> case x of
          Nothing      -> error "impossible"
          Just (x', _) -> return $ Just (x', show a')
        return $ getPredicate (ssExpect ss) a'

instance {-# OVERLAPPABLE #-}
        ( Arbitrary a, Show a, Testable (ShouldSatisfy filter expect b)
        , HasPredicate filter a)
      => Testable (ShouldSatisfy filter expect (a -> b)) where
    property ss = forAllShrink arbitrary shrink go
        where go x | getPredicate (ssFilter ss) x = ($ x) <$> ss
                   | otherwise                    = discard

instance ( Testable (ShouldSatisfy filter expect a)
         , Testable (ShouldSatisfy filter expect b))
      => Testable (ShouldSatisfy filter expect (a :<|> b)) where
    property ss = property (fstAlt <$> ss) .&. property (sndAlt <$> ss)

-- * Utils

fstAlt :: (a :<|> b) -> a
fstAlt (a :<|> _) = a

sndAlt :: (a :<|> b) -> b
sndAlt (_ :<|> b) = b

-- Used to store the current request and response so that in case of failure we
-- have the failing test in a user-friendly form.
currentReq :: MVar (Maybe (Request, String))
currentReq = unsafePerformIO $ newMVar Nothing
{-# NOINLINE currentReq #-}

prettyErr :: IO String
prettyErr = do
    Just (req, resp) <- readMVar currentReq
    return $ show req ++ "Body:\n" ++ showReqBody (requestBody req)
          ++ "\n\nResponse:\n" ++ resp
  where
    showReqBody (RequestBodyLBS x) = show x
    showReqBody _                  = error "expecting RequestBodyLBS"
