-- | This module contains QuickCheck-related logic.
module Servant.QuickCheck.Internal.Testable where

import           Control.Concurrent.MVar  (MVar, modifyMVar_, newMVar, readMVar)
import           Control.Monad.Except     (runExceptT)
import           GHC.Generics             (Generic)
import           Network.HTTP.Client      (Request, RequestBody (..),
                                           requestBody)
import           Servant.API              ((:<|>)(..))
import           Servant.Client           (ServantError (..), ClientM)
import           System.IO.Unsafe         (unsafePerformIO)
import           Test.QuickCheck          (Arbitrary (..), discard)
import           Test.QuickCheck.Property (Testable (..), forAllShrink,
                                           ioProperty, (.&.))

import Servant.QuickCheck.Internal.Predicates


-- * ShouldMatch

-- | Two corresponding client functions. Used for checking that APIs match.
data ShouldMatch a = ShouldMatch a a
  deriving (Eq, Show, Read, Generic)

instance (Show a, Eq a) => Testable (ShouldMatch (ClientM a)) where
    property (ShouldMatch e1 e2) = ioProperty $ do
        e1' <- runExceptT e1
        e2' <- runExceptT e2
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

instance (Arbitrary a, Show a, Testable (ShouldMatch b))
      => Testable (ShouldMatch (a -> b)) where
    property (ShouldMatch f1 f2) = forAllShrink arbitrary shrink go
      where go x = ShouldMatch (f1 x) (f2 x)

instance (Testable (ShouldMatch a), Testable (ShouldMatch b))
      => Testable (ShouldMatch (a :<|> b)) where
    property (ShouldMatch (a1 :<|> b1) (a2 :<|> b2))
      = property (ShouldMatch a1 a2) .&. property (ShouldMatch b1 b2)

-- * ShouldSatisfy

data ShouldSatisfy filter expect a = ShouldSatisfy
  { ssVal :: a
  , ssFilter :: Predicates filter
  , ssExpect :: Predicates expect
  } deriving (Functor)

instance (Show a, Eq a, HasPredicate expect (Either ServantError a))
      => Testable (ShouldSatisfy filter expect (ClientM a)) where
    property (ShouldSatisfy a _ e) = ioProperty $ do
        a' <- runExceptT a
        modifyMVar_ currentReq $ \x -> case x of
          Nothing      -> error "impossible"
          Just (x', _) -> return $ Just (x', show a')
        return $ getPredicate e a'

instance ( Arbitrary a, Show a, Testable (ShouldSatisfy filter expect b)
         , HasPredicate filter a)
      => Testable (ShouldSatisfy filter expect (a -> b)) where
    property (ShouldSatisfy g f e) = forAllShrink arbitrary shrink go
        where go x | getPredicate f x = ShouldSatisfy (g x) f e
                   | otherwise        = discard

instance ( Testable (ShouldSatisfy filter expect a)
         , Testable (ShouldSatisfy filter expect b))
      => Testable (ShouldSatisfy filter expect (a :<|> b)) where
    property (ShouldSatisfy (a :<|> b) f e)
      = property (ShouldSatisfy a f e) .&. property (ShouldSatisfy b f e)

-- * Utils

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
