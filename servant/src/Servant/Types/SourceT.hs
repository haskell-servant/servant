{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Servant.Types.SourceT where

import           Control.Monad.Except
                 (ExceptT (..), runExceptT, throwError)
import           Control.Monad.Morph
                 (MFunctor (..))
import           Control.Monad.Trans.Class
                 (MonadTrans (..))
import qualified Data.Attoparsec.ByteString as A
import qualified Data.ByteString            as BS
import           Data.Functor.Classes
                 (Show1 (..), showsBinaryWith, showsPrec1, showsUnaryWith)
import           Data.Functor.Identity
                 (Identity (..))
import           Prelude ()
import           Prelude.Compat             hiding
                 (readFile)
import           System.IO
                 (Handle, IOMode (..), withFile)
import qualified Test.QuickCheck            as QC

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Control.Monad.Except (runExcept)
-- >>> import Data.Foldable (toList)
-- >>> import qualified Data.Attoparsec.ByteString.Char8 as A8

-- | This is CPSised ListT.
--
-- @since 0.15
--
newtype SourceT m a = SourceT
    { unSourceT :: forall b. (StepT m a -> m b) -> m b
    }

mapStepT :: (StepT m a -> StepT m b) -> SourceT m a -> SourceT m b
mapStepT f (SourceT m) = SourceT $ \k -> m (k . f)
{-# INLINE mapStepT #-}

-- | @ListT@ with additional constructors.
--
-- @since 0.15
--
data StepT m a
    = Stop
    | Error String      -- we can this argument configurable.
    | Skip (StepT m a)  -- Note: not sure about this constructor
    | Yield a (StepT m a)
    | Effect (m (StepT m a))
  deriving Functor

-- | Create 'SourceT' from 'Step'.
--
-- /Note:/ often enough you want to use 'SourceT' directly.
fromStepT :: StepT m a -> SourceT m a
fromStepT s = SourceT ($ s)

-------------------------------------------------------------------------------
-- SourceT instances
-------------------------------------------------------------------------------

instance Functor m => Functor (SourceT m) where
    fmap f = mapStepT (fmap f)

-- | >>> toList (source [1..10])
-- [1,2,3,4,5,6,7,8,9,10]
--
instance Identity ~ m => Foldable (SourceT m) where
    foldr f z (SourceT m) = foldr f z (runIdentity (m Identity))

instance (Applicative m, Show1 m) => Show1 (SourceT m) where
    liftShowsPrec sp sl d (SourceT m) = showsUnaryWith
        (liftShowsPrec sp sl)
        "fromStepT" d (Effect (m pure'))
      where
        pure' (Effect s) = s
        pure' s          = pure s

instance (Applicative m, Show1 m, Show a) => Show (SourceT m a) where
    showsPrec = showsPrec1

-- | >>> hoist (Just . runIdentity) (source [1..3]) :: SourceT Maybe Int
-- fromStepT (Effect (Just (Yield 1 (Yield 2 (Yield 3 Stop)))))
instance MFunctor SourceT where
    hoist f (SourceT m) = SourceT $ \k -> k $
        Effect $ f $ fmap (hoist f) $ m return

-- | >>> source "xy" <> source "z" :: SourceT Identity Char
-- fromStepT (Effect (Identity (Yield 'x' (Yield 'y' (Yield 'z' Stop)))))
--
instance Functor m => Semigroup (SourceT m a) where
    SourceT withL <> SourceT withR = SourceT $ \ret ->
        withL $ \l ->
        withR $ \r ->
        ret $ l <> r

-- | >>> mempty :: SourceT Maybe Int
-- fromStepT (Effect (Just Stop))
instance Functor m => Monoid (SourceT m a) where
    mempty = fromStepT mempty
    mappend = (<>)

-- | Doesn't generate 'Error' constructors. 'SourceT' doesn't shrink.
instance (QC.Arbitrary a, Monad m) => QC.Arbitrary (SourceT m a) where
    arbitrary = fromStepT <$> QC.arbitrary

-- An example of above instance. Not doctested because it's volatile.
--
-- >>> import Test.QuickCheck as QC
-- >>> import Test.QuickCheck.Gen as QC
-- >>> import Test.QuickCheck.Random as QC
-- >>> let generate (QC.MkGen g) = g (QC.mkQCGen 44) 10
--
-- >>> generate (arbitrary :: QC.Gen (SourceT Identity Int))
-- fromStepT (Effect (Identity (Yield (-10) (Yield 3 (Skip (Yield 1 Stop))))))

-------------------------------------------------------------------------------
-- StepT instances
-------------------------------------------------------------------------------

instance Identity ~ m => Foldable (StepT m) where
    foldr f z = go where
        go Stop                  = z
        go (Error _)             = z
        go (Skip s)              = go s
        go (Yield a s)           = f a (go s)
        go (Effect (Identity s)) = go s

instance (Applicative m, Show1 m) => Show1 (StepT m) where
    liftShowsPrec sp sl = go where
        go _ Stop        = showString "Stop"
        go d (Skip s)    = showsUnaryWith
            go
            "Skip" d s
        go d (Error err) = showsUnaryWith
            showsPrec
            "Error" d err
        go d (Effect ms) = showsUnaryWith
            (liftShowsPrec go goList)
            "Effect" d ms
        go d (Yield x s) = showsBinaryWith
            sp go
            "Yield" d x s

        goList = liftShowList sp sl

instance (Applicative m, Show1 m, Show a) => Show (StepT m a) where
    showsPrec = showsPrec1

-- | >>> lift [1,2,3] :: StepT [] Int
-- Effect [Yield 1 Stop,Yield 2 Stop,Yield 3 Stop]
--
instance MonadTrans StepT where
    lift = Effect . fmap (`Yield` Stop)

instance MFunctor StepT where
    hoist f = go where
        go Stop        = Stop
        go (Error err) = Error err
        go (Skip s)    = Skip (go s)
        go (Yield x s) = Yield x (go s)
        go (Effect ms) = Effect (f (fmap go ms))

instance Functor m => Semigroup (StepT m a) where
    Stop      <> r = r
    Error err <> _ = Error err
    Skip s    <> r = Skip (s <> r)
    Yield x s <> r = Yield x (s <> r)
    Effect ms <> r = Effect ((<> r) <$> ms)

-- | >>> mempty :: StepT [] Int
-- Stop
--
-- >>> mempty :: StepT Identity Int
-- Stop
--
instance Functor m => Monoid (StepT m a) where
    mempty = Stop
    mappend = (<>)

-- | Doesn't generate 'Error' constructors.
instance (QC.Arbitrary a, Monad m) => QC.Arbitrary (StepT m a) where
    arbitrary = QC.sized arb where
        arb n | n <= 0    = pure Stop
              | otherwise = QC.frequency
                  [ (1, pure Stop)
                  , (1, Skip <$> arb')
                  , (1, Effect . return <$> arb')
                  , (8, Yield <$> QC.arbitrary <*> arb')
                  ]
          where
            arb' = arb (n - 1)

    shrink Stop        = []
    shrink (Error _)   = [Stop]
    shrink (Skip s)    = [s]
    shrink (Effect _)  = []
    shrink (Yield x s) =
        [ Yield x' s | x' <- QC.shrink x ] ++
        [ Yield x s' | s' <- QC.shrink s ]

-------------------------------------------------------------------------------
-- Operations
-------------------------------------------------------------------------------

-- | Create pure 'SourceT'.
--
-- >>> source "foo" :: SourceT Identity Char
-- fromStepT (Effect (Identity (Yield 'f' (Yield 'o' (Yield 'o' Stop)))))
--
source :: [a] -> SourceT m a
source = fromStepT . foldr Yield Stop

-- | Get the answers.
--
-- >>> runSourceT (source "foo" :: SourceT Identity Char)
-- ExceptT (Identity (Right "foo"))
--
-- >>> runSourceT (source "foo" :: SourceT [] Char)
-- ExceptT [Right "foo"]
--
runSourceT :: Monad m => SourceT m a -> ExceptT String m [a]
runSourceT (SourceT m) = ExceptT (m (runExceptT . runStepT))

runStepT :: Monad m => StepT m a -> ExceptT String m [a]
runStepT Stop        = return []
runStepT (Error err) = throwError err
runStepT (Skip s)    = runStepT s
runStepT (Yield x s) = fmap (x :) (runStepT s)
runStepT (Effect ms) = lift ms >>= runStepT

{-
-- | >>> uncons (foldr Yield Stop "foo" :: StepT Identity Char)
-- Identity (Just ('f',Yield 'o' (Yield 'o' Stop)))
--
uncons :: Monad m => StepT m a -> m (Maybe (a, StepT m a))
uncons Stop        = return Nothing
uncons (Skip s)    = uncons s
uncons (Yield x s) = return (Just (x, s))
uncons (Effect ms) = ms >>= uncons
uncons (Error _) =
-}

-- | Filter values.
--
-- >>> toList $ mapMaybe (\x -> if odd x then Just x else Nothing) (source [0..10]) :: [Int]
-- [1,3,5,7,9]
--
-- >>> mapMaybe (\x -> if odd x then Just x else Nothing) (source [0..2]) :: SourceT Identity Int
-- fromStepT (Effect (Identity (Skip (Yield 1 (Skip Stop)))))
--
-- Illustrates why we need 'Skip'.
mapMaybe :: Functor m => (a -> Maybe b) -> SourceT m a -> SourceT m b
mapMaybe p (SourceT m) = SourceT $ \k -> m (k . mapMaybeStep p)

mapMaybeStep :: Functor m => (a -> Maybe b) -> StepT m a -> StepT m b
mapMaybeStep p = go where
    go Stop        = Stop
    go (Error err) = Error err
    go (Skip s)    = Skip (go s)
    go (Effect ms) = Effect (fmap go ms)
    go (Yield x s) = case p x of
        Nothing -> Skip (go s)
        Just y  -> Yield y (go s)

-- | Run action for each value in the 'SourceT'.
--
-- >>> foreach fail print (source "abc")
-- 'a'
-- 'b'
-- 'c'
--
foreach
    :: Monad m
    => (String -> m ())  -- ^ error handler
    -> (a -> m ())
    -> SourceT m a
    -> m ()
foreach f g src = unSourceT src (foreachStep f g)

-- | See 'foreach'.
foreachStep
    :: Monad m
    => (String -> m ())  -- ^ error handler
    -> (a -> m ())
    -> StepT m a
    -> m ()
foreachStep f g = go where
    go Stop        = return ()
    go (Skip s)    = go s
    go (Yield x s) = g x >> go s
    go (Error err) = f err
    go (Effect ms) = ms >>= go

-------------------------------------------------------------------------------
-- Monadic
-------------------------------------------------------------------------------

fromAction :: Functor m => (a -> Bool) -> m a -> SourceT m a
fromAction stop action = SourceT ($ fromActionStep stop action)
{-# INLINE fromAction #-}

fromActionStep :: Functor m => (a -> Bool) -> m a -> StepT m a
fromActionStep stop action = loop where
    loop = Effect $ fmap step action
    step x
        | stop x    = Stop
        | otherwise = Yield x loop
{-# INLINE fromActionStep #-}

-------------------------------------------------------------------------------
-- File
-------------------------------------------------------------------------------

-- | Read file.
--
-- >>> foreach fail BS.putStr (readFile "servant.cabal")
-- cabal-version:       2.2
-- name:                servant
-- ...
--
readFile :: FilePath -> SourceT IO BS.ByteString
readFile fp =
    SourceT $ \k ->
    withFile fp ReadMode $ \hdl ->
    k (readHandle hdl)
  where
    readHandle :: Handle -> StepT IO BS.ByteString
    readHandle hdl = fromActionStep BS.null (BS.hGet hdl 4096)

-------------------------------------------------------------------------------
-- Attoparsec
-------------------------------------------------------------------------------

-- | Transform using @attoparsec@ parser.
--
-- Note: @parser@ should not accept empty input!
--
-- >>> let parser = A.skipWhile A8.isSpace_w8 >> A.takeWhile1 A8.isDigit_w8
--
-- >>> runExcept $ runSourceT $ transformWithAtto parser (source ["1 2 3"])
-- Right ["1","2","3"]
--
-- >>> runExcept $ runSourceT $ transformWithAtto parser (source ["1", "2", "3"])
-- Right ["123"]
--
-- >>> runExcept $ runSourceT $ transformWithAtto parser (source ["1", "2 3", "4"])
-- Right ["12","34"]
--
-- >>> runExcept $ runSourceT $ transformWithAtto parser (source ["foobar"])
-- Left "Failed reading: takeWhile1"
--
transformWithAtto :: Monad m => A.Parser a -> SourceT m BS.ByteString -> SourceT m a
transformWithAtto parser = mapStepT (transformStepWithAtto parser)

transformStepWithAtto
    :: forall a m. Monad m
    => A.Parser a -> StepT m BS.ByteString -> StepT m a
transformStepWithAtto parser = go (A.parse parser) where
    p0 = A.parse parser

    go :: (BS.ByteString -> A.Result a)
       -> StepT m BS.ByteString -> StepT m a
    go _ (Error err)  = Error err
    go p (Skip s)     = Skip (go p s)
    go p (Effect ms)  = Effect (fmap (go p) ms)
    go p Stop         = case p mempty of
        A.Fail _ _ err -> Error err
        A.Done _ a     -> Yield a Stop
        A.Partial _    -> Stop
    go p (Yield bs0 s) = loop p bs0 where
        loop p' bs
            | BS.null bs = Skip (go p' s)
            | otherwise  = case p' bs of
                A.Fail _ _ err -> Error err
                A.Done bs' a   -> Yield a (loop p0 bs')
                A.Partial p''  -> Skip (go p'' s)
