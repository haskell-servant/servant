#line 304 "Announcement.anansi"

#line 171 "Announcement.anansi"

{-# LANGUAGE OverloadedStrings #-}
module Spec (main) where

import Main (server, api, Species(..))
import Test.Hspec
import Test.QuickCheck.Instances
import Servant.QuickCheck
import Test.QuickCheck (Arbitrary(..))
import Database.PostgreSQL.Simple (connectPostgreSQL)

spec :: Spec
spec = describe "the species application" $ beforeAll check $ do
  let pserver = do
        conn <- connectPostgreSQL "dbname=servant-quickcheck"
        return $ server conn


  it "should not return 500s" $ do
    withServantServer api pserver $ \url ->
      serverSatisfies api url defaultArgs (not500 <%> mempty)

  it "should not return top-level json" $ do
    withServantServer api pserver $ \url ->
      serverSatisfies api url defaultArgs (onlyJsonObjects <%> mempty)

  it "should return valid locations for 201" $ do
    withServantServer api pserver $ \url ->
      serverSatisfies api url defaultArgs (createContainsValidLocation <%> mempty)


main :: IO ()
main = do
  hspec spec

instance Arbitrary Species where
  arbitrary = Species <$> arbitrary <*> arbitrary
