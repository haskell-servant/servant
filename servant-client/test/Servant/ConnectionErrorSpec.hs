{-# LANGUAGE CPP                    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -freduction-depth=100 #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Servant.ConnectionErrorSpec (spec) where

import           Prelude ()
import           Prelude.Compat

import           Control.Exception
                 (fromException)
import           Data.Maybe
                 (isJust)
import           Data.Monoid ()
import           Data.Proxy
import qualified Network.HTTP.Client                  as C
import           Test.Hspec

import           Servant.API
                 (Get, JSON)
import           Servant.Client
import           Servant.ClientTestUtils


spec :: Spec
spec = describe "Servant.ConnectionErrorSpec" $ do
    connectionErrorSpec

type ConnectionErrorAPI = Get '[JSON] Int

connectionErrorAPI :: Proxy ConnectionErrorAPI
connectionErrorAPI = Proxy

connectionErrorSpec :: Spec
connectionErrorSpec = describe "Servant.Client.ClientError" $
    it "correctly catches ConnectionErrors when the HTTP request can't go through" $ do
        let getInt = client connectionErrorAPI
        let baseUrl' = BaseUrl Http "example.invalid" 80 ""
        let isHttpError (Left (ConnectionError e)) = isJust $ fromException @C.HttpException e
            isHttpError _ = False
        (isHttpError <$> runClient getInt baseUrl') `shouldReturn` True
