{-# LANGUAGE OverloadedStrings #-}

module AppSpec (spec) where

import App (app)
import Network.Wai.Test
import Test.Hspec
import Test.Hspec.Wai
import Web.Spock (spockAsApp)

-- main :: IO ()
-- main = hspec spec

spec :: Spec
spec =
  do
    describe "GET /" $
      with (spockAsApp app) $
        do
          it "serves the home page" $
            get "/" `shouldRespondWith` 200
    describe "GET with Network.WAi.Test" $
      it "Get home" $
        do
          appl <- spockAsApp app
          flip runSession appl $
            do
              response <- Network.Wai.Test.request (setPath defaultRequest "/")
              assertStatus 200 response
              assertBodyContains "ユーザー登録" response
              assertBodyContains "ログイン" response
