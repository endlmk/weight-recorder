{-# LANGUAGE OverloadedStrings #-}

module AppSpec (spec) where

import App (app)
import Test.Hspec
import Test.Hspec.Wai
import Web.Spock (spockAsApp)

-- main :: IO ()
-- main = hspec spec

spec :: Spec
spec =
  with (spockAsApp app) $
    do
      describe "GET /" $
        do
          it "serves the home page" $
            get "/" `shouldRespondWith` "Hello World!" {matchStatus = 200}
      describe "Get /hello/:name" $
        do
          it "returns hello to spock" $
            get "/hello/spock" `shouldRespondWith` "Hello spock, you are visitor number 1"
          it "returns hello to uhura" $
            get "/hello/uhura" `shouldRespondWith` "Hello uhura, you are visitor number 1"
