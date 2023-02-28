{-# LANGUAGE OverloadedStrings #-}

module Model.UserSpec (spec) where

import Database.HDBC
import Database.HDBC.Sqlite3
import Model.User (NewUser (NewUser), insertUser)
import Test.Hspec

spec :: Spec
spec =
  do
    describe "User Model" $
      do
        it "inserts new user" $
          do
            c <- connectSqlite3 "weight.db"
            _ <- insertUser (NewUser "test" "pass") c
            st <- prepare c "SELECT COUNT(*) FROM user"
            _ <- execute st []
            (fetchRow st) `shouldReturn` Just [SqlInt64 1]
