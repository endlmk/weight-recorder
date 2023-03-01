{-# LANGUAGE OverloadedStrings #-}

module Model.UserSpec (spec) where

import Database.HDBC
import Database.HDBC.Sqlite3
import Model.User (NewUser (NewUser), insertUser)
import System.IO.Temp (withSystemTempFile)
import System.Process (callCommand)
import Test.Hspec

spec :: Spec
spec =
  do
    describe "User Model" $
      do
        it "inserts new user" $
          do
            withSystemTempFile "test.db" $
              \path _ -> do
                callCommand $ "sqlite3 " ++ path ++ "< data/schema.sql"
                c <- connectSqlite3 path
                _ <- insertUser (NewUser "test" "pass") c
                st <- prepare c "SELECT COUNT(*) FROM user"
                _ <- execute st []
                fetchRow st `shouldReturn` Just [SqlInt64 1]
