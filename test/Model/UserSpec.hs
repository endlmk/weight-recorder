{-# LANGUAGE OverloadedStrings #-}

module Model.UserSpec (spec) where

import Database.HDBC
import Database.HDBC.Sqlite3
import Entity.User qualified as User
import Model.User (NewUser (NewUser), insertUser, selectUser)
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
        it "select user" $
          do
            withSystemTempFile "test.db" $
              \path _ ->
                do
                  callCommand $ "sqlite3 " ++ path ++ "< data/schema.sql"
                  c <- connectSqlite3 path
                  _ <- insertUser (NewUser "test" "pass") c
                  u <- selectUser "test" "pass" c
                  case u of
                    Just usr -> do
                      User.id usr `shouldBe` 1
                      User.name usr `shouldBe` "test"
                    Nothing -> u `shouldNotBe` Nothing
        it "not select none exist user" $
          do
            withSystemTempFile "test.db" $
              \path _ ->
                do
                  callCommand $ "sqlite3 " ++ path ++ "< data/schema.sql"
                  c <- connectSqlite3 path
                  _ <- insertUser (NewUser "test" "pass") c
                  selectUser "test1" "pass" c `shouldReturn` Nothing
        it "not select worng password" $
          do
            withSystemTempFile "test.db" $
              \path _ ->
                do
                  callCommand $ "sqlite3 " ++ path ++ "< data/schema.sql"
                  c <- connectSqlite3 path
                  _ <- insertUser (NewUser "test" "pass") c
                  selectUser "test1" "aaaaa" c `shouldReturn` Nothing
