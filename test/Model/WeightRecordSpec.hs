module Model.WeightRecordSpec (spec) where

import Data.Time
import Database.HDBC
import Database.HDBC.Sqlite3
import Model.WeightRecord (NewWRecord (NewWRecord), insertNewWRecord)
import System.IO.Temp (withSystemTempFile)
import System.Process (callCommand)
import Test.Hspec

spec :: Spec
spec =
  do
    describe "Weight Record Model" $
      do
        it "insert new weigt record" $
          do
            withSystemTempFile "test.db" $
              \path _ -> do
                callCommand $ "sqlite3 " ++ path ++ "< data/schema.sql"
                c <- connectSqlite3 path
                dt <- parseTimeM True defaultTimeLocale "%F" "2020-06-20"
                _ <- insertNewWRecord (NewWRecord 1 dt 54.6) c
                st <- prepare c "SELECT COUNT(*) FROM weight_record"
                _ <- execute st []
                fetchRow st `shouldReturn` Just [SqlInt64 1]