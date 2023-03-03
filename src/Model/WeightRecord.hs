{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.WeightRecord (NewWRecord (NewWRecord, nwrUserId, nwrTime, nwrWeight), insertNewWRecord, selectWRecord) where

import Control.Exception (catch)
import Data.Functor.ProductIsomorphic
import Data.Time qualified as TM
import Database.HDBC (IConnection, SqlError, withTransaction)
import Database.HDBC.Query.TH (makeRelationalRecord)
import Database.HDBC.Record (runQuery)
import Database.HDBC.Record qualified as DHR
import Database.Relational (Pi, defaultConfig)
import Database.Relational qualified as HRR
import Entity.WeightRecord (WeightRecord (userId))
import Entity.WeightRecord qualified as WRecord
import GHC.Generics (Generic)
import System.IO (hPrint, stderr)

data NewWRecord = NewWRecord
  { nwrUserId :: !Int,
    nwrTime :: !TM.LocalTime,
    nwrWeight :: !Double
  }
  deriving (Show, Generic)

makeRelationalRecord ''NewWRecord

piNewRecord :: Pi WRecord.WeightRecord NewWRecord
piNewRecord = NewWRecord |$| WRecord.userId' |*| WRecord.time' |*| WRecord.weight'

insertNewWRecord :: IConnection c => NewWRecord -> c -> IO Integer
insertNewWRecord wr conn = do
  let ins = HRR.typedInsert' defaultConfig WRecord.tableOfWeightRecord piNewRecord
  withTransaction conn $
    \conn' ->
      DHR.runInsert conn' ins wr
        `catch` \e -> do
          hPrint stderr (e :: SqlError)
          return 0

selectWRecord :: IConnection c => Int -> c -> IO [WRecord.WeightRecord]
selectWRecord userId conn = DHR.runQuery conn q userId
  where
    q = HRR.relationalQuery . HRR.relation' . HRR.placeholder $
      \ph -> do
        a <- HRR.query WRecord.weightRecord
        HRR.wheres $ a HRR.! WRecord.userId' HRR..=. ph
        HRR.desc $ a HRR.! WRecord.time'
        return a
