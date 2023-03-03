{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.WeightRecord (NewWRecord (NewWRecord, nwrUserId, nwrTime, nwrWeight), insertNewWRecord) where

import Control.Exception (catch)
import Data.Functor.ProductIsomorphic
import Data.Time qualified as TM
import Database.HDBC (IConnection, SqlError, withTransaction)
import Database.HDBC.Query.TH (makeRelationalRecord)
import Database.HDBC.Record qualified as DHR
import Database.Relational (Pi, defaultConfig)
import Database.Relational qualified as HRR
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