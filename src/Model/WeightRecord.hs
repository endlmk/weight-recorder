module Model.WeightRecord (NewWRecord (NewWRecord, nwrUserId, nwrTime, nwrWeight), insertNewWRecord) where

import Data.Time qualified as TM
import Database.HDBC (IConnection)

data NewWRecord = NewWRecord
  { nwrUserId :: !Int,
    nwrTime :: !TM.LocalTime,
    nwrWeight :: !Double
  }

insertNewWRecord :: IConnection c => NewWRecord -> c -> IO Integer
insertNewWRecord _ _ = do return 0