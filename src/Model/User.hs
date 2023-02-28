module Model.User (NewUser (NewUser, nuName, nuPassword), insertUser) where

import Database.HDBC

data NewUser = NewUser
  { nuName :: !String,
    nuPassword :: !String
  }

insertUser :: IConnection c => NewUser -> c -> IO Integer
insertUser u conn = do return 0