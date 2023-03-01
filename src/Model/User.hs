{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.User (NewUser (NewUser, nuName, nuPassword), insertUser) where

import Data.Functor.ProductIsomorphic
import Database.HDBC
import Database.HDBC.Query.TH (makeRelationalRecord)
import Database.Relational
import Entity.User qualified as User
import GHC.Generics (Generic)

data NewUser = NewUser
  { nuName :: !String,
    nuPassword :: !String
  }
  deriving (Show, Generic)

makeRelationalRecord ''NewUser

piNewUser :: Pi User.User NewUser
piNewUser = NewUser |$| User.name' |*| User.password'

insertUser :: IConnection c => NewUser -> c -> IO Integer
insertUser u conn = do return 0
