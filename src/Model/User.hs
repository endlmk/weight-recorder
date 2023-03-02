{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.User (NewUser (NewUser, nuName, nuPassword), insertUser, selectUser) where

import Control.Exception (catch)
import Crypto.BCrypt (hashPasswordUsingPolicy, slowerBcryptHashingPolicy, validatePassword)
import Data.ByteString qualified as BS
import Data.Functor.ProductIsomorphic
import Data.Text (pack, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Database.HDBC
import Database.HDBC.Query.TH (makeRelationalRecord)
import Database.HDBC.Record qualified as DHR
import Database.Relational
import Database.Relational qualified as HRR
import Entity.User qualified as User
import GHC.Generics (Generic)
import System.IO (hPrint, hPutStrLn, stderr)

data NewUser = NewUser
  { nuName :: !String,
    nuPassword :: !String
  }
  deriving (Show, Generic)

makeRelationalRecord ''NewUser

piNewUser :: Pi User.User NewUser
piNewUser = NewUser |$| User.name' |*| User.password'

insertUser :: IConnection c => NewUser -> c -> IO Integer
insertUser u conn = do
  mHashed <-
    hashPasswordUsingPolicy slowerBcryptHashingPolicy $
      enc . nuPassword $
        u
  case mHashed of
    Nothing -> do
      hPutStrLn stderr "Failed to hash password"
      return 0
    Just hashed -> do
      let ins = HRR.typedInsert' defaultConfig User.tableOfUser piNewUser
          u' = u {nuPassword = dec hashed}
      withTransaction conn $
        \conn' ->
          DHR.runInsert conn' ins u'
            `catch` \e -> do
              hPrint stderr (e :: SqlError)
              return 0

enc :: String -> BS.ByteString
enc = encodeUtf8 . pack

dec :: BS.ByteString -> String
dec = unpack . decodeUtf8

selectUser :: IConnection c => String -> String -> c -> IO (Maybe User.User)
selectUser name pass c = do
  user <- DHR.runQuery c q name >>= DHR.listToUnique
  return $ user >>= checkHash
  where
    q = HRR.relationalQuery . HRR.relation' . HRR.placeholder $
      \ph -> do
        a <- HRR.query User.user
        HRR.wheres $ a HRR.! User.name' HRR..=. ph
        return a
    checkHash user
      | validated = Just user
      | otherwise = Nothing
      where
        hashed = User.password user
        validated = validatePassword (enc hashed) (enc pass)
