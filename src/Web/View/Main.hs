{-# LANGUAGE OverloadedStrings #-}

module Web.View.Main where

import Data.Text qualified as TXT
import Entity.User qualified as User
import Text.Mustache (Template, automaticCompile, object, substitute, (~>))
import Text.Mustache.Types (Value)
import Web.Core (WRAction, WRConfig (wrcTplRoots), WRState (wrstMainTemplate), wrconUser)
import Web.Spock (getContext, getState, html)

loadMainTemplate :: WRConfig -> IO Template
loadMainTemplate cfg = do
  compiled <- automaticCompile (wrcTplRoots cfg) "main.mustache"
  case compiled of
    Left err -> error (show err)
    Right template -> return template

mainView :: Maybe TXT.Text -> WRAction a
mainView mMes =
  do
    u <- wrconUser <$> getContext
    case u of
      Just user -> do
        uv <- userValue user
        tpl <- wrstMainTemplate <$> getState
        let v =
              object $
                appendMessage
                  mMes
                  ["user" ~> uv]
        html $ substitute tpl v
        where
          appendMessage (Just mes) ps = "message" ~> mes : ps
          appendMessage Nothing ps = ps
      _ -> error "No user data in context"

userValue :: User.User -> WRAction Value
userValue u = return $ object ["id" ~> User.id u, "name" ~> User.name u]