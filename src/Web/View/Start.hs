{-# LANGUAGE OverloadedStrings #-}

module Web.View.Start (startView, loadStartTemplate) where

import Data.Text qualified as TXT
import Text.Mustache (Template, automaticCompile)
import Web.Core (WRAction, WRConfig (wrcTplRoots))
import Web.Spock (html)

loadStartTemplate :: WRConfig -> IO Template
loadStartTemplate cfg = do
  compiled <- automaticCompile (wrcTplRoots cfg) "start.mustache"
  case compiled of
    Left err -> error (show err)
    Right template -> return template

startView :: Maybe TXT.Text -> WRAction a
startView mMes = do html "ユーザー登録 ログイン"