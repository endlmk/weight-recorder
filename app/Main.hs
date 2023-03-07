{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Paths_weight_recorder (getDataDir)
import System.FilePath ((</>))
import Web.Core (WRConfig (WRConfig, wrcTplRoots))
import Web.WeightRecorder (runWeightRecorder)

main :: IO ()
main = do
  dataDir <- getDataDir
  runWeightRecorder WRConfig {wrcTplRoots = [dataDir </> "templates"]}