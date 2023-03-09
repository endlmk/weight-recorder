{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Paths_weight_recorder (getDataDir)
import System.FilePath ((</>))
import Web.Core (WRConfig (..))
import Web.WeightRecorder (runWeightRecorder)

main :: IO ()
main = do
  dataDir <- getDataDir
  runWeightRecorder WRConfig {wrcDBPath = "weight.db", wrcTplRoots = [dataDir </> "templates"]}