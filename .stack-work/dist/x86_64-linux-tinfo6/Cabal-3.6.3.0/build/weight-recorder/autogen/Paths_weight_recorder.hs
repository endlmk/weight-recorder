{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_weight_recorder (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude


#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/endlmk/dev/haskell/weight-recorder/.stack-work/install/x86_64-linux-tinfo6/5035c02d961b0b1ce897391d97fd54f066f684aac74f25719b21e30d684fe6ec/9.2.5/bin"
libdir     = "/home/endlmk/dev/haskell/weight-recorder/.stack-work/install/x86_64-linux-tinfo6/5035c02d961b0b1ce897391d97fd54f066f684aac74f25719b21e30d684fe6ec/9.2.5/lib/x86_64-linux-ghc-9.2.5/weight-recorder-0.1.0.0-2VtK7hIpEU45MBp8NMUHfq-weight-recorder"
dynlibdir  = "/home/endlmk/dev/haskell/weight-recorder/.stack-work/install/x86_64-linux-tinfo6/5035c02d961b0b1ce897391d97fd54f066f684aac74f25719b21e30d684fe6ec/9.2.5/lib/x86_64-linux-ghc-9.2.5"
datadir    = "/home/endlmk/dev/haskell/weight-recorder/.stack-work/install/x86_64-linux-tinfo6/5035c02d961b0b1ce897391d97fd54f066f684aac74f25719b21e30d684fe6ec/9.2.5/share/x86_64-linux-ghc-9.2.5/weight-recorder-0.1.0.0"
libexecdir = "/home/endlmk/dev/haskell/weight-recorder/.stack-work/install/x86_64-linux-tinfo6/5035c02d961b0b1ce897391d97fd54f066f684aac74f25719b21e30d684fe6ec/9.2.5/libexec/x86_64-linux-ghc-9.2.5/weight-recorder-0.1.0.0"
sysconfdir = "/home/endlmk/dev/haskell/weight-recorder/.stack-work/install/x86_64-linux-tinfo6/5035c02d961b0b1ce897391d97fd54f066f684aac74f25719b21e30d684fe6ec/9.2.5/etc"

getBinDir     = catchIO (getEnv "weight_recorder_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "weight_recorder_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "weight_recorder_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "weight_recorder_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "weight_recorder_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "weight_recorder_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
