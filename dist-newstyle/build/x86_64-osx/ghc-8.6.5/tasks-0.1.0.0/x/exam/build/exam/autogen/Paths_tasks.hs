{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_tasks (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
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
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/mihailgorsenin/.cabal/bin"
libdir     = "/Users/mihailgorsenin/.cabal/lib/x86_64-osx-ghc-8.6.5/tasks-0.1.0.0-inplace-exam"
dynlibdir  = "/Users/mihailgorsenin/.cabal/lib/x86_64-osx-ghc-8.6.5"
datadir    = "/Users/mihailgorsenin/.cabal/share/x86_64-osx-ghc-8.6.5/tasks-0.1.0.0"
libexecdir = "/Users/mihailgorsenin/.cabal/libexec/x86_64-osx-ghc-8.6.5/tasks-0.1.0.0"
sysconfdir = "/Users/mihailgorsenin/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "tasks_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "tasks_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "tasks_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "tasks_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "tasks_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "tasks_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
