{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_hw01 (
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

bindir     = "C:\\Users\\AdamG\\Desktop\\Haskell\\hw01\\.stack-work\\install\\93891ca3\\bin"
libdir     = "C:\\Users\\AdamG\\Desktop\\Haskell\\hw01\\.stack-work\\install\\93891ca3\\lib\\x86_64-windows-ghc-8.6.5\\hw01-0.1.0.0-EwTcdr1DbUH2piWD7UlMhx-hw01"
dynlibdir  = "C:\\Users\\AdamG\\Desktop\\Haskell\\hw01\\.stack-work\\install\\93891ca3\\lib\\x86_64-windows-ghc-8.6.5"
datadir    = "C:\\Users\\AdamG\\Desktop\\Haskell\\hw01\\.stack-work\\install\\93891ca3\\share\\x86_64-windows-ghc-8.6.5\\hw01-0.1.0.0"
libexecdir = "C:\\Users\\AdamG\\Desktop\\Haskell\\hw01\\.stack-work\\install\\93891ca3\\libexec\\x86_64-windows-ghc-8.6.5\\hw01-0.1.0.0"
sysconfdir = "C:\\Users\\AdamG\\Desktop\\Haskell\\hw01\\.stack-work\\install\\93891ca3\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "hw01_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hw01_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "hw01_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "hw01_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hw01_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hw01_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
