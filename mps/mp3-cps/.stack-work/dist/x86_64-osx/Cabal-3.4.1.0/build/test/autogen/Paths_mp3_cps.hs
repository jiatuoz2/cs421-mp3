{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_mp3_cps (
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

bindir     = "/Users/jiatuoz/Documents/Haskell/cs421-mp3/.stack-work/install/x86_64-osx/c8402eccb6a6a39d0b53ffc442be07b71523e4662df592d79e4c983470f754c0/9.0.2/bin"
libdir     = "/Users/jiatuoz/Documents/Haskell/cs421-mp3/.stack-work/install/x86_64-osx/c8402eccb6a6a39d0b53ffc442be07b71523e4662df592d79e4c983470f754c0/9.0.2/lib/x86_64-osx-ghc-9.0.2/mp3-cps-0.1.0.0-900mnvyq4c27T7OV8esKUv-test"
dynlibdir  = "/Users/jiatuoz/Documents/Haskell/cs421-mp3/.stack-work/install/x86_64-osx/c8402eccb6a6a39d0b53ffc442be07b71523e4662df592d79e4c983470f754c0/9.0.2/lib/x86_64-osx-ghc-9.0.2"
datadir    = "/Users/jiatuoz/Documents/Haskell/cs421-mp3/.stack-work/install/x86_64-osx/c8402eccb6a6a39d0b53ffc442be07b71523e4662df592d79e4c983470f754c0/9.0.2/share/x86_64-osx-ghc-9.0.2/mp3-cps-0.1.0.0"
libexecdir = "/Users/jiatuoz/Documents/Haskell/cs421-mp3/.stack-work/install/x86_64-osx/c8402eccb6a6a39d0b53ffc442be07b71523e4662df592d79e4c983470f754c0/9.0.2/libexec/x86_64-osx-ghc-9.0.2/mp3-cps-0.1.0.0"
sysconfdir = "/Users/jiatuoz/Documents/Haskell/cs421-mp3/.stack-work/install/x86_64-osx/c8402eccb6a6a39d0b53ffc442be07b71523e4662df592d79e4c983470f754c0/9.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "mp3_cps_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "mp3_cps_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "mp3_cps_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "mp3_cps_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "mp3_cps_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "mp3_cps_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
