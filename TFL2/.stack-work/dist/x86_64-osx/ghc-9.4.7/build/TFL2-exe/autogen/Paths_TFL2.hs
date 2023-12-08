{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_TFL2 (
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
bindir     = "/Users/nechit/TFL2/.stack-work/install/x86_64-osx/be9ae8cf2f034137334e6703af82526d71d7ca6a7c2dde0bf3a696df37b31c9f/9.4.7/bin"
libdir     = "/Users/nechit/TFL2/.stack-work/install/x86_64-osx/be9ae8cf2f034137334e6703af82526d71d7ca6a7c2dde0bf3a696df37b31c9f/9.4.7/lib/x86_64-osx-ghc-9.4.7/TFL2-0.1.0.0-FYgFJYzLehME9kQxIjJh9F-TFL2-exe"
dynlibdir  = "/Users/nechit/TFL2/.stack-work/install/x86_64-osx/be9ae8cf2f034137334e6703af82526d71d7ca6a7c2dde0bf3a696df37b31c9f/9.4.7/lib/x86_64-osx-ghc-9.4.7"
datadir    = "/Users/nechit/TFL2/.stack-work/install/x86_64-osx/be9ae8cf2f034137334e6703af82526d71d7ca6a7c2dde0bf3a696df37b31c9f/9.4.7/share/x86_64-osx-ghc-9.4.7/TFL2-0.1.0.0"
libexecdir = "/Users/nechit/TFL2/.stack-work/install/x86_64-osx/be9ae8cf2f034137334e6703af82526d71d7ca6a7c2dde0bf3a696df37b31c9f/9.4.7/libexec/x86_64-osx-ghc-9.4.7/TFL2-0.1.0.0"
sysconfdir = "/Users/nechit/TFL2/.stack-work/install/x86_64-osx/be9ae8cf2f034137334e6703af82526d71d7ca6a7c2dde0bf3a696df37b31c9f/9.4.7/etc"

getBinDir     = catchIO (getEnv "TFL2_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "TFL2_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "TFL2_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "TFL2_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "TFL2_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "TFL2_sysconfdir") (\_ -> return sysconfdir)




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
