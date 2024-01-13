{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_TFL1 (
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
bindir     = "/Users/nechit/ToFL/TFL5/.stack-work/install/x86_64-osx/10eae8e5b937dfbd15a35563668b8f9bd6524b2606071277b2cbb085cf2bc965/9.4.7/bin"
libdir     = "/Users/nechit/ToFL/TFL5/.stack-work/install/x86_64-osx/10eae8e5b937dfbd15a35563668b8f9bd6524b2606071277b2cbb085cf2bc965/9.4.7/lib/x86_64-osx-ghc-9.4.7/TFL1-0.1.0.0-6QlOvm8Ienv8tseky65Qxj"
dynlibdir  = "/Users/nechit/ToFL/TFL5/.stack-work/install/x86_64-osx/10eae8e5b937dfbd15a35563668b8f9bd6524b2606071277b2cbb085cf2bc965/9.4.7/lib/x86_64-osx-ghc-9.4.7"
datadir    = "/Users/nechit/ToFL/TFL5/.stack-work/install/x86_64-osx/10eae8e5b937dfbd15a35563668b8f9bd6524b2606071277b2cbb085cf2bc965/9.4.7/share/x86_64-osx-ghc-9.4.7/TFL1-0.1.0.0"
libexecdir = "/Users/nechit/ToFL/TFL5/.stack-work/install/x86_64-osx/10eae8e5b937dfbd15a35563668b8f9bd6524b2606071277b2cbb085cf2bc965/9.4.7/libexec/x86_64-osx-ghc-9.4.7/TFL1-0.1.0.0"
sysconfdir = "/Users/nechit/ToFL/TFL5/.stack-work/install/x86_64-osx/10eae8e5b937dfbd15a35563668b8f9bd6524b2606071277b2cbb085cf2bc965/9.4.7/etc"

getBinDir     = catchIO (getEnv "TFL1_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "TFL1_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "TFL1_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "TFL1_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "TFL1_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "TFL1_sysconfdir") (\_ -> return sysconfdir)




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
