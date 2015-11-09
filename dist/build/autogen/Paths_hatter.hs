module Paths_hatter (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/shivanshu/.cabal/bin"
libdir     = "/home/shivanshu/.cabal/lib/x86_64-linux-ghc-7.10.2/hatter-0.1.0.0-9djrJL3xsAm8tZuCvyH4X7"
datadir    = "/home/shivanshu/.cabal/share/x86_64-linux-ghc-7.10.2/hatter-0.1.0.0"
libexecdir = "/home/shivanshu/.cabal/libexec"
sysconfdir = "/home/shivanshu/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "hatter_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hatter_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "hatter_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hatter_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hatter_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
