module Paths_haskell6 (
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

bindir     = "/home/root-om/Documents/Progs/Haskell/Haskell 6/haskell-6/.cabal-sandbox/bin"
libdir     = "/home/root-om/Documents/Progs/Haskell/Haskell 6/haskell-6/.cabal-sandbox/lib/x86_64-linux-ghc-7.10.3/haskell6-0.1.0.0-HuqvgYYIk7M2cRAQvpnzCc"
datadir    = "/home/root-om/Documents/Progs/Haskell/Haskell 6/haskell-6/.cabal-sandbox/share/x86_64-linux-ghc-7.10.3/haskell6-0.1.0.0"
libexecdir = "/home/root-om/Documents/Progs/Haskell/Haskell 6/haskell-6/.cabal-sandbox/libexec"
sysconfdir = "/home/root-om/Documents/Progs/Haskell/Haskell 6/haskell-6/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "haskell6_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "haskell6_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "haskell6_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "haskell6_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "haskell6_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
