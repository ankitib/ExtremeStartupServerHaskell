module Paths_extremeStartupServer (
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
version = Version {versionBranch = [0,0,1], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/ankit/Library/Haskell/bin"
libdir     = "/Users/ankit/Library/Haskell/ghc-7.8.3-x86_64/lib/extremeStartupServer-0.0.1"
datadir    = "/Users/ankit/Library/Haskell/share/ghc-7.8.3-x86_64/extremeStartupServer-0.0.1"
libexecdir = "/Users/ankit/Library/Haskell/libexec"
sysconfdir = "/Users/ankit/Library/Haskell/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "extremeStartupServer_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "extremeStartupServer_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "extremeStartupServer_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "extremeStartupServer_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "extremeStartupServer_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
