module Paths_Project (
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
version = Version {versionBranch = [1,0], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/Xu/Library/Haskell/bin"
libdir     = "/Users/Xu/Library/Haskell/ghc-7.8.3-x86_64/lib/Project-1.0"
datadir    = "/Users/Xu/Library/Haskell/share/ghc-7.8.3-x86_64/Project-1.0"
libexecdir = "/Users/Xu/Library/Haskell/libexec"
sysconfdir = "/Users/Xu/Library/Haskell/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Project_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Project_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Project_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Project_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Project_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
