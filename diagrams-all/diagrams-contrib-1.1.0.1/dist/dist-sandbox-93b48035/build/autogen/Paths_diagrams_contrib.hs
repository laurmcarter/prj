module Paths_diagrams_contrib (
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
version = Version {versionBranch = [1,1,0,1], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/kcarter/scrap/haskell/diagrams-all/bin"
libdir     = "/home/kcarter/scrap/haskell/diagrams-all/lib/x86_64-linux-ghc-7.6.3/diagrams-contrib-1.1.0.1"
datadir    = "/home/kcarter/scrap/haskell/diagrams-all/share/x86_64-linux-ghc-7.6.3/diagrams-contrib-1.1.0.1"
libexecdir = "/home/kcarter/scrap/haskell/diagrams-all/libexec"
sysconfdir = "/home/kcarter/scrap/haskell/diagrams-all/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "diagrams_contrib_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "diagrams_contrib_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "diagrams_contrib_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "diagrams_contrib_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "diagrams_contrib_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
