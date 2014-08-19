module Paths_sunroof_compiler (
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
version = Version {versionBranch = [0,2,1], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/kcarter/scrap/haskell/sunroof-test/lib/sunroof-compiler-0.2/.cabal-sandbox/bin"
libdir     = "/home/kcarter/scrap/haskell/sunroof-test/lib/sunroof-compiler-0.2/.cabal-sandbox/lib/x86_64-linux-ghc-7.8.2/sunroof-compiler-0.2.1"
datadir    = "/home/kcarter/scrap/haskell/sunroof-test/lib/sunroof-compiler-0.2/.cabal-sandbox/share/x86_64-linux-ghc-7.8.2/sunroof-compiler-0.2.1"
libexecdir = "/home/kcarter/scrap/haskell/sunroof-test/lib/sunroof-compiler-0.2/.cabal-sandbox/libexec"
sysconfdir = "/home/kcarter/scrap/haskell/sunroof-test/lib/sunroof-compiler-0.2/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "sunroof_compiler_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "sunroof_compiler_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "sunroof_compiler_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "sunroof_compiler_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "sunroof_compiler_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
