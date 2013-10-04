module Paths_mysql (
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
version = Version {versionBranch = [0,1,1,4], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/kevin/AACS/onping2.0/root/onping/.cabal-sandbox/bin"
libdir     = "/home/kevin/AACS/onping2.0/root/onping/.cabal-sandbox/lib/x86_64-linux-ghc-7.6.3/mysql-0.1.1.4"
datadir    = "/home/kevin/AACS/onping2.0/root/onping/.cabal-sandbox/share/x86_64-linux-ghc-7.6.3/mysql-0.1.1.4"
libexecdir = "/home/kevin/AACS/onping2.0/root/onping/.cabal-sandbox/libexec"
sysconfdir = "/home/kevin/AACS/onping2.0/root/onping/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "mysql_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "mysql_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "mysql_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "mysql_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "mysql_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
