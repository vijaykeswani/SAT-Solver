module Paths_SAT_Solver (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/vijay/.cabal/bin"
libdir     = "/home/vijay/.cabal/lib/SAT-Solver-0.1.0.0/ghc-7.4.2"
datadir    = "/home/vijay/.cabal/share/SAT-Solver-0.1.0.0"
libexecdir = "/home/vijay/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "SAT_Solver_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "SAT_Solver_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "SAT_Solver_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "SAT_Solver_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
