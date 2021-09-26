{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_hw0_lambda (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
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
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/neverbehave/Documents/cse230-fa21-00-lambda-NeverBehave/.stack-work/install/x86_64-linux-nix/7a4cf781df63929184d8af1683e997f72f6d6b8edb08156cfa51f031171b449f/8.10.2/bin"
libdir     = "/home/neverbehave/Documents/cse230-fa21-00-lambda-NeverBehave/.stack-work/install/x86_64-linux-nix/7a4cf781df63929184d8af1683e997f72f6d6b8edb08156cfa51f031171b449f/8.10.2/lib/x86_64-linux-ghc-8.10.2/hw0-lambda-0.1.0.0-JyAtmslYaLvILy8EaYthT2-test"
dynlibdir  = "/home/neverbehave/Documents/cse230-fa21-00-lambda-NeverBehave/.stack-work/install/x86_64-linux-nix/7a4cf781df63929184d8af1683e997f72f6d6b8edb08156cfa51f031171b449f/8.10.2/lib/x86_64-linux-ghc-8.10.2"
datadir    = "/home/neverbehave/Documents/cse230-fa21-00-lambda-NeverBehave/.stack-work/install/x86_64-linux-nix/7a4cf781df63929184d8af1683e997f72f6d6b8edb08156cfa51f031171b449f/8.10.2/share/x86_64-linux-ghc-8.10.2/hw0-lambda-0.1.0.0"
libexecdir = "/home/neverbehave/Documents/cse230-fa21-00-lambda-NeverBehave/.stack-work/install/x86_64-linux-nix/7a4cf781df63929184d8af1683e997f72f6d6b8edb08156cfa51f031171b449f/8.10.2/libexec/x86_64-linux-ghc-8.10.2/hw0-lambda-0.1.0.0"
sysconfdir = "/home/neverbehave/Documents/cse230-fa21-00-lambda-NeverBehave/.stack-work/install/x86_64-linux-nix/7a4cf781df63929184d8af1683e997f72f6d6b8edb08156cfa51f031171b449f/8.10.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "hw0_lambda_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hw0_lambda_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "hw0_lambda_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "hw0_lambda_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hw0_lambda_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hw0_lambda_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
