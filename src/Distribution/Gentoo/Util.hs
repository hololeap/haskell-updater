{- |
   Module      : Distribution.Gentoo.Util
   Description : Utility functions
   Copyright   : (c) Ivan Lazar Miljenovic 2009
   License     : GPL-2 or later

   Common utility functions.
 -}

{-# LANGUAGE LambdaCase #-}

module Distribution.Gentoo.Util
       ( BSFilePath
       , concatMapM
       , breakAll
       , ghcLoc
       , ghcPkgLoc
       , ghcLibDir
       , ghcRawOut
       , ghcPkgRawOut
       , showPackageId
       ) where

import Control.Monad.IO.Class
import qualified Data.List as L
import Data.ByteString.Char8(ByteString)
import System.Directory (findExecutable, canonicalizePath)
import System.Process (readProcess)
import Text.PrettyPrint (render)

import Distribution.Pretty (Pretty(..))
import Distribution.Simple.Utils (die')
import qualified Distribution.Types.PackageId as Cabal
    ( PackageId
    -- , PackageIdentifier(..)
    )
import qualified Distribution.Verbosity as V

-- Alias used to indicate that this ByteString represents a FilePath
type BSFilePath = ByteString

concatMapM   :: (a -> IO [b]) -> [a] -> IO [b]
concatMapM f = fmap concat . traverse f

breakAll   :: (a -> Bool) -> [a] -> [[a]]
breakAll p = L.groupBy (const (not . p))

ghcLoc :: MonadIO m => m FilePath
ghcLoc = findExe "ghc"

ghcPkgLoc :: MonadIO m => m FilePath
ghcPkgLoc = findExe "ghc-pkg"

-- | Find an executable in $PATH. If it doesn't exist, 'die'' with an
--   error.
findExe
    :: MonadIO m
    => String -- ^ The executable to search for
    -> m FilePath
findExe exe = liftIO $ findExecutable exe >>= \case
    Just e  -> pure e
    Nothing -> die' V.normal $
        "Could not find '" ++ show exe ++ "' executable on system"

-- | The directory where GHC has all its libraries, etc.
ghcLibDir :: MonadIO m => m FilePath
ghcLibDir = liftIO . canonicalizePath =<< ghcRawOut ["--print-libdir"]

-- | Get the first line of output from calling @ghc@ with the given
-- arguments.
ghcRawOut :: MonadIO m => [String] -> m String
ghcRawOut args = ghcLoc >>= flip rawSysStdOutLine args

-- | Get the first line of output from calling @ghc-pkg@ with the given
-- arguments.
ghcPkgRawOut :: MonadIO m => [String] -> m String
ghcPkgRawOut args = ghcPkgLoc >>= flip rawCommand args

-- | Get only the first line of output
rawSysStdOutLine :: MonadIO m => FilePath -> [String] -> m String
rawSysStdOutLine app = fmap (head . lines) . rawCommand app

-- | Run a command and return its stdout
rawCommand :: MonadIO m => FilePath -> [String] -> m String
rawCommand cmd args = liftIO $ readProcess cmd args ""

showPackageId :: Cabal.PackageId -> String
showPackageId = render . pretty
