{- |
   Module      : Distribution.Gentoo.Util
   Description : Utility functions
   Copyright   : (c) Ivan Lazar Miljenovic 2009
   License     : GPL-2 or later

   Common utility functions.
 -}

module Distribution.Gentoo.Util
       ( BSFilePath
       , concatMapM
       , breakAll
       , showPackageId
       , rawSysStdOutLine
       , rawCommand
       , findExe
       , readProcessOrDie
       , firstLine
       , die'
       ) where

import Control.Monad.IO.Class
import qualified Data.List as L
import Data.ByteString.Char8(ByteString)
import Data.Maybe (listToMaybe)
import System.Directory (findExecutable)
import System.Exit (ExitCode(..))
import System.Process (readProcess, readProcessWithExitCode)
import Text.PrettyPrint (render)

import Distribution.Pretty (Pretty(..))
import qualified Distribution.Types.PackageId as Cabal
    ( PackageId
    -- , PackageIdentifier(..)
    )
import qualified Distribution.Simple.Utils as Cabal (die')
import qualified Distribution.Verbosity as V

-- Alias used to indicate that this ByteString represents a FilePath
type BSFilePath = ByteString

concatMapM :: Applicative f => (a -> f [b]) -> [a] -> f [b]
concatMapM f = fmap concat . traverse f

breakAll :: (a -> Bool) -> [a] -> [[a]]
breakAll p = L.groupBy (const (not . p))

-- | Get only the first line of output
rawSysStdOutLine :: MonadIO m => FilePath -> [String] -> m String
rawSysStdOutLine app = fmap (head . lines) . rawCommand app

-- | Run a command and return its stdout
rawCommand :: MonadIO m => FilePath -> [String] -> m String
rawCommand cmd args = liftIO $ readProcess cmd args ""

-- | Render a 'Cabal.PackageId' as a 'String'
showPackageId :: Cabal.PackageId -> String
showPackageId = render . pretty

-- | Find an executable in $PATH. If it doesn't exist, 'die'' with an
--   error.
findExe
    :: MonadIO m
    => String -- ^ The executable to search for
    -> m FilePath
findExe exe = liftIO $ findExecutable exe >>= check
  where
    check (Just e) = pure e
    check Nothing = die' $ unwords
        ["Could not find", show exe, "executable on system"]

-- | Run a process with the given arguments, then return the stdout. This will
--   'die'' on a non-zero exit code and output the stdout and stderr buffers.
readProcessOrDie
    :: MonadIO m
    => FilePath
    -> [String]
    -> m String
readProcessOrDie exe args = do
    (ec, out, err) <- liftIO $ readProcessWithExitCode exe args ""
    case ec of
        ExitSuccess -> pure out
        ExitFailure i ->
            die' $ unlines $ map unwords
                [ ["Failed to run command:", show exe] ++ map show args
                , ["exit code:", show i]
                , ["stdout:", show out]
                , ["stderr:", show err]
                ]

-- | Return just the first line of input, if it exists
firstLine :: String -> Maybe String
firstLine = listToMaybe . lines

-- | Exit the process with an unsuccessful error code and a message / stack
--   trace.
--
--   See 'Cabal.die''. This always uses 'V.normal' verbosity, which should
--   output useful info regardless of the verbosity setting passed in from the
--   command-line.
die' :: MonadIO m => String -> m a
die' = liftIO . Cabal.die' V.normal
