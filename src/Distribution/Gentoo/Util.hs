
-- These extensions are for ParseException
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
   Module      : Distribution.Gentoo.Util
   Description : Utility functions
   Copyright   : (c) Ivan Lazar Miljenovic 2009
   License     : GPL-2 or later

   Common utility functions.
 -}

module Distribution.Gentoo.Util
       (
         -- * Monoidal maps
         Mappings
       , mappings
         -- * Parse errors
       , ParseException (..)
       , throwParseError
       , parseM
         -- * Misc
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

import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Functor.Identity
-- import qualified Data.List as L
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Typeable (Typeable)
import System.Directory (findExecutable)
import System.Exit (ExitCode (..))
import System.Process (readProcess, readProcessWithExitCode)
import Text.Parsec (Parsec, SourceName, Stream, parse)
import Text.Parsec.Error (ParseError)
import Text.PrettyPrint (render)

import Distribution.Pretty (Pretty(..))
import qualified Distribution.Types.PackageId as Cabal
    ( PackageId
    -- , PackageIdentifier(..)
    )
import qualified Distribution.Simple.Utils as Cabal (die')
import qualified Distribution.Verbosity as V

import qualified Data.MonoidMap as MMap
import Data.MonoidMap (MonoidMap)

-- | Two-way mappings between two sets of data
type Mappings k v = (MonoidMap k v, MonoidMap v k)

mappings :: (Ord k, Ord v) => k -> v -> Mappings k v
mappings k v = (MMap.singleton k v, MMap.singleton v k)

-- | Wrapper to give 'ParseError' an 'Exception' instance
newtype ParseException = ParseException ParseError
    deriving stock Typeable
    deriving newtype Show
    deriving anyclass Exception

throwParseError :: MonadThrow m => ParseError -> m a
throwParseError = throwM . ParseException

parseM :: (Stream s Identity t, MonadThrow m)
    => Parsec s () a -> SourceName -> s -> m a
parseM p n = either throwParseError pure . parse p n

concatMapM :: Applicative f => (a -> f [b]) -> [a] -> f [b]
concatMapM f = fmap concat . traverse f

breakAll :: (Char -> Bool) -> Text -> [Text]
breakAll p = T.groupBy (const (not . p))

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
