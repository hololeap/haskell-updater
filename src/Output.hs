{- |
   Module      : Main
   Description : The haskell-updater executable
   License     : GPL-2 or later

   Fancy output facility.
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Output (
                MonadSay(..)
              , SayT
              , runSayT
              , sayIO
              , vsayIO
              , pkgListPrintLn
              , printList
              , Verbosity(..)
              ) where

import Control.Monad.Reader
import System.IO (hPutStrLn, stderr)

import Distribution.Gentoo.Packages

-- output mode (chattiness)
data Verbosity = Quiet
               | Normal
               | Verbose
     deriving (Eq, Ord, Show, Read)

-- | A class for monads which carry 'Verbosity' and can 'say' something. It
--   is left without any stringent requirements (such as 'MonadIO') so non-IO
--   tests can be run on 'MonadSay' actions.
class Monad m => MonadSay m where
    say  :: String -> m () -- ^ Say something under normal conditions
    vsay :: String -> m () -- ^ Say something only when 'Verbose' is selected

-- | A simpler 'MonadSay' when we don't want to set up the full environment yet
type SayT = ReaderT Verbosity

runSayT :: Verbosity -> SayT m a -> m a
runSayT v r = runReaderT r v

instance MonadIO m => MonadSay (SayT m) where
    say s = do
        v <- ask
        liftIO $ sayIO v s
    vsay s = do
        v <- ask
        liftIO $ vsayIO v s

-- Default implementation of 'say'
sayIO :: Verbosity -> String -> IO ()
sayIO v msg = do
    case v of
        Quiet   -> return ()
        Normal  -> hPutStrLn stderr msg
        Verbose -> hPutStrLn stderr msg

-- Default implementation of 'vsay'
vsayIO :: Verbosity -> String -> IO ()
vsayIO v msg = do
    case v of
        Quiet   -> return ()
        Normal  -> return ()
        Verbose -> hPutStrLn stderr msg

-- Print a bullet list of values with one value per line.
printList :: (MonadSay m, Foldable t)
    => (a -> String) -> t a -> m ()
printList f = mapM_ (say . (++) "  * " . f)

-- Print a list of packages, with a description of what they are.
pkgListPrintLn :: (MonadSay m, Foldable t)
    => String -> t Package -> m ()
pkgListPrintLn desc pkgs = do
      if null pkgs
        then say $ unwords ["No", desc, "packages found!"]
        else do say $ unwords ["Found the following"
                              , desc, "packages:"]
                printList printPkg pkgs
      say ""
