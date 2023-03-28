{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Distribution.Gentoo.Env
    (
      -- * Environment monad transformer
      -- ** Env
      Env
    , collectEnv
    , MonadEnv(..)
      -- ** EnvT
    , EnvT
    , EnvIO
    , runEnvT
      -- ** EnvWriterT
    , EnvWriterT(..)
    , runEnvWriterT
      -- ** EnvId
    , EnvId
    , runEnvId
      -- * GHC-specific environment
    , CabalPkg(..)
    , ConfMap
    , GhcConfMap(..)
    , GhcConfFiles(..)
    , GentooConfMap(..)
    , ghcLibDir
      -- * Portage-specific environment
    ) where

import Control.Monad.Reader
import Control.Monad.Writer
import qualified Data.ByteString.Char8 as BS

import qualified Data.Map.Strict as Map
import Data.Functor.Identity(Identity(..))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import System.Directory( doesDirectoryExist
                       , listDirectory
                       )
import System.FilePath((</>), takeExtension)

import qualified Distribution.InstalledPackageInfo as Cabal
    ( InstalledPackageInfo(..)
    , parseInstalledPackageInfo
    )
import qualified Distribution.Types.PackageId as Cabal
    ( PackageId
    -- , PackageIdentifier(..)
    )

import Distribution.Gentoo.Types
import Distribution.Gentoo.Util
import Output


-- -----------------------------------------------------------------------------
-- Environment monad
-- -----------------------------------------------------------------------------

-- | All data which is needed for @haskell-updater@ calculations. Ideally, this
--   will be gathered initially and will be cached in memory, although this may
--   need to be tweaked if the memory footprint gets out of hand.
data Env = Env
    { envGhcConfMap :: GhcConfMap
    , envGhcConfFiles :: GhcConfFiles
    , envGentooConfMap :: GentooConfMap
    } deriving (Show, Eq, Ord)

-- | Simply holds the 'Verbosity' (retrieved from the command line options) and
--   the environment.
type EnvT = ReaderT (Verbosity, Env)

type EnvIO = EnvT IO

-- | Can be used on 'EnvIO' and arbitrary transformer stacks
runEnvT :: Verbosity -> Env -> EnvT m a -> m a
runEnvT v e m = runReaderT m (v,e)


-- | Uses 'sayIO' and 'vsayIO'
instance MonadSay EnvIO where
    say s = do
        v <- askVerbosity
        liftIO $ sayIO v s
    vsay s = do
        v <- askVerbosity
        liftIO $ vsayIO v s

-- | Saves the output of 'say' and 'vsay' into a @('Seq' String)@ so output
--   can be examined without locking us into @IO@.
newtype EnvWriterT m a
    = EnvWriterT (EnvT (WriterT (Seq String) m) a)
    deriving stock (Functor)
    deriving newtype
        (Applicative, Monad, MonadWriter (Seq String), MonadIO, MonadEnv)

instance Monad m => MonadSay (EnvWriterT m) where
    say s = do
        v <- askVerbosity
        case v of
            Quiet   -> pure ()
            Normal  -> tell $ Seq.singleton s
            Verbose -> tell $ Seq.singleton s
    vsay s = do
        v <- askVerbosity
        case v of
            Quiet   -> pure ()
            Normal  -> pure ()
            Verbose -> tell $ Seq.singleton s

runEnvWriterT :: Verbosity -> Env -> EnvWriterT m a -> m (a, Seq String)
runEnvWriterT v e (EnvWriterT m) = runWriterT (runEnvT v e m)

-- | A variation that carries Env but does not need any other special
--   functions
type EnvId = EnvT Identity

runEnvId :: Verbosity -> Env -> EnvId a -> a
runEnvId v e = runIdentity . runEnvT v e

class Monad m => MonadEnv m where
    askVerbosity :: m Verbosity
    askGhcConfMap :: m GhcConfMap
    askGhcConfFiles :: m GhcConfFiles
    askGentooConfMap :: m GentooConfMap

instance Monad m => MonadEnv (EnvT m) where
    askVerbosity = asks fst
    askGhcConfMap = asks $ envGhcConfMap . snd
    askGhcConfFiles = asks $ envGhcConfFiles . snd
    askGentooConfMap = asks $ envGentooConfMap . snd

-- -----------------------------------------------------------------------------
-- Collecting environment
-- -----------------------------------------------------------------------------

-- | This is what needs to be run to collect the environment before calculations
collectEnv :: MonadIO m => Verbosity -> m Env
collectEnv v = runSayT v $ do
    vsay "reading '*.conf' files from GHC package database"
    ghcConfs <- listConfFiles GHCConfs
    ghcMap  <- foldConf ghcConfs
    vsay $ "got " ++ show (Map.size ghcMap) ++ " '*.conf' files"

    vsay "reading '*.conf' files from special Gentoo package database"
    gentooConfs <- listConfFiles GentooConfs
    gentooMap <- foldConf gentooConfs
    vsay $ "got " ++ show (Map.size gentooMap) ++ " '*.conf' files"

    pure $ Env
            (GhcConfMap ghcMap)
            (GhcConfFiles ghcConfs)
            (GentooConfMap gentooMap)

-- -----------------------------------------------------------------------------
-- Collecting GHC-specific environment
-- -----------------------------------------------------------------------------

-- Fold Gentoo .conf files from the current GHC version and
-- create a Map
foldConf :: (MonadSay m, MonadIO m, Foldable t)
    => t FilePath -> m ConfMap
foldConf = foldM addConf Map.empty

-- | Add this .conf file to the Map
addConf :: (MonadSay m, MonadIO m) => ConfMap -> FilePath -> m ConfMap
addConf cmp conf = do
    cont <- liftIO $ BS.readFile conf
    -- empty files are created for
    -- phony packages like CABAL_CORE_LIB_GHC_PV
    -- and binary-only packages.
    if BS.null cont
        then return cmp
        else case Cabal.parseInstalledPackageInfo cont of
            Right (ws, ipi) -> do
                let i  = Cabal.sourcePackageId ipi
                    dn = showPackageId i
                vsay $ unwords [conf, "resolved:", show dn]
                unless (null ws) $ do
                    vsay "    Warnings:"
                    forM_ ws $ \w -> do
                        vsay $ "        " ++ w
                pure $ pushConf cmp i conf ipi
            Left ne -> do
                        say $ unwords [ "failed to parse"
                                      , show conf
                                      , ":"
                                      ]
                        forM_ ne $ \e -> say $ "    " ++ show e
                        return cmp

pushConf :: ConfMap -> Cabal.PackageId -> FilePath -> Cabal.InstalledPackageInfo -> ConfMap
pushConf m k p ipi = Map.insertWith Set.union k (Set.singleton (CabalPkg p ipi)) m

-- Return the Gentoo .conf files found in this GHC libdir
listConfFiles :: MonadIO m => ConfSubdir -> m (Set FilePath)
listConfFiles subdir = liftIO $ do
    dir <- ghcLibDir
    let gDir = dir </> subdirToDirname subdir
    exists <- doesDirectoryExist gDir
    if exists
        then do conts <- Set.fromList <$> listDirectory gDir
                return $ Set.map (gDir </>)
                    $ Set.filter isConf conts
        else return Set.empty
  where
    isConf file = takeExtension file == ".conf"

-- -----------------------------------------------------------------------------
-- Utilities
-- -----------------------------------------------------------------------------

subdirToDirname :: ConfSubdir -> FilePath
subdirToDirname subdir =
    case subdir of
        GHCConfs    -> "package.conf.d"
        GentooConfs -> "gentoo"


