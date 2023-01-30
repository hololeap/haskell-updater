{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}


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
                       , listDirectory )
import System.FilePath((</>), takeExtension)

import qualified Distribution.InstalledPackageInfo as Cabal
    ( InstalledPackageInfo(..)
    , parseInstalledPackageInfo
    )
import qualified Distribution.Types.PackageId as Cabal
    ( PackageId
    -- , PackageIdentifier(..)
    )

import Distribution.Gentoo.Util
import Output


-- | Metadata for a @.conf@ file in one of the GHC package databases
data CabalPkg
    = CabalPkg
    {
      -- | Location of @.conf@ file
      cabalConfPath :: FilePath
      -- | Contents gathered by 'parseInstalledPackageInfo'
    , cabalConfInfo :: Cabal.InstalledPackageInfo -- ^ Parsed contents
    }
    deriving (Show, Eq)

-- | Cannot derive automatically, as 'Cabal.InstalledPackageInfo' does not have
--   an 'Ord' instance. Compares on the 'FilePath' only.
instance Ord CabalPkg where
    CabalPkg fp1 _ `compare` CabalPkg fp2 _ = fp1 `compare` fp2

-- | Differentiates between the package database used by GHC and the special
--   package database used to assist @haskell-updater@.
data ConfSubdir = GHCConfs -- ^ GHC's database
                | GentooConfs -- ^ assists @haskell-updater@

-- | Unique (normal) or multiple (broken) mapping
type ConfMap = Map.Map Cabal.PackageId (Set CabalPkg)

-- | The 'ConfMap' from GHC's package index
newtype GhcConfMap
    = GhcConfMap { getGhcConfMap :: ConfMap }
    deriving (Show, Eq, Ord, Semigroup, Monoid)

-- | All @.conf@ files from GHC's package index
--
--   Needed for 'getOrphanBroken'
newtype GhcConfFiles
    = GhcConfFiles { getGhcConfFiles :: Set FilePath }
    deriving (Show, Eq, Ord, Semigroup, Monoid)

-- | The 'ConfMap' from the special Gentoo package index
newtype GentooConfMap
    = GentooConfMap { getGentooConfMap :: ConfMap }
    deriving (Show, Eq, Ord, Semigroup, Monoid)


-- -----------------------------------------------------------------------------
-- Environment monad
-- -----------------------------------------------------------------------------

-- | All data which is needed for @haskell-updater@ calculations. Ideally, this
--   will be gathered initially and will be cached in memory, although this may
--   need to be tweaked if the memory footprint gets out of hand.
type Env = (GhcConfMap, GhcConfFiles, GentooConfMap)

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
    askGhcConfMap :: m GhcConfMap
    askGhcConfFiles :: m GhcConfFiles
    askGentooConfMap :: m GentooConfMap
    askVerbosity :: m Verbosity

instance Monad m => MonadEnv (EnvT m) where
    askGhcConfMap = asks (\(_,(m,_,_)) -> m)
    askGhcConfFiles = asks (\(_,(_,s,_)) -> s)
    askGentooConfMap = asks (\(_,(_,_,m)) -> m)
    askVerbosity = asks fst

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

    pure
        ( GhcConfMap ghcMap
        , GhcConfFiles ghcConfs
        , GentooConfMap gentooMap
        )

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
pushConf m k p ipi = Map.insertWith (Set.union) k (Set.singleton (CabalPkg p ipi)) m

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
