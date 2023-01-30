{- |
   Module      : Distribution.Gentoo.GHC
   Description : Find GHC-related breakages on Gentoo.
   Copyright   : (c) Ivan Lazar Miljenovic 2009
   License     : GPL-2 or later

   This module defines helper functions to find broken packages in
   GHC, or else find packages installed with older versions of GHC.
 -}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Distribution.Gentoo.GHC
       (
         CabalPkg(..)
       , Cabal.PackageId
       , oldGhcPkgs
       , brokenPkgs
       , allInstalledPackages
       ) where

import qualified Data.ByteString.Char8 as BS
import Data.Char(isDigit)
import Data.Either (rights)
import Data.Maybe (catMaybes, isJust)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import System.FilePath((</>), pathSeparator)
import Control.Monad.Reader

-- Cabal imports
import qualified Distribution.InstalledPackageInfo as Cabal
    ( InstalledPackageInfo(..)
    , parseInstalledPackageInfo
    )
import Distribution.Parsec (simpleParsec)
import qualified Distribution.Types.PackageId as Cabal
    ( PackageId
    -- , PackageIdentifier(..)
    )
import qualified Distribution.Types.LibraryName as Cabal
    ( LibraryName(..)
    )

-- haskell-updater imports
import Distribution.Gentoo.Env
import Distribution.Gentoo.Util
import Distribution.Gentoo.Packages
import Output



-- -----------------------------------------------------------------------------
-- ConfMap manipulation
-- -----------------------------------------------------------------------------


-- | Attempt to match the provided broken package to one of the
-- installed packages.
matchConf
    :: ConfMap
    -> Cabal.PackageId
    -> Either Cabal.PackageId (Set CabalPkg)
matchConf = tryMaybe . flip Map.lookup

-- -----------------------------------------------------------------------------
-- Checking for broken backages
-- -----------------------------------------------------------------------------

-- | Finding broken packages in this install of GHC.
--   Returns: broken, unknown_packages, unknown_files
brokenPkgs :: (MonadEnv m, MonadSay m, MonadIO m)
    => m (Set Package, Set Cabal.PackageId, Set FilePath)
brokenPkgs = do
    (pns, brokenConfs) <- findBrokenConfs
    (pkgs, orphanGentooFiles) <- checkPkgs brokenConfs
    pure (pkgs, pns, orphanGentooFiles)

-- | Returns: broken, unknown_files
checkPkgs :: (MonadSay m, MonadIO m)
    => Set FilePath -> m (Set Package, Set FilePath)
checkPkgs brokenConfs = do
       files_to_pkgs <- liftIO $ resolveFiles brokenConfs
       let gentooFiles = Map.keysSet files_to_pkgs
           pkgs = Set.fromList $ Map.elems files_to_pkgs
           orphanGentooFiles = brokenConfs Set.\\ gentooFiles
       vsay $ unwords [ "checkPkgs: searching for gentoo .conf orphans"
                      , show (length orphanGentooFiles)
                      , "of"
                      , show (length brokenConfs)
                      ]
       return (pkgs, orphanGentooFiles)

-- | .conf files from broken packages of this GHC version
-- Returns two lists:
-- * @['Cabal.PackageId']@ - list of broken cabal packages we could not resolve
--                           to Gentoo's .conf files
-- * @['FilePath']@ - list of '.conf' files resolved from broken
--                    PackageId reported by 'ghc-pkg check'
findBrokenConfs :: (MonadEnv m, MonadSay m, MonadIO m) => m (Set Cabal.PackageId, Set FilePath)
findBrokenConfs =
    do GentooConfMap cnfs <- askGentooConfMap
       vsay "brokenConfs: getting broken output from 'ghc-pkg'"
       ghc_pkg_brokens <- getBrokenGhcPkg
       vsay $ unwords ["brokenConfs: resolving Cabal package names to gentoo equivalents."
                      , show (length ghc_pkg_brokens)
                      , "Cabal packages are broken:"
                      , unwords $ showPackageId <$> Set.toList ghc_pkg_brokens
                      ]

       (orphan_broken, orphan_confs) <- getOrphanBroken
       vsay $ unwords [ "brokenConfs: ghc .conf orphans:"
                      , show (length orphan_broken)
                      , "are orphan:"
                      , unwords $ showPackageId <$> Set.toList orphan_broken
                      ]

       installed_but_not_registered <- getNotRegistered
       vsay $ unwords [ "brokenConfs: ghc .conf not registered:"
                      , show (length installed_but_not_registered)
                      , "are not registered:"
                      , unwords $ showPackageId <$> Set.toList installed_but_not_registered
                      ]

       registered_twice <- getRegisteredTwice
       vsay $ unwords [ "brokenConfs: ghc .conf registered twice:"
                      , show (length registered_twice)
                      , "are registered twice:"
                      , unwords $ showPackageId <$> Set.toList registered_twice
                      ]

       let all_broken = Set.unions [ ghc_pkg_brokens
                                   , orphan_broken
                                   , installed_but_not_registered
                                   , registered_twice
                                   ]
       let (known_broken, orphans) = partitionEithers $ Set.map (matchConf cnfs) all_broken
       return (known_broken, orphan_confs <> Set.map cabalConfPath orphans)
  where
    partitionEithers :: (Ord a, Ord b) => Set (Either a (Set b)) -> (Set a, Set b)
    partitionEithers = Set.foldr'
        (\e (sa,sb) -> case e of
            Left  a   -> (Set.insert a sa, sb             )
            Right sb' -> (             sa, sb <> sb')
        )
        (Set.empty, Set.empty)

-- Return the closure of all packages affected by breakage
-- in format of ["name-version", ... ]
getBrokenGhcPkg :: (MonadSay m, MonadIO m) => m (Set Cabal.PackageId)
getBrokenGhcPkg = do
    s <- ghcPkgRawOut ["check", "--simple-output"]
    Set.fromList . catMaybes <$> traverse check (words s)
  where
    check :: MonadSay m => String -> m (Maybe Cabal.PackageId)
    check s = do
        let mpid = simpleParsec s
        unless (isJust mpid) $
            say $ unwords ["Unable to parse as PackageId:", show s]
        pure mpid

-- | Around Jan 2015 we have started to install
--   all the .conf files in 'src_install()' phase.
--   Here we pick orphan ones and notify user about it.
getOrphanBroken :: (MonadEnv m, MonadIO m) => m (Set Cabal.PackageId, Set FilePath)
getOrphanBroken = do
    GhcConfFiles registered_confs <- askGhcConfFiles
    confs_to_pkgs <- liftIO $ resolveFiles registered_confs
    let conf_files = Map.keysSet confs_to_pkgs
        orphan_conf_files = Set.toList $ registered_confs Set.\\ conf_files
    orphan_packages <- fmap rights $
        forM orphan_conf_files $ \conf -> do
            bs <- liftIO $ BS.readFile conf
            let ipi = Cabal.parseInstalledPackageInfo bs
            pure $ Cabal.sourcePackageId . snd <$> ipi
    return (Set.fromList orphan_packages, Set.fromList orphan_conf_files)

-- Return packages, that seem to have
-- been installed via emerge (have gentoo/.conf entry),
-- but are not registered in package.conf.d.
-- Usually happens on manual cleaning or
-- due to unregistration bugs in old eclass.
getNotRegistered :: MonadEnv m => m (Set Cabal.PackageId)
getNotRegistered = do
    GentooConfMap installed_confs  <- askGentooConfMap
    GhcConfMap    registered_confs <- askGhcConfMap
    return $ Map.keysSet installed_confs Set.\\ Map.keysSet registered_confs

-- Return packages, that seem to have
-- been installed more, than once.
-- It usually happens this way:
--  1. user installs dev-lang/ghc-7.8.4-r0 (comes with bundled transformers-3.0.0.0-ghc-7.8.4-{abi}.conf)
--  2. user installs dev-haskell/transformers-0.4.3.0 (registered as transformers-0.4.3.0-{abi}.conf)
--  3. user upgrades up to dev-lang/ghc-7.8.4-r4 (comes with bundled transformers-0.4.3.0-ghc-7.8.4-{abi}.conf)
-- this way we have single package registered twice:
--   transformers-0.4.3.0-ghc-7.8.4-{abi}.conf
--   transformers-0.4.3.0-{abi}.conf
-- It's is easy to fix just by reinstalling transformers.
getRegisteredTwice :: MonadEnv m => m (Set Cabal.PackageId)
getRegisteredTwice = do
    GhcConfMap registered_confs <- askGhcConfMap
    let registered_twice = Map.filter filt registered_confs
    return $ Map.keysSet registered_twice
  where
    filt :: Set CabalPkg -> Bool
    filt s =
        -- Filter out any internal libraries that are registered, or we will
        -- get false positives for conf files such as
        -- attoparsec-0.14.4-Jlxx7B6z3OC4EY7gp6Rf4n-attoparsec-internal.conf
        let noInternals = Set.filter
                (\(CabalPkg _ ipi) -> case Cabal.sourceLibName ipi of
                    Cabal.LMainLibName -> True
                    Cabal.LSubLibName _ -> False
                )
                s
        in length noInternals > 1


-- -----------------------------------------------------------------------------
-- Checking for old packages
-- -----------------------------------------------------------------------------

-- Finding packages installed with other versions of GHC
oldGhcPkgs :: (MonadSay m, MonadIO m) => m (Set Package)
oldGhcPkgs =
    do thisGhc <- ghcLibDir
       vsay $ "oldGhcPkgs ghc lib: " ++ show thisGhc
       let thisGhc' = BS.pack thisGhc
       -- It would be nice to do this, but we can't assume
       -- some crazy user hasn't deleted one of these dirs
       -- libFronts' <- filterM doesDirectoryExist libFronts
       Set.fromList . notGHC <$> checkLibDirs thisGhc' libFronts

-- Find packages installed by other versions of GHC in this possible
-- library directory.
checkLibDirs :: (MonadSay m, MonadIO m)
    => BSFilePath -> [BSFilePath] -> m [Package]
checkLibDirs thisGhc libDirs =
    do vsay $ "checkLibDir ghc libs: " ++ show (thisGhc, libDirs)
       liftIO $ pkgsHaveContent (hasDirMatching wanted)
  where
    wanted dir = isValid dir && (not . isInvalid) dir

    isValid dir = any (`isGhcLibDir` dir) libDirs

    -- Invalid if it's this GHC
    isInvalid fp = fp == thisGhc || BS.isPrefixOf (thisGhc `BS.snoc` pathSeparator) fp

-- A valid GHC library directory starting at libdir has a name of
-- "ghc", then a hyphen and then a version number.
isGhcLibDir :: BSFilePath -> BSFilePath -> Bool
isGhcLibDir libdir dir = go ghcDirName
  where
    -- This is hacky because FilePath doesn't work on Bytestrings...
    libdir' = BS.snoc libdir pathSeparator
    ghcDirName = BS.pack "ghc"

    go dn = BS.isPrefixOf ghcDir dir
            -- Any possible version starts with a digit
            && isDigit (BS.index dir ghcDirLen)
      where
        ghcDir = flip BS.snoc '-' $ BS.append libdir' dn
        ghcDirLen = BS.length ghcDir


-- The possible places GHC could have installed lib directories
libFronts :: [BSFilePath]
libFronts = map BS.pack
            $ do lib <- ["lib", "lib64"]
                 return $ "/" </> "usr" </> lib

-- -----------------------------------------------------------------------------
-- Return all installed haskell packages
-- -----------------------------------------------------------------------------

allInstalledPackages :: MonadIO m => m (Set Package)
allInstalledPackages = do libDir <- ghcLibDir
                          let libDir' = BS.pack libDir
                          fmap (Set.fromList . notGHC) $ liftIO $ pkgsHaveContent
                                       $ hasDirMatching (==libDir')

-- -----------------------------------------------------------------------------
-- Common helper utils, etc.
-- -----------------------------------------------------------------------------

tryMaybe     :: (a -> Maybe b) -> a -> Either a b
tryMaybe f a = maybe (Left a) Right $ f a
