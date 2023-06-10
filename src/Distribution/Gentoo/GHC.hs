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

module Distribution.Gentoo.GHC
       (
         CabalPkg(..)
       , Cabal.PackageId
       , oldGhcPkgs
       , brokenPkgs
       , allInstalledPackages
       ) where

import Control.Monad.Reader
import qualified Data.ByteString.Char8 as BS
import Data.Char (isDigit)
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import Data.Maybe (catMaybes, isJust)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import System.Exit (ExitCode(..))
import System.FilePath(pathSeparator)
import System.Process (readProcessWithExitCode)
import Text.Parsec
import Text.Parsec.String

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
import Data.MonoidMap (MonoidMap(..))
import Distribution.Gentoo.Env
import Distribution.Gentoo.Packages
import Distribution.Gentoo.Types
import Distribution.Gentoo.Util
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
matchConf = tryMaybe . flip Map.lookup . unMonoidMap

-- -----------------------------------------------------------------------------
-- Checking for broken packages
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
checkPkgs :: (MonadEnv m, MonadSay m)
    => Set FilePath -> m (Set Package, Set FilePath)
checkPkgs brokenConfs = do
    vMappings <- askVersionMappings
    cMappings <- askContentMappings
    let MonoidMap filesToPkgs = resolveFiles vMappings cMappings brokenConfs
        gentooFiles = Map.keysSet filesToPkgs
        pkgs = Set.unions $ Map.elems filesToPkgs
        orphanGentooFiles = brokenConfs Set.\\ gentooFiles
    vsay $ unwords [ "checkPkgs: searching for gentoo .conf orphans"
                    , show (length orphanGentooFiles)
                    , "of"
                    , show (length brokenConfs)
                    ]
    return (pkgs, orphanGentooFiles)

-- | .conf files from broken packages of this GHC version
-- Returns two lists:
-- * @'Set' 'Cabal.PackageId'@ - set of broken cabal packages we could not resolve
--                               to Gentoo's .conf files
-- * @'Set' 'FilePath'@ - set of '.conf' files resolved from broken
--                        PackageId reported by 'ghc-pkg check'
findBrokenConfs :: (MonadEnv m, MonadSay m, MonadIO m) => m (Set Cabal.PackageId, Set FilePath)
findBrokenConfs = do
    GentooConfMap cnfs <- askGentooConfMap
    vsay "brokenConfs: getting broken output from 'ghc-pkg'"
    ghc_pkg_brokens <- getBrokenGhcPkg
    vsay $ unwords $
        ["brokenConfs: resolving Cabal package names to gentoo equivalents."
        , show (length ghc_pkg_brokens)
        , "Cabal packages are broken:"
        ] ++ (showPackageId <$> Set.toList ghc_pkg_brokens)

    (orphan_broken, orphan_confs) <- getOrphanBroken
    vsay $ unwords $
        [ "brokenConfs: ghc .conf orphans:"
        , show (length orphan_broken)
        , "are orphan:"
        ] ++ (showPackageId <$> Set.toList orphan_broken)

    installed_but_not_registered <- getNotRegistered
    vsay $ unwords $
        [ "brokenConfs: ghc .conf not registered:"
        , show (length installed_but_not_registered)
        , "are not registered:"
        ] ++ (showPackageId <$> Set.toList installed_but_not_registered)

    registered_twice <- getRegisteredTwice
    vsay $ unwords $
        [ "brokenConfs: ghc .conf registered twice:"
        , show (length registered_twice)
        , "are registered twice:"
        ] ++ (showPackageId <$> Set.toList registered_twice)

    let all_broken = Set.unions
            [ ghc_pkg_brokens
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

-- | Runs @ghc-pkg check --simple-output@ and parses each broken package as
--   a 'Cabal.PackageId', returning all of them as a 'Set'.
getBrokenGhcPkg :: (MonadSay m, MonadIO m) => m (Set Cabal.PackageId)
getBrokenGhcPkg = do
    ghcPkg <- findExe "ghc-pkg"
    -- Use 'readProcessWithExitCode' so it can ignore a non-zero exit code
    -- (@ghc-pkg@ tends to return these when it finds broken packages).
    (ec, out, _) <- liftIO
        $ readProcessWithExitCode ghcPkg ["check", "--simple-output"] ""
    case ec of
        -- No output from @ghc-pkg@ when nothing is broken
        ExitSuccess -> pure mempty
        ExitFailure _ -> do
            mIds <- idsFromOutput out >>= traverse check
            pure $ Set.fromList $ catMaybes mIds
  where
    check :: MonadSay m => String -> m (Maybe Cabal.PackageId)
    check s = do
        let mpid = simpleParsec s
        unless (isJust mpid) $
            say $ unwords ["Unable to parse as PackageId:", show s]
        pure mpid

    -- Grabs the first line of stdout and splits it into words
    idsFromOutput :: MonadIO m => String -> m [String]
    idsFromOutput s = case firstLine s of
        Just l  -> pure $ words l
        Nothing -> die' $ unlines $ "Could not parse ghc-pkg output:" : [s]

-- | Around Jan 2015 we have started to install
--   all the .conf files in 'src_install()' phase.
--   Here we pick orphan ones and notify user about it.
--
getOrphanBroken
    :: (MonadEnv m, MonadIO m)
    => m
        ( Set Cabal.PackageId -- ^ > brokenConfs: ghc .conf orphans
        , Set FilePath
        )
getOrphanBroken = do
    GhcConfFiles registeredConfs <- askGhcConfFiles
    vMappings <- askVersionMappings
    cMappings <- askContentMappings
    let MonoidMap confsToPkgs = resolveFiles vMappings cMappings registeredConfs
        conf_files = Map.keysSet confsToPkgs
        orphan_conf_files = Set.toList $ registeredConfs Set.\\ conf_files
    orphan_packages <- traverse parseConf orphan_conf_files
    return (Set.fromList orphan_packages, Set.fromList orphan_conf_files)
  where
    parseConf :: MonadIO m => FilePath -> m Cabal.PackageId
    parseConf conf = do
        bs <- liftIO $ BS.readFile conf
        let ipi = Cabal.parseInstalledPackageInfo bs
        case Cabal.sourcePackageId . snd <$> ipi of
            Left es -> die' $ unwords $
                [ "Error parsing"
                , show conf
                , ":"
                ] ++ NE.toList es
            Right x -> pure x

-- Return packages, that seem to have
-- been installed via emerge (have gentoo/.conf entry),
-- but are not registered in package.conf.d.
-- Usually happens on manual cleaning or
-- due to unregistration bugs in old eclass.
getNotRegistered :: MonadEnv m => m (Set Cabal.PackageId)
getNotRegistered = do
    GentooConfMap (MonoidMap installedConfs)  <- askGentooConfMap
    GhcConfMap    (MonoidMap registeredConfs) <- askGhcConfMap
    return $ Map.keysSet installedConfs Set.\\ Map.keysSet registeredConfs

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
    GhcConfMap (MonoidMap registeredConfs) <- askGhcConfMap
    let registered_twice = Map.filter filt registeredConfs
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
oldGhcPkgs :: (MonadEnv m, MonadSay m) => m (Set Package)
oldGhcPkgs = do
    thisGhc <- askGhcLibDir
    vsay $ "oldGhcPkgs ghc lib: " ++ show thisGhc
    -- It would be nice to do this, but we can't assume
    -- some crazy user hasn't deleted one of these dirs
    -- libFronts' <- filterM doesDirectoryExist libFronts
    notGHC <$> checkLibDirs thisGhc libFronts

-- Find packages installed by other versions of GHC in this possible
-- library directory.
checkLibDirs :: (MonadEnv m, MonadSay m)
    => GhcLibDir -> [FilePath] -> m (Set Package)
checkLibDirs (GhcLibDir thisGhc) libDirs = do
    vMappings <- askVersionMappings
    cMappings <- askContentMappings
    vsay $ "checkLibDir ghc libs: " ++ show (thisGhc, libDirs)
    pure $ pkgsHaveContent vMappings cMappings (hasDirMatching wanted)
  where
    wanted dir = isValid dir && (not . isInvalid) dir

    isValid dir = any (`isGhcLibDir` dir) libDirs

    -- Invalid if it's this GHC
    isInvalid fp = fp == thisGhc || L.isPrefixOf (thisGhc ++ [pathSeparator]) fp

-- A valid GHC library directory starting at libdir has a name of
-- "ghc", then a hyphen and then a version number.
isGhcLibDir :: FilePath -> FilePath -> Bool
isGhcLibDir libdir dir = case parse parser "" dir of
    Left _ -> False
    Right _ -> True
  where
    -- The parser just needs to check for validity, not return anything
    parser :: Parser ()
    parser = do
        _ <- string libdir
        _ <- char pathSeparator
        _ <- string "ghc-"
        _ <- satisfy isDigit
        pure ()


-- -----------------------------------------------------------------------------
-- Return all installed haskell packages
-- -----------------------------------------------------------------------------

allInstalledPackages :: MonadEnv m => m (Set Package)
allInstalledPackages = do
    GhcLibDir libDir <- askGhcLibDir
    vMappings <- askVersionMappings
    cMappings <- askContentMappings
    let pkgs = pkgsHaveContent vMappings cMappings $ hasDirMatching (==libDir)
    pure $ notGHC pkgs

-- -----------------------------------------------------------------------------
-- Common helper utils, etc.
-- -----------------------------------------------------------------------------

tryMaybe     :: (a -> Maybe b) -> a -> Either a b
tryMaybe f a = maybe (Left a) Right $ f a
