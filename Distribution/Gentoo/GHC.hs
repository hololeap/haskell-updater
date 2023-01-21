{- |
   Module      : Distribution.Gentoo.GHC
   Description : Find GHC-related breakages on Gentoo.
   Copyright   : (c) Ivan Lazar Miljenovic 2009
   License     : GPL-2 or later

   This module defines helper functions to find broken packages in
   GHC, or else find packages installed with older versions of GHC.
 -}

{-# LANGUAGE LambdaCase #-}

module Distribution.Gentoo.GHC
       ( CabalPkg(..)
       , Cabal.PackageId
       , showPackageId
       , ghcVersion
       , ghcLoc
       , ghcLibDir
       , oldGhcPkgs
       , brokenPkgs
       , allInstalledPackages
       ) where

import Distribution.Gentoo.Util
import Distribution.Gentoo.Packages

-- Cabal imports
import qualified Distribution.InstalledPackageInfo as Cabal
    ( InstalledPackageInfo(..)
    , parseInstalledPackageInfo
    )
import Distribution.Parsec (simpleParsec)
import Distribution.Pretty (Pretty(..))
import Distribution.Simple.Utils (die')
import qualified Distribution.Types.PackageId as Cabal
    ( PackageId
    -- , PackageIdentifier(..)
    )
import qualified Distribution.Types.LibraryName as Cabal
    ( LibraryName(..)
    )
import qualified Distribution.Verbosity as V
import Text.PrettyPrint (render)

-- Other imports
import Data.Char(isDigit)
import Data.Either (rights)
import Data.Maybe (catMaybes, isJust)
import qualified Data.Map.Strict as Map
import qualified Data.ByteString.Char8 as BS
import           Data.Set (Set)
import qualified Data.Set as Set
import System.FilePath((</>), takeExtension, pathSeparator)
import System.Directory( canonicalizePath
                       , doesDirectoryExist
                       , findExecutable
                       , listDirectory )
import System.Process (readProcess)
import Control.Monad

import Output

data CabalPkg
    = CabalPkg
    { cabalConfPath :: FilePath
    , cabalConfInfo :: Cabal.InstalledPackageInfo
    }
    deriving (Show, Eq)

instance Ord CabalPkg where
    CabalPkg fp1 _ `compare` CabalPkg fp2 _ = fp1 `compare` fp2

-- | Unique (normal) or multiple (broken) mapping
type ConfMap = Map.Map Cabal.PackageId (Set CabalPkg)

-- -----------------------------------------------------------------------------
-- ConfMap manipulation
-- -----------------------------------------------------------------------------

-- Return the Gentoo .conf files found in this GHC libdir
listConfFiles :: ConfSubdir -> IO (Set FilePath)
listConfFiles subdir = do
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

-- Fold Gentoo .conf files from the current GHC version and
-- create a Map
foldConf :: Foldable t => Verbosity -> t FilePath -> IO ConfMap
foldConf v = foldM (addConf v) Map.empty

-- | Add this .conf file to the Map
addConf :: Verbosity -> ConfMap -> FilePath -> IO ConfMap
addConf v cmp conf = do
    cont <- BS.readFile conf
    -- empty files are created for
    -- phony packages like CABAL_CORE_LIB_GHC_PV
    -- and binary-only packages.
    if BS.null cont
        then return cmp
        else case Cabal.parseInstalledPackageInfo cont of
            Right (ws, ipi) -> do
                let i  = Cabal.sourcePackageId ipi
                    dn = showPackageId i
                vsay v $ unwords [conf, "resolved:", show dn]
                unless (null ws) $ do
                    vsay v $ "    Warnings:"
                    forM_ ws $ \w -> do
                        vsay v $ "        " ++ w
                pure $ pushConf cmp i conf ipi
            Left ne -> do
                        say v $ unwords [ "failed to parse"
                                        , show conf
                                        , ":"
                                        ]
                        forM_ ne $ \e -> say v $ "    " ++ show e
                        return cmp

pushConf :: ConfMap -> Cabal.PackageId -> FilePath -> Cabal.InstalledPackageInfo -> ConfMap
pushConf m k p ipi = Map.insertWith (Set.union) k (Set.singleton (CabalPkg p ipi)) m

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
brokenPkgs :: Verbosity -> IO (Set Package, Set Cabal.PackageId, Set FilePath)
brokenPkgs v = findBrokenConfs v >>= checkPkgs v

-- | Returns: broken, unknown_files
checkPkgs :: Verbosity
             -> (Set Cabal.PackageId, Set FilePath)
             -> IO (Set Package, Set Cabal.PackageId, Set FilePath)
checkPkgs v (pns, brokenConfs) = do
       files_to_pkgs <- resolveFiles brokenConfs
       let gentooFiles = Map.keysSet files_to_pkgs
           pkgs = Set.fromList $ Map.elems files_to_pkgs
           orphan_gentoo_files = brokenConfs Set.\\ gentooFiles
       vsay v $ unwords [ "checkPkgs: searching for gentoo .conf orphans"
                        , show (length orphan_gentoo_files)
                        , "of"
                        , show (length brokenConfs)
                        ]
       return (pkgs, pns, orphan_gentoo_files)

-- | .conf files from broken packages of this GHC version
-- Returns two lists:
-- * @['Cabal.PackageId']@ - list of broken cabal packages we could not resolve
--                           to Gentoo's .conf files
-- * @['FilePath']@ - list of '.conf' files resolved from broken
--                    PackageId reported by 'ghc-pkg check'
findBrokenConfs :: Verbosity -> IO (Set Cabal.PackageId, Set FilePath)
findBrokenConfs v =
    do vsay v "brokenConfs: getting broken output from 'ghc-pkg'"
       ghc_pkg_brokens <- getBrokenGhcPkg v
       vsay v $ unwords ["brokenConfs: resolving Cabal package names to gentoo equivalents."
                        , show (length ghc_pkg_brokens)
                        , "Cabal packages are broken:"
                        , unwords $ showPackageId <$> Set.toList ghc_pkg_brokens
                        ]

       (orphan_broken, orphan_confs) <- getOrphanBroken
       vsay v $ unwords [ "brokenConfs: ghc .conf orphans:"
                        , show (length orphan_broken)
                        , "are orphan:"
                        , unwords $ showPackageId <$> Set.toList orphan_broken
                        ]

       installed_but_not_registered <- getNotRegistered v
       vsay v $ unwords [ "brokenConfs: ghc .conf not registered:"
                        , show (length installed_but_not_registered)
                        , "are not registered:"
                        , unwords $ showPackageId <$> Set.toList installed_but_not_registered
                        ]

       registered_twice <- getRegisteredTwice v
       vsay v $ unwords [ "brokenConfs: ghc .conf registered twice:"
                        , show (length registered_twice)
                        , "are registered twice:"
                        , unwords $ showPackageId <$> Set.toList registered_twice
                        ]

       let all_broken = Set.unions [ ghc_pkg_brokens
                                   , orphan_broken
                                   , installed_but_not_registered
                                   , registered_twice
                                   ]

       vsay v "brokenConfs: reading '*.conf' files"
       cnfs <- listConfFiles GentooConfs >>= foldConf v
       vsay v $ "brokenConfs: got " ++ show (Map.size cnfs) ++ " '*.conf' files"
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
getBrokenGhcPkg :: Verbosity -> IO (Set Cabal.PackageId)
getBrokenGhcPkg v = do
    s <- ghcPkgRawOut ["check", "--simple-output"]
    Set.fromList . catMaybes <$> traverse check (words s)
  where
    check :: String -> IO (Maybe Cabal.PackageId)
    check s = do
        let mpid = simpleParsec s
        unless (isJust mpid) $
            say v $ unwords ["Unable to parse as PackageId:", show s]
        pure mpid

-- | Around Jan 2015 we have started to install
--   all the .conf files in 'src_install()' phase.
--   Here we pick orphan ones and notify user about it.
getOrphanBroken :: IO (Set Cabal.PackageId, Set FilePath)
getOrphanBroken = do
    registered_confs <- listConfFiles GHCConfs
    confs_to_pkgs <- resolveFiles registered_confs
    let conf_files = Map.keysSet confs_to_pkgs
        orphan_conf_files = Set.toList $ registered_confs Set.\\ conf_files
    orphan_packages <- fmap rights $
        forM orphan_conf_files $ \conf -> do
            bs <- BS.readFile conf
            let ipi = Cabal.parseInstalledPackageInfo bs
            pure $ Cabal.sourcePackageId . snd <$> ipi
    return (Set.fromList orphan_packages, Set.fromList orphan_conf_files)

-- Return packages, that seem to have
-- been installed via emerge (have gentoo/.conf entry),
-- but are not registered in package.conf.d.
-- Usually happens on manual cleaning or
-- due to unregistration bugs in old eclass.
getNotRegistered :: Verbosity -> IO (Set Cabal.PackageId)
getNotRegistered v = do
    installed_confs  <- listConfFiles GentooConfs >>= foldConf v
    registered_confs <- listConfFiles GHCConfs >>= foldConf v
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
getRegisteredTwice :: Verbosity -> IO (Set Cabal.PackageId)
getRegisteredTwice v = do
    registered_confs <- listConfFiles GHCConfs >>= foldConf v
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
oldGhcPkgs :: Verbosity -> IO (Set Package)
oldGhcPkgs v =
    do thisGhc <- ghcLibDir
       vsay v $ "oldGhcPkgs ghc lib: " ++ show thisGhc
       let thisGhc' = BS.pack thisGhc
       -- It would be nice to do this, but we can't assume
       -- some crazy user hasn't deleted one of these dirs
       -- libFronts' <- filterM doesDirectoryExist libFronts
       Set.fromList . notGHC <$> checkLibDirs v thisGhc' libFronts

-- Find packages installed by other versions of GHC in this possible
-- library directory.
checkLibDirs :: Verbosity -> BSFilePath -> [BSFilePath] -> IO [Package]
checkLibDirs v thisGhc libDirs =
    do vsay v $ "checkLibDir ghc libs: " ++ show (thisGhc, libDirs)
       pkgsHaveContent (hasDirMatching wanted)
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

allInstalledPackages :: IO (Set Package)
allInstalledPackages = do libDir <- ghcLibDir
                          let libDir' = BS.pack libDir
                          fmap (Set.fromList . notGHC) $ pkgsHaveContent
                                       $ hasDirMatching (==libDir')

-- -----------------------------------------------------------------------------
-- Common helper utils, etc.
-- -----------------------------------------------------------------------------

-- Get only the first line of output
rawSysStdOutLine     :: FilePath -> [String] -> IO String
rawSysStdOutLine app = fmap (head . lines) . rawCommand app

-- | Run a command and return its stdout
rawCommand          :: FilePath -> [String] -> IO String
rawCommand cmd args = readProcess cmd args ""

-- Get the first line of output from calling GHC with the given
-- arguments.
ghcRawOut      :: [String] -> IO String
ghcRawOut args = ghcLoc >>= flip rawSysStdOutLine args

-- | Find an executable in $PATH. If it doesn't exist, 'die'' with an
--   error.
findExe
    :: String -- ^ The executable to search for
    -> IO FilePath
findExe exe = findExecutable exe >>= \case
    Just e  -> pure e
    Nothing -> die' V.normal $
        "Could not find '" ++ show exe ++ "' executable on system"

ghcLoc :: IO FilePath
ghcLoc = findExe "ghc"

ghcPkgLoc :: IO FilePath
ghcPkgLoc = findExe "ghc-pkg"

-- The version of GHC installed.
ghcVersion :: IO String
ghcVersion = dropWhile (not . isDigit) <$> ghcRawOut ["--version"]

-- The directory where GHC has all its libraries, etc.
ghcLibDir :: IO FilePath
ghcLibDir = canonicalizePath =<< ghcRawOut ["--print-libdir"]

ghcPkgRawOut      :: [String] -> IO String
ghcPkgRawOut args = ghcPkgLoc >>= flip rawCommand args

showPackageId :: Cabal.PackageId -> String
showPackageId = render . pretty

data ConfSubdir = GHCConfs
                | GentooConfs

subdirToDirname :: ConfSubdir -> FilePath
subdirToDirname subdir =
    case subdir of
        GHCConfs    -> "package.conf.d"
        GentooConfs -> "gentoo"

tryMaybe     :: (a -> Maybe b) -> a -> Either a b
tryMaybe f a = maybe (Left a) Right $ f a
