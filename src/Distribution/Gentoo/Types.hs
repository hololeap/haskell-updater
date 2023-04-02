{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
   Module      : Distribution.Gentoo.Types
   Description : Dealing with installed packages on Gentoo.
   Copyright   : Copyright 1999-2023 Gentoo Authors
   License     : GPL-2 or later

   Types needed by various other modules.
-}

module Distribution.Gentoo.Types
    (
      -- * Portage
      Category
    , Pkg
    , VerPkg
    , VCatPkg
    , Slot
    , Package (..)
    , Content (..)
    , isDir
    , pathOf
    , VersionMappings
    , ContentMappings
    , PackageMappings
      -- * GHC
    , GhcLibDir (..)
    , CabalPkg (..)
    , ConfSubdir (..)
    , ConfMap
    , GhcConfMap (..)
    , GhcConfFiles (..)
    , GentooConfMap (..)
    , CabalPkgMap
    ) where

import qualified Data.Set as Set

import qualified Distribution.InstalledPackageInfo as Cabal
    (InstalledPackageInfo (..))
import qualified Distribution.Types.PackageId as Cabal
    (PackageId)

import Distribution.Gentoo.Util


-- -----------------------------------------------------------------------------
-- Portage-specific environment
-- -----------------------------------------------------------------------------

-- Representation of a cat/pkgname in Gentoo.  Note that this is
-- overly simplified.

type Category = String
type Pkg = String -- ^ Package name.
type VerPkg = String -- ^ Package name with version.
type VCatPkg = (Category, VerPkg)
type Slot = String

-- | When we are (re-)building packages, we don't care about the
--   version, just the slot.
data Package = Package Category Pkg Slot
             deriving(Eq, Ord, Show, Read)

-- | Representation of individual lines in a @CONTENTS@ file.
data Content = Dir FilePath
             | Obj FilePath
               deriving (Eq, Show, Ord)

isDir         :: Content -> Bool
isDir (Dir _) = True
isDir _       = False

pathOf           :: Content -> FilePath
pathOf (Dir dir) = dir
pathOf (Obj obj) = obj

type VersionMappings = Mappings VCatPkg Package
type ContentMappings = Mappings VCatPkg Content
type PackageMappings = Mappings Package Cabal.PackageId

-- -----------------------------------------------------------------------------
-- GHC-specific environment
-- -----------------------------------------------------------------------------

newtype GhcLibDir = GhcLibDir FilePath
    deriving (Show, Eq, Ord)

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
type ConfMap = MonoidMap Cabal.PackageId CabalPkg

-- | The 'ConfMap' from GHC's package index
newtype GhcConfMap
    = GhcConfMap { getGhcConfMap :: ConfMap }
    deriving (Show, Eq, Ord, Semigroup, Monoid)

-- | All @.conf@ files from GHC's package index
--
--   Needed for 'getOrphanBroken'
newtype GhcConfFiles
    = GhcConfFiles { getGhcConfFiles :: Set.Set FilePath }
    deriving (Show, Eq, Ord, Semigroup, Monoid)

-- | The 'ConfMap' from the special Gentoo package index
newtype GentooConfMap
    = GentooConfMap { getGentooConfMap :: ConfMap }
    deriving (Show, Eq, Ord, Semigroup, Monoid)

-- | Mapping of 'CabalPkg's to their Cabal package name. This is useful for
--   later generating the mapping between Portage 'Package's and Cabal
--   'Cabal.PackageId's.
type CabalPkgMap = MonoidMap FilePath Cabal.PackageId
