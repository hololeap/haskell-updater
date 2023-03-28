{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
   Module      : Distribution.Gentoo.Types
   Description : Dealing with installed packages on Gentoo.
   Copyright   : Copyright 1999-2023 Gentoo Authors
   License     : GPL-2 or later

   Types needed by various other modules.
-}

module Distribution.Gentoo.Types
    ( BSFilePath
      -- * Portage
    , Category
    , Pkg
    , VerPkg
    , VCatPkg
    , Slot
    , Package (..)
      -- * GHC
    , Content (..)
    , GhcLibDir (..)
    , CabalPkg (..)
    , ConfSubdir (..)
    , ConfMap
    , GhcConfMap (..)
    , GhcConfFiles (..)
    , GentooConfMap (..)
    ) where

import Data.ByteString.Char8 (ByteString)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import qualified Distribution.InstalledPackageInfo as Cabal
    (InstalledPackageInfo (..))
import qualified Distribution.Types.PackageId as Cabal
    (PackageId)

-- Alias used to indicate that this ByteString represents a FilePath
type BSFilePath = ByteString

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
data Package = Package Category Pkg (Maybe Slot)
             deriving(Eq, Ord, Show, Read)

-- | Representation of individual lines in a @CONTENTS@ file.
data Content = Dir BSFilePath
             | Obj BSFilePath
               deriving (Eq, Show, Ord)

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
type ConfMap = Map.Map Cabal.PackageId (Set.Set CabalPkg)

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

