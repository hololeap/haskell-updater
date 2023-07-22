{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{- |
   Module      : Distribution.Gentoo.Packages
   Description : Dealing with installed packages on Gentoo.
   Copyright   : (c) Ivan Lazar Miljenovic, Lennart Kolmodin 2009
   License     : GPL-2 or later

   This module defines helper functions that deal with installed
   packages in Gentoo.
-}
module Distribution.Gentoo.Packages
       ( Package
       , Content
       , notGHC
       , printPkg
       , resolveFiles
       , pkgsHaveContent
       , hasDirMatching
       , gatherMappings
       , libFronts
       ) where

-- import Control.Exception (throwIO)
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
-- import Control.Monad.Reader
import Data.Char (isDigit, isAlphaNum, isSpace)
-- import Data.Either (fromRight)
import Data.Foldable (foldMap')
import Data.List (isPrefixOf)
-- import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid (Ap (..))
import           Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import System.Directory( doesDirectoryExist
--                        , doesFileExist
                       , listDirectory)
import System.OsPath
import Text.Parsec
-- import Text.Parsec.Char
import Text.Parsec.Text
-- import Text.Read (readMaybe)

import Data.MonoidMap (MonoidMap(..))
import Distribution.Gentoo.Types
import Distribution.Gentoo.Util

-- -----------------------------------------------------------------------------

-- Package equality, ignoring the Slot (i.e. same category and package
-- name).
samePackageAs :: Package -> Package -> Bool
samePackageAs (Package c1 p1 _) (Package c2 p2 _)
  = c1 == c2 && p1 == p2

ghcPkg :: Package
ghcPkg = Package [osp| "dev-lang" |] "ghc" "0"

-- Return all packages that are not a version of GHC.
notGHC :: Set Package -> Set Package
notGHC = S.filter (isNot ghcPkg)
  where
    isNot p1 = not . samePackageAs p1

-- Pretty-print the Package name based on how PMs expect it
printPkg :: MonadThrow m => Package -> m String
printPkg (Package c p s) = do
    cs <- decodeUtf c
    pure $ cs ++ "/" ++ T.unpack p ++ ":" ++ T.unpack s

-- Determine which slot the specific version of the package is in and
-- create the appropriate Package value.
toPackage :: MonadThrow m => VCatPkg -> m Package
toPackage cp@(c,vp) = do
    sl <- parseSlot cp
    p <- stripVersion vp
    pure $ Package c p sl

-- | Remove the version information from the package name.
stripVersion :: MonadThrow m => VerPkg -> m Pkg
stripVersion osPath = parseM parser "doot" osPath

pkgPath        :: VCatPkg -> OsPath
pkgPath (c, vp) = pkgDBDir </> c </> vp

pkgDBDir :: OsPath
pkgDBDir = [osp| "/var/db/pkg" |]

-- -----------------------------------------------------------------------------

-- Parsing the CONTENTS file of installed packages.



-- Searching predicates.

hasContentMatching   :: (OsPath -> Bool) -> [Content] -> Bool
hasContentMatching p = any (p . pathOf)

hasDirMatching   :: (OsPath -> Bool) -> [Content] -> Bool
hasDirMatching p = hasContentMatching p . filter isDir

-- | Parse the CONTENTS file, gathering relevant file paths and building
--   needed mappings
parseContents :: MonadIO m
    => VCatPkg
    -> m [Content]
parseContents cp =
    liftIO (parseFromFile contentsParser cFile) >>= \case
        Left e -> throwParseError e
        Right ps -> pure ps
  where
    cFile = pkgPath cp ++ "/" ++ "CONTENTS"

-- | Parse a @CONTENTS@ file
contentsParser :: Parser [Content]
contentsParser = many $ dirParser <|> objParser
  where
    dirParser :: Parser Content
    dirParser = do
        _ <- string "dir"
        _ <- spaces
        p <- many $ satisfy (not . isSpace)
        _ <- untilEOL
        pure $ Dir p

    objParser :: Parser Content
    objParser = do
        _ <- string "obj"
        _ <- spaces
        p <- many $ satisfy (not . isSpace)
        _ <- spaces
        _ <- untilEOL
        pure $ Obj p

-- Parse the SLOT file
parseSlot :: MonadIO m => VCatPkg -> m Slot
parseSlot cp =
    liftIO (parseFromFile slotParser cFile) >>= \case
        Left e -> throwParseError e
        Right s -> pure s
  where
    cFile = pkgPath cp </> [osp| "SLOT" |]

slotParser :: Parser Slot
slotParser = do
    s <- many $ noneOf ['/']
    pure s

untilEOL :: Parser String
untilEOL = many (noneOf ['\n', '\r']) <* endOfLine

-- -----------------------------------------------------------------------------

-- | Find all the packages that contain given files.
resolveFiles
    :: VersionMappings
    -> ContentMappings
    -> Set FilePath
    -> MonoidMap FilePath Package
resolveFiles (MonoidMap vMap, _) (_, MonoidMap cMap) = foldMap' $ \p ->
    fromMaybe mempty
        $ M.lookup (Obj p) cMap
        >>= foldMap' (\v -> MonoidMap . M.singleton p <$> M.lookup v vMap)
-- = M.fromList . expand <$> forPkg grep
--   where
--     fps' = S.map Obj fps
--     expand :: [(Package, [Content])] -> [(FilePath, Package)]
--     expand pfs = [ (fn, pn)
--                  | (pn, conts) <- pfs
--                  , Obj fn <- conts
--                  ]
--     grep pn cont = Just (pn, filter (\e -> S.member e fps') cont)

-- | Build up the needed portage mappings from packages found in
--   @/var/db/pkg@
gatherMappings :: MonadIO m
    => CabalPkgMap
    -> m (VersionMappings, ContentMappings, PackageMappings)
gatherMappings (MonoidMap cPkgMap) = do
    categories <- installedCats
    getAp $ flip foldMap' categories $ \cat -> do
        maybePkgs <- Ap $ liftIO $ listDirectory (pkgDBDir </> cat)
        packages <- gatherPackages cat maybePkgs
        foldMap'
            (\pkg -> do
                let vcp = (cat, pkg)
                Ap $ parseContents vcp >>= mkMappings vcp)
            packages
  where
    isPackage :: MonadIO m => VCatPkg -> Ap m Bool
    isPackage vcp@(_, vp) = do
        c1 <- Ap $ liftIO $ doesDirectoryExist $ pkgPath vcp
        let c2 = not $ "-MERGING-" `isPrefixOf` vp
        return $ c1 && c2

    mkMappings :: MonadIO m
        => VCatPkg
        -> [Content]
        -> m (VersionMappings, ContentMappings, PackageMappings)
    mkMappings vcp cs = do
        pkg <- toPackage vcp
        let vMappings = mappings vcp pkg
        let (cMappings, pMappings) = foldMap' (\c ->
                ( mappings vcp c
                , maybe mempty
                    (foldMap' $ mappings pkg)
                    (M.lookup (pathOf c) cPkgMap)
                )) cs
        pure (vMappings, cMappings, pMappings)

    gatherPackages :: MonadIO m => Category -> [VerPkg] -> Ap m [VerPkg]
    gatherPackages cat = filterM (isPackage . (cat,))

-- | Find which packages have Content information that matches the
--   provided predicate; to be used with the searching predicates
--   above.
pkgsHaveContent
    :: VersionMappings
    -> ContentMappings
    -> ([Content] -> Bool)
    -> S.Set Package
pkgsHaveContent (MonoidMap vMap, _) (_, MonoidMap cMap) p =
    let filtered = M.filterWithKey (\c _ -> p [c]) cMap
        vpkgs = S.unions $ M.elems filtered
    in foldMap' (vMap M.!) vpkgs

-- Determine if this is a valid Category (such that at least one
-- package in that category has been installed).
isCat :: MonadIO m => String -> m Bool
isCat fp = do
    isD <- liftIO $ doesDirectoryExist (pkgDBDir </> fp)
    pure $ isD && isCat' fp
  where
    isCat' ('.':_) = False
    isCat' "world" = False
    isCat' _       = True

-- Return all Categories known in this system.
installedCats :: MonadIO m => m [Category]
installedCats = filterM isCat =<< liftIO (listDirectory pkgDBDir)

-- The possible places GHC could have installed lib directories
libFronts :: [FilePath]
libFronts = [ "/usr/lib", "/usr/lib64" ]
