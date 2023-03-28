{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# Language TupleSections #-}

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
       , libFronts
       ) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Char(isDigit, isAlphaNum, isSpace)
import Data.List(isPrefixOf)
import Data.Maybe
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Set (Set)
import qualified Data.Set as S
import System.Directory( doesDirectoryExist
                       , listDirectory)
import System.FilePath((</>))
import Text.Parsec
import Text.Parsec.Text

import Distribution.Gentoo.Types
import Distribution.Gentoo.Util

-- -----------------------------------------------------------------------------

-- Package equality, ignoring the Slot (i.e. same category and package
-- name).
samePackageAs :: Package -> Package -> Bool
samePackageAs (Package c1 p1 _) (Package c2 p2 _)
  = c1 == c2 && p1 == p2

ghcPkg :: Package
ghcPkg = Package "dev-lang" "ghc" "0"

-- Return all packages that are not a version of GHC.
notGHC :: [Package] -> [Package]
notGHC = filter (isNot ghcPkg)
  where
    isNot p1 = not . samePackageAs p1

-- Pretty-print the Package name based on how PMs expect it
printPkg :: Package -> String
printPkg (Package c p s) = c ++ "/" ++ p ++ ":" ++ s

-- Determine which slot the specific version of the package is in and
-- create the appropriate Package value.
toPackage           :: VCatPkg -> IO Package
toPackage cp@(c,vp) = do sl <- parseSlot cp
                         let p = stripVersion vp
                         return $ Package c p sl

-- | Remove the version information from the package name.
stripVersion :: VerPkg -> Pkg
stripVersion = concat . takeUntilVer . breakAll partSep
  where
    partSep x = x `elem` ['-', '_']

    -- Only the last bit that matches isVer is the real version bit.
    -- Note that this doesn't check that the last non-version bit is
    -- not a hyphen followed by digits.
    takeUntilVer = concat . init . breakAll isVer

    isVer as = isVerFront (init as) && isAlphaNum (last as)
    isVerFront ('-':as) = all (\a -> isDigit a || a == '.') as
    isVerFront _        = False

pkgPath        :: VCatPkg -> FilePath
pkgPath (c, vp) = pkgDBDir </> c </> vp

pkgDBDir :: FilePath
pkgDBDir = "/var/db/pkg"

-- -----------------------------------------------------------------------------

-- Parsing the CONTENTS file of installed packages.

isDir         :: Content -> Bool
isDir (Dir _) = True
isDir _       = False

pathOf           :: Content -> FilePath
pathOf (Dir dir) = dir
pathOf (Obj obj) = obj

-- Searching predicates.

hasContentMatching   :: (FilePath -> Bool) -> [Content] -> Bool
hasContentMatching p = any (p . pathOf)

hasDirMatching   :: (FilePath -> Bool) -> [Content] -> Bool
hasDirMatching p = hasContentMatching p . filter isDir

-- | Parse a @CONTENTS@ file.
parseContents :: MonadIO m
    => VCatPkg
    -> m [Content]
parseContents cp =
    liftIO (parseFromFile contentsParser cFile) >>= \case
        Left e -> throwParseError e
        Right ps -> pure ps
  where
    cFile = pkgPath cp </> "CONTENTS"

-- | Parser for a @CONTENTS@ file
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
parseSlot cp = do
    liftIO (parseFromFile slotParser cFile) >>= \case
        Left e -> throwParseError e
        Right ps -> pure ps
  where
    cFile = pkgPath cp </> "SLOT"

slotParser :: Parser Slot
slotParser = do
    s <- many $ noneOf ['/']
    pure s

untilEOL :: Parser String
untilEOL = many (noneOf ['\n', '\r']) <* endOfLine

-- -----------------------------------------------------------------------------

-- | Find all the packages that contain given files.
resolveFiles :: Set FilePath -> IO (Map FilePath Package)
resolveFiles fps = M.fromList . expand <$> forPkg grep
  where
    fps' = S.map Obj fps
    expand :: [(Package, [Content])] -> [(FilePath, Package)]
    expand pfs = [ (fn, pn)
                 | (pn, conts) <- pfs
                 , Obj fn <- conts
                 ]
    grep pn cont = Just (pn, filter (\e -> S.member e fps') cont)

-- | Run predecate 'p' for each installed package
--   and gather all 'Just' values
forPkg :: (Package -> [Content] -> Maybe a) -> IO [a]
forPkg p = do
    categories <- installedCats
    catMaybes <$> do
        flip concatMapM categories $ \cat -> do
            maybe_pkgs <- listDirectory (pkgDBDir </> cat)
            packages <- filterM (isPackage . (cat,)) maybe_pkgs
            forM packages $ \pkg -> do
                let cp = (cat, pkg)
                cpn <- toPackage cp
                cont <- parseContents cp
                return $ p cpn cont
  where
    isPackage :: VCatPkg -> IO Bool
    isPackage vcp@(_, vp) = do
        c1 <- doesDirectoryExist $ pkgPath vcp
        let c2 = not $ "-MERGING-" `isPrefixOf` vp
        return $ c1 && c2

-- Find which packages have Content information that matches the
-- provided predicate; to be used with the searching predicates
-- above.
pkgsHaveContent   :: ([Content] -> Bool) -> IO [Package]
pkgsHaveContent p = forPkg p'
    where p' pn cont = if p cont
                           then Just pn
                           else Nothing

-- Determine if this is a valid Category (such that at least one
-- package in that category has been installed).
isCat    :: String -> IO Bool
isCat fp = do isD <- doesDirectoryExist (pkgDBDir </> fp)
              return $ isD && isCat' fp
  where
    isCat' ('.':_) = False
    isCat' "world" = False
    isCat' _       = True

-- Return all Categories known in this system.
installedCats :: IO [Category]
installedCats = filterM isCat =<< listDirectory pkgDBDir

-- The possible places GHC could have installed lib directories
libFronts :: [FilePath]
libFronts = [ "/usr/lib", "/usr/lib64" ]
