{- |
   Module      : Main
   Description : The haskell-updater executable
   Copyright   : (c) Ivan Lazar Miljenovic, Stephan Friedrichs, Emil Karlson 2010
   License     : GPL-2 or later

   The executable module of haskell-updater, which finds Haskell
   packages to rebuild after a dep upgrade or a GHC upgrade.
-}

{-# LANGUAGE FlexibleContexts #-}

module Main (main) where


import           Control.Monad.Reader
import           Data.Char             (toLower, isDigit)
import           Data.Either           (partitionEithers)
import           Data.Map              (Map)
import qualified Data.List             as L
import qualified Data.Map              as M
import           Data.Maybe            (fromJust)
import           Data.Set              (Set)
import qualified Data.Set              as Set
import           Data.Version          (showVersion)
import qualified Paths_haskell_updater as Paths (version)
import           System.Console.GetOpt
import           System.Environment    (getArgs, getProgName)
import           System.Exit           (ExitCode (..), exitSuccess, exitWith)
import           System.IO             (hPutStrLn, stderr)
import           System.Process        (rawSystem)

import Distribution.Gentoo.Env
import Distribution.Gentoo.GHC
import Distribution.Gentoo.Packages
import Distribution.Gentoo.PkgManager
import Output

main :: IO ()
main = do args <- getArgs
          defPM <- defaultPM
          case parseArgs defPM args of
              Left err -> die err
              Right a  -> runAction a

runAction :: RunModifier -> IO a
runAction rm
    | showHelp rm    = runSayT v help
    | showVer rm     = runSayT v version
    | otherwise      = runDriver rm
  where
    v = verbosity rm

-- set of packages to rebuild at pass number
type DriverHistory = M.Map (Set.Set Package) Int

initialHistory :: DriverHistory
initialHistory = M.empty

dumpHistory :: MonadSay m => DriverHistory -> m ()
dumpHistory historyMap = do
    say "Updater's past history:"
    forM_ historyList $ \(n, entry) ->
      say $ unwords $ ["Pass", show n, ":"] ++ map printPkg (Set.toList entry)
  where historyList :: [(Int, Set.Set Package)]
        historyList = L.sort [ (n, entry) | (entry, n) <- M.toList historyMap ]

runDriver :: RunModifier -> IO a
runDriver rm = do
    runSayT v $ systemInfo rm t
    collectEnv v >>= \e -> runEnvT v e (updaterPass 1 initialHistory)
    runSayT v $ success "done!"
  where v = verbosity rm
        t = target rm
        updaterPass :: Int -> DriverHistory -> EnvIO ()
        updaterPass n pastHistory = do
            ps <- getTargetPackages t
            when (listOnly rm) $ do
                mapM_ (liftIO . putStrLn . printPkg) ps
                success "done!"
            when (null ps) $
                success "\nNothing to build!"
            when (ps `M.member` pastHistory) $ do
                dumpHistory pastHistory
                die "Updater stuck in the loop and can't progress"

            exitCode <- buildPkgs rm (Set.toList ps)

            -- don't try rerun rebuilder for cases where there
            -- is no chance to converge to empty set
            when (target rm == AllInstalled) $ liftIO $
                exitWith exitCode

            -- continue rebuild attempts
            updaterPass (n + 1) $ M.insert ps n pastHistory

data BuildTarget = OnlyInvalid
                 | AllInstalled -- Rebuild every haskell package
                   deriving (Eq, Ord, Show, Read)

getTargetPackages :: BuildTarget -> EnvIO (Set Package)
getTargetPackages t =
    case t of
        OnlyInvalid -> do say "Searching for packages installed with a different version of GHC."
                          say ""
                          old <- oldGhcPkgs
                          pkgListPrintLn "old" (Set.toList old)

                          say "Searching for Haskell libraries with broken dependencies."
                          say ""
                          (broken, unknown_packages, unknown_files) <- brokenPkgs
                          printUnknownPackagesLn (map showPackageId (Set.toList unknown_packages))
                          printUnknownFilesLn (Set.toList unknown_files)
                          pkgListPrintLn "broken" (notGHC (Set.toList broken))

                          return $ old <> broken

        AllInstalled -> do say "Searching for packages installed with the current version of GHC."
                           say ""
                           pkgs <- allInstalledPackages
                           pkgListPrintLn "installed" (Set.toList pkgs)
                           return pkgs

  where printUnknownPackagesLn [] = return ()
        printUnknownPackagesLn ps =
            do say "The following packages are orphan (not installed by your package manager):"
               printList id ps
               say ""
        printUnknownFilesLn [] = return ()
        printUnknownFilesLn fs =
            do say $ "The following files are orphan (not installed by your package manager):"
               printList id fs
               say $ "It is strongly advised to remove orphans:"
               say $ "    One of known sources of orphans is packages installed before 01 Jan 2015."
               say $ "    If you know it's your case you can easily remove such files:"
               say $ "        # rm -v -- `qfile -o $(ghc --print-libdir)/package.conf.d/*.conf $(ghc --print-libdir)/gentoo/*.conf`"
               say $ "        # ghc-pkg recache"
               say $ "    It will likely need one more 'haskell-updater' run."
               say ""

-- Full haskell-updater state
data RunModifier = RM { pkgmgr   :: PkgManager
                      , flags    :: [PMFlag]
                      , withCmd  :: WithCmd
                      , rawPMArgs :: [String]
                      , verbosity :: Verbosity
                      , listOnly :: Bool
                      , showHelp :: Bool
                      , showVer :: Bool
                      , target   :: BuildTarget
                      }
                   deriving (Eq, Ord, Show, Read)

data WithCmd = RunOnly
             | PrintOnly
             | PrintAndRun
               deriving (Eq, Ord, Show, Read)

type WithUserCmd = Either String WithCmd

withCmdMap :: Map String WithCmd
withCmdMap = M.fromList [ ("print", PrintOnly)
                        , ("run", RunOnly)
                        , ("print-and-run", PrintAndRun)
                        ]

defaultWithCmd :: String
defaultWithCmd = "print-and-run"

runCmd :: MonadIO m => WithCmd -> String -> [String] -> m ExitCode
runCmd mode cmd args = case mode of
        RunOnly     -> runCommand cmd args
        PrintOnly   -> liftIO $ putStrLn cmd_line >> exitSuccess
        PrintAndRun -> liftIO $ putStrLn cmd_line >> runCommand cmd args
    where cmd_line = unwords (cmd:args)

runCommand :: MonadIO m => String -> [String] -> m ExitCode
runCommand cmd = liftIO . rawSystem cmd

buildPkgs :: MonadIO m => RunModifier -> [Package] -> m ExitCode
buildPkgs rm ps = runCmd (withCmd rm) cmd args
    where
      (cmd, args) = buildCmd (pkgmgr rm) (flags rm) (rawPMArgs rm) ps

-- -----------------------------------------------------------------------------
-- Command-line flags

data Flag = HelpFlag
          | VersionFlag
          | PM String
          | CustomPMFlag String
          | FixInvalid
          | RebuildAll
          | Pretend
          | NoDeep
          | QuietFlag
          | VerboseFlag
          | ListOnlyFlag
          | Cmd String
          deriving (Eq, Ord, Show, Read)

parseArgs :: PkgManager -> [String] -> Either String RunModifier
parseArgs defPM args = argParser defPM $ getOpt' Permute options args

argParser :: PkgManager
          -> ([Flag], [String], [String], [String])
          -> Either String RunModifier
argParser dPM (fls, nonoptions, unrecognized, errs)
    | (not . null) errs         = Left $ unwords $ "Errors in arguments:" : errs
    | (not . null) unrecognized = Left $ unwords $ "Unknown options:" : unrecognized
    | (not . null) bPms         = Left $ unwords $ "Unknown package managers:" : bPms
    | (not . null) bCmds        = Left $ unwords $ "Unknown action:" : bCmds
    | otherwise                 = Right rm
  where
      (fls', ts) = partitionBy flagToTarget fls
      (fls'', pms) = partitionBy flagToPM fls'
      (bPms, pms') = partitionBy isValidPM pms
      (opts, cmds') = partitionBy flagToCmd fls''
      (bCmds, cmds) = partitionBy isValidCmd cmds'
      pm = emptyElse dPM last pms'
      opts' = Set.fromList opts
      cmd = emptyElse (fromJust $ M.lookup defaultWithCmd withCmdMap) last cmds
      hasFlag = flip Set.member opts'
      pmFlags = bool id (PretendBuild:) (hasFlag Pretend)
                . return $ bool UpdateDeep UpdateAsNeeded (hasFlag NoDeep)
      rm = RM { pkgmgr   = pm
              , flags    = pmFlags
              , withCmd  = cmd
              , rawPMArgs = nonoptions
              , verbosity = case () of
                                _ | hasFlag VerboseFlag -> Verbose
                                _ | hasFlag QuietFlag   -> Quiet
                                _                       -> Normal
              , listOnly  = hasFlag ListOnlyFlag
              , showVer   = hasFlag VersionFlag
              , showHelp = hasFlag HelpFlag
              , target   = last $ OnlyInvalid : ts
              }

flagToTarget             :: Flag -> Either Flag BuildTarget
flagToTarget FixInvalid  = Right OnlyInvalid
flagToTarget RebuildAll  = Right AllInstalled
flagToTarget f           = Left f

flagToPM                   :: Flag -> Either Flag PkgManager
flagToPM (CustomPMFlag pm) = Right $ stringToCustomPM pm
flagToPM (PM pm)           = Right $ choosePM pm
flagToPM f                 = Left f

flagToCmd :: Flag -> Either Flag WithUserCmd
flagToCmd (Cmd cmd) = Right $ chooseCmd cmd
flagToCmd f = Left f

chooseCmd :: String -> WithUserCmd
chooseCmd cmd = chooseCmd' $ map toLower cmd
  where
    chooseCmd' :: String -> WithUserCmd
    chooseCmd' "run"   = Right RunOnly
    chooseCmd' "print" = Right PrintOnly
    chooseCmd' "print-and-run" = Right PrintAndRun
    chooseCmd' c = Left c

isValidCmd :: WithUserCmd -> Either String WithCmd
isValidCmd = id

options :: [OptDescr Flag]
options =
    [ Option ['c']      ["dep-check"]       (NoArg FixInvalid)
      "Check dependencies of Haskell packages."
    -- deprecated alias for 'dep-check'
    , Option ['u']      ["upgrade"]         (NoArg FixInvalid)
      "Rebuild Haskell packages after a GHC upgrade."
    , Option ['a']      ["all"]             (NoArg RebuildAll)
      "Rebuild all Haskell libraries built with current GHC."
    , Option ['P']      ["package-manager"] (ReqArg PM "PM")
      $ "Use package manager PM, where PM can be one of:\n"
            ++ pmList ++ defPM
    , Option ['C']      ["custom-pm"]     (ReqArg CustomPMFlag "command")
      "Use custom command as package manager;\n\
      \ignores the --pretend and --no-deep flags."
    , Option ['p']      ["pretend"]         (NoArg Pretend)
      "Only pretend to build packages."
    , Option []         ["no-deep"]         (NoArg NoDeep)
      "Don't pull deep dependencies (--deep with emerge)."
    , Option ['l'] ["list-only"]            (NoArg ListOnlyFlag)
      "Output only list of packages for rebuild. One package per line."
    , Option ['V']      ["version"]         (NoArg VersionFlag)
      "Version information."
    , Option []          ["action"]          (ReqArg Cmd "action")
      $ "Specify whether to run the PM command or just print it\n"
            ++ actionList ++ defAction
    , Option ['q']      ["quiet"]           (NoArg QuietFlag)
      "Print only fatal errors (to stderr)."
    , Option ['v']      ["verbose"]         (NoArg VerboseFlag)
      "Be more elaborate (to stderr)."
    , Option ['h', '?'] ["help"]            (NoArg HelpFlag)
      "Print this help message."
    ]
    where
      pmList = unlines . map (" * " ++) $ definedPMs
      defPM = "The last valid value of PM specified is chosen.\n\
              \The default package manager is: " ++ defaultPMName ++ ",\n\
              \which can be overriden with the \"PACKAGE_MANAGER\"\n\
              \environment variable."
      actionList = unlines . map (" * " ++) $ M.keys withCmdMap
      defAction = "The last specified action is chosen.\n\
                       \The default action is: " ++ defaultWithCmd

-- -----------------------------------------------------------------------------
-- Printing information.

help :: (MonadSay m, MonadIO m) => m a
help = progInfo >>= success

version :: (MonadSay m, MonadIO m) => m a
version = fmap (++ '-' : showVersion Paths.version) (liftIO getProgName) >>= success

progInfo :: (MonadSay m, MonadIO m) => m String
progInfo = do pName <- liftIO getProgName
              return $ usageInfo (header pName) options
  where
    header pName = unlines [ pName ++ " -- Find and rebuild packages broken due to any of:"
                           , "            * GHC upgrade"
                           , "            * Haskell dependency upgrade"
                           , ""
                           , "Usage: " ++ pName ++ " [Options [-- [PM options]]"
                           , ""
                           , ""
                           , "Options:"]

systemInfo :: MonadIO m => RunModifier -> BuildTarget -> SayT m ()
systemInfo rm t = do
    ver    <- ghcVersion
    pName  <- liftIO getProgName
    let pVer = showVersion Paths.version
    pLoc   <- ghcLoc
    libDir <- ghcLibDir
    say $ "Running " ++ pName ++ "-" ++ pVer ++ " using GHC " ++ ver
    say $ "  * Executable: " ++ pLoc
    say $ "  * Library directory: " ++ libDir
    say $ "  * Package manager (PM): " ++ nameOfPM (pkgmgr rm)
    unless (null (rawPMArgs rm)) $
        say $ "  * PM auxiliary arguments: " ++ unwords (rawPMArgs rm)
    say $ "  * Mode: " ++ show t
    say ""

-- -----------------------------------------------------------------------------
-- Utility functions

success :: (MonadSay m, MonadIO m) => String -> m b
success msg = do say msg
                 liftIO exitSuccess

die :: MonadIO m => String -> m a
die msg = liftIO $ do
    putErrLn ("ERROR: " ++ msg)
    exitWith (ExitFailure 1)

putErrLn :: MonadIO m => String -> m ()
putErrLn = liftIO . hPutStrLn stderr

bool       :: a -> a -> Bool -> a
bool f t b = if b then t else f

partitionBy   :: (a -> Either l r) -> [a] -> ([l], [r])
partitionBy f = partitionEithers . map f

-- If the list is empty, return the provided value; otherwise use the function.
emptyElse        :: b -> ([a] -> b) -> [a] -> b
emptyElse e _ [] = e
emptyElse _ f as = f as

-- The version of GHC installed.
ghcVersion :: MonadIO m => m String
ghcVersion = dropWhile (not . isDigit) <$> ghcRawOut ["--version"]
