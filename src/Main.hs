{- |
   Module      : Main
   Description : The haskell-updater executable
   Copyright   : (c) Ivan Lazar Miljenovic, Stephan Friedrichs, Emil Karlson 2010
   License     : GPL-2 or later

   The executable module of haskell-updater, which finds Haskell
   packages to rebuild after a dep upgrade or a GHC upgrade.
-}

{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Distribution.Gentoo.CmdLine
import qualified Distribution.Gentoo.CmdLine.Types as CmdLine -- (CmdLineArgs, BuildTarget)
import Distribution.Gentoo.GHC
    ( MonadPkgState (oldGhcPkgs, brokenPkgs, allInstalledPkgs)
    , ghcVersion, ghcLibDir, ghcLoc, unCPV
    )
import Distribution.Gentoo.Packages
import Distribution.Gentoo.PkgManager
import Distribution.Gentoo.Types as Types
import Distribution.Gentoo.Types.HUMode as Mode
import Distribution.Gentoo.Util (These(..), toListNE)

import           Control.Monad         (unless, when)
import qualified Control.Monad         as CM
import           Control.Monad.Reader  (runReaderT)
import           Control.Monad.State.Strict
    (StateT, evalStateT, get, put, MonadIO, liftIO)
import           Data.Bifoldable       (bifoldMap)
import           Data.Foldable         (toList)
import qualified Data.List             as L
import           Data.Sequence         (ViewR(..), viewr, (|>))
import qualified Data.Sequence         as Seq
import qualified Data.Set              as Set
import           Data.Version          (showVersion)
import qualified Paths_haskell_updater as Paths (version)
import           System.Console.GetOpt
import           System.Environment    (getArgs, getProgName)
import           System.Exit           (ExitCode (..), exitSuccess, exitWith)
import           System.IO             (hPutStrLn, stderr)

import Output

main :: IO ()
main = do args <- getArgs
          defPM <- defaultPM
          case parseArgs defPM args of
              Left err -> die err
              Right (cmdArgs, rawArgs)  -> runAction cmdArgs rawArgs

runAction :: CmdLine.CmdLineArgs -> RawPMArgs -> IO a
runAction cmdArgs rawArgs = do

    mode <- either die pure $ mkHUMode cmdArgs rawArgs
    case mode of
        HelpMode -> help
        VersionMode -> version
        RunMode rm pm -> flip runReaderT rm $ do

            vsay "Command line args:"
            liftIO getArgs >>= vsay . show
            vsay $ show cmdArgs
            vsay ""
            vsay "Internal representation for haskell-updater mode:"
            vsay $ show (RunMode rm pm)
            vsay ""

            runUpdater pm rawArgs (rm, getExtraRawArgs pm)

dumpHistory :: MonadSay m => RunHistory -> m ()
dumpHistory historySeq = do
    say "Updater's past history:"
    CM.forM_ historyList $ \(n, entry, ec) -> say $ unwords
        [ "Pass"
        , show n ++ ":"
        , show $ printPkg <$> Set.toList entry
        , show ec
        ]
    say ""
  where historyList :: [(Int, Set.Set Package, ExitCode)]
        historyList =
            [ (n, entry, ec)
            | ((entry, ec), n) <- zip (toList historySeq) [1..]
            ]

-- | An action that controls the looping mechanism inside 'runUpdater'. This
--   holds the logic for what action to take after running @emerge@ (e.g. exit
--   with success/error or continue to the next iteration).
type UpdaterLoop m
    =  BuildPkgs
    -> RunHistory
    -> m ()

-- | Run the main part of @haskell-updater@ (e.g. not @--help@ or
--   @--version@).
runUpdater
    :: PkgManager
    -> RawPMArgs
    -> BuildPkgsIn Env
    -> Env a
runUpdater pkgMgr userArgs bpi = do
    systemInfo pkgMgr userArgs
    bps <- getPackageState pkgMgr
    let ps = buildPkgsPending bps
    case runMode pkgMgr of
        Left (ListMode _) -> listPkgs ps
        Right (PortageListMode _) -> listPkgs ps
        _ -> case getLoopType pkgMgr of
            UntilNoPending -> loopUntilNoPending bps Seq.empty
            UntilNoChange -> loopUntilNoChange bps Seq.empty
            NoLoop -> do
                exitCode <- buildPkgs bps bpi
                liftIO $ exitWith exitCode
    success "done!"
  where
    listPkgs :: PendingPackages -> Env ()
    listPkgs ps = do
        mapM_ (liftIO . putStrLn . printPkg) (getPkgs ps)
        success "done!"

    loopUntilNoPending :: UpdaterLoop Env
    loopUntilNoPending bps hist
        -- Stop when there are no more pending packages
        | Set.null (getPkgs ps) = alertDone Nothing

        -- Look to see if the current set of broken haskell packages matches
        -- any in the history. If it does, this means we're in a loop
        | getPkgs ps `elem` (fst <$> toList hist) = alertStuck hist Nothing

        -- Otherwise, keep going
        | otherwise = updateAndContinue loopUntilNoPending bps hist

        where ps = buildPkgsPending bps

    loopUntilNoChange :: UpdaterLoop Env
    loopUntilNoChange bps hist = case viewr hist of
        -- If there is no history, the first update still needs to be run
        EmptyR -> updateAndContinue loopUntilNoChange bps hist

        -- There is at least one entry in the history
        (prevHist :> (lastPkgSet, lastEC)) ->
            let nothingChanged, cmdSuccess, noTargets :: Bool

                -- Is the broken package set identical to what it was
                -- before the last update?
                nothingChanged = getPkgs ps == lastPkgSet

                -- Did the last update command succeed?
                cmdSuccess = case lastEC of
                    ExitSuccess -> True
                    ExitFailure _ -> False

                -- Are there no targets for the package manager?
                noTargets = emptyTargets (buildPkgsTargets bps)

                -- Alert that we're finished, but add a warning if there are
                -- still broken packages on the system
                done = alertDone $ if Set.null (getPkgs ps)
                    then Nothing
                    else Just successIncomplete

                -- Alert that we're stuck, but add a note if it failed on its
                -- first run
                stuck = alertStuck hist $ if Seq.null prevHist
                    then Just stuckOnePass
                    else Nothing

            in case (nothingChanged, cmdSuccess) of
                -- The success state: The last update completed and
                -- nothing changed
                (True, True) -> done

                -- Stuck state: The last update failed and nothing changed
                (True, False) -> stuck

                (False, _)
                    -- A change happened, but there are no more targets
                    | noTargets -> done

                    -- A change happened and we have targets still: continue
                    | otherwise -> updateAndContinue loopUntilNoChange bps hist

      where
        ps = buildPkgsPending bps

        -- This mostly comes up with the 'UntilNoChange' loop type, so we'll limit
        -- it to that for now.
        stuckOnePass =
            [ "NOTE: The updater loop failed after one pass, with no changes to the state of"
            , "broken Haskell packages on the system. This is often caused by the package"
            , "manager failing during dependency resolution. Check the output to be sure."
            ]

        successIncomplete =
            [ "WARNING: The updater loop appears to have completed, but there are still"
            , "broken Haskell packages detected on the system!"
            ]

    -- Rebuild the packages then retrieve fresh package state
    updateAndContinue :: UpdaterLoop Env -> UpdaterLoop Env
    updateAndContinue f bps hist = do
        -- Exit with failure if there are no targets for the package manager
        when (emptyTargets (buildPkgsTargets bps)) alertNoTargets

        exitCode <- buildPkgs bps bpi

        bps' <- getPackageState pkgMgr
        let ps = buildPkgsPending bps'
            hist' = hist |> (getPkgs ps, exitCode)

        f bps' hist'

    emptyTargets :: Set.Set Types.Target -> Bool
    emptyTargets = all $ \case
        TargetInvalid ps -> Set.null (getPkgs ps)
        TargetAll ps -> Set.null (getPkgs ps)
        CustomTarget _ -> False

    alertDone maybeMsg = success $ unlines
        $ "Nothing to build!"
        : maybe [] ("":) maybeMsg

    alertStuck hist maybeMsg = do
        dumpHistory hist
        liftIO $ die $ unlines
            $ "Updater stuck in the loop and can't progress"
            : maybe [] ("":) maybeMsg

    alertNoTargets = liftIO $ die "No targets to pass to the package manager!"

-- | As needed, query @ghc-pkg check@ for broken packages, scan the filesystem
--   for installed packages, and look for misc breakages. Return the results
--   summarized for use with 'buildPkgs'.
getPackageState
    :: (MonadSay m, MonadPkgState m)
    => PkgManager
    -> m BuildPkgs
getPackageState pkgMgr =
    case runMode pkgMgr of
        Left mode -> fromRunMode mode
        Right (PortageBasicMode (Left PreservedRebuild)) -> do
            ps <- InvalidPending <$> getInvalid
            let ct = CustomTarget "@preserved-rebuild"
            pure $ BuildNormal pkgMgr ps (Set.singleton ct)
        Right (PortageBasicMode (Right targ)) -> fromRunMode (BasicMode targ)
        Right (PortageListMode targ) -> fromRunMode (ListMode targ)
        Right (ReinstallAtomsMode targ) -> flip evalStateT Nothing $ do
            aps <- getAll
            let pending = \case
                    OnlyInvalid -> withIPCache InvalidPending
                    AllInstalled -> pure $ AllPending aps
                normalTarg = \case
                    OnlyInvalid -> withIPCache TargetInvalid
                    AllInstalled -> pure $ TargetAll aps
                extraTargs = bifoldMap
                            (Set.singleton . \case
                                WorldTarget -> CustomTarget "@world"
                                WorldFullTarget -> CustomTarget "@world"
                            )
                            (Set.fromList . map CustomTarget . toListNE)
            (p,ts) <- case targ of
                These ta th -> do
                    p <- pending ta
                    nt <- normalTarg ta
                    pure (p, Set.insert nt (extraTargs th))
                This ta -> do
                    p <- pending ta
                    nt <- normalTarg ta
                    pure (p, Set.singleton nt)
                That th -> do
                    p <- InvalidPending <$> getInvalid
                    pure (p, extraTargs th)
            pure $ BuildRAMode p ts aps
  where
    fromRunMode
        :: (MonadSay m, MonadPkgState m)
        => RunMode
        -> m BuildPkgs
    fromRunMode mode = do
        ps <- case getTarget mode of
            OnlyInvalid -> InvalidPending <$> getInvalid
            AllInstalled -> AllPending <$> getAll
        pure $ BuildNormal pkgMgr ps Set.empty

    getInvalid :: (MonadSay m, MonadPkgState m) => m InvalidPkgs
    getInvalid = do
        say "Searching for packages installed with a different version of GHC."
        say ""
        old <- oldGhcPkgs
        pkgListPrintLn "old" old

        say "Searching for Haskell libraries with broken dependencies."
        say ""
        (broken, unknown_packages, unknown_files) <- brokenPkgs
        let broken' = Set.fromList broken
        printUnknownPackagesLn (map unCPV unknown_packages)
        printUnknownFilesLn unknown_files
        pkgListPrintLn "broken" (notGHC broken')

        return $ InvalidPkgs $ old <> broken'

    getAll :: (MonadSay m, MonadPkgState m) => m AllPkgs
    getAll = do
        say "Searching for packages installed with the current version of GHC."
        say ""
        pkgs <- allInstalledPkgs
        pkgListPrintLn "installed" pkgs
        return $ AllPkgs pkgs

    printUnknownPackagesLn [] = return ()
    printUnknownPackagesLn ps = do
        say "The following packages are orphan (not installed by your package manager):"
        printList id ps
        say ""
    printUnknownFilesLn [] = return ()
    printUnknownFilesLn fs = do
        say "The following files are orphan (not installed by your package manager):"
        printList id fs
        say "It is strongly advised to remove orphans:"
        say "    One of known sources of orphans is packages installed before 01 Jan 2015."
        say "    If you know it's your case you can easily remove such files:"
        say "        # rm -v -- `qfile -o $(ghc --print-libdir)/package.conf.d/*.conf $(ghc --print-libdir)/gentoo/*.conf`"
        say "        # ghc-pkg recache"
        say "    It will likely need one more 'haskell-updater' run."
        say ""

    withIPCache
        :: (MonadSay m, MonadPkgState m)
        => (InvalidPkgs -> a)
        -> StateT (Maybe InvalidPkgs) m a
    withIPCache f = fmap f $ get >>= \case
        Nothing -> do
            ips <- getInvalid
            put (Just ips)
            pure ips
        Just ips -> pure ips

-- -----------------------------------------------------------------------------
-- Printing information.

help :: IO a
help = progInfo >>= sayIO . success

version :: IO a
version = fmap (++ '-' : showVersion Paths.version) getProgName >>= sayIO . success

progInfo :: IO String
progInfo = do pName <- getProgName
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

systemInfo
    :: (MonadSay m, MonadIO m)
    => PkgManager
    -> RawPMArgs
    -> m ()
systemInfo pkgMgr rawArgs = do
    ver    <- liftIO ghcVersion
    pName  <- liftIO getProgName
    let pVer = showVersion Paths.version
    pLoc   <- liftIO ghcLoc
    libDir <- liftIO ghcLibDir
    say $ "Running " ++ pName ++ "-" ++ pVer ++ " using GHC " ++ ver
    say $ "  * Executable: " ++ pLoc
    say $ "  * Library directory: " ++ libDir
    say $ "  * Package manager (PM): " ++ nameOfPM (toPkgManager pkgMgr)
    unless (null rawArgs) $
        say $ "  * PM auxiliary arguments: " ++ unwords rawArgs
    say $ "  * Targets: " ++ L.intercalate ", " ts
    say $ "  * Mode: " ++ argString m
    say ""
  where
    (m, ts) = case runMode pkgMgr of
        Left mode -> (:[]) <$> printRunMode mode
        Right (PortageBasicMode (Left PreservedRebuild)) ->
            (CmdLine.BasicMode, [argString CmdLine.PreservedRebuild])
        Right (PortageBasicMode (Right targ)) ->
            (:[]) <$> printRunMode (BasicMode targ)
        Right (PortageListMode targ) ->
            (:[]) <$> printRunMode (ListMode targ)
        Right (ReinstallAtomsMode targ) ->
            let strs = bifoldMap
                    ((:[]) . \case
                        OnlyInvalid -> argString CmdLine.OnlyInvalid
                        AllInstalled -> argString CmdLine.AllInstalled
                    )
                    (bifoldMap
                        ((:[]) . \case
                            WorldTarget -> argString CmdLine.WorldTarget
                            WorldFullTarget -> unwords
                                [argString CmdLine.WorldTarget, "(full)"]
                        )
                        toListNE
                    )
                    targ
            in (CmdLine.ReinstallAtomsMode, strs)

    printRunMode :: RunMode -> (CmdLine.RunMode, String)
    printRunMode = \case
        BasicMode OnlyInvalid ->
            (CmdLine.BasicMode, argString CmdLine.OnlyInvalid)
        BasicMode AllInstalled ->
            (CmdLine.BasicMode, argString CmdLine.AllInstalled)
        ListMode OnlyInvalid ->
            (CmdLine.ListMode, argString CmdLine.OnlyInvalid)
        ListMode AllInstalled ->
            (CmdLine.ListMode, argString CmdLine.AllInstalled)

-- -----------------------------------------------------------------------------
-- Utility functions

success :: (MonadSay m, MonadIO m) => String -> m a
success msg = do say msg
                 liftIO exitSuccess

die     :: String -> IO a
die msg = do putErrLn ("ERROR: " ++ msg)
             exitWith (ExitFailure 1)

putErrLn :: String -> IO ()
putErrLn = hPutStrLn stderr