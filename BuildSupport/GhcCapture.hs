-- | Records the exact compiler commands of every package build (#2648).
--
-- @tools/headless_init_import_audit.py@ must preprocess test-headless/ with
-- the arguments GHC really received. Cabal renders them inside
-- 'Distribution.Simple.GHC.Build' (per component and per build way, with the
-- component's own @-tmp@ output directory), writes them to a temporary
-- response file and deletes it, and nothing it exports reproduces them. So,
-- for the duration of the build only, the configured @ghc@ program runs
-- through @BuildSupport/ghc-capture-wrapper.sh@, which records each command
-- verbatim and runs the real compiler with it unchanged.
--
-- When the build succeeds, the records are grouped by the unit each command
-- compiled (its @-this-unit-id@) and every unit built replaces its slot,
-- @\<build dir\>/ghc-capture/units/\<unit id\>/@, atomically. Units this
-- build did not compile keep their previous slot. A failed build publishes
-- nothing. The slot's @meta@ binds it to the configuration and compiler it
-- was built with.
module BuildSupport.GhcCapture (withGhcCapture) where

import Control.Exception (throwIO)
import Control.Monad (forM, forM_, unless, when)
import Data.List (isPrefixOf, nub, sort)
import Distribution.Simple (UserHooks (buildHook))
import Distribution.Simple.LocalBuildInfo (interpretSymbolicPathLBI)
import Distribution.Simple.Program (ghcProgram, lookupProgram, updateProgram)
import Distribution.Simple.Program.Types
    (ConfiguredProgram (..), ProgramLocation (..), programPath)
import Distribution.Simple.Utils (cabalVersion)
import Distribution.Pretty (prettyShow)
import Distribution.Types.LocalBuildInfo (LocalBuildInfo (withPrograms), buildDir)
import Distribution.Utils.Path (sameDirectory)
import GHC.Fingerprint (getFileHash)
import GHC.ResponseFile (expandResponse)
import System.Directory
    ( canonicalizePath, createDirectoryIfMissing, doesDirectoryExist
    , doesFileExist, doesPathExist, getPermissions, listDirectory
    , makeAbsolute, removeDirectoryRecursive, renameDirectory
    , setOwnerExecutable, setPermissions )
import System.FilePath (takeDirectory, (</>))
import System.IO (IOMode (WriteMode), hPutStr, hSetEncoding, utf8, withFile)
import System.IO.Error (userError)
import System.Process (getCurrentPid)

withGhcCapture :: UserHooks -> UserHooks
withGhcCapture hooks = hooks{buildHook = capturing (buildHook hooks)}
  where
    capturing inner pkg lbi uhooks flags = do
        root <- makeAbsolute (interpretSymbolicPathLBI lbi sameDirectory)
        build <- makeAbsolute (interpretSymbolicPathLBI lbi (buildDir lbi))
        ghc <- maybe (throwIO (userError "GhcCapture: no configured ghc program"))
                     pure (lookupProgram ghcProgram (withPrograms lbi))
        let capture = build </> "ghc-capture"
        pid <- getCurrentPid
        let session = capture </> ("session-" ++ show pid)
        exists <- doesPathExist session
        when exists (removeDirectoryRecursive session)
        createDirectoryIfMissing True session
        let wrapper = session </> "ghc-capture-wrapper.sh"
        readFile (root </> "BuildSupport" </> "ghc-capture-wrapper.sh")
            >>= writeFile wrapper
        getPermissions wrapper >>= setPermissions wrapper . setOwnerExecutable True
        let wrapped = ghc
                { programLocation = UserSpecified wrapper
                , programOverrideEnv = programOverrideEnv ghc
                    ++ [ ("SYNARCHY_CAPTURE_REAL_GHC", Just (programPath ghc))
                       , ("SYNARCHY_CAPTURE_DIR", Just session) ] }
        -- What this build compiles with: the package configuration and
        -- the compiler program's bytes.
        real <- canonicalizePath (programPath ghc)
        ghcHash <- getFileHash real
        configHash <- getFileHash (takeDirectory build </> "setup-config")
        inner pkg lbi{withPrograms = updateProgram wrapped (withPrograms lbi)} uhooks flags
        -- The build succeeded: publish its records, bound to those.
        let meta = unlines
                [ "session\t" ++ show pid
                , "package-root\t" ++ root
                , "ghc\t" ++ programPath ghc
                , "ghc-canonical\t" ++ real
                , "ghc-md5\t" ++ show ghcHash
                , "setup-config-md5\t" ++ show configHash
                , "cabal-library\t" ++ prettyShow cabalVersion ]
        promote capture session meta

-- | Group the session's successful commands by unit and replace each built
-- unit's slot; then drop every session directory, this one included.
promote :: FilePath -> FilePath -> String -> IO ()
promote capture session meta = do
    records <- sort . filter ("inv." `isPrefixOf`) <$> listDirectory session
    units <- forM records $ \record -> do
        args <- decodedArgs (session </> record)
        pure (unitOf args, record)
    let byUnit = [ (unit, [record | (Just u, record) <- units, u == unit])
                 | unit <- nub [u | (Just u, _) <- units] ]
        slots = capture </> "units"
    createDirectoryIfMissing True slots
    forM_ byUnit $ \(unit, members) -> do
        let slot = slots </> unit
            staged = slots </> ("." ++ unit ++ ".new")
            retired = slots </> ("." ++ unit ++ ".old")
        forM_ [staged, retired] $ \path -> do
            present <- doesPathExist path
            when present (removeDirectoryRecursive path)
        createDirectoryIfMissing True staged
        forM_ members $ \record ->
            renameDirectory (session </> record) (staged </> record)
        withFile (staged </> "meta") WriteMode $ \h ->
            hSetEncoding h utf8 >> hPutStr h meta
        present <- doesDirectoryExist slot
        when present (renameDirectory slot retired)
        renameDirectory staged slot
        when present (removeDirectoryRecursive retired)
    sessions <- filter ("session-" `isPrefixOf`) <$> listDirectory capture
    forM_ sessions $ \name -> removeDirectoryRecursive (capture </> name)

-- | A record's arguments with each response file expanded exactly as GHC
-- expands its own command line ('expandResponse', what
-- 'getArgsWithResponseFiles' runs), reading the byte copy the wrapper kept.
decodedArgs :: FilePath -> IO [String]
decodedArgs record = do
    raw <- splitNul <$> readStrict (record </> "argv")
    expandResponse =<< forM (zip [0 :: Int ..] raw) (\(i, arg) -> case arg of
        '@' : _ -> do
            let copy = record </> ("rsp." ++ show i)
            ok <- doesFileExist copy
            unless ok (throwIO (userError ("GhcCapture: missing " ++ copy)))
            pure ('@' : copy)
        _ -> pure arg)
  where
    splitNul s = case break (== '\0') s of
        (a, _ : rest) -> a : splitNul rest
        (a, []) -> [a | not (null a)]

-- | Read strictly, in the locale encoding, as GHC reads response files.
readStrict :: FilePath -> IO String
readStrict path = do
    text <- readFile path
    length text `seq` pure text

unitOf :: [String] -> Maybe String
unitOf ("-this-unit-id" : unit : _) = Just unit
unitOf (_ : rest) = unitOf rest
unitOf [] = Nothing
