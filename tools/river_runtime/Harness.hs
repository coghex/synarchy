-- | The hydraulic harness archive runner (#2719, requirement 8).
--
--   Runs the selected catalog fixtures through the selected solver
--   adapters at every standard placement (origin, an ordinary
--   translation, and a translation across a wrapped cylindrical seam),
--   compares the origin run with each translation, reproduces the eight
--   archived legacy characterization cases, and writes a JSON evidence
--   archive:
--
--   * @results.json@ — fixture definitions, runs, check results and
--     comparisons.
--   * @manifest.json@ — the source commit, SHA-256 of every harness and
--     solver source, the executable's hash, each fixture's identity and
--     content hash, step counts and intervals, and the classification
--     "characterization, not behaviour approval".
--
--   Neither file records a time, a path outside the repository, or the
--   output directory, so a rerun on identical inputs writes identical
--   bytes. Exit status is 1 when any run recorded a check violation or a
--   characterization case failed to reproduce the archived baseline; the
--   archive is written either way.
--
--   Usage (from the repository root):
--
--   > cabal run -v0 exe:river-runtime-harness -- --out <new-directory>
--   >     [--fixture <name>]... [--adapter legacy]... [--repo-root <path>]
--   > cabal run -v0 exe:river-runtime-harness -- --list
module Main (main) where

import UPrelude
import Data.Aeson ((.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT
import qualified Data.ByteString.Lazy as BL
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing, doesDirectoryExist,
                         doesFileExist, listDirectory)
import System.Environment (getArgs, getExecutablePath)
import System.Exit (ExitCode(..), exitWith)
import System.FilePath ((</>), takeExtension)
import System.IO (stderr)
import System.Process (readProcessWithExitCode)
import RiverRuntime.Harness.Adapter
import RiverRuntime.Harness.Archive
import RiverRuntime.Harness.Catalog
import RiverRuntime.Harness.Compare
import RiverRuntime.Harness.Fixture
import RiverRuntime.Harness.Legacy (legacyAdapter)
import RiverRuntime.Harness.Placement
import RiverRuntime.Harness.Run

data Options = Options
    { optOut      ∷ Maybe FilePath
    , optFixtures ∷ [Text]
    , optAdapters ∷ [Text]
    , optRoot     ∷ FilePath
    , optList     ∷ Bool
    }

classification ∷ Text
classification = "characterization, not behaviour approval"

-- | The adapters this runner can name.
knownAdapters ∷ [(Text, Adapter)]
knownAdapters = [("legacy", legacyAdapter)]

-- | Sources whose hashes the manifest records, besides every file
--   under @tools/river_runtime@: the solver the legacy adapter wraps and
--   the state it seeds.
solverSources ∷ [FilePath]
solverSources =
    [ "src/Sim/Chunk.hs"
    , "src/Sim/Fluid/Active.hs"
    , "src/Sim/Fluid/Reaction.hs"
    , "src/Sim/Fluid/Types.hs"
    , "src/Sim/State/Types.hs"
    , "src/Sim/Topology.hs"
    , "src/World/Fluid/Exact.hs"
    , "src/World/Fluid/Types.hs"
    ]

baselineArchive ∷ FilePath
baselineArchive = "docs/evidence/river-runtime/baseline-solver.json"

usage ∷ Text
usage = T.unlines
    [ "usage: river-runtime-harness --out <new-directory> [--fixture <name>]..."
    , "                             [--adapter legacy]... [--repo-root <path>]"
    , "       river-runtime-harness --list" ]

parseArgs ∷ [String] → Either Text Options
parseArgs = go (Options Nothing [] [] "." False)
  where
    go o [] = Right o
    go o ("--out" : v : rest)       = go o { optOut = Just v } rest
    go o ("--fixture" : v : rest)   = go o { optFixtures = optFixtures o <> [T.pack v] } rest
    go o ("--adapter" : v : rest)   = go o { optAdapters = optAdapters o <> [T.pack v] } rest
    go o ("--repo-root" : v : rest) = go o { optRoot = v } rest
    go o ("--list" : rest)          = go o { optList = True } rest
    go _ (other : _) = Left ("unrecognized argument: " <> T.pack other)

die' ∷ Text → IO α
die' msg = TIO.hPutStrLn stderr msg ≫ exitWith (ExitFailure 2)

sha256Lazy ∷ BL.ByteString → Text
sha256Lazy = sha256Hex

main ∷ IO ()
main = do
    opts ← either (\e → die' (e <> "\n" <> usage)) pure . parseArgs =≪ getArgs
    when (optList opts) $ do
        forM_ experiments $ \e →
            TIO.putStrLn (fxName (heFixture e) <> "\t" <> fxDescription (heFixture e))
        exitWith ExitSuccess
    out ← maybe (die' ("--out is required\n" <> usage)) pure (optOut opts)
    selected ← case optFixtures opts of
        [] → pure experiments
        names → forM names $ \n →
            maybe (die' ("unknown fixture: " <> n)) pure (findExperiment n)
    adapters ← forM (if null (optAdapters opts) then ["legacy"] else optAdapters opts) $ \n →
        maybe (die' ("unknown adapter: " <> n)) (\a → pure (n, a)) (lookup n knownAdapters)
    exists ← doesDirectoryExist out
    when exists $ die' ("output directory already exists: " <> T.pack out)
    let root = optRoot opts

    -- Every fixture × adapter × placement, then the comparisons.
    fixtureResults ← forM selected $ \e → do
        let fx = heFixture e
        runs ← forM adapters $ \(_, adapter) → forM standardPlacements $ \pl →
            either (\err → die' ("run failed: " <> fxName fx <> " at " <> plName pl
                                 <> ": " <> err)) pure
                   (runFixture adapter (RunConfig pl referenceInterval) fx)
        comparisons ← fmap concat $ forM (zip adapters runs) $ \((an, _), trs) →
            case trs of
                (base : translated) → forM translated $ \tr → do
                    cmp ← either (\err → die' ("comparison failed: " <> err)) pure
                        (compareSeries (heMilestones e) (trajectorySeries base)
                                       (trajectorySeries tr))
                    pure (comparisonJson (an <> "@" <> runName base)
                                         (an <> "@" <> runName tr) cmp)
                [] → pure []
        let fixtureBytes = A.encode (fixtureJson fx)
        pure ( fx, sha256Lazy fixtureBytes, concat runs
             , A.object [ "fixture" .= fixtureJson fx
                        , "fixture_sha256" .= sha256Lazy fixtureBytes
                        -- Full final cells for each adapter's origin
                        -- run; translations carry the hash, which matches
                        -- when the normalized state is identical.
                        , "runs" .= [ trajectoryJson (i ≡ (0 ∷ Int)) tr
                                    | trs ← runs, (i, tr) ← zip [0 ..] trs ]
                        , "comparisons" .= comparisons ] )

    -- The archived characterization, reproduced through the harness.
    archived ← readBaseline (root </> baselineArchive)
    characterization ← forM characterizationCases $ \cc → do
        tr ← either (\err → die' ("characterization failed: " <> ccName cc <> ": " <> err))
                    pure (runFixture legacyAdapter
                            (RunConfig originPlacement referenceInterval)
                            (characterizationFixture cc))
        let samples = characterizationSamples cc tr
            reproduced = fmap (\m → lookup (ccName cc) m ≡ Just samples) archived
        pure (cc, samples, reproduced, tr)

    let results = A.object
            [ "classification" .= classification
            , "fixtures" .= [ v | (_, _, _, v) ← fixtureResults ]
            , "characterization" .= A.object
                [ "source_archive" .= baselineArchive
                , "adapter" .= ("legacy" ∷ Text)
                , "cases" .= [ characterizationJson cc s r | (cc, s, r, _) ← characterization ]
                , "violations" .= sum [ length (trViolations tr) | (_, _, _, tr) ← characterization ]
                ]
            ]
        resultBytes = A.encode results <> "\n"

    sources ← sourceHashes root
    commit ← git root ["rev-parse", "HEAD"]
    dirty ← git root (["status", "--porcelain", "--"] <> map fst sources)
    exeHash ← sha256Lazy ⊚ (BL.readFile =≪ getExecutablePath)
    baselineHash ← hashIfPresent (root </> baselineArchive)
    let manifest = A.object
            [ "classification" .= classification
            , "source_commit" .= fmap T.strip commit
            , "source_dirty" .= fmap (not . T.null . T.strip) dirty
            , "sources" .= [ A.object [ "path" .= p, "sha256" .= h ] | (p, h) ← sources ]
            , "executable_sha256" .= exeHash
            , "production_code_modified" .= False
            , "adapters" .= [ adapterSummary n a | (n, a) ← adapters ]
            , "fixtures" .= [ A.object
                                [ "name" .= fxName fx, "sha256" .= h
                                , "duration_us" .= unLogicalTime (fxDuration fx)
                                , "interval_us" .= unLogicalTime referenceInterval
                                , "steps" .= (unLogicalTime (fxDuration fx)
                                              `div` unLogicalTime referenceInterval)
                                , "placements" .= map plName standardPlacements ]
                            | (fx, h, _, _) ← fixtureResults ]
            , "characterization_cases" .= map ccName characterizationCases
            , "baseline_archive" .= A.object [ "path" .= baselineArchive, "sha256" .= baselineHash ]
            , "results" .= A.object [ "path" .= ("results.json" ∷ Text)
                                    , "sha256" .= sha256Lazy resultBytes ]
            ]
    createDirectoryIfMissing True out
    BL.writeFile (out </> "results.json") resultBytes
    BL.writeFile (out </> "manifest.json") (A.encode manifest <> "\n")

    let runViolations = sum [ length (trViolations tr) | (_, _, trs, _) ← fixtureResults, tr ← trs ]
        charViolations = sum [ length (trViolations tr) | (_, _, _, tr) ← characterization ]
        notReproduced = [ ccName cc | (cc, _, Just False, _) ← characterization ]
    TIO.putStrLn $ T.unwords
        [ "wrote", T.pack out <> ":"
        , tshow (length fixtureResults), "fixtures,"
        , tshow (sum [ length trs | (_, _, trs, _) ← fixtureResults ]), "runs,"
        , tshow (runViolations + charViolations), "check violations,"
        , case archived of
            Nothing → "baseline archive not found"
            Just _ → tshow (length characterizationCases - length notReproduced) <> "/"
                     <> tshow (length characterizationCases) <> " archived cases reproduced" ]
    unless (null notReproduced) $
        TIO.hPutStrLn stderr ("not reproduced: " <> T.intercalate ", " notReproduced)
    when (runViolations + charViolations > 0 ∨ not (null notReproduced)) $
        exitWith (ExitFailure 1)
  where
    runName tr = plName (rcPlacement (trConfig tr))
    adapterSummary n a = let ai = adapterInfo a in A.object
        [ "name" .= n, "config" .= aiConfig ai
        , "intervals_us" .= map unLogicalTime (aiIntervals ai) ]

-- | The archived baseline's per-case samples, if the file exists.
readBaseline ∷ FilePath → IO (Maybe [(Text, [(Int, Int, Int)])])
readBaseline path = do
    present ← doesFileExist path
    if not present then pure Nothing else do
        bytes ← BL.readFile path
        case A.eitherDecode bytes ≫= AT.parseEither parser of
            Left err → die' ("unreadable baseline archive " <> T.pack path <> ": " <> T.pack err)
            Right v → pure (Just v)
  where
    parser = A.withObject "baseline" $ \o → do
        cases ← o A..: "cases"
        forM cases $ A.withObject "case" $ \c → do
            name ← c A..: "name"
            samples ← c A..: "samples"
            rows ← forM samples $ A.withObject "sample" $ \s →
                (,,) ⊚ s A..: "sourceUnits" ⊛ s A..: "targetUnits" ⊛ s A..: "totalUnits"
            pure (name, rows)

hashIfPresent ∷ FilePath → IO (Maybe Text)
hashIfPresent path = do
    present ← doesFileExist path
    if present then Just . sha256Lazy ⊚ BL.readFile path else pure Nothing

-- | Every source under @tools/river_runtime@ plus 'solverSources', as
--   repository-relative paths in sorted order with their SHA-256.
sourceHashes ∷ FilePath → IO [(FilePath, Text)]
sourceHashes root = do
    tool ← walk "tools/river_runtime"
    let paths = L.sort (tool <> solverSources)
    forM paths $ \p → (,) p . sha256Lazy ⊚ BL.readFile (root </> p)
  where
    walk rel = do
        isDir ← doesDirectoryExist (root </> rel)
        if not isDir
        then pure [ rel | takeExtension rel ≡ ".hs" ]
        else fmap concat . mapM (walk . (rel </>)) . L.sort =≪ listDirectory (root </> rel)

git ∷ FilePath → [String] → IO (Maybe Text)
git root args = do
    (code, out, _) ← readProcessWithExitCode "git" (["-C", root] <> args) ""
    pure $ case code of
        ExitSuccess → Just (T.pack out)
        ExitFailure _ → Nothing
