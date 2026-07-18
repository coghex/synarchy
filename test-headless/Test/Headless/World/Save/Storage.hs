{-# LANGUAGE UnicodeSyntax, OverloadedStrings, ScopedTypeVariables #-}
-- | The atomic save-publication storage transaction gate (issue #762,
--   persistence-overhaul C1): "World.Save.Storage"'s write-validate-
--   publish-rotate transaction ('publishGeneration') and load-source
--   selection ('selectLoadGeneration'). No engine — every fixture below
--   is a synthetic literal (the same pattern
--   'Test.Headless.Save.Snapshot'/'Test.Headless.World.Save.Components'
--   use), driven against a REAL scratch directory on disk (the module
--   under test does real filesystem I/O — temp files, fsync, atomic
--   rename — so unlike its sibling gates this one cannot stay purely
--   in-memory). The real multi-thread, real-restart path is gated by
--   'tools/save_storage_probe.py'.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "atomic save storage"'@.
module Test.Headless.World.Save.Storage (spec) where

import UPrelude
import Test.Hspec
import Control.Exception (finally, catch, SomeException)
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.Serialize as S
import qualified Data.Text as T
import Data.Either (isLeft)
import System.Directory
    ( getTemporaryDirectory, createDirectoryIfMissing, removeDirectoryRecursive
    , doesDirectoryExist, doesFileExist, listDirectory, removeFile
    , getPermissions, setPermissions, Permissions(..), createFileLink )
import System.FilePath ((</>))

import World.Save.Storage
import World.Save.Envelope (encodeSessionSnapshot)
import World.Save.Snapshot
import World.Save.Snapshot.Adapter (SaveRequestMeta(..), snapshotSaveMetadata)
import World.Save.Types
    ( SaveMetadata(..), SaveData(..), BuildingSnapshot(..), UnitSnapshot(..) )
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)
import World.Page.Types (WorldPageId(..))
import World.Render.Zoom.Types (ZoomMapMode(..))
import Engine.Graphics.Camera (CameraFacing(..))
import Structure.Palette (emptyTexPalette)
import Item.Ground (emptyGroundItems)
import World.Spoil.Types (emptySpoilPiles)
import World.Flora.Harvest (emptyFloraHarvests)
import World.Flora.CropPlot (emptyCropPlots)
import World.Edit.Types (emptyWorldEdits)
import Craft.Bills (emptyCraftBills)
import Power.Types (emptyPowerNodes)

-- ---------------------------------------------------------------------
-- Fixtures (mirror Test.Headless.Save.Snapshot's minimal* pattern)
-- ---------------------------------------------------------------------

page1 ∷ WorldPageId
page1 = WorldPageId "page1"

-- | 'WorldGenParams''s manual cereal instance derives a few nested
--   fields from wgpSeed/wgpWorldSize on decode (see
--   'Test.Headless.World.Save.Components''s identical helper); reaching
--   that fixpoint before use isn't load-bearing for what this file
--   checks (only the top-level 'smSeed' scalar, never full structural
--   equality), but costs nothing and removes any doubt.
canon ∷ WorldGenParams → WorldGenParams
canon gp = case S.decode (S.encode gp) of
    Right gp' → gp'
    Left err  → error ("canon: " <> err)

minimalPage ∷ WorldGenParams → PageSnapshot
minimalPage gp = PageSnapshot
    { pgsPageId       = page1
    , pgsGenParams    = gp
    , pgsCameraX      = 0
    , pgsCameraY      = 0
    , pgsTimeHour     = 12
    , pgsTimeMinute   = 0
    , pgsDateYear     = 1
    , pgsDateMonth    = 1
    , pgsDateDay      = 1
    , pgsMapMode      = ZMDefault
    , pgsEdits        = emptyWorldEdits
    , pgsMineDesignations      = HM.empty
    , pgsConstructDesignations = HM.empty
    , pgsGroundItems  = emptyGroundItems
    , pgsSpoilPiles   = emptySpoilPiles
    , pgsBuildings    = BuildingSnapshot { bsnInstances = HM.empty, bsnNextId = 1 }
    , pgsUnits        = UnitSnapshot { usnInstances = HM.empty, usnNextId = 1 }
    , pgsUnitSimStates = HM.empty
    , pgsFloraHarvests = emptyFloraHarvests
    , pgsChopDesignations = HM.empty
    , pgsCraftBills   = emptyCraftBills
    , pgsPowerNodes   = emptyPowerNodes
    , pgsTillDesignations = HM.empty
    , pgsCropPlots    = emptyCropPlots
    , pgsPlantDesignations = HM.empty
    , pgsIdentity     = Nothing
    }

minimalGlobals ∷ SessionGlobals
minimalGlobals = SessionGlobals
    { sgGameTime       = 0
    , sgTexPalette     = emptyTexPalette
    , sgNextItemId     = 1
    , sgNextBuildingId = 1
    , sgNextUnitId     = 1
    , sgLuaModules     = HM.singleton "pause" "placeholder-blob"
    , sgActivePage     = page1
    , sgVisiblePages   = [page1]
    , sgLiveCamera     = LiveCameraSnapshot
        { lcsOwnerPage = Just page1
        , lcsX = 10, lcsY = 20, lcsZoom = 2, lcsFacing = FaceSouth
        }
    }

-- | A valid, captured snapshot whose seed is the one distinguishing
--   feature between fixtures — enough that a test can tell, by
--   inspecting the decoded 'SaveData''s metadata, EXACTLY which
--   generation a load actually selected.
snapshotWithSeed ∷ Word64 → SessionSnapshot
snapshotWithSeed seed =
    case captureSessionSnapshot minimalGlobals
            [minimalPage (canon defaultWorldGenParams { wgpSeed = seed })] of
        Right s   → s
        Left errs → error ("snapshotWithSeed: invalid fixture: " <> show errs)

-- | Build a real, encoded (metadata, bytes) pair for a distinctive save
--   request — everything 'publishGeneration' needs.
buildEncoded ∷ Word64 → Text → Text → (SaveMetadata, BS.ByteString)
buildEncoded seed name ts =
    let snap = snapshotWithSeed seed
        meta = snapshotSaveMetadata (SaveRequestMeta name ts) snap
    in (meta, encodeSessionSnapshot meta snap)

-- | A STRUCTURALLY VALID, fully checksummed envelope (every checksum
--   agrees — 'encodeSessionSnapshot' always computes real ones,
--   regardless of content) whose snapshot has NO world pages at all —
--   bypassing 'captureSessionSnapshot''s own "no persistable pages"
--   guard via a direct record update, since that guard is exactly what
--   makes this case otherwise unconstructible through the normal
--   capture path. The one deliberately-uncorrupted way to prove
--   'checkWorldCount' classifies as 'GenerationIncompatible' (never a
--   fallback trigger), as opposed to every OTHER test file in this
--   module, which corrupts real bytes to produce 'GenerationCorrupt'.
emptyPagesBytes ∷ BS.ByteString
emptyPagesBytes =
    let snap = (snapshotWithSeed 1) { snapPages = HM.empty }
        meta = snapshotSaveMetadata (SaveRequestMeta "slot" "t-empty") snap
    in encodeSessionSnapshot meta snap

-- ---------------------------------------------------------------------
-- Scratch directory
-- ---------------------------------------------------------------------

-- | A scratch directory, wiped clean before and after use (mirrors
--   'Test.Headless.Core.ConfigState''s withTempDir — the suite runs
--   sequentially, so a single fixed path under the system temp dir,
--   never inside the repo or 'saves/', is safe to reuse across 'it's).
withTempSlotDir ∷ (FilePath → IO a) → IO a
withTempSlotDir action = do
    tmp ← getTemporaryDirectory
    let dir = tmp </> "synarchy-save-storage-spec"
    reset dir
    action dir `finally` reset dir
  where
    -- A fault-injection test below may leave 'd' as a plain FILE (the
    -- directory-create-failure case occupies the path with one) or as a
    -- read-only DIRECTORY (the candidate-create-failure case) instead of
    -- an ordinary writable directory — handle both, or a later test's
    -- own setup would fail at this same path.
    reset d = do
        isDir ← doesDirectoryExist d
        when isDir $ do
            perms ← getPermissions d
            setPermissions d (perms { writable = True })
            removeDirectoryRecursive d
        isFile ← doesFileExist d
        when isFile $ removeFile d

-- | Publish for real via the transaction under test, failing the test
--   (rather than the assertion under test) if setup itself can't
--   publish — every scenario below builds its fixture generations this
--   way, so the transaction is proven correct once (the "publishes
--   successfully" tests) and then trusted as its own fixture builder.
publishOK ∷ FilePath → Text → Word64 → Text → Text → IO SaveMetadata
publishOK dir slot seed name ts = do
    let (meta, bytes) = buildEncoded seed name ts
    r ← publishGeneration dir slot meta bytes
    case r of
        Right _warnings → pure meta
        Left failure    → do
            expectationFailure ("test setup: publish failed: "
                <> T.unpack (renderPublishFailure failure))
            error "unreachable"

authPath, prevPath ∷ FilePath → FilePath
authPath dir = dir </> authoritativeFileName
prevPath dir = dir </> previousGenerationFileName

-- | Best-effort delete, swallowing "already gone" — used to simulate an
--   authoritative generation that vanished mid-rotation.
forceRemoveFile ∷ FilePath → IO ()
forceRemoveFile path =
    removeFile path `catch` \(_ ∷ SomeException) → pure ()

-- | Flip one byte (XOR 0xFF) — corrupts whatever checksum-covered region
--   it falls in without disturbing the file's length. Mirrors
--   'Test.Headless.World.Save.Envelope''s identical helper.
flipByteAt ∷ Int → BS.ByteString → BS.ByteString
flipByteAt idx bs =
    BS.take idx bs
        <> BS.singleton (BS.index bs idx `xor` 0xFF)
        <> BS.drop (idx + 1) bs

-- | A version field (bytes 4..7 of the envelope header) flipped away
--   from whatever this build actually writes — a COHERENT envelope
--   (magic + every checksum intact) declaring a version this reader
--   doesn't recognise, i.e. exactly the "well-formed but incompatible"
--   shape, never routine corruption.
corruptEnvelopeVersion ∷ BS.ByteString → BS.ByteString
corruptEnvelopeVersion bytes =
    let versionBytes = BS.take 4 (BS.drop 4 bytes)
        bumped = BS.map (`xor` 0xFF) versionBytes
    in BS.take 4 bytes <> bumped <> BS.drop 8 bytes

spec ∷ Spec
spec = do
    describe "publishGeneration" $ do
        it "publishes a first generation with no previous generation" $
            withTempSlotDir $ \dir → do
                let (meta, bytes) = buildEncoded 1 "slot" "t1"
                r ← publishGeneration dir "slot" meta bytes
                r `shouldBe` Right []
                BS.readFile (authPath dir) `shouldReturn` bytes
                doesFileExist (prevPath dir) `shouldReturn` False
                entries ← listDirectory dir
                entries `shouldBe` [authoritativeFileName]

        it "a second publish retains the first generation as the \
           \previous generation" $
            withTempSlotDir $ \dir → do
                let (_, bytesA) = buildEncoded 1 "slot" "t1"
                    (metaB, bytesB) = buildEncoded 2 "slot" "t2"
                _ ← publishOK dir "slot" 1 "slot" "t1"
                r ← publishGeneration dir "slot" metaB bytesB
                r `shouldBe` Right []
                BS.readFile (authPath dir) `shouldReturn` bytesB
                BS.readFile (prevPath dir) `shouldReturn` bytesA

        it "a third publish retains only the second and third \
           \complete generations" $
            withTempSlotDir $ \dir → do
                let (_, bytesB) = buildEncoded 2 "slot" "t2"
                    (metaC, bytesC) = buildEncoded 3 "slot" "t3"
                _ ← publishOK dir "slot" 1 "slot" "t1"
                _ ← publishOK dir "slot" 2 "slot" "t2"
                r ← publishGeneration dir "slot" metaC bytesC
                r `shouldBe` Right []
                BS.readFile (authPath dir) `shouldReturn` bytesC
                BS.readFile (prevPath dir) `shouldReturn` bytesB
                -- The first generation, staged out of the way during
                -- rotation (requirement 5), must be cleaned up once the
                -- new publication is durable -- no stray file survives.
                entries ← listDirectory dir
                entries `shouldMatchList`
                    [authoritativeFileName, previousGenerationFileName]

        it "never publishes a candidate that fails to decode, leaving \
           \an existing authoritative generation untouched" $
            withTempSlotDir $ \dir → do
                original ← publishOK dir "slot" 1 "slot" "t1"
                origBytes ← BS.readFile (authPath dir)
                r ← publishGeneration dir "slot" original (BS.pack [0,1,2,3,4])
                case r of
                    Left f  → pfPhase f `shouldBe` PhaseCandidateValidate
                    Right _ → expectationFailure "expected a validate failure"
                BS.readFile (authPath dir) `shouldReturn` origBytes
                doesFileExist (prevPath dir) `shouldReturn` False

        it "never publishes a candidate whose re-read metadata does not \
           \match the intended save request" $
            withTempSlotDir $ \dir → do
                let (rightMeta, bytes) = buildEncoded 1 "slot" "t1"
                    wrongMeta = rightMeta { smName = "different-slot" }
                r ← publishGeneration dir "slot" wrongMeta bytes
                case r of
                    Left f  → pfPhase f `shouldBe` PhaseCandidateValidate
                    Right _ → expectationFailure "expected a validate failure"
                doesFileExist (authPath dir) `shouldReturn` False

        it "never publishes a candidate whose COMPLETE metadata doesn't \
           \match the request even when only name/timestamp agree -- a \
           \self-consistent candidate belonging to a different world \
           \must not slip through on two matching fields alone" $
            withTempSlotDir $ \dir → do
                let (rightMeta, bytes) = buildEncoded 1 "slot" "t1"
                    wrongMeta = rightMeta { smSeed = smSeed rightMeta + 1 }
                r ← publishGeneration dir "slot" wrongMeta bytes
                case r of
                    Left f  → pfPhase f `shouldBe` PhaseCandidateValidate
                    Right _ → expectationFailure "expected a validate failure"
                doesFileExist (authPath dir) `shouldReturn` False

        it "reports a directory-create failure when the slot path is \
           \occupied by a plain file, without touching it" $
            withTempSlotDir $ \dir → do
                let (meta, bytes) = buildEncoded 1 "slot" "t1"
                BS.writeFile dir "occupying this path with a plain file"
                r ← publishGeneration dir "slot" meta bytes
                case r of
                    Left f  → pfPhase f `shouldBe` PhaseDirectoryCreate
                    Right _ → expectationFailure "expected a directory-create failure"
                contents ← BS.readFile dir
                contents `shouldBe` "occupying this path with a plain file"

        it "reports a candidate-create failure when the slot directory \
           \is not writable" $
            withTempSlotDir $ \dir → do
                let (meta, bytes) = buildEncoded 1 "slot" "t1"
                createDirectoryIfMissing True dir
                perms ← getPermissions dir
                setPermissions dir (perms { writable = False })
                r ← publishGeneration dir "slot" meta bytes
                    `finally` setPermissions dir (perms { writable = True })
                case r of
                    Left f  → pfPhase f `shouldBe` PhaseCandidateCreate
                    Right _ → expectationFailure "expected a candidate-create failure"

        it "reports an unsafe-path failure and never writes through a \
           \slot directory that is itself a symlink" $
            withTempSlotDir $ \dir → do
                let target = dir <> "-target"
                createDirectoryIfMissing True target
                createFileLink target dir
                (`finally` (removeFile dir >> removeDirectoryRecursive target)) $ do
                    let (meta, bytes) = buildEncoded 1 "slot" "t1"
                    r ← publishGeneration dir "slot" meta bytes
                    case r of
                        Left f  → pfPhase f `shouldBe` PhaseUnsafePath
                        Right _ → expectationFailure "expected an unsafe-path failure"
                    doesFileExist (target </> authoritativeFileName)
                        `shouldReturn` False
                    listDirectory target `shouldReturn` []

        it "reports a rotate-previous failure without destroying the \
           \existing authoritative generation, when the previous-\
           \generation path is blocked" $
            withTempSlotDir $ \dir → do
                _ ← publishOK dir "slot" 1 "slot" "t1"
                origBytes ← BS.readFile (authPath dir)
                createDirectoryIfMissing True (prevPath dir)
                let (metaB, bytesB) = buildEncoded 2 "slot" "t2"
                r ← publishGeneration dir "slot" metaB bytesB
                case r of
                    Left f  → pfPhase f `shouldBe` PhaseRotatePrevious
                    Right _ → expectationFailure "expected a rotate-previous failure"
                BS.readFile (authPath dir) `shouldReturn` origBytes
                doesDirectoryExist (prevPath dir) `shouldReturn` True

        it "reports a publish-rename failure without publishing a \
           \partial generation, on a fresh slot whose authoritative \
           \path is blocked" $
            withTempSlotDir $ \dir → do
                let (meta, bytes) = buildEncoded 1 "slot" "t1"
                createDirectoryIfMissing True (authPath dir)
                r ← publishGeneration dir "slot" meta bytes
                case r of
                    Left f  → pfPhase f `shouldBe` PhasePublishRename
                    Right _ → expectationFailure "expected a publish-rename failure"
                doesDirectoryExist (authPath dir) `shouldReturn` True
                doesFileExist (prevPath dir) `shouldReturn` False

        it "cleans up a stale leftover temporary candidate on the next \
           \successful publish" $
            withTempSlotDir $ \dir → do
                createDirectoryIfMissing True dir
                BS.writeFile (dir </> "world-synworld-tmp99999")
                    "leftover from an earlier interrupted publish"
                _ ← publishOK dir "slot" 1 "slot" "t1"
                entries ← listDirectory dir
                entries `shouldBe` [authoritativeFileName]

        it "never sweeps a file that merely shares the temp-candidate \
           \prefix without matching its digit-suffix naming convention \
           \(a real unrelated file, not a transaction artifact)" $
            withTempSlotDir $ \dir → do
                createDirectoryIfMissing True dir
                BS.writeFile (dir </> "world-synworld-tmp-notes")
                    "a real file a user or another tool left here"
                _ ← publishOK dir "slot" 1 "slot" "t1"
                BS.readFile (dir </> "world-synworld-tmp-notes")
                    `shouldReturn` "a real file a user or another tool left here"

        it "removes a stale pre-#759 world_gen.yaml companion on a \
           \successful publish, without disturbing an unrelated file" $
            withTempSlotDir $ \dir → do
                createDirectoryIfMissing True dir
                BS.writeFile (dir </> "world_gen.yaml") "stale pre-#759 companion"
                BS.writeFile (dir </> "notes.txt") "an unrelated user file"
                _ ← publishOK dir "slot" 1 "slot" "t1"
                doesFileExist (dir </> "world_gen.yaml") `shouldReturn` False
                BS.readFile (dir </> "notes.txt")
                    `shouldReturn` "an unrelated user file"
                -- A second publish must also leave the unrelated file alone.
                _ ← publishOK dir "slot" 2 "slot" "t2"
                BS.readFile (dir </> "notes.txt")
                    `shouldReturn` "an unrelated user file"

    describe "selectLoadGeneration" $ do
        it "selects the authoritative generation when both generations \
           \are present and valid, never combining them" $
            withTempSlotDir $ \dir → do
                _ ← publishOK dir "slot" 1 "slot" "t1"
                _ ← publishOK dir "slot" 2 "slot" "t2"
                sel ← selectLoadGeneration dir "slot"
                case sel of
                    Right s → do
                        lsSource s `shouldBe` FromAuthoritative
                        smSeed (sdMetadata (lsSaveData s)) `shouldBe` 2
                    Left err → expectationFailure (T.unpack err)

        it "falls back to the previous generation when the \
           \authoritative file is missing" $
            withTempSlotDir $ \dir → do
                _ ← publishOK dir "slot" 1 "slot" "t1"
                _ ← publishOK dir "slot" 2 "slot" "t2"
                forceRemoveFile (authPath dir)
                sel ← selectLoadGeneration dir "slot"
                case sel of
                    Right s → do
                        lsSource s `shouldBe` FromPrevious
                        smSeed (sdMetadata (lsSaveData s)) `shouldBe` 1
                    Left err → expectationFailure (T.unpack err)

        it "falls back to the previous generation when the \
           \authoritative file is truncated" $
            withTempSlotDir $ \dir → do
                _ ← publishOK dir "slot" 1 "slot" "t1"
                _ ← publishOK dir "slot" 2 "slot" "t2"
                whole ← BS.readFile (authPath dir)
                BS.writeFile (authPath dir) (BS.take (BS.length whole `div` 2) whole)
                sel ← selectLoadGeneration dir "slot"
                case sel of
                    Right s → do
                        lsSource s `shouldBe` FromPrevious
                        smSeed (sdMetadata (lsSaveData s)) `shouldBe` 1
                    Left err → expectationFailure (T.unpack err)

        it "falls back to the previous generation when the \
           \authoritative file has bad framing (magic)" $
            withTempSlotDir $ \dir → do
                _ ← publishOK dir "slot" 1 "slot" "t1"
                _ ← publishOK dir "slot" 2 "slot" "t2"
                whole ← BS.readFile (authPath dir)
                BS.writeFile (authPath dir) (BS.cons 0x00 (BS.drop 1 whole))
                sel ← selectLoadGeneration dir "slot"
                case sel of
                    Right s  → lsSource s `shouldBe` FromPrevious
                    Left err → expectationFailure (T.unpack err)

        it "falls back to the previous generation when the \
           \authoritative file fails checksum validation" $
            withTempSlotDir $ \dir → do
                _ ← publishOK dir "slot" 1 "slot" "t1"
                _ ← publishOK dir "slot" 2 "slot" "t2"
                whole ← BS.readFile (authPath dir)
                BS.writeFile (authPath dir) (flipByteAt (BS.length whole - 1) whole)
                sel ← selectLoadGeneration dir "slot"
                case sel of
                    Right s  → lsSource s `shouldBe` FromPrevious
                    Left err → expectationFailure (T.unpack err)

        it "does NOT fall back when the authoritative generation is \
           \present but semantically incompatible with this build, \
           \even though a valid previous generation exists" $
            withTempSlotDir $ \dir → do
                _ ← publishOK dir "slot" 1 "slot" "t1"
                _ ← publishOK dir "slot" 2 "slot" "t2"
                whole ← BS.readFile (authPath dir)
                BS.writeFile (authPath dir) (corruptEnvelopeVersion whole)
                sel ← selectLoadGeneration dir "slot"
                case sel of
                    Right s  → expectationFailure
                        ("expected no fallback, got " <> show (lsSource s))
                    Left err → T.unpack err `shouldContain` "incompatible"

        it "does NOT fall back when the authoritative generation is \
           \structurally valid (every checksum agrees) but has no world \
           \pages -- checkWorldCount is a content-validation failure, \
           \not storage corruption -- even though a valid previous \
           \generation exists" $
            withTempSlotDir $ \dir → do
                _ ← publishOK dir "slot" 1 "slot" "t1"
                _ ← publishOK dir "slot" 2 "slot" "t2"
                BS.writeFile (authPath dir) emptyPagesBytes
                sel ← selectLoadGeneration dir "slot"
                case sel of
                    Right s  → expectationFailure
                        ("expected no fallback, got " <> show (lsSource s))
                    Left err → T.unpack err `shouldContain` "incompatible"

        it "reports an unsafe-path failure and never reads through a \
           \slot directory that is itself a symlink" $
            withTempSlotDir $ \dir → do
                let target = dir <> "-target"
                _ ← publishOK target "slot" 1 "slot" "t1"
                createFileLink target dir
                sel ← selectLoadGeneration dir "slot"
                    `finally` (removeFile dir
                                >> removeDirectoryRecursive target)
                case sel of
                    Left err → T.unpack err `shouldContain` "symlink"
                    Right s  → expectationFailure
                        ("expected an unsafe-path failure, got " <> show s)

        it "recovers the still-intact current authoritative generation \
           \when interrupted right after staging the old previous \
           \generation out of the way, before rotation ever begins \
           \(requirement 5/6: staging never destroys the displaced \
           \generation until the new publication is durable)" $
            withTempSlotDir $ \dir → do
                _ ← publishOK dir "slot" 1 "slot" "t1"
                _ ← publishOK dir "slot" 2 "slot" "t2"
                -- Simulate the exact on-disk state a crash immediately
                -- after staging (and before the rotate rename) would
                -- leave: the old previous generation sitting under a
                -- staged name, the current authoritative untouched, and
                -- NO previous-generation file at all (never rotated).
                staged ← BS.readFile (prevPath dir)
                forceRemoveFile (prevPath dir)
                BS.writeFile (dir </> "world-synworld-stale77777") staged
                sel ← selectLoadGeneration dir "slot"
                case sel of
                    Right s → do
                        lsSource s `shouldBe` FromAuthoritative
                        smSeed (sdMetadata (lsSaveData s)) `shouldBe` 2
                    Left err → expectationFailure (T.unpack err)
                -- A subsequent successful publish must sweep the
                -- leftover staged file like any other owned artifact.
                _ ← publishOK dir "slot" 3 "slot" "t3"
                entries ← listDirectory dir
                entries `shouldMatchList`
                    [authoritativeFileName, previousGenerationFileName]

        it "reports a failure (not a valid loadable save) when neither \
           \generation is valid" $
            withTempSlotDir $ \dir → do
                createDirectoryIfMissing True dir
                sel ← selectLoadGeneration dir "slot"
                sel `shouldSatisfy` isLeft

        it "is read-only: a recovered load never rewrites or promotes \
           \the previous generation, nor repairs the authoritative file" $
            withTempSlotDir $ \dir → do
                _ ← publishOK dir "slot" 1 "slot" "t1"
                _ ← publishOK dir "slot" 2 "slot" "t2"
                whole ← BS.readFile (authPath dir)
                let corrupt = flipByteAt (BS.length whole - 1) whole
                BS.writeFile (authPath dir) corrupt
                prevBefore ← BS.readFile (prevPath dir)
                _ ← selectLoadGeneration dir "slot"
                BS.readFile (authPath dir) `shouldReturn` corrupt
                BS.readFile (prevPath dir) `shouldReturn` prevBefore
                entries ← listDirectory dir
                entries `shouldMatchList`
                    [authoritativeFileName, previousGenerationFileName]

        it "never selects a partial candidate: a stray leftover \
           \temporary file alongside a valid authoritative generation \
           \is ignored" $
            withTempSlotDir $ \dir → do
                _ ← publishOK dir "slot" 1 "slot" "t1"
                BS.writeFile (dir </> "world-synworld-tmp88888")
                    "an interrupted write, never fully validated"
                sel ← selectLoadGeneration dir "slot"
                case sel of
                    Right s  → lsSource s `shouldBe` FromAuthoritative
                    Left err → expectationFailure (T.unpack err)

        it "never selects a partial candidate: a stray leftover \
           \temporary file alongside a previous-only recovery is \
           \ignored" $
            withTempSlotDir $ \dir → do
                _ ← publishOK dir "slot" 1 "slot" "t1"
                _ ← publishOK dir "slot" 2 "slot" "t2"
                forceRemoveFile (authPath dir)
                BS.writeFile (dir </> "world-synworld-tmp88888")
                    "an interrupted write, never fully validated"
                sel ← selectLoadGeneration dir "slot"
                case sel of
                    Right s → do
                        lsSource s `shouldBe` FromPrevious
                        smSeed (sdMetadata (lsSaveData s)) `shouldBe` 1
                    Left err → expectationFailure (T.unpack err)
