{-# LANGUAGE ScopedTypeVariables #-}
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
import qualified Data.HashSet as HS
import qualified Data.Serialize as S
import qualified Data.Text as T
import Data.Either (isLeft)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import System.Directory
    ( getTemporaryDirectory, createDirectoryIfMissing, removeDirectoryRecursive
    , doesDirectoryExist, doesFileExist, listDirectory, removeFile
    , getPermissions, setPermissions, Permissions(..), createFileLink
    , withCurrentDirectory )
import System.FilePath ((</>), takeDirectory)
import System.IO (stderr, openBinaryTempFile)

import Engine.Core.Log
    ( initLogger, defaultLogConfig, LogConfig(..), LogBackend(..), LoggerState
    , LogEntry(..) )
import World.Save.Serialize
    ( listSaves, listSavesWithSeams, ListingSeams(..), productionListingSeams
    , loadWorld, savesDirectory, saveExtension, SaveListing(..), loadPhaseFor )
import World.Save.Storage
import World.Save.Storage.Durable (syncDirectory)
import World.Save.Envelope
    ( encodeSessionSnapshot, metadataComponentId, metadataComponentVersion
    , currentEnvelopeVersion, LoadProgress(..)
    , decodeSaveEnvelopeMetadataClassified, GenerationFailure(..) )
import World.Save.Envelope.Codec
    (encodeEnvelope, decodeEnvelope, DecodedEnvelope(..))
import World.Save.Envelope.Types
    ( defaultEnvelopeLimits, ComponentId(..), EnvelopeManifest(..)
    , ComponentDescriptor(..) )
import World.Save.Component (componentKnownIds, componentRequiredIds)
import World.Save.Component.Types
    (ComponentPhase(..), coreSessionComponentId)
import Engine.Load.Status (LoadPhase(..))
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
import World.Edit.Types (emptyWorldEdits)
import Craft.Bills (emptyCraftBills)
import Unit.Transfer.Orders (emptyTransferOrders)
import Power.Types (emptyPowerNodes)
import Building.Knowledge (emptyContainerKnowledge)
import World.Construct.Attempt (firstConstructAttemptId)
import World.Flora.Identity (firstPlantedFloraCursor)
import Test.Headless.Harness.GeneratedIds (fixtureGeneratedWorldIdForPage)

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
    , pgsConstructNextAttempt = firstConstructAttemptId
    , pgsGroundItems  = emptyGroundItems
    , pgsSpoilPiles   = emptySpoilPiles
    , pgsBuildings    = BuildingSnapshot { bsnInstances = HM.empty, bsnNextId = 1 }
    , pgsUnits        = UnitSnapshot { usnInstances = HM.empty, usnNextId = 1 }
    , pgsUnitSimStates = HM.empty
    , pgsFloraHarvests = emptyFloraHarvests
    , pgsChopDesignations = HM.empty
    , pgsPendingChopMigration = HM.empty
    , pgsPendingFloraHarvests = HM.empty
    , pgsPlantedFloraCursor = firstPlantedFloraCursor
    , pgsCraftBills   = emptyCraftBills
    , pgsTransferOrders = emptyTransferOrders
    , pgsPowerNodes   = emptyPowerNodes
    , pgsTillDesignations = HM.empty
    , pgsCropPlots    = HM.empty
    , pgsPlantDesignations = HM.empty
    , pgsContainerKnowledge = emptyContainerKnowledge
    , pgsIdentity     = Nothing
    , pgsGeneratedId  = Just (fixtureGeneratedWorldIdForPage page1)
    }

minimalGlobals ∷ SessionGlobals
minimalGlobals = SessionGlobals
    { sgGameTime       = 0
    , sgTexPalette     = emptyTexPalette
    , sgNextItemId     = 1
    , sgNextBuildingId = 1
    , sgNextUnitId     = 1
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
        meta = snapshotSaveMetadata (SaveRequestMeta name ts False) snap
    in (meta, encodeSessionSnapshot meta snap [])

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
        meta = snapshotSaveMetadata (SaveRequestMeta "slot" "t-empty" False) snap
    in encodeSessionSnapshot meta snap []

-- | A STRUCTURALLY VALID, fully checksummed envelope whose snapshot
--   names an ACTIVE PAGE that its own page set does not contain — every
--   component decodes and self-validates cleanly, and only
--   'assembleSnapshot''s cross-component 'validateSessionSnapshot' pass
--   rejects it. That makes it the one fixture here that reaches a real
--   'AssemblePhase' failure through the production load path, as opposed
--   to 'emptyPagesBytes', which a component's OWN validator catches
--   first. Built by direct record update for the same reason
--   'emptyPagesBytes' is: 'captureSessionSnapshot' refuses to produce it.
activePageMissingBytes ∷ BS.ByteString
activePageMissingBytes =
    let snap = (snapshotWithSeed 1) { snapActivePage = WorldPageId "ghost" }
        meta = snapshotSaveMetadata (SaveRequestMeta "slot" "t-ghost" False) snap
    in encodeSessionSnapshot meta snap []

-- | A STRUCTURALLY VALID envelope carrying a genuinely unrecognized
--   OPTIONAL component (round-4 review) -- exactly what a corrupted-
--   authoritative-with-a-valid-.prev recovery scenario would leave
--   behind if that .prev generation predates some now-retired optional
--   component. Only "metadata" (required) is a real, known id; the
--   payload bytes themselves are never decoded by
--   'foreignOptionalDataCheck', only the manifest's own component id
--   list matters.
foreignBytesFor ∷ Word64 → Text → BS.ByteString
foreignBytesFor seed name =
    let (meta, _) = buildEncoded seed name "t-foreign"
        specs = [ (metadataComponentId, metadataComponentVersion, True
                  , S.encode meta)
                , (ComponentId "retired-thing", 1, False, BS.pack [7, 7, 7]) ]
    in case encodeEnvelope defaultEnvelopeLimits currentEnvelopeVersion specs of
        Right b → b
        Left e  → error ("foreignBytesFor: test setup: " <> show e)

-- ---------------------------------------------------------------------
-- Scratch directory
-- ---------------------------------------------------------------------

-- | A scratch directory, wiped clean before and after use (mirrors
--   'Test.Headless.Core.ConfigState''s withTempDir — the suite runs
--   sequentially, so a single fixed path under the system temp dir,
--   never inside the repo or 'saves/', is safe to reuse across 'it's).
--
--   Deliberately nests the slot dir handed to each test TWO levels below
--   the system temp directory (@root\/saves\/slot@, mirroring production's
--   @\<resource root\>\/saves\/\<slot\>@) rather than directly under it: on
--   macOS the system temp directory itself (@\/tmp@) is a symlink to
--   @\/private\/tmp@, and 'World.Save.Storage.rejectSymlinkedSlotDir'
--   checks a slot's immediate parent as well as the slot itself
--   (requirement 12) — a flat @tmp\/slot@ layout would make every test
--   below spuriously trip that check via an OS-level symlink this suite
--   has nothing to do with. @savesLikeDir@ (the slot's immediate parent)
--   is always a REAL directory this module creates itself, so it only
--   ever looks like a symlink when a test deliberately makes it one.
withTempSlotDir ∷ (FilePath → IO a) → IO a
withTempSlotDir action = do
    tmp ← getTemporaryDirectory
    let root = tmp </> "synarchy-save-storage-spec-root"
        savesLikeDir = root </> "saves"
        dir = savesLikeDir </> "slot"
    reset root
    createDirectoryIfMissing True savesLikeDir
    action dir `finally` reset root
  where
    -- A fault-injection test below may leave 'root' (or 'dir' nested
    -- inside it) as a plain FILE or a read-only DIRECTORY instead of an
    -- ordinary writable tree — handle both at the top level (removing a
    -- directory ENTRY only ever needs write permission on its PARENT,
    -- never on the entry itself, so nothing nested needs its own
    -- permissions restored), or a later test's own setup would fail at
    -- this same path.
    reset root = do
        isDir ← doesDirectoryExist root
        when isDir $ do
            perms ← getPermissions root
            setPermissions root (perms { writable = True })
            removeDirectoryRecursive root
        isFile ← doesFileExist root
        when isFile $ removeFile root

-- | Publish for real via the transaction under test, failing the test
--   (rather than the assertion under test) if setup itself can't
--   publish — every scenario below builds its fixture generations this
--   way, so the transaction is proven correct once (the "publishes
--   successfully" tests) and then trusted as its own fixture builder.
publishOK ∷ FilePath → Text → Word64 → Text → Text → IO SaveMetadata
publishOK dir slot seed name ts = do
    let (meta, bytes) = buildEncoded seed name ts
    r ← publishGeneration dir slot meta bytes HS.empty HS.empty
    case r of
        Right _warnings → pure meta
        Left failure    → do
            expectationFailure ("test setup: publish failed: "
                <> T.unpack (renderPublishFailure failure))
            error "unreachable"

authPath, prevPath ∷ FilePath → FilePath
authPath dir = dir </> authoritativeFileName
prevPath dir = dir </> previousGenerationFileName

-- ---------------------------------------------------------------------
-- Directory-sync fault injection (issue #2229)
-- ---------------------------------------------------------------------

-- | A directory-sync seam ('publishGenerationWithSeams') that RECORDS
--   every directory it is asked to sync, in call order, and throws for
--   the ones @failOn@ selects.
--
--   Fault injection rather than a real @fsync@ failure: a directory
--   fsync does not fail on demand on a healthy filesystem, and the
--   assertions below are about which directories this transaction syncs
--   and what it reports when one cannot be — never about the kernel's
--   own behaviour.
recordingSync ∷ IORef [FilePath] → (FilePath → Bool) → FilePath → IO ()
recordingSync seen failOn path = do
    modifyIORef' seen (⧺ [path])
    when (failOn path) $
        ioError (userError ("injected directory-sync failure: " <> path))

-- | The two directories a publication that CREATES @dir@ must make
--   durable, in the order 'World.Save.Storage.slotOwnerDirectories'
--   syncs them: the slot's own owner (@\<root\>\/saves@ under
--   'withTempSlotDir') and that directory's owner (the scratch root).
ownerDirsOf ∷ FilePath → (FilePath, FilePath)
ownerDirsOf dir = (takeDirectory dir, takeDirectory (takeDirectory dir))

-- | Publish through the injected-sync seam, recording every sync.
publishWithSync
    ∷ IORef [FilePath] → (FilePath → Bool) → FilePath → Word64 → Text
    → IO (Either PublishFailure [Text])
publishWithSync seen failOn dir seed ts =
    let (meta, bytes) = buildEncoded seed "slot" ts
    in publishGenerationWithSeams openBinaryTempFile BS.readFile
           (recordingSync seen failOn) dir "slot" meta bytes
           HS.empty HS.empty

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

-- | Re-encode a structurally valid envelope with ONE component's
--   declared schema version changed, leaving every payload byte alone.
--   Checksums are recomputed by 'encodeEnvelope', so the result is a
--   COHERENT envelope (nothing a storage-corruption check can see) that
--   nonetheless carries a component version this reader has no decoder
--   for — the one way to reach a per-component 'DecodePhase' failure
--   through the real load path, since the envelope codec itself never
--   inspects component versions.
reversionComponent ∷ ComponentId → Word32 → BS.ByteString → BS.ByteString
reversionComponent target newVer bytes =
    case decodeEnvelope defaultEnvelopeLimits currentEnvelopeVersion
             known required bytes of
        Left err → error ("reversionComponent: decode: " <> show err)
        Right de →
            let specs = [ ( cdId d
                          , if cdId d ≡ target then newVer else cdVersion d
                          , cdRequired d
                          , HM.lookupDefault BS.empty (cdId d) (dePayloads de) )
                        | d ← emComponents (deManifest de) ]
            in case encodeEnvelope defaultEnvelopeLimits currentEnvelopeVersion
                        specs of
                Left err → error ("reversionComponent: encode: " <> show err)
                Right b  → b
  where
    known    = HS.insert metadataComponentId componentKnownIds
    required = HS.insert metadataComponentId componentRequiredIds

-- | Put a slot into the exact recovery topology issue #1203 is about: a
--   STORAGE-corrupt authoritative generation (one flipped checksum byte —
--   the same corruption the 'selectLoadGeneration' fallback tests below
--   use, so it is classified 'GenerationCorrupt', never
--   'GenerationIncompatible') sitting on top of a fully valid previous
--   generation. In that state 'selectLoadGeneration' is right now serving
--   the slot from 'previousGenerationFileName' — verified as a
--   precondition by every caller, never assumed. Returns the previous
--   generation's exact bytes, so a later assertion can prove it was
--   neither staged aside nor overwritten by rotation.
corruptAuthOverValidPrev ∷ FilePath → IO BS.ByteString
corruptAuthOverValidPrev dir = do
    _ ← publishOK dir "slot" 1 "slot" "t1"
    _ ← publishOK dir "slot" 2 "slot" "t2"
    whole ← BS.readFile (authPath dir)
    BS.writeFile (authPath dir) (flipByteAt (BS.length whole - 1) whole)
    BS.readFile (prevPath dir)

-- | Assert through the PRODUCTION selector both which generation slot a
--   load resolves to AND which complete generation is actually sitting
--   there, via the fixture's distinctive seed — never merely that
--   selection returned 'Right' (a file existing on disk somewhere is not
--   the invariant; being SELECTED is).
shouldSelect ∷ FilePath → GenerationSource → Word64 → Expectation
shouldSelect dir source seed = do
    sel ← selectLoadGeneration HS.empty HS.empty dir "slot"
    case sel of
        Right s → do
            lsSource s `shouldBe` source
            smSeed (sdMetadata (lsSaveData s)) `shouldBe` seed
        Left err → expectationFailure (T.unpack (lfMessage err))

-- | A throwaway logger for the 'listSaves' tests below (they only need
--   somewhere for its 'logWarn' calls to go).
testLogger ∷ IO LoggerState
testLogger = initLogger defaultLogConfig { lcBackend = LogToHandle stderr }

-- | A logger that KEEPS what it was told, in emission order (issue
--   #2333). Containment is not only "the other slots still list" but
--   "the failure is REPORTED", once, naming the file that failed and
--   why -- and a stderr logger can prove neither.
capturingLogger ∷ IO (LoggerState, IO [Text])
capturingLogger = do
    seen ← newIORef ([] ∷ [Text])
    logger ← initLogger defaultLogConfig
        { lcBackend = LogToCallback (\e → modifyIORef' seen (⧺ [leMessage e])) }
    pure (logger, readIORef seen)

-- | Only the diagnostics 'listSaves' itself emitted.
listingDiagnostics ∷ [Text] → [Text]
listingDiagnostics = filter ("listSaves:" `T.isPrefixOf`)

-- | 'listSaves' with the ROOT survey asserted to have succeeded: a case
--   about per-entry containment is not about the root, so a 'Left' here
--   is a broken fixture rather than the outcome under test.
listSavesOK ∷ LoggerState → IO [SaveListing]
listSavesOK logger = do
    result ← listSaves logger HS.empty
    case result of
        Right listings → pure listings
        Left err → do
            expectationFailure
                ("listSaves refused the whole survey: " <> T.unpack err)
            pure []

-- | 'World.Save.Serialize.listSaves' resolves everything relative to the
--   process's CURRENT DIRECTORY (its own @savesDirectory@ constant is a
--   bare relative @"saves"@), unlike every other function this file
--   tests — so exercising it for real means temporarily chdir'ing into
--   an isolated scratch root, wiped clean before and after (mirrors
--   'withTempSlotDir', same sequential-suite safety rationale).
withSavesRoot ∷ IO a → IO a
withSavesRoot action = do
    tmp ← getTemporaryDirectory
    let root = tmp </> "synarchy-listsaves-symlink-spec"
    reset root
    createDirectoryIfMissing True root
    withCurrentDirectory root action `finally` reset root
  where
    reset root = do
        isDir ← doesDirectoryExist root
        when isDir (removeDirectoryRecursive root)

spec ∷ Spec
spec = do
    describe "publishGeneration" $ do
        it "publishes a first generation with no previous generation" $
            withTempSlotDir $ \dir → do
                let (meta, bytes) = buildEncoded 1 "slot" "t1"
                r ← publishGeneration dir "slot" meta bytes HS.empty HS.empty
                r `shouldBe` Right []
                BS.readFile (authPath dir) `shouldReturn` bytes
                doesFileExist (prevPath dir) `shouldReturn` False
                entries ← listDirectory dir
                entries `shouldBe` [authoritativeFileName]

        -- Issue #2229: the entry NAMING a newly created slot
        -- directory is part of the durability boundary too.
        it "syncs BOTH owning directories when it creates the slot \
           \directory -- saves/ (so the entry naming the slot survives) \
           \and saves/'s own owner, which listSaves routinely creates \
           \without syncing, so an existing saves/ never proves its own \
           \naming entry is durable" $
            withTempSlotDir $ \dir → do
                let (savesLike, root) = ownerDirsOf dir
                -- withTempSlotDir already created savesLike: this is
                -- exactly the "saves/ exists but was never synced" case.
                doesDirectoryExist savesLike `shouldReturn` True
                seen ← newIORef []
                r ← publishWithSync seen (const False) dir 1 "t1"
                r `shouldBe` Right []
                synced ← readIORef seen
                -- Owners first, innermost outwards, and both BEFORE the
                -- slot's own post-rename syncs.
                take 2 synced `shouldBe` [savesLike, root]
                drop 2 synced `shouldSatisfy` all (≡ dir)

        it "adds NO owner-directory sync to the ordinary overwrite of an \
           \ESTABLISHED slot -- one already holding a generation, whose \
           \owning entry the publication that established it made \
           \durable" $
            withTempSlotDir $ \dir → do
                let (savesLike, root) = ownerDirsOf dir
                _ ← publishOK dir "slot" 1 "slot" "t1"
                seen ← newIORef []
                r ← publishWithSync seen (const False) dir 2 "t2"
                r `shouldBe` Right []
                synced ← readIORef seen
                synced `shouldSatisfy` all (≡ dir)
                synced `shouldNotSatisfy` elem savesLike
                synced `shouldNotSatisfy` elem root
                -- ...and it really did sync its own directory, so the
                -- assertion above is not vacuously true of an empty list.
                synced `shouldSatisfy` not . null

        it "reports an owner-directory sync failure as its OWN phase, \
           \naming the directory that actually failed, BEFORE success \
           \-- and publishes nothing" $
            withTempSlotDir $ \dir → do
                let (savesLike, _root) = ownerDirsOf dir
                seen ← newIORef []
                r ← publishWithSync seen (≡ savesLike) dir 1 "t1"
                case r of
                    Left f → do
                        pfPhase f `shouldBe` PhaseOwnerDirectorySync
                        pfPath f `shouldBe` Just savesLike
                        pfReason f `shouldSatisfy`
                            T.isInfixOf "injected directory-sync failure"
                    Right _ → expectationFailure
                        "expected an owner-directory sync failure"
                doesFileExist (authPath dir) `shouldReturn` False

        it "reports a failure of the SECOND owner (saves/'s own owner, \
           \the resolved resource root) the same way, naming THAT \
           \directory -- the sync reached only after the first owner \
           \already succeeded, so a single-owner implementation would \
           \publish here" $
            withTempSlotDir $ \dir → do
                let (savesLike, root) = ownerDirsOf dir
                seen ← newIORef []
                r ← publishWithSync seen (≡ root) dir 1 "t1"
                case r of
                    Left f → do
                        pfPhase f `shouldBe` PhaseOwnerDirectorySync
                        pfPath f `shouldBe` Just root
                        pfReason f `shouldSatisfy`
                            T.isInfixOf "injected directory-sync failure"
                    Right _ → expectationFailure
                        "expected a resource-root owner sync failure"
                -- It really did get past the first owner, so this is the
                -- SECOND sync failing and not the first one misreported.
                readIORef seen `shouldReturn` [savesLike, root]
                doesFileExist (authPath dir) `shouldReturn` False

        it "distinguishes the SLOT's own directory sync from its \
           \owners' -- a failure inside the slot still reports \
           \PhaseDirectorySync naming the slot" $
            withTempSlotDir $ \dir → do
                seen ← newIORef []
                r ← publishWithSync seen (≡ dir) dir 1 "t1"
                case r of
                    Left f → do
                        pfPhase f `shouldBe` PhaseDirectorySync
                        pfPath f `shouldBe` Just dir
                    Right _ → expectationFailure
                        "expected a slot-directory sync failure"

        it "RETRIES the owner-directory sync after one failed -- the \
           \empty slot directory the failed attempt left behind is not \
           \an established slot, so the retry must not report success \
           \having skipped the sync it still owes" $
            withTempSlotDir $ \dir → do
                let (savesLike, root) = ownerDirsOf dir
                firstSeen ← newIORef []
                first ← publishWithSync firstSeen (≡ savesLike) dir 1 "t1"
                first `shouldSatisfy` isLeft
                -- The leftover this is all about: a slot directory with
                -- no generation in it.
                doesDirectoryExist dir `shouldReturn` True
                doesFileExist (authPath dir) `shouldReturn` False
                retrySeen ← newIORef []
                retry ← publishWithSync retrySeen (const False) dir 1 "t1"
                retry `shouldBe` Right []
                synced ← readIORef retrySeen
                take 2 synced `shouldBe` [savesLike, root]

        it "a second publish retains the first generation as the \
           \previous generation" $
            withTempSlotDir $ \dir → do
                let (_, bytesA) = buildEncoded 1 "slot" "t1"
                    (metaB, bytesB) = buildEncoded 2 "slot" "t2"
                _ ← publishOK dir "slot" 1 "slot" "t1"
                r ← publishGeneration dir "slot" metaB bytesB HS.empty HS.empty
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
                r ← publishGeneration dir "slot" metaC bytesC HS.empty HS.empty
                r `shouldBe` Right []
                BS.readFile (authPath dir) `shouldReturn` bytesC
                BS.readFile (prevPath dir) `shouldReturn` bytesB
                -- The first generation, staged out of the way during
                -- rotation (requirement 5), must be cleaned up once the
                -- new publication is durable -- no stray file survives.
                entries ← listDirectory dir
                entries `shouldMatchList`
                    [authoritativeFileName, previousGenerationFileName]

        it "refuses to publish when the PREVIOUS generation (not just \
           \the authoritative one) carries an optional component this \
           \build does not recognize (round-4 review) -- a real \
           \corrupted-authoritative recovery leaves the actually-loaded, \
           \still-valid generation sitting at .prev, where an ordinary \
           \publish would otherwise stage it aside and sweep it away \
           \(cleanupAfterPublish) without this check ever having looked \
           \at it" $
            withTempSlotDir $ \dir → do
                let (_metaA, bytesA) = buildEncoded 1 "slot" "t1"
                    foreignPrevBytes = foreignBytesFor 2 "slot"
                createDirectoryIfMissing True dir
                BS.writeFile (authPath dir) bytesA
                BS.writeFile (prevPath dir) foreignPrevBytes
                let (metaC, bytesC) = buildEncoded 3 "slot" "t3"
                r ← publishGeneration dir "slot" metaC bytesC HS.empty HS.empty
                case r of
                    Left f  → do
                        pfPhase f `shouldBe` PhaseForeignOptionalData
                        pfReason f `shouldSatisfy` T.isInfixOf "retired-thing"
                    Right _ → expectationFailure
                        "expected a foreign-optional-data refusal"
                -- Refusing must happen BEFORE any rotation -- both
                -- generations stay exactly as they were, never staged.
                BS.readFile (authPath dir) `shouldReturn` bytesA
                BS.readFile (prevPath dir) `shouldReturn` foreignPrevBytes

        it "still publishes normally when an ORDINARY previous \
           \generation (no foreign data) sits at .prev -- the round-4 \
           \fix must not add friction to the routine rotation case" $
            withTempSlotDir $ \dir → do
                _ ← publishOK dir "slot" 1 "slot" "t1"
                _ ← publishOK dir "slot" 2 "slot" "t2"
                let (metaC, bytesC) = buildEncoded 3 "slot" "t3"
                r ← publishGeneration dir "slot" metaC bytesC HS.empty HS.empty
                r `shouldBe` Right []
                BS.readFile (authPath dir) `shouldReturn` bytesC

        it "never publishes a candidate that fails to decode, leaving \
           \an existing authoritative generation untouched" $
            withTempSlotDir $ \dir → do
                original ← publishOK dir "slot" 1 "slot" "t1"
                origBytes ← BS.readFile (authPath dir)
                r ← publishGeneration dir "slot" original (BS.pack [0,1,2,3,4]) HS.empty HS.empty
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
                r ← publishGeneration dir "slot" wrongMeta bytes HS.empty HS.empty
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
                r ← publishGeneration dir "slot" wrongMeta bytes HS.empty HS.empty
                case r of
                    Left f  → pfPhase f `shouldBe` PhaseCandidateValidate
                    Right _ → expectationFailure "expected a validate failure"
                doesFileExist (authPath dir) `shouldReturn` False

        it "reports a directory-create failure when the slot path is \
           \occupied by a plain file, without touching it" $
            withTempSlotDir $ \dir → do
                let (meta, bytes) = buildEncoded 1 "slot" "t1"
                BS.writeFile dir "occupying this path with a plain file"
                r ← publishGeneration dir "slot" meta bytes HS.empty HS.empty
                case r of
                    Left f  → pfPhase f `shouldBe` PhaseDirectoryCreate
                    Right _ → expectationFailure "expected a directory-create failure"
                contents ← BS.readFile dir
                contents `shouldBe` "occupying this path with a plain file"

        it "reports a candidate-create failure when opening the unique \
           \candidate fails" $
            withTempSlotDir $ \dir → do
                let (meta, bytes) = buildEncoded 1 "slot" "t1"
                createDirectoryIfMissing True dir
                let failCreate _ _ = ioError (userError "injected create failure")
                r ← publishGenerationWithCandidateCreator failCreate dir "slot" meta bytes HS.empty HS.empty
                case r of
                    Left f  → pfPhase f `shouldBe` PhaseCandidateCreate
                    Right _ → expectationFailure "expected a candidate-create failure"

        -- Issue #2227: an existing generation that is PRESENT but
        -- unreadable is neither absent nor confirmed free of foreign
        -- optional data, yet the old preflight answered "no foreign
        -- data" for it and the rest of the transaction then destroyed
        -- exactly that file. Both refusals below are driven through
        -- 'publishGenerationWithSeams', whose reader seam fails ONE
        -- exact generation path and delegates every other read to the
        -- production 'BS.readFile' -- so each generation is proved
        -- independently, and neither test depends on filesystem mode
        -- bits, which CI's root containers ignore.
        let failReadOf victim path
                | path ≡ victim =
                    ioError (userError "injected generation read failure")
                | otherwise = BS.readFile path
            -- An exact directory listing is stricter than asking
            -- 'isOwnedArtifactName' about each entry: it rejects a
            -- leftover candidate ('candidateTemplate'), a staged
            -- previous generation ('staleTemplate'), AND anything else
            -- a refusal might have left behind.
            expectOnlyGenerations dir = do
                entries ← listDirectory dir
                entries `shouldMatchList`
                    [authoritativeFileName, previousGenerationFileName]

        it "refuses the publish and names the AUTHORITATIVE generation \
           \when that file is present but cannot be read during the \
           \preflight (#2227) -- the loader classifies an unreadable \
           \authoritative file GenerationCorrupt, so the recovering \
           \topology would otherwise rename the candidate straight over \
           \an intact file a POSIX rename never needed to read" $
            withTempSlotDir $ \dir → do
                _ ← publishOK dir "slot" 1 "slot" "t1"
                _ ← publishOK dir "slot" 2 "slot" "t2"
                authBefore ← BS.readFile (authPath dir)
                prevBefore ← BS.readFile (prevPath dir)
                let (metaC, bytesC) = buildEncoded 3 "slot" "t3"
                r ← publishGenerationWithSeams openBinaryTempFile
                        (failReadOf (authPath dir)) syncDirectory dir "slot"
                        metaC bytesC HS.empty HS.empty
                case r of
                    Left f  → do
                        pfPhase f `shouldBe` PhaseExistingGenerationRead
                        pfPath f `shouldBe` Just (authPath dir)
                        pfReason f `shouldSatisfy`
                            T.isInfixOf "injected generation read failure"
                    Right _ → expectationFailure
                        "expected an existing-generation read refusal"
                BS.readFile (authPath dir) `shouldReturn` authBefore
                BS.readFile (prevPath dir) `shouldReturn` prevBefore
                expectOnlyGenerations dir

        it "refuses the publish and names the PREVIOUS generation when \
           \that file is present but cannot be read during the preflight \
           \(#2227) -- an intact authoritative file beside it would \
           \otherwise take the ordinary retained topology, which stages \
           \.prev aside and sweeps it away once the new generation is \
           \durable" $
            withTempSlotDir $ \dir → do
                _ ← publishOK dir "slot" 1 "slot" "t1"
                _ ← publishOK dir "slot" 2 "slot" "t2"
                authBefore ← BS.readFile (authPath dir)
                prevBefore ← BS.readFile (prevPath dir)
                let (metaC, bytesC) = buildEncoded 3 "slot" "t3"
                r ← publishGenerationWithSeams openBinaryTempFile
                        (failReadOf (prevPath dir)) syncDirectory dir "slot"
                        metaC bytesC HS.empty HS.empty
                case r of
                    Left f  → do
                        pfPhase f `shouldBe` PhaseExistingGenerationRead
                        pfPath f `shouldBe` Just (prevPath dir)
                        pfReason f `shouldSatisfy`
                            T.isInfixOf "injected generation read failure"
                    Right _ → expectationFailure
                        "expected an existing-generation read refusal"
                BS.readFile (authPath dir) `shouldReturn` authBefore
                BS.readFile (prevPath dir) `shouldReturn` prevBefore
                expectOnlyGenerations dir

        it "still publishes when the unreadable path is not a generation \
           \file at all (#2227) -- the refusal keys on the two exact \
           \generation names, never on the seam having been supplied" $
            withTempSlotDir $ \dir → do
                _ ← publishOK dir "slot" 1 "slot" "t1"
                let (metaB, bytesB) = buildEncoded 2 "slot" "t2"
                r ← publishGenerationWithSeams openBinaryTempFile
                        (failReadOf (dir </> "unrelated-note.txt"))
                        syncDirectory dir "slot" metaB bytesB
                        HS.empty HS.empty
                r `shouldBe` Right []
                BS.readFile (authPath dir) `shouldReturn` bytesB

        it "reports an unsafe-path failure and never writes through a \
           \slot directory that is itself a symlink" $
            withTempSlotDir $ \dir → do
                let target = dir <> "-target"
                createDirectoryIfMissing True target
                createFileLink target dir
                (`finally` (removeFile dir >> removeDirectoryRecursive target)) $ do
                    let (meta, bytes) = buildEncoded 1 "slot" "t1"
                    r ← publishGeneration dir "slot" meta bytes HS.empty HS.empty
                    case r of
                        Left f  → pfPhase f `shouldBe` PhaseUnsafePath
                        Right _ → expectationFailure "expected an unsafe-path failure"
                    doesFileExist (target </> authoritativeFileName)
                        `shouldReturn` False
                    listDirectory target `shouldReturn` []

        it "reports an unsafe-path failure and never writes through a \
           \slot whose IMMEDIATE PARENT (standing in for saves/ itself) \
           \is a symlink, without walking any further up (a raw OS-level \
           \ancestor symlink like macOS's /tmp must never trip this)" $
            withTempSlotDir $ \dir → do
                let savesLikeDir = takeDirectory dir
                    target = savesLikeDir <> "-target"
                createDirectoryIfMissing True target
                removeDirectoryRecursive savesLikeDir
                createFileLink target savesLikeDir
                (`finally` (removeFile savesLikeDir
                             >> removeDirectoryRecursive target)) $ do
                    let (meta, bytes) = buildEncoded 1 "slot" "t1"
                    r ← publishGeneration dir "slot" meta bytes HS.empty HS.empty
                    case r of
                        Left f  → pfPhase f `shouldBe` PhaseUnsafePath
                        Right _ → expectationFailure "expected an unsafe-path failure"
                    listDirectory target `shouldReturn` []

        it "never touches (stages, rotates, or destroys) the sole \
           \previous generation when publishing from an authoritative-\
           \missing 'previous-only' recovery state -- there is nothing \
           \to rotate it out of the way for, so it simply becomes the \
           \new previous generation untouched, never at risk of being \
           \destroyed before the new candidate is durable" $
            withTempSlotDir $ \dir → do
                _ ← publishOK dir "slot" 1 "slot" "t1"
                _ ← publishOK dir "slot" 2 "slot" "t2"
                -- Simulate an earlier interrupted rotation that left the
                -- slot in exactly the recovery state
                -- 'selectLoadGeneration' is designed to recover from:
                -- authoritative missing, previous generation intact.
                forceRemoveFile (authPath dir)
                prevBefore ← BS.readFile (prevPath dir)
                let (metaC, bytesC) = buildEncoded 3 "slot" "t3"
                r ← publishGeneration dir "slot" metaC bytesC HS.empty HS.empty
                r `shouldBe` Right []
                BS.readFile (authPath dir) `shouldReturn` bytesC
                BS.readFile (prevPath dir) `shouldReturn` prevBefore

        it "reports a rotate-previous failure without destroying the \
           \existing authoritative generation, when the previous-\
           \generation path is blocked" $
            withTempSlotDir $ \dir → do
                _ ← publishOK dir "slot" 1 "slot" "t1"
                origBytes ← BS.readFile (authPath dir)
                createDirectoryIfMissing True (prevPath dir)
                let (metaB, bytesB) = buildEncoded 2 "slot" "t2"
                r ← publishGeneration dir "slot" metaB bytesB HS.empty HS.empty
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
                r ← publishGeneration dir "slot" meta bytes HS.empty HS.empty
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

    -- Issue #1203. Before this, 'publishValidated' decided whether to
    -- stage + rotate solely from @doesFileExist authPath@, so a publish
    -- over a CORRUPT authoritative generation staged the valid .prev (the
    -- slot's live recovery source) aside and rotated the corrupt file onto
    -- .prev. An interruption between that rotation's directory sync and
    -- the publish rename then left NO selectable generation at all, with
    -- the only complete one stranded under a staged name selection never
    -- consults. Publication now classifies the authoritative file with the
    -- loader's own 'decodeGenerationFile', and skips staging/rotation
    -- entirely in the topologies where .prev is what the slot is loading
    -- from -- so the sequence produces exactly TWO durable states here
    -- (pre-rename and post-rename), each asserted below through the
    -- production selector.
    describe "publishing over a corrupt authoritative generation \
             \(issue #1203)" $ do
        it "leaves the valid previous generation selectable when the \
           \publish is interrupted before the publish rename -- the \
           \revised sequence stages nothing and rotates nothing in this \
           \topology, so every pre-rename interruption point is this one \
           \durable state" $
            withTempSlotDir $ \dir → do
                prevBefore ← corruptAuthOverValidPrev dir
                -- Precondition: the slot really is recovering from .prev.
                shouldSelect dir FromPrevious 1
                let (metaC, bytesC) = buildEncoded 3 "slot" "t3"
                    failCreate _ _ = ioError (userError "injected interruption")
                r ← publishGenerationWithCandidateCreator failCreate dir
                        "slot" metaC bytesC HS.empty HS.empty
                case r of
                    Left f  → pfPhase f `shouldBe` PhaseCandidateCreate
                    Right _ → expectationFailure
                        "expected the injected interruption to abort the publish"
                -- The recovery source is byte-identical and STILL what a
                -- real load resolves to.
                BS.readFile (prevPath dir) `shouldReturn` prevBefore
                shouldSelect dir FromPrevious 1

        it "takes neither the staging nor the rotation branch at all -- \
           \proven by blocking the rotation TARGET, which a pre-#1203 \
           \publish would have tripped over on its way to overwriting the \
           \recovery source, and which this one never reaches" $
            withTempSlotDir $ \dir → do
                _ ← publishOK dir "slot" 1 "slot" "t1"
                whole ← BS.readFile (authPath dir)
                BS.writeFile (authPath dir)
                    (flipByteAt (BS.length whole - 1) whole)
                -- A directory at the previous-generation path makes the
                -- rotate rename fail outright (the same block the
                -- PhaseRotatePrevious test above relies on), so REACHING
                -- rotation at all is directly observable from outside.
                createDirectoryIfMissing True (prevPath dir)
                let (metaC, bytesC) = buildEncoded 3 "slot" "t3"
                r ← publishGeneration dir "slot" metaC bytesC HS.empty HS.empty
                r `shouldBe` Right []
                BS.readFile (authPath dir) `shouldReturn` bytesC
                doesDirectoryExist (prevPath dir) `shouldReturn` True

        it "selects the newly published generation when the publish is \
           \interrupted immediately after the publish rename, before the \
           \final directory sync and cleanup sweep -- the only other \
           \durable state this sequence produces" $
            withTempSlotDir $ \dir → do
                prevBefore ← corruptAuthOverValidPrev dir
                let (_, bytesC) = buildEncoded 3 "slot" "t3"
                -- Reproduce that exact durable state: the validated
                -- candidate atomically renamed onto the authoritative
                -- path (a rename is never observed half-applied), the
                -- previous generation still exactly where it was.
                BS.writeFile (authPath dir) bytesC
                shouldSelect dir FromAuthoritative 3
                BS.readFile (prevPath dir) `shouldReturn` prevBefore

        it "publishes without staging, rotating, or overwriting the valid \
           \previous generation the slot is recovering from -- which stays \
           \a complete, retained previous generation afterwards" $
            withTempSlotDir $ \dir → do
                prevBefore ← corruptAuthOverValidPrev dir
                shouldSelect dir FromPrevious 1
                let (metaC, bytesC) = buildEncoded 3 "slot" "t3"
                r ← publishGeneration dir "slot" metaC bytesC HS.empty HS.empty
                r `shouldBe` Right []
                BS.readFile (authPath dir) `shouldReturn` bytesC
                -- The corrupt authoritative file was never rotated ONTO
                -- .prev, and .prev was never staged aside: byte-identical,
                -- still in place, and no staged artifact was ever created.
                BS.readFile (prevPath dir) `shouldReturn` prevBefore
                entries ← listDirectory dir
                entries `shouldMatchList`
                    [authoritativeFileName, previousGenerationFileName]
                shouldSelect dir FromAuthoritative 3
                -- The formerly recovered generation is still a REAL
                -- retained previous generation, not merely a leftover
                -- file: prove it through the production selector by making
                -- the new authoritative generation unavailable.
                forceRemoveFile (authPath dir)
                shouldSelect dir FromPrevious 1

        it "publishes over a corrupt authoritative generation that has no \
           \previous generation at all, leaving the new generation \
           \selectable and never manufacturing a previous generation out \
           \of the corrupt bytes" $
            withTempSlotDir $ \dir → do
                _ ← publishOK dir "slot" 1 "slot" "t1"
                whole ← BS.readFile (authPath dir)
                BS.writeFile (authPath dir)
                    (flipByteAt (BS.length whole - 1) whole)
                doesFileExist (prevPath dir) `shouldReturn` False
                let (metaC, bytesC) = buildEncoded 3 "slot" "t3"
                r ← publishGeneration dir "slot" metaC bytesC HS.empty HS.empty
                r `shouldBe` Right []
                BS.readFile (authPath dir) `shouldReturn` bytesC
                doesFileExist (prevPath dir) `shouldReturn` False
                shouldSelect dir FromAuthoritative 3

        it "still stages and rotates when the authoritative generation is \
           \coherent but semantically INCOMPATIBLE rather than corrupt -- \
           \selectLoadGeneration never falls back past one, so .prev is \
           \NOT serving this slot and ordinary rotation applies exactly as \
           \it did before #1203" $
            withTempSlotDir $ \dir → do
                _ ← publishOK dir "slot" 1 "slot" "t1"
                _ ← publishOK dir "slot" 2 "slot" "t2"
                whole ← BS.readFile (authPath dir)
                let incompatible = corruptEnvelopeVersion whole
                BS.writeFile (authPath dir) incompatible
                -- Precondition: this really is the no-fallback case.
                sel0 ← selectLoadGeneration HS.empty HS.empty dir "slot"
                case sel0 of
                    Left err → T.unpack (lfMessage err) `shouldContain` "incompatible"
                    Right s  → expectationFailure
                        ("expected no fallback, got " <> show (lsSource s))
                let (metaC, bytesC) = buildEncoded 3 "slot" "t3"
                r ← publishGeneration dir "slot" metaC bytesC HS.empty HS.empty
                r `shouldBe` Right []
                BS.readFile (authPath dir) `shouldReturn` bytesC
                -- Rotated, not preserved: the incompatible generation is
                -- what now sits at .prev, and the staged old one is swept.
                BS.readFile (prevPath dir) `shouldReturn` incompatible
                entries ← listDirectory dir
                entries `shouldMatchList`
                    [authoritativeFileName, previousGenerationFileName]
                shouldSelect dir FromAuthoritative 3

    describe "selectLoadGeneration" $ do
        it "selects the authoritative generation when both generations \
           \are present and valid, never combining them" $
            withTempSlotDir $ \dir → do
                _ ← publishOK dir "slot" 1 "slot" "t1"
                _ ← publishOK dir "slot" 2 "slot" "t2"
                sel ← selectLoadGeneration HS.empty HS.empty dir "slot"
                case sel of
                    Right s → do
                        lsSource s `shouldBe` FromAuthoritative
                        smSeed (sdMetadata (lsSaveData s)) `shouldBe` 2
                    Left err → expectationFailure (T.unpack (lfMessage err))

        it "falls back to the previous generation when the \
           \authoritative file is missing" $
            withTempSlotDir $ \dir → do
                _ ← publishOK dir "slot" 1 "slot" "t1"
                _ ← publishOK dir "slot" 2 "slot" "t2"
                forceRemoveFile (authPath dir)
                sel ← selectLoadGeneration HS.empty HS.empty dir "slot"
                case sel of
                    Right s → do
                        lsSource s `shouldBe` FromPrevious
                        smSeed (sdMetadata (lsSaveData s)) `shouldBe` 1
                    Left err → expectationFailure (T.unpack (lfMessage err))

        it "falls back to the previous generation when the \
           \authoritative file is truncated" $
            withTempSlotDir $ \dir → do
                _ ← publishOK dir "slot" 1 "slot" "t1"
                _ ← publishOK dir "slot" 2 "slot" "t2"
                whole ← BS.readFile (authPath dir)
                BS.writeFile (authPath dir) (BS.take (BS.length whole `div` 2) whole)
                sel ← selectLoadGeneration HS.empty HS.empty dir "slot"
                case sel of
                    Right s → do
                        lsSource s `shouldBe` FromPrevious
                        smSeed (sdMetadata (lsSaveData s)) `shouldBe` 1
                    Left err → expectationFailure (T.unpack (lfMessage err))

        it "falls back to the previous generation when the \
           \authoritative file has bad framing (magic)" $
            withTempSlotDir $ \dir → do
                _ ← publishOK dir "slot" 1 "slot" "t1"
                _ ← publishOK dir "slot" 2 "slot" "t2"
                whole ← BS.readFile (authPath dir)
                BS.writeFile (authPath dir) (BS.cons 0x00 (BS.drop 1 whole))
                sel ← selectLoadGeneration HS.empty HS.empty dir "slot"
                case sel of
                    Right s  → lsSource s `shouldBe` FromPrevious
                    Left err → expectationFailure (T.unpack (lfMessage err))

        it "falls back to the previous generation when the \
           \authoritative file fails checksum validation" $
            withTempSlotDir $ \dir → do
                _ ← publishOK dir "slot" 1 "slot" "t1"
                _ ← publishOK dir "slot" 2 "slot" "t2"
                whole ← BS.readFile (authPath dir)
                BS.writeFile (authPath dir) (flipByteAt (BS.length whole - 1) whole)
                sel ← selectLoadGeneration HS.empty HS.empty dir "slot"
                case sel of
                    Right s  → lsSource s `shouldBe` FromPrevious
                    Left err → expectationFailure (T.unpack (lfMessage err))

        it "does NOT fall back when the authoritative generation is \
           \present but semantically incompatible with this build, \
           \even though a valid previous generation exists" $
            withTempSlotDir $ \dir → do
                _ ← publishOK dir "slot" 1 "slot" "t1"
                _ ← publishOK dir "slot" 2 "slot" "t2"
                whole ← BS.readFile (authPath dir)
                BS.writeFile (authPath dir) (corruptEnvelopeVersion whole)
                sel ← selectLoadGeneration HS.empty HS.empty dir "slot"
                case sel of
                    Right s  → expectationFailure
                        ("expected no fallback, got " <> show (lsSource s))
                    Left err → T.unpack (lfMessage err) `shouldContain` "incompatible"

        it "does NOT fall back when the authoritative generation is \
           \structurally valid (every checksum agrees) but has no world \
           \pages -- checkWorldCount is a content-validation failure, \
           \not storage corruption -- even though a valid previous \
           \generation exists" $
            withTempSlotDir $ \dir → do
                _ ← publishOK dir "slot" 1 "slot" "t1"
                _ ← publishOK dir "slot" 2 "slot" "t2"
                BS.writeFile (authPath dir) emptyPagesBytes
                sel ← selectLoadGeneration HS.empty HS.empty dir "slot"
                case sel of
                    Right s  → expectationFailure
                        ("expected no fallback, got " <> show (lsSource s))
                    Left err → T.unpack (lfMessage err) `shouldContain` "incompatible"

        it "reports an unsafe-path failure and never reads through a \
           \slot directory that is itself a symlink" $
            withTempSlotDir $ \dir → do
                let target = dir <> "-target"
                _ ← publishOK target "slot" 1 "slot" "t1"
                createFileLink target dir
                sel ← selectLoadGeneration HS.empty HS.empty dir "slot"
                    `finally` (removeFile dir
                                >> removeDirectoryRecursive target)
                case sel of
                    Left err → T.unpack (lfMessage err) `shouldContain` "symlink"
                    Right s  → expectationFailure
                        ("expected an unsafe-path failure, got " <> show s)

        it "falls back to the previous generation when the \
           \AUTHORITATIVE FILE ITSELF (not the slot directory, which is \
           \perfectly ordinary) is a symlink -- publishGeneration never \
           \leaves one there, so this can only come from outside the \
           \transaction" $
            withTempSlotDir $ \dir → do
                _ ← publishOK dir "slot" 1 "slot" "t1"
                _ ← publishOK dir "slot" 2 "slot" "t2"
                let decoy = dir <> "-decoy.txt"
                BS.writeFile decoy "not a real save"
                removeFile (authPath dir)
                createFileLink decoy (authPath dir)
                sel ← selectLoadGeneration HS.empty HS.empty dir "slot"
                    `finally` removeFile decoy
                case sel of
                    Right s → do
                        lsSource s `shouldBe` FromPrevious
                        smSeed (sdMetadata (lsSaveData s)) `shouldBe` 1
                    Left err → expectationFailure (T.unpack (lfMessage err))

        it "reports a failure (not a hybrid) when BOTH the authoritative \
           \and previous generation files are themselves symlinks" $
            withTempSlotDir $ \dir → do
                _ ← publishOK dir "slot" 1 "slot" "t1"
                _ ← publishOK dir "slot" 2 "slot" "t2"
                let decoy = dir <> "-decoy.txt"
                BS.writeFile decoy "not a real save"
                removeFile (authPath dir)
                removeFile (prevPath dir)
                createFileLink decoy (authPath dir)
                createFileLink decoy (prevPath dir)
                sel ← selectLoadGeneration HS.empty HS.empty dir "slot"
                    `finally` removeFile decoy
                sel `shouldSatisfy` isLeft

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
                sel ← selectLoadGeneration HS.empty HS.empty dir "slot"
                case sel of
                    Right s → do
                        lsSource s `shouldBe` FromAuthoritative
                        smSeed (sdMetadata (lsSaveData s)) `shouldBe` 2
                    Left err → expectationFailure (T.unpack (lfMessage err))
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
                sel ← selectLoadGeneration HS.empty HS.empty dir "slot"
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
                _ ← selectLoadGeneration HS.empty HS.empty dir "slot"
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
                sel ← selectLoadGeneration HS.empty HS.empty dir "slot"
                case sel of
                    Right s  → lsSource s `shouldBe` FromAuthoritative
                    Left err → expectationFailure (T.unpack (lfMessage err))

        it "never selects a partial candidate: a stray leftover \
           \temporary file alongside a previous-only recovery is \
           \ignored" $
            withTempSlotDir $ \dir → do
                _ ← publishOK dir "slot" 1 "slot" "t1"
                _ ← publishOK dir "slot" 2 "slot" "t2"
                forceRemoveFile (authPath dir)
                BS.writeFile (dir </> "world-synworld-tmp88888")
                    "an interrupted write, never fully validated"
                sel ← selectLoadGeneration HS.empty HS.empty dir "slot"
                case sel of
                    Right s → do
                        lsSource s `shouldBe` FromPrevious
                        smSeed (sdMetadata (lsSaveData s)) `shouldBe` 1
                    Left err → expectationFailure (T.unpack (lfMessage err))

    describe "World.Save.Serialize.listSaves symlink containment \
             \(issue #762 round 4 — the same containment check applies \
             \to listing, not just publish/select)" $ do
        it "never lists (or reads through) a slot whose directory is \
           \itself a symlink" $
            withSavesRoot $ do
                logger ← testLogger
                let target = "leaked-target"
                _ ← publishOK target "leaked" 1 "leaked" "t1"
                createDirectoryIfMissing True savesDirectory
                createFileLink target (savesDirectory </> "leaked")
                saves ← listSavesOK logger
                map slName saves `shouldNotContain` ["leaked"]

        it "never lists any slot when saves/ itself is a symlink" $
            withSavesRoot $ do
                logger ← testLogger
                let target = "leaked-target"
                _ ← publishOK (target </> "leaked") "leaked" 1 "leaked" "t1"
                createFileLink target savesDirectory
                saves ← listSavesOK logger
                saves `shouldBe` []

        it "never lists (or reads through) a legacy flat-file save \
           \whose file is itself a symlink" $
            withSavesRoot $ do
                logger ← testLogger
                let decoy = "decoy-legacy-target"
                BS.writeFile decoy "not a real save"
                createDirectoryIfMissing True savesDirectory
                createFileLink decoy (savesDirectory </> "leaked.synworld")
                saves ← listSavesOK logger
                map slName saves `shouldNotContain` ["leaked"]

        it "loadWorld never reads through a legacy flat-file save whose \
           \file is itself a symlink" $
            withSavesRoot $ do
                logger ← testLogger
                let decoy = "decoy-legacy-target2"
                BS.writeFile decoy "not a real save"
                createDirectoryIfMissing True savesDirectory
                createFileLink decoy (savesDirectory </> "leaked2.synworld")
                result ← loadWorld logger "leaked2" HS.empty HS.empty
                result `shouldSatisfy` isLeft

        it "falls back to the previous generation when listing, if the \
           \AUTHORITATIVE FILE ITSELF (not the slot directory) is a \
           \symlink -- listSaves has its own read path, separate from \
           \selectLoadGeneration, and must apply the identical check" $
            withSavesRoot $ do
                logger ← testLogger
                let slot = savesDirectory </> "leaked"
                _ ← publishOK slot "leaked" 1 "leaked" "t1"
                _ ← publishOK slot "leaked" 2 "leaked" "t2"
                let decoy = "decoy-listing-target"
                BS.writeFile decoy "not a real save"
                removeFile (authPath slot)
                createFileLink decoy (authPath slot)
                saves ← listSavesOK logger `finally` removeFile decoy
                case filter ((≡ "leaked") . slName) saves of
                    [entry] → do
                        slRecovered entry `shouldBe` True
                        smSeed (slMetadata entry) `shouldBe` 1
                    other → expectationFailure
                        ("expected exactly one 'leaked' listing, got "
                            <> show other)

        it "never lists a slot when BOTH its authoritative and previous \
           \generation files are themselves symlinks" $
            withSavesRoot $ do
                logger ← testLogger
                let slot = savesDirectory </> "leaked"
                _ ← publishOK slot "leaked" 1 "leaked" "t1"
                _ ← publishOK slot "leaked" 2 "leaked" "t2"
                let decoy = "decoy-listing-target2"
                BS.writeFile decoy "not a real save"
                removeFile (authPath slot)
                removeFile (prevPath slot)
                createFileLink decoy (authPath slot)
                createFileLink decoy (prevPath slot)
                saves ← listSavesOK logger `finally` removeFile decoy
                map slName saves `shouldNotContain` ["leaked"]

    -- Issue #2333: the three generation reads inside 'listSaves' were
    -- bare 'BS.readFile' calls, so ONE unreadable file (a permission
    -- failure, a special file, a file removed between the existence
    -- check and the read) escaped the traversal as an 'IOException' and
    -- no listing was built at all -- taking down the main menu's save
    -- list and both autosave slot verbs, whose Lua wrappers turn an
    -- escaped Haskell exception into a Lua error their callers'
    -- fallbacks never see.
    --
    -- Every fixture below fails ONE exact path through
    -- 'listSavesWithSeams'' reader seam and delegates every other read
    -- to the production 'BS.readFile', for the same reason #2227's
    -- publish tests do: filesystem mode bits are ignored by CI's root
    -- containers, and a directory standing in for an unreadable
    -- generation never reaches the read at all ('doesFileExist' sends
    -- it down the missing-generation path instead).
    --
    -- Each case also asserts the DIAGNOSTIC, through a capturing
    -- logger: containment that silently swallowed the failure would
    -- pass a "the healthy slot still lists" assertion just as well.
    describe "World.Save.Serialize.listSaves read containment \
             \(issue #2333 -- a failing generation read is blamed on its \
             \own slot, never on the whole listing)" $ do
        let healthySlot   = savesDirectory </> "healthy"
            failingSlot   = savesDirectory </> "wounded"
            legacyPath    = savesDirectory </> "relic" <> saveExtension
            injected      = "injected generation read failure"
            -- The reader seam #2227 established, in listing's shape: one
            -- victim path throws, everything else reads for real.
            failReadOf victim = productionListingSeams
                { lsReadGeneration = \path →
                    if path ≡ victim
                        then ioError (userError injected)
                        else BS.readFile path
                }
            -- Records every generation path actually READ, in order, so
            -- a case can assert a file was never consulted at all.
            recordingSeams ref = productionListingSeams
                { lsReadGeneration = \path → do
                    modifyIORef' ref (⧺ [path])
                    BS.readFile path
                }
            -- A healthy sibling every case keeps, because "the rest of
            -- the listing survives" is half of what containment means.
            plantHealthy = do
                _ ← publishOK healthySlot "healthy" 9 "healthy" "t9"
                pure ()
            namesOf = map slName
            listWith seams logger = do
                result ← listSavesWithSeams seams logger HS.empty
                case result of
                    Right listings → pure listings
                    Left err → do
                        expectationFailure
                            ("expected a per-entry failure, but the whole \
                             \survey was refused: " <> T.unpack err)
                        pure []

        it "lists a slot whose AUTHORITATIVE generation cannot be read \
           \from its previous generation instead, marked recovered -- \
           \the same fallback-eligible classification \
           \decodeGenerationFile gives an unreadable file on the LOAD \
           \path" $
            withSavesRoot $ do
                plantHealthy
                _ ← publishOK failingSlot "wounded" 1 "wounded" "t1"
                _ ← publishOK failingSlot "wounded" 2 "wounded" "t2"
                (logger, readSeen) ← capturingLogger
                listings ← listWith (failReadOf (authPath failingSlot)) logger
                namesOf listings `shouldMatchList` ["healthy", "wounded"]
                case filter ((≡ "wounded") . slName) listings of
                    [entry] → do
                        slRecovered entry `shouldBe` True
                        -- The PREVIOUS generation's own seed, so this is
                        -- the earlier generation and not the one whose
                        -- read failed.
                        smSeed (slMetadata entry) `shouldBe` 1
                    other → expectationFailure
                        ("expected exactly one 'wounded' listing, got "
                            <> show other)
                case filter ((≡ "healthy") . slName) listings of
                    [entry] → slRecovered entry `shouldBe` False
                    other → expectationFailure
                        ("expected exactly one 'healthy' listing, got "
                            <> show other)
                diagnostics ← listingDiagnostics <$> readSeen
                case diagnostics of
                    [one] → do
                        T.unpack one `shouldContain` authPath failingSlot
                        T.unpack one `shouldContain` injected
                    other → expectationFailure
                        ("expected exactly one listing diagnostic, got "
                            <> show other)

        it "skips ONLY the slot whose authoritative generation cannot be \
           \read when it has no previous generation, naming the exact \
           \path and the read error once" $
            withSavesRoot $ do
                plantHealthy
                -- A single publish: no previous generation exists, so
                -- the fallback has nothing to recover from.
                _ ← publishOK failingSlot "wounded" 1 "wounded" "t1"
                doesFileExist (prevPath failingSlot) `shouldReturn` False
                (logger, readSeen) ← capturingLogger
                listings ← listWith (failReadOf (authPath failingSlot)) logger
                namesOf listings `shouldBe` ["healthy"]
                diagnostics ← listingDiagnostics <$> readSeen
                case diagnostics of
                    [one] → do
                        T.unpack one `shouldContain` authPath failingSlot
                        T.unpack one `shouldContain` injected
                        T.unpack one `shouldContain` "no previous generation"
                    other → expectationFailure
                        ("expected exactly one listing diagnostic, got "
                            <> show other)

        it "completes the previous-generation fallback as UNUSABLE when \
           \the PREVIOUS file is the one that cannot be read, naming \
           \that path -- the authoritative generation here is corrupt \
           \bytes, so the fallback is genuinely reached" $
            withSavesRoot $ do
                plantHealthy
                _ ← publishOK failingSlot "wounded" 1 "wounded" "t1"
                _ ← publishOK failingSlot "wounded" 2 "wounded" "t2"
                whole ← BS.readFile (authPath failingSlot)
                BS.writeFile (authPath failingSlot)
                    (flipByteAt (BS.length whole - 1) whole)
                (logger, readSeen) ← capturingLogger
                listings ← listWith (failReadOf (prevPath failingSlot)) logger
                namesOf listings `shouldBe` ["healthy"]
                diagnostics ← listingDiagnostics <$> readSeen
                case diagnostics of
                    [one] → do
                        T.unpack one `shouldContain` prevPath failingSlot
                        T.unpack one `shouldContain` injected
                    other → expectationFailure
                        ("expected exactly one listing diagnostic, got "
                            <> show other)

        it "skips ONLY the legacy flat file that cannot be read, naming \
           \its path and the read error once" $
            withSavesRoot $ do
                plantHealthy
                let (_, bytes) = buildEncoded 5 "relic" "t5"
                BS.writeFile legacyPath bytes
                -- Precondition: the fixture really is a listable legacy
                -- save, so the assertion below is about the READ and not
                -- about an unrelated decode failure.
                (baseLogger, _) ← capturingLogger
                base ← listWith productionListingSeams baseLogger
                namesOf base `shouldMatchList` ["healthy", "relic"]
                (logger, readSeen) ← capturingLogger
                listings ← listWith (failReadOf legacyPath) logger
                namesOf listings `shouldBe` ["healthy"]
                diagnostics ← listingDiagnostics <$> readSeen
                case diagnostics of
                    [one] → do
                        T.unpack one `shouldContain` legacyPath
                        T.unpack one `shouldContain` injected
                    other → expectationFailure
                        ("expected exactly one listing diagnostic, got "
                            <> show other)

        it "still drops a semantically INCOMPATIBLE authoritative \
           \generation WITHOUT consulting the previous one -- \
           \requirement 7's no-fallback rule is about what the bytes \
           \MEAN, and containing unreadable bytes must not turn it into \
           \a fallback" $
            withSavesRoot $ do
                plantHealthy
                _ ← publishOK failingSlot "wounded" 1 "wounded" "t1"
                _ ← publishOK failingSlot "wounded" 2 "wounded" "t2"
                whole ← BS.readFile (authPath failingSlot)
                BS.writeFile (authPath failingSlot)
                    (corruptEnvelopeVersion whole)
                -- Precondition: this fixture is INCOMPATIBLE at listing
                -- depth, not corrupt. A corrupt one would legitimately
                -- fall back, and the assertion below would then be
                -- testing nothing.
                planted ← BS.readFile (authPath failingSlot)
                case decodeSaveEnvelopeMetadataClassified HS.empty planted of
                    Left (GenerationIncompatible _ _) → pure ()
                    other → expectationFailure
                        ("fixture must classify as GenerationIncompatible, \
                         \got " <> show (() <$ other))
                reads' ← newIORef ([] ∷ [FilePath])
                (logger, readSeen) ← capturingLogger
                listings ← listWith (recordingSeams reads') logger
                namesOf listings `shouldBe` ["healthy"]
                consulted ← readIORef reads'
                consulted `shouldContain` [authPath failingSlot]
                consulted `shouldNotContain` [prevPath failingSlot]
                diagnostics ← listingDiagnostics <$> readSeen
                length diagnostics `shouldBe` 1

        -- Requirement 4: enumerating saves/ is the ONE failure that
        -- cannot be blamed on a slot. Reporting it as an empty listing
        -- would tell every consumer "there are no saves", which is the
        -- answer that lets an autosave cycle rotate over slots it never
        -- managed to look at.
        it "reports a FAILED ENUMERATION of saves/ as a refusal naming \
           \the directory and the error, never as an empty listing, and \
           \logs nothing itself -- the reason is what its two public \
           \consumers each report once" $
            withSavesRoot $ do
                plantHealthy
                (logger, readSeen) ← capturingLogger
                let blindSeams = productionListingSeams
                        { lsEnumerateSaves = \_ →
                            ioError (userError "injected enumeration failure")
                        }
                result ← listSavesWithSeams blindSeams logger HS.empty
                case result of
                    Right listings → expectationFailure
                        ("expected a refusal, got " <> show (namesOf listings))
                    Left reason → do
                        T.unpack reason `shouldContain` savesDirectory
                        T.unpack reason
                            `shouldContain` "injected enumeration failure"
                readSeen `shouldReturn` []

        it "reports a saves/ that cannot even be CREATED the same way, \
           \through the production seams -- a plain file occupying the \
           \name needs no injection at all" $
            withSavesRoot $ do
                BS.writeFile savesDirectory "not a directory"
                (logger, readSeen) ← capturingLogger
                result ← listSaves logger HS.empty
                case result of
                    Right listings → expectationFailure
                        ("expected a refusal, got " <> show (namesOf listings))
                    Left reason → T.unpack reason `shouldContain` savesDirectory
                readSeen `shouldReturn` []

    -- Issue #1919: 'failedAtPhase' used to be recovered by
    -- substring-matching the rendered failure text. It is now derived
    -- from the structured 'LoadProgress' every failing decode carries, so
    -- these two blocks pin BOTH halves: the mapping itself, exhaustively
    -- and purely, and the transport, through the real selection path.
    describe "load-phase derivation (issue #1919)" $ do
        it "maps every ComponentPhase to the checkpoint the substring \
           \parser reported for it" $
            [ (ph, loadPhaseFor (ReachedComponents [ph]))
            | ph ← [DecodePhase, MigratePhase, ValidatePhase, AssemblePhase] ]
                `shouldBe`
            [ (DecodePhase,   LoadEnvelopeValidated)
            , (MigratePhase,  LoadComponentsDecoded)
              -- No LoadPhase constructor sits between "every component
              -- migrated" and "the whole session assembled", so a
              -- per-component validate failure and a cross-component
              -- assemble failure both bottom out here.
            , (ValidatePhase, LoadComponentsMigrated)
            , (AssemblePhase, LoadComponentsMigrated)
            ]

        it "maps the two non-component progresses to their own \
           \checkpoints" $ do
            loadPhaseFor ReachedNothing  `shouldBe` LoadPaused
            loadPhaseFor ReachedEnvelope `shouldBe` LoadEnvelopeValidated

        it "resolves a MIXED-phase failure list to the FURTHEST point \
           \every component reached, in the parser's own precedence -- \
           \order within the list never matters" $ do
            loadPhaseFor (ReachedComponents [DecodePhase, AssemblePhase])
                `shouldBe` LoadComponentsMigrated
            loadPhaseFor (ReachedComponents [AssemblePhase, DecodePhase])
                `shouldBe` LoadComponentsMigrated
            loadPhaseFor (ReachedComponents [DecodePhase, MigratePhase])
                `shouldBe` LoadComponentsDecoded
            loadPhaseFor (ReachedComponents [MigratePhase, ValidatePhase])
                `shouldBe` LoadComponentsMigrated
            loadPhaseFor (ReachedComponents [DecodePhase, DecodePhase])
                `shouldBe` LoadEnvelopeValidated

        it "treats a component failure list with no phases at all as \
           \'the envelope was coherent' -- structurally unreachable, but \
           \it must not silently regress to LoadPaused" $
            loadPhaseFor (ReachedComponents []) `shouldBe` LoadEnvelopeValidated

        it "cannot be influenced by the failure MESSAGE: a diagnostic \
           \stuffed with every phase word AND the old \
           \'incompatible with this build' marker still reports the \
           \progress its structure actually carries" $ do
            let misleading = LoadFailure ReachedNothing
                    "save 'x' is incompatible with this build: \
                    \[core-session v3 AssemblePhase] ... \
                    \[units v1 MigratePhase] ... [world-pages v2 DecodePhase]"
            loadPhaseFor (lfProgress misleading) `shouldBe` LoadPaused
            -- And the converse: real component progress reports its own
            -- phase even when the message mentions nothing at all.
            loadPhaseFor (lfProgress
                (LoadFailure (ReachedComponents [MigratePhase]) "something broke"))
                `shouldBe` LoadComponentsDecoded

    describe "load-phase transport through selectLoadGeneration (#1919)" $ do
        it "a coherent but envelope-INCOMPATIBLE authoritative generation \
           \reports ReachedEnvelope -- no component was ever reached" $
            withTempSlotDir $ \dir → do
                _ ← publishOK dir "slot" 1 "slot" "t1"
                whole ← BS.readFile (authPath dir)
                BS.writeFile (authPath dir) (corruptEnvelopeVersion whole)
                sel ← selectLoadGeneration HS.empty HS.empty dir "slot"
                case sel of
                    Right s → expectationFailure
                        ("expected a failure, got " <> show (lsSource s))
                    Left f  → do
                        lfProgress f `shouldBe` ReachedEnvelope
                        loadPhaseFor (lfProgress f)
                            `shouldBe` LoadEnvelopeValidated

        it "a component declared at an unsupported schema version reports \
           \ReachedComponents [DecodePhase]" $
            withTempSlotDir $ \dir → do
                _ ← publishOK dir "slot" 1 "slot" "t1"
                whole ← BS.readFile (authPath dir)
                BS.writeFile (authPath dir)
                    (reversionComponent coreSessionComponentId 999 whole)
                sel ← selectLoadGeneration HS.empty HS.empty dir "slot"
                case sel of
                    Right s → expectationFailure
                        ("expected a failure, got " <> show (lsSource s))
                    Left f  → do
                        lfProgress f `shouldBe` ReachedComponents [DecodePhase]
                        loadPhaseFor (lfProgress f)
                            `shouldBe` LoadEnvelopeValidated

        it "a per-component VALIDATE failure reports \
           \ReachedComponents [ValidatePhase]" $
            withTempSlotDir $ \dir → do
                _ ← publishOK dir "slot" 1 "slot" "t1"
                BS.writeFile (authPath dir) emptyPagesBytes
                sel ← selectLoadGeneration HS.empty HS.empty dir "slot"
                case sel of
                    Right s → expectationFailure
                        ("expected a failure, got " <> show (lsSource s))
                    Left f  → do
                        lfProgress f `shouldBe` ReachedComponents [ValidatePhase]
                        loadPhaseFor (lfProgress f)
                            `shouldBe` LoadComponentsMigrated

        it "a CROSS-COMPONENT assembly failure -- every component decoded \
           \and self-validated, only the whole-session invariants \
           \rejected it -- reports ReachedComponents [AssemblePhase]" $
            withTempSlotDir $ \dir → do
                _ ← publishOK dir "slot" 1 "slot" "t1"
                BS.writeFile (authPath dir) activePageMissingBytes
                sel ← selectLoadGeneration HS.empty HS.empty dir "slot"
                case sel of
                    Right s → expectationFailure
                        ("expected a failure, got " <> show (lsSource s))
                    Left f  → do
                        lfProgress f `shouldBe` ReachedComponents [AssemblePhase]
                        loadPhaseFor (lfProgress f)
                            `shouldBe` LoadComponentsMigrated

        it "a storage-corrupt authoritative generation with NO previous \
           \generation reports ReachedNothing" $
            withTempSlotDir $ \dir → do
                _ ← publishOK dir "slot" 1 "slot" "t1"
                whole ← BS.readFile (authPath dir)
                BS.writeFile (authPath dir)
                    (flipByteAt (BS.length whole - 1) whole)
                sel ← selectLoadGeneration HS.empty HS.empty dir "slot"
                case sel of
                    Right s → expectationFailure
                        ("expected a failure, got " <> show (lsSource s))
                    Left f  → do
                        lfProgress f `shouldBe` ReachedNothing
                        loadPhaseFor (lfProgress f) `shouldBe` LoadPaused

        it "a symlinked slot directory reports ReachedNothing -- nothing \
           \was ever read" $
            withTempSlotDir $ \dir → do
                let target = dir <> "-target"
                _ ← publishOK target "slot" 1 "slot" "t1"
                createFileLink target dir
                sel ← selectLoadGeneration HS.empty HS.empty dir "slot"
                case sel of
                    Right s → expectationFailure
                        ("expected a failure, got " <> show (lsSource s))
                    Left f  → lfProgress f `shouldBe` ReachedNothing

        it "a CORRUPT authoritative over an INCOMPATIBLE previous keeps \
           \the PREVIOUS candidate's structured progress -- the composite \
           \message names both, but the progress reported is the one the \
           \selection actually got furthest with" $
            withTempSlotDir $ \dir → do
                _ ← publishOK dir "slot" 1 "slot" "t1"
                _ ← publishOK dir "slot" 2 "slot" "t2"
                whole ← BS.readFile (authPath dir)
                BS.writeFile (authPath dir)
                    (flipByteAt (BS.length whole - 1) whole)
                prev ← BS.readFile (prevPath dir)
                BS.writeFile (prevPath dir) (corruptEnvelopeVersion prev)
                sel ← selectLoadGeneration HS.empty HS.empty dir "slot"
                case sel of
                    Right s → expectationFailure
                        ("expected a failure, got " <> show (lsSource s))
                    Left f  → do
                        -- The message lacks the authoritative
                        -- compatibility phrase the old parser keyed on,
                        -- which is exactly why it used to fall through to
                        -- LoadPaused.
                        T.unpack (lfMessage f) `shouldContain`
                            "previous generation is also unusable"
                        lfProgress f `shouldBe` ReachedEnvelope
                        loadPhaseFor (lfProgress f)
                            `shouldBe` LoadEnvelopeValidated

        it "a CORRUPT authoritative over a COMPONENT-incompatible previous \
           \keeps that previous candidate's component phases" $
            withTempSlotDir $ \dir → do
                _ ← publishOK dir "slot" 1 "slot" "t1"
                _ ← publishOK dir "slot" 2 "slot" "t2"
                whole ← BS.readFile (authPath dir)
                BS.writeFile (authPath dir)
                    (flipByteAt (BS.length whole - 1) whole)
                prev ← BS.readFile (prevPath dir)
                BS.writeFile (prevPath dir)
                    (reversionComponent coreSessionComponentId 999 prev)
                sel ← selectLoadGeneration HS.empty HS.empty dir "slot"
                case sel of
                    Right s → expectationFailure
                        ("expected a failure, got " <> show (lsSource s))
                    Left f  → lfProgress f
                        `shouldBe` ReachedComponents [DecodePhase]

        it "loadWorld propagates the derived phase all the way to its \
           \caller, so engine.getLoadStatus().failedAtPhase reports it" $
            withSavesRoot $ do
                logger ← testLogger
                let slot = savesDirectory </> "phased"
                _ ← publishOK slot "phased" 1 "phased" "t1"
                BS.writeFile (authPath slot) emptyPagesBytes
                result ← loadWorld logger "phased" HS.empty HS.empty
                case result of
                    Right _ → expectationFailure "expected a load failure"
                    Left (phase, _) → phase `shouldBe` LoadComponentsMigrated
