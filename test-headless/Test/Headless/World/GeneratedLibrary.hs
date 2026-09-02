{-# LANGUAGE ScopedTypeVariables #-}
-- | The shared generated-world library gate (issue #2024, world-map
--   epic #2017, WML-4): "World.GeneratedLibrary"'s atomic publication,
--   registry reconciliation, failure-bearing reference census and
--   conservative cleanup. No engine — every save fixture is a synthetic
--   'SessionSnapshot' published through the REAL save transaction
--   ('World.Save.Storage.publishGeneration') into a scratch @saves/@,
--   and the library runs against a scratch root beside it, so nothing
--   here touches the developer's own @saves/@ or @generated-worlds/@.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "generated world library"'@.
module Test.Headless.World.GeneratedLibrary (spec) where

import UPrelude
import Test.Hspec
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, wait, poll, concurrently)
import Control.Exception (Exception, SomeException, finally, throwIO, try)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.IORef as IORef
import qualified Data.List as L
import qualified Data.Serialize as S
import qualified Data.Set as Set
import qualified Data.Text as T
import System.Directory
    ( getTemporaryDirectory, createDirectoryIfMissing, createDirectory
    , createDirectoryLink, copyFile, doesDirectoryExist, doesFileExist
    , doesPathExist, listDirectory, removeDirectoryRecursive, removeFile
    , removePathForcibly )
import System.FilePath ((</>))
import System.IO (hClose, hGetLine)
import System.Process
    ( createProcess, proc, StdStream(..), CreateProcess(..), waitForProcess
    , readProcess )

import World.GeneratedLibrary
import World.GeneratedLibrary.Layout
import World.GeneratedLibrary.Types (RegistryFile(..), emptyReconcileReport)
import World.Page.GeneratedId (GeneratedWorldId, renderGeneratedWorldId)
import World.Save.Storage
    ( publishGeneration, renderPublishFailure, authoritativeFileName
    , previousGenerationFileName )
import World.Save.Envelope
    (encodeSessionSnapshot, metadataComponentId, currentEnvelopeVersion)
import World.Save.Envelope.Codec
    (encodeEnvelope, decodeEnvelope, DecodedEnvelope(..))
import World.Save.Envelope.Types
    (defaultEnvelopeLimits, EnvelopeManifest(..), ComponentDescriptor(..))
import World.Save.Component (componentKnownIds, componentRequiredIds)
import World.Save.Compat.MetadataV2 (SaveMetadataV2(..))
import World.Save.Snapshot
import World.Save.Snapshot.Adapter (SaveRequestMeta(..), snapshotSaveMetadata)
import World.Save.Types (BuildingSnapshot(..), UnitSnapshot(..))
import World.Generate.Types (defaultWorldGenParams)
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
import Unit.Transfer.Orders (emptyTransferOrders)
import Power.Types (emptyPowerNodes)
import Building.Knowledge (emptyContainerKnowledge)
import World.Construct.Attempt (firstConstructAttemptId)
import World.Flora.Identity (firstPlantedFloraCursor)
import Test.Headless.Harness.GeneratedIds (fixtureGeneratedWorldId)

-- ---------------------------------------------------------------------
-- Scratch root
-- ---------------------------------------------------------------------

-- | A scratch RESOURCE ROOT holding @saves/@ and the library beside it,
--   wiped before and after each use. Nested two levels below the
--   system temp directory for the same reason
--   'Test.Headless.World.Save.Storage' nests: macOS's @\/tmp@ is itself
--   a symlink, and every containment check here inspects a managed
--   directory's immediate parent.
withScratch ∷ (FilePath → IO a) → IO a
withScratch action = do
    tmp ← getTemporaryDirectory
    let root = tmp </> "synarchy-generated-library-spec-root"
    removePathForcibly root
    createDirectoryIfMissing True (root </> "saves")
    action root `finally` removePathForcibly root

configFor ∷ FilePath → LibraryConfig
configFor root = LibraryConfig
    { lcRoot           = root </> libraryDirectory
    , lcSavesDirectory = root </> "saves"
    , lcLockWaitMicros = 2_000_000
    }

libraryRoot ∷ FilePath → FilePath
libraryRoot root = root </> libraryDirectory

openOK ∷ FilePath → IO Library
openOK root = openLibrary (configFor root) ≫= orFail "openLibrary"

orFail ∷ String → Either LibraryFailure a → IO a
orFail what = either (\f → do
    expectationFailure (what <> ": " <> T.unpack (renderLibraryFailure f))
    error "unreachable") pure

publishOK ∷ Library → GeneratedWorldId → [PayloadFile] → IO PublishReport
publishOK lib gid files = publishEntry lib gid files ≫= orFail "publishEntry"

cleanupOK ∷ Library → IO CleanupReport
cleanupOK lib = cleanupLibrary lib HS.empty ≫= orFail "cleanupLibrary"

reconcileOK ∷ Library → IO ([LibraryEntry], ReconcileReport)
reconcileOK lib = reconcileLibrary lib ≫= orFail "reconcileLibrary"

-- | Run @action@ with @gids@ pinned, failing the test if the pin itself
--   could not be taken.
pinned ∷ Library → [GeneratedWorldId] → IO a → IO a
pinned lib gids action = withPinnedReferences lib gids action ≫= orFail "withPinnedReferences"

failedIn ∷ LibraryPhase → Either LibraryFailure a → Bool
failedIn phase = either ((≡ phase) . glfPhase) (const False)

-- | Like 'failedIn', for a result whose success value has no 'Show'.
expectPhase ∷ LibraryPhase → Either LibraryFailure a → Expectation
expectPhase phase = either ((`shouldBe` phase) . glfPhase)
                           (const (expectationFailure ("expected a " <> show phase <> " failure")))

-- | The names in the library root, ascending.
rootNames ∷ FilePath → IO [FilePath]
rootNames root = L.sort ⊚ listDirectory (libraryRoot root)

finalDir ∷ FilePath → GeneratedWorldId → FilePath
finalDir root gid = libraryRoot root </> entryDirectoryName gid

registryPath ∷ FilePath → FilePath
registryPath root = libraryRoot root </> registryFileName

copyEntryDirectory ∷ FilePath → FilePath → IO ()
copyEntryDirectory source target = do
    createDirectory target
    names ← listDirectory source
    forM_ names $ \name → copyFile (source </> name) (target </> name)

tokenOf ∷ GeneratedWorldId → Text
tokenOf = renderGeneratedWorldId

-- | Flip one byte (XOR 0xFF) without changing the length.
flipByteAt ∷ Int → BS.ByteString → BS.ByteString
flipByteAt idx bs =
    BS.take idx bs
        <> BS.singleton (BS.index bs idx `xor` 0xFF)
        <> BS.drop (idx + 1) bs

-- | A crash, simulated: thrown from a publish hook, never caught by the
--   transaction, so the disk holds exactly what a crash would leave.
data Interrupt = Interrupt deriving Show
instance Exception Interrupt

interrupting ∷ IO (Either LibraryFailure PublishReport) → IO ()
interrupting action = do
    r ← try action
    case r of
        Left Interrupt → pure ()
        Right outcome  → expectationFailure
            ("publication was not interrupted: " <> show outcome)

-- ---------------------------------------------------------------------
-- Library fixtures
-- ---------------------------------------------------------------------

gidA, gidB, gidC ∷ GeneratedWorldId
gidA = fixtureGeneratedWorldId "library:a"
gidB = fixtureGeneratedWorldId "library:b"
gidC = fixtureGeneratedWorldId "library:c"

payload1, payload2 ∷ [PayloadFile]
payload1 = [ PayloadFile "map.manifest" (BC.pack "manifest one")
           , PayloadFile "root.page"    (BS.pack [1 .. 64]) ]
payload2 = [ PayloadFile "map.manifest" (BC.pack "manifest two")
           , PayloadFile "root.page"    (BS.pack [1 .. 64])
           , PayloadFile "coarse.page"  (BS.replicate 300 7) ]

digestOf ∷ [PayloadFile] → BS.ByteString
digestOf files = case validatePayload files of
    Right descriptors → inventoryDigest descriptors
    Left err          → error ("digestOf: " <> T.unpack err)

-- | The committed entry for @gid@, or a failed expectation.
committed ∷ Library → GeneratedWorldId → IO LibraryEntry
committed lib gid = do
    found ← lookupEntry lib gid ≫= orFail "lookupEntry"
    case found of
        Just e | leStatus e ≡ EntryCommitted → pure e
        other → do
            expectationFailure ("expected a committed entry, found " <> show other)
            error "unreachable"

-- ---------------------------------------------------------------------
-- Save fixtures (mirror Test.Headless.World.Save.Storage's minimal*)
-- ---------------------------------------------------------------------

minimalPage ∷ WorldPageId → GeneratedWorldId → PageSnapshot
minimalPage pid gid = PageSnapshot
    { pgsPageId       = pid
    , pgsGenParams    = defaultWorldGenParams
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
    , pgsCropPlots    = emptyCropPlots
    , pgsPlantDesignations = HM.empty
    , pgsContainerKnowledge = emptyContainerKnowledge
    , pgsIdentity     = Nothing
    , pgsGeneratedId  = Just gid
    }

-- | A valid session whose pages each descend from one of @gids@.
sessionWith ∷ [GeneratedWorldId] → SessionSnapshot
sessionWith gids =
    let pids  = [ WorldPageId ("page" <> tshow i) | i ← [1 .. length gids] ]
        pages = zipWith minimalPage pids gids
        active = fromMaybe (WorldPageId "page1") (listToMaybe pids)
        globals = SessionGlobals
            { sgGameTime       = 0
            , sgTexPalette     = emptyTexPalette
            , sgNextItemId     = 1
            , sgNextBuildingId = 1
            , sgNextUnitId     = 1
            , sgActivePage     = active
            , sgVisiblePages   = pids
            , sgLiveCamera     = LiveCameraSnapshot
                { lcsOwnerPage = Just active
                , lcsX = 10, lcsY = 20, lcsZoom = 2, lcsFacing = FaceSouth }
            }
    in case captureSessionSnapshot globals pages of
        Right s   → s
        Left errs → error ("sessionWith: invalid fixture: " <> show errs)

-- | Encoded, current-format save bytes referencing @gids@.
saveBytes ∷ Text → Text → [GeneratedWorldId] → BS.ByteString
saveBytes slot ts gids =
    let snap = sessionWith gids
        meta = snapshotSaveMetadata (SaveRequestMeta slot ts False) snap
    in encodeSessionSnapshot meta snap []

-- | Publish a generation into @saves/\<slot\>@ through the real save
--   transaction, so every slot fixture has the exact on-disk shape a
--   player's save has — including a rotated @world.synworld.prev@ on a
--   second publish.
saveSlot ∷ FilePath → Text → Text → [GeneratedWorldId] → IO ()
saveSlot root slot ts gids = do
    let snap = sessionWith gids
        meta = snapshotSaveMetadata (SaveRequestMeta slot ts False) snap
        bytes = encodeSessionSnapshot meta snap []
        dir = root </> "saves" </> T.unpack slot
    r ← publishGeneration dir slot meta bytes HS.empty HS.empty
    case r of
        Right _ → pure ()
        Left f  → expectationFailure ("save fixture: " <> T.unpack (renderPublishFailure f))

slotDir ∷ FilePath → Text → FilePath
slotDir root slot = root </> "saves" </> T.unpack slot

-- | A generation whose @"metadata"@ predates generated-world identity
--   (component v2): positively readable, naming no id at all. Built the
--   way 'Test.Headless.World.Save.Storage' re-versions a component —
--   a real current envelope with ONE component's payload and version
--   swapped, every other component and every checksum intact.
preIdentityBytes ∷ BS.ByteString
preIdentityBytes =
    let bytes = saveBytes "ancient" "t0" [gidC]
        meta = SaveMetadataV2
            { sm2Name = "ancient", sm2Seed = 7, sm2WorldSize = 32
            , sm2PlateCount = 3, sm2Timestamp = "2026-01-01T00:00:00Z"
            , sm2WorldName = Nothing, sm2WorldGloss = Nothing
            , sm2Autosave = False }
        known    = HS.insert metadataComponentId componentKnownIds
        required = HS.insert metadataComponentId componentRequiredIds
    in case decodeEnvelope defaultEnvelopeLimits currentEnvelopeVersion known required bytes of
        Left err → error ("preIdentityBytes: decode: " <> show err)
        Right de →
            let isMeta d = cdId d ≡ metadataComponentId
                specs = [ ( cdId d
                          , if isMeta d then 2 else cdVersion d
                          , cdRequired d
                          , if isMeta d then S.encode meta
                            else HM.lookupDefault BS.empty (cdId d) (dePayloads de) )
                        | d ← emComponents (deManifest de) ]
            in case encodeEnvelope defaultEnvelopeLimits currentEnvelopeVersion specs of
                Right b → b
                Left e  → error ("preIdentityBytes: encode: " <> show e)

-- ---------------------------------------------------------------------
-- The gate
-- ---------------------------------------------------------------------

spec ∷ Spec
spec = do
    layoutSpec
    recordSpec
    publishSpec
    referenceSpec
    registrySpec
    containmentSpec
    coordinationSpec

-- Layout: identity text never becomes a path ------------------------------

layoutSpec ∷ Spec
layoutSpec = describe "layout" $ do
    it "maps an id to exactly its 32-character rendering and back through classification" $ do
        let name = entryDirectoryName gidA
        length name `shouldBe` 32
        classifyLibraryName name `shouldBe` FinalEntryName (tokenOf gidA)
        classifyLibraryName (transientDirectoryName StagingDir gidA 7 9)
            `shouldBe` TransientName StagingDir (tokenOf gidA)
        classifyLibraryName (transientDirectoryName DisplacedDir gidB 1 2)
            `shouldBe` TransientName DisplacedDir (tokenOf gidB)
        classifyLibraryName (transientDirectoryName TombstoneDir gidC 0 0)
            `shouldBe` TransientName TombstoneDir (tokenOf gidC)
        classifyLibraryName registryFileName `shouldBe` RegistryName
        classifyLibraryName (registryTempTemplate <> "12345")
            `shouldBe` RegistryTempName
        classifyLibraryName (registryTempTemplate <> "12345-7")
            `shouldBe` RegistryTempName
        classifyLibraryName (pinFileName gidA 4 2) `shouldBe` PinName (tokenOf gidA)
        classifyLibraryName lockFileName `shouldBe` LockName

    it "classifies every malformed, non-canonical, traversal-shaped or separator-bearing name as unfamiliar" $ do
        let tok = entryDirectoryName gidA
            bad = [ "", ".", "..", "../" <> tok, tok <> "/x", "x/" <> tok
                  , map toUpper' tok, take 31 tok, tok <> "0"
                  , tok <> ".staging-", tok <> ".staging-x1", tok <> ".staging"
                  , tok <> ".staging-1-", tok <> ".staging-1-2-3", tok <> ".pin-"
                  , tok <> ".unknown-1", tok <> "..staging-1", "notes", "README"
                  , registryTempTemplate, registryTempTemplate <> "1-notes"
                  , registryTempTemplate <> "x1", registryTempTemplate <> "1-"
                  , registryTempTemplate <> "1-2-3" ]
            toUpper' c = if c ≥ 'a' ∧ c ≤ 'f' then toEnum (fromEnum c - 32) else c
        forM_ bad $ \name →
            (name, classifyLibraryName name) `shouldBe` (name, UnfamiliarName)

    it "refuses every payload name that is not one safe, unreserved path component" $ do
        let bad = [ "", "a/b", "a\\b", ".", "..", ".hidden", "a\tb"
                  , T.replicate 129 "x", T.pack entryRecordFileName
                  , T.pack registryFileName, T.pack lockFileName ]
        forM_ bad $ \name → validatePayloadName name `shouldSatisfy` isLeft'
        validatePayloadName "map.manifest" `shouldBe` Right ()
        validatePayload [] `shouldSatisfy` isLeft'
        validatePayload [PayloadFile "Map" "1", PayloadFile "map" "2"] `shouldSatisfy` isLeft'
        fmap (map pdName) (validatePayload (reverse payload2))
            `shouldBe` Right ["coarse.page", "map.manifest", "root.page"]

    it "digests an inventory independently of descriptor order" $ do
        digestOf payload2 `shouldBe` digestOf (reverse payload2)
        digestOf payload1 `shouldNotBe` digestOf payload2
  where
    isLeft' = either (const True) (const False)

-- Records: framing catches truncation, bit flips and foreign bytes -----------

recordSpec ∷ Spec
recordSpec = describe "record files" $ do
    let descriptors = either (error . T.unpack) id (validatePayload payload1)
        rec = EntryRecord gidA descriptors "2026-09-01T00:00:00Z"
        reg = RegistryFile [ RegistryRow gidA (digestOf payload1) 76 2 "t"
                           , RegistryRow gidB (digestOf payload2) 376 3 "t" ]

    it "round-trips an entry record and a registry" $ do
        decodeEntryRecord (encodeEntryRecord rec) `shouldBe` Right rec
        decodeRegistry (encodeRegistry reg) `shouldBe` Right reg

    it "rejects a truncated, bit-flipped or foreign record" $ do
        let bytes = encodeEntryRecord rec
        decodeEntryRecord (BS.take (BS.length bytes - 1) bytes) `shouldSatisfy` isLeft'
        decodeEntryRecord (flipByteAt 20 bytes) `shouldSatisfy` isLeft'
        decodeEntryRecord (BC.pack "not a record at all, but long enough to have a trailer")
            `shouldSatisfy` isLeft'
        decodeEntryRecord (encodeRegistry reg) `shouldSatisfy` isLeft'
        let rbytes = encodeRegistry reg
        decodeRegistry (BS.take 12 rbytes) `shouldSatisfy` isLeft'
        decodeRegistry (flipByteAt (BS.length rbytes - 1) rbytes) `shouldSatisfy` isLeft'
        decodeRegistry (encodeEntryRecord rec) `shouldSatisfy` isLeft'

    it "rejects a registry whose rows are out of order or duplicated" $ do
        decodeRegistry (encodeRegistry (RegistryFile (reverse (rfRows reg))))
            `shouldSatisfy` isLeft'
        decodeRegistry (encodeRegistry (RegistryFile (rfRows reg ⧺ take 1 (rfRows reg))))
            `shouldSatisfy` isLeft'
  where
    isLeft' = either (const True) (const False)

-- Publication ------------------------------------------------------------------

publishSpec ∷ Spec
publishSpec = describe "publication" $ do
    it "publishes a new entry whose record binds the id to the inventory, and indexes it" $
        withScratch $ \root → do
            lib ← openOK root
            report ← publishOK lib gidA payload1
            prOutcome report `shouldBe` PublishedNew
            prWarnings report `shouldBe` []
            e ← committed lib gidA
            fmap erId (leRecord e) `shouldBe` Just gidA
            fmap (map pdName . erFiles) (leRecord e) `shouldBe` Just ["map.manifest", "root.page"]
            leDigest e `shouldBe` Just (digestOf payload1)
            forM_ payload1 $ \f →
                BS.readFile (finalDir root gidA </> T.unpack (pfName f)) `shouldReturn` pfBytes f
            inv ← listEntries lib ≫= orFail "listEntries"
            liSource inv `shouldBe` FromRegistryFile
            map rrId (liRows inv) `shouldBe` [gidA]
            map rrInventoryDigest (liRows inv) `shouldBe` [digestOf payload1]
            names ← rootNames root
            names `shouldBe` L.sort [entryDirectoryName gidA, registryFileName, lockFileName]

    it "refuses an unacceptable payload before writing anything" $
        withScratch $ \root → do
            lib ← openOK root
            publishEntry lib gidA [] `shouldReturn'` LibPayloadIdentity
            publishEntry lib gidA [PayloadFile "../escape" "x"] `shouldReturn'` LibPayloadIdentity
            publishEntry lib gidA [PayloadFile (T.pack entryRecordFileName) "x"]
                `shouldReturn'` LibPayloadIdentity
            names ← rootNames root
            names `shouldBe` [lockFileName]

    it "republishing identical content is idempotent; different content replaces and leaves no displaced copy" $
        withScratch $ \root → do
            lib ← openOK root
            _ ← publishOK lib gidA payload1
            again ← publishOK lib gidA payload1
            prOutcome again `shouldBe` PublishedUnchanged
            replaced ← publishOK lib gidA payload2
            prOutcome replaced `shouldBe` PublishedReplaced
            prWarnings replaced `shouldBe` []
            e ← committed lib gidA
            leDigest e `shouldBe` Just (digestOf payload2)
            doesFileExist (finalDir root gidA </> "coarse.page") `shouldReturn` True
            names ← rootNames root
            names `shouldBe` L.sort [entryDirectoryName gidA, registryFileName, lockFileName]
            inv ← listEntries lib ≫= orFail "listEntries"
            map rrInventoryDigest (liRows inv) `shouldBe` [digestOf payload2]

    it "interrupted before the commit rename: no final entry, and a staging directory cleanup identifies and sweeps" $
        withScratch $ \root → do
            lib ← openOK root
            let hooks = noPublishHooks { phAfterStaged = \_ → throwIO Interrupt }
            interrupting (publishEntryWith hooks lib gidA payload1)
            lookupEntry lib gidA `shouldReturn` Right Nothing
            names ← rootNames root
            staging ← case [ n | n ← names
                               , classifyLibraryName n ≡ TransientName StagingDir (tokenOf gidA) ] of
                [one] → pure (libraryRoot root </> one)
                other → do
                    expectationFailure ("expected exactly one staging directory, found " <> show other)
                    error "unreachable"
            -- The staging directory is complete on disk — it simply is
            -- not an entry, because it does not sit at the final name.
            doesFileExist (staging </> entryRecordFileName) `shouldReturn` True
            inv ← listEntries lib ≫= orFail "listEntries"
            liRows inv `shouldBe` []
            report ← cleanupOK lib
            crTransientsRemoved report `shouldBe` [staging]
            after ← rootNames root
            after `shouldBe` L.sort [registryFileName, lockFileName]

    it "a candidate whose staged bytes do not match what was written is rejected, with the registry left valid" $
        withScratch $ \root → do
            lib ← openOK root
            _ ← publishOK lib gidB payload1
            let corrupt staging = BS.writeFile (staging </> "root.page") (BS.pack [9, 9, 9])
                hooks = noPublishHooks { phAfterPayloadWritten = corrupt }
            publishEntryWith hooks lib gidA payload1 `shouldReturn'` LibPayloadValidate
            lookupEntry lib gidA `shouldReturn` Right Nothing
            names ← rootNames root
            names `shouldBe` L.sort [entryDirectoryName gidB, registryFileName, lockFileName]
            inv ← listEntries lib ≫= orFail "listEntries"
            liSource inv `shouldBe` FromRegistryFile
            map rrId (liRows inv) `shouldBe` [gidB]
            -- The library is still usable: the next publication succeeds.
            report ← publishOK lib gidA payload1
            prOutcome report `shouldBe` PublishedNew
            (_, rc) ← reconcileOK lib
            rc `shouldBe` emptyReconcileReport

    it "interrupted republish after displacement: the old complete entry is restored by reconciliation" $
        withScratch $ \root → do
            lib ← openOK root
            _ ← publishOK lib gidA payload1
            let hooks = noPublishHooks { phAfterDisplaced = \_ → throwIO Interrupt }
            interrupting (publishEntryWith hooks lib gidA payload2)
            lookupEntry lib gidA `shouldReturn` Right Nothing
            names ← rootNames root
            [ n | n ← names, classifyLibraryName n ≡ TransientName DisplacedDir (tokenOf gidA) ]
                `shouldSatisfy` ((≡ 1) . length)
            (entries, rc) ← reconcileOK lib
            rcRecovered rc `shouldBe` [gidA]
            map leStatus entries `shouldBe` [EntryCommitted]
            e ← committed lib gidA
            leDigest e `shouldBe` Just (digestOf payload1)
            -- Reconciliation restored; cleanup sweeps the abandoned
            -- staging copy of the interrupted republish.
            report ← pinned lib [gidA] (cleanupOK lib)
            length (crTransientsRemoved report) `shouldBe` 1
            after ← rootNames root
            after `shouldBe` L.sort [entryDirectoryName gidA, registryFileName, lockFileName]

    it "interrupted republish before displacement: the old entry is untouched" $
        withScratch $ \root → do
            lib ← openOK root
            _ ← publishOK lib gidA payload1
            let hooks = noPublishHooks { phAfterStaged = \_ → throwIO Interrupt }
            interrupting (publishEntryWith hooks lib gidA payload2)
            e ← committed lib gidA
            leDigest e `shouldBe` Just (digestOf payload1)
  where
    shouldReturn' action phase = do
        r ← action
        unless (failedIn phase r) $
            expectationFailure ("expected a " <> show phase <> " failure, got " <> show r)

-- References and cleanup ----------------------------------------------------------

referenceSpec ∷ Spec
referenceSpec = describe "references and cleanup" $ do
    it "two descendant saves resolve to one entry, and deleting one leaves it referenced" $
        withScratch $ \root → do
            lib ← openOK root
            _ ← publishOK lib gidA payload1
            saveSlot root "first" "t1" [gidA]
            saveSlot root "second" "t2" [gidA]
            names ← rootNames root
            length [ n | n ← names, classifyLibraryName n ≡ FinalEntryName (tokenOf gidA) ]
                `shouldBe` 1
            r1 ← cleanupOK lib
            crRetainedReferenced r1 `shouldBe` [gidA]
            crRemoved r1 `shouldBe` []
            rsSourcesRead (crReferences r1) `shouldBe` 2
            removeDirectoryRecursive (slotDir root "first")
            r2 ← cleanupOK lib
            crRetainedReferenced r2 `shouldBe` [gidA]
            doesDirectoryExist (finalDir root gidA) `shouldReturn` True
            removeDirectoryRecursive (slotDir root "second")
            r3 ← cleanupOK lib
            crRemoved r3 `shouldBe` [gidA]
            doesDirectoryExist (finalDir root gidA) `shouldReturn` False

    it "removes an entry no save references and retains one at least one save references" $
        withScratch $ \root → do
            lib ← openOK root
            _ ← publishOK lib gidA payload1
            _ ← publishOK lib gidB payload2
            saveSlot root "keep" "t1" [gidA]
            report ← cleanupOK lib
            crRemoved report `shouldBe` [gidB]
            crRetainedReferenced report `shouldBe` [gidA]
            crDeletionSuppressed report `shouldBe` False
            crWarnings report `shouldBe` []
            doesDirectoryExist (finalDir root gidA) `shouldReturn` True
            doesDirectoryExist (finalDir root gidB) `shouldReturn` False
            names ← rootNames root
            names `shouldBe` L.sort [entryDirectoryName gidA, registryFileName, lockFileName]
            inv ← listEntries lib ≫= orFail "listEntries"
            map rrId (liRows inv) `shouldBe` [gidA]

    it "a multi-page save protects every id it names, not only the active page's" $
        withScratch $ \root → do
            lib ← openOK root
            forM_ [(gidA, payload1), (gidB, payload2), (gidC, payload1)] $ \(g, p) →
                publishOK lib g p
            saveSlot root "two-pages" "t1" [gidA, gidB]
            report ← cleanupOK lib
            crRemoved report `shouldBe` [gidC]
            L.sort (crRetainedReferenced report) `shouldBe` L.sort [gidA, gidB]

    it "a retained previous generation protects what it references" $
        withScratch $ \root → do
            lib ← openOK root
            _ ← publishOK lib gidA payload1
            _ ← publishOK lib gidB payload2
            saveSlot root "rotating" "t1" [gidA]
            saveSlot root "rotating" "t2" [gidB]
            doesFileExist (slotDir root "rotating" </> previousGenerationFileName)
                `shouldReturn` True
            r1 ← cleanupOK lib
            crRemoved r1 `shouldBe` []
            L.sort (crRetainedReferenced r1) `shouldBe` L.sort [gidA, gidB]
            removeFile (slotDir root "rotating" </> previousGenerationFileName)
            r2 ← cleanupOK lib
            crRemoved r2 `shouldBe` [gidA]
            crRetainedReferenced r2 `shouldBe` [gidB]

    it "a legacy flat save protects its ids, and a pre-identity generation is positively read as naming none" $
        withScratch $ \root → do
            lib ← openOK root
            _ ← publishOK lib gidA payload1
            _ ← publishOK lib gidB payload2
            BS.writeFile (root </> "saves" </> "flat.synworld") (saveBytes "flat" "t1" [gidA])
            createDirectory (slotDir root "ancient")
            BS.writeFile (slotDir root "ancient" </> authoritativeFileName) preIdentityBytes
            report ← cleanupOK lib
            rsIndeterminate (crReferences report) `shouldBe` []
            rsSourcesRead (crReferences report) `shouldBe` 2
            crRemoved report `shouldBe` [gidB]
            crRetainedReferenced report `shouldBe` [gidA]

    it "an unreadable, corrupt or incompatible save suppresses every deletion and names the slot" $
        withScratch $ \root → do
            lib ← openOK root
            _ ← publishOK lib gidA payload1
            saveSlot root "fine" "t1" [gidC]
            -- Garbage where a generation should be.
            createDirectory (slotDir root "garbage")
            BS.writeFile (slotDir root "garbage" </> authoritativeFileName) (BC.pack "not a save")
            r1 ← cleanupOK lib
            crDeletionSuppressed r1 `shouldBe` True
            crRemoved r1 `shouldBe` []
            map fst (rsIndeterminate (crReferences r1)) `shouldBe` ["garbage"]
            doesDirectoryExist (finalDir root gidA) `shouldReturn` True
            removeDirectoryRecursive (slotDir root "garbage")
            -- A coherent envelope declaring a version this build does not
            -- read: incompatible, not corrupt — and just as indeterminate.
            let bytes = saveBytes "future" "t2" [gidB]
                bumped = BS.take 4 bytes <> BS.map (`xor` 0xFF) (BS.take 4 (BS.drop 4 bytes))
                                        <> BS.drop 8 bytes
            createDirectory (slotDir root "future")
            BS.writeFile (slotDir root "future" </> authoritativeFileName) bumped
            r2 ← cleanupOK lib
            crDeletionSuppressed r2 `shouldBe` True
            map fst (rsIndeterminate (crReferences r2)) `shouldBe` ["future"]
            removeDirectoryRecursive (slotDir root "future")
            -- A corrupt PREVIOUS generation beside a fine authoritative
            -- one is indeterminate too: the loader can still fall back
            -- to it.
            BS.writeFile (slotDir root "fine" </> previousGenerationFileName) (BC.pack "torn")
            r3 ← cleanupOK lib
            crDeletionSuppressed r3 `shouldBe` True
            map fst (rsIndeterminate (crReferences r3)) `shouldBe` ["fine"]
            removeFile (slotDir root "fine" </> previousGenerationFileName)
            r4 ← cleanupOK lib
            crDeletionSuppressed r4 `shouldBe` False
            crRemoved r4 `shouldBe` [gidA]

    it "a symlinked save slot is indeterminate, never followed" $
        withScratch $ \root → do
            lib ← openOK root
            _ ← publishOK lib gidA payload1
            createDirectoryIfMissing True (root </> "elsewhere")
            createDirectoryLink (root </> "elsewhere") (slotDir root "linked")
            report ← cleanupOK lib
            crDeletionSuppressed report `shouldBe` True
            map fst (rsIndeterminate (crReferences report)) `shouldBe` ["linked"]

    it "a pinned id is retained exactly as a referenced one, and pins compose" $
        withScratch $ \root → do
            lib ← openOK root
            _ ← publishOK lib gidA payload1
            _ ← publishOK lib gidB payload2
            pinned lib [gidA] $ do
                pinned lib [gidA, gidB] $ do
                    pinnedReferences lib `shouldReturn` Set.fromList [gidA, gidB]
                    r ← cleanupOK lib
                    crRemoved r `shouldBe` []
                    L.sort (crRetainedPinned r) `shouldBe` L.sort [gidA, gidB]
                pinnedReferences lib `shouldReturn` Set.fromList [gidA]
                r ← cleanupOK lib
                crRemoved r `shouldBe` [gidB]
                crRetainedPinned r `shouldBe` [gidA]
            pinnedReferences lib `shouldReturn` Set.empty
            r ← cleanupOK lib
            crRemoved r `shouldBe` [gidA]

    it "cleanup detaches to a tombstone before deleting, and a leftover tombstone is swept next run" $
        withScratch $ \root → do
            lib ← openOK root
            _ ← publishOK lib gidA payload1
            -- A tombstone from an interrupted earlier run: detached,
            -- never deleted. Identifiable by name; swept as abandoned.
            let tomb = libraryRoot root </> transientDirectoryName TombstoneDir gidB 1 1
            createDirectory tomb
            BS.writeFile (tomb </> "leftover") "x"
            report ← cleanupOK lib
            crTransientsRemoved report `shouldBe` [tomb]
            crRemoved report `shouldBe` [gidA]
            names ← rootNames root
            names `shouldBe` L.sort [registryFileName, lockFileName]

    it "a registry candidate left by a crash is recognized and swept under the lock" $
        withScratch $ \root → do
            lib ← openOK root
            let temp = libraryRoot root </> (registryTempTemplate <> "12345")
            BS.writeFile temp "interrupted registry"
            classifyLibraryName (registryTempTemplate <> "12345")
                `shouldBe` RegistryTempName
            report ← cleanupOK lib
            crTransientsRemoved report `shouldBe` [temp]
            doesPathExist temp `shouldReturn` False
            rootNames root `shouldReturn` L.sort [registryFileName, lockFileName]

    it "unfamiliar names in the root are reported and never touched" $
        withScratch $ \root → do
            lib ← openOK root
            let tok = entryDirectoryName gidA
                strangers = [ "notes", map upper tok, tok <> ".staging-", "2024" ]
                upper c = if c ≥ 'a' ∧ c ≤ 'f' then toEnum (fromEnum c - 32) else c
                -- A file that merely STARTS like a registry candidate.
                lookalike = registryTempTemplate <> "1-notes"
            forM_ strangers $ \n → createDirectory (libraryRoot root </> n)
            BS.writeFile (libraryRoot root </> "README") "keep me"
            BS.writeFile (libraryRoot root </> lookalike) "mine"
            report ← cleanupOK lib
            L.sort (rcUnfamiliar (crReconcile report))
                `shouldBe` L.sort (map (libraryRoot root </>) ("README" : lookalike : strangers))
            crTransientsRemoved report `shouldBe` []
            forM_ strangers $ \n → doesDirectoryExist (libraryRoot root </> n) `shouldReturn` True
            BS.readFile (libraryRoot root </> "README") `shouldReturn` "keep me"
            BS.readFile (libraryRoot root </> lookalike) `shouldReturn` "mine"

-- Registry ---------------------------------------------------------------------------

registrySpec ∷ Spec
registrySpec = describe "registry" $ do
    it "adds a final the registry missed, drops a row whose final is absent, and rebuilds a torn or absent registry" $
        withScratch $ \root → do
            lib ← openOK root
            _ ← publishOK lib gidA payload1
            _ ← publishOK lib gidB payload2
            inv ← listEntries lib ≫= orFail "listEntries"
            (rowA, rowB) ← case liRows inv of
                [a, b] → pure (a, b)
                other  → do
                    expectationFailure ("expected two rows, found " <> show other)
                    error "unreachable"
            -- Registry missing B.
            BS.writeFile (registryPath root) (encodeRegistry (RegistryFile [rowA]))
            (_, rc1) ← reconcileOK lib
            rcAdded rc1 `shouldBe` [gidB]
            rcDropped rc1 `shouldBe` []
            -- Registry naming a C that does not exist, and a wrong digest for A.
            let ghost = RegistryRow gidC (digestOf payload1) 1 1 "never"
                wrongA = rowA { rrInventoryDigest = digestOf payload2 }
            BS.writeFile (registryPath root)
                (encodeRegistry (RegistryFile (L.sortOn rrId [wrongA, rowB, ghost])))
            (_, rc2) ← reconcileOK lib
            rcDropped rc2 `shouldBe` [gidC]
            rcCorrected rc2 `shouldBe` [gidA]
            -- Torn: a flipped byte, then a truncation, then absence.
            regBytes ← BS.readFile (registryPath root)
            BS.writeFile (registryPath root) (flipByteAt (BS.length regBytes - 3) regBytes)
            torn ← listEntries lib ≫= orFail "listEntries"
            liSource torn `shouldSatisfy` (≢ FromRegistryFile)
            map rrId (liRows torn) `shouldBe` L.sort [gidA, gidB]
            (_, rc3) ← reconcileOK lib
            rcRegistryRebuilt rc3 `shouldSatisfy` isJust
            BS.writeFile (registryPath root) (BS.take 5 regBytes)
            (_, rc4) ← reconcileOK lib
            rcRegistryRebuilt rc4 `shouldSatisfy` isJust
            removeFile (registryPath root)
            (_, rc5) ← reconcileOK lib
            rcRegistryRebuilt rc5 `shouldBe` Just "registry file is absent"
            -- Every repair ended with the same two entries on disk and
            -- the same two rows in the registry.
            final ← listEntries lib ≫= orFail "listEntries"
            liSource final `shouldBe` FromRegistryFile
            liRows final `shouldBe` L.sortOn rrId [rowA, rowB]
            (_, rc6) ← reconcileOK lib
            rc6 `shouldBe` emptyReconcileReport
            forM_ [gidA, gidB] $ \g → doesDirectoryExist (finalDir root g) `shouldReturn` True

    it "an unreadable final is retained and reported, never indexed and never deleted" $
        withScratch $ \root → do
            lib ← openOK root
            _ ← publishOK lib gidA payload1
            _ ← publishOK lib gidB payload2
            let recordA = finalDir root gidA </> entryRecordFileName
            bytes ← BS.readFile recordA
            BS.writeFile recordA (flipByteAt (BS.length bytes - 1) bytes)
            (entries, rc) ← reconcileOK lib
            map fst (rcUnreadable rc) `shouldBe` [finalDir root gidA]
            rcDropped rc `shouldBe` [gidA]
            [ leName e | e ← entries, leStatus e ≡ EntryCommitted ] `shouldBe` [tokenOf gidB]
            inv ← listEntries lib ≫= orFail "listEntries"
            map rrId (liRows inv) `shouldBe` [gidB]
            report ← cleanupOK lib
            crRetainedUnreadable report `shouldBe` [finalDir root gidA]
            crRemoved report `shouldBe` [gidB]
            doesDirectoryExist (finalDir root gidA) `shouldReturn` True
            -- A record that names a different id than the directory it
            -- sits in is unreadable in the same way.
            BS.writeFile recordA bytes
            _ ← publishOK lib gidC payload2
            removeDirectoryRecursive (finalDir root gidC)
            createDirectory (finalDir root gidC)
            BS.writeFile (finalDir root gidC </> entryRecordFileName) bytes
            forM_ payload1 $ \f →
                BS.writeFile (finalDir root gidC </> T.unpack (pfName f)) (pfBytes f)
            found ← lookupEntry lib gidC ≫= orFail "lookupEntry"
            fmap leStatus found `shouldSatisfy` \s → case s of
                Just (EntryUnreadable why) → "names" `T.isInfixOf` why
                _                          → False
            r2 ← cleanupOK lib
            crRetainedUnreadable r2 `shouldBe` [finalDir root gidC]
            doesDirectoryExist (finalDir root gidC) `shouldReturn` True

    it "a displaced copy beside a complete final is post-commit garbage; beside an unreadable one it is retained" $
        withScratch $ \root → do
            lib ← openOK root
            _ ← publishOK lib gidA payload1
            let displaced = libraryRoot root </> transientDirectoryName DisplacedDir gidA 3 3
            createDirectory displaced
            BS.writeFile (displaced </> "stale") "x"
            r1 ← pinned lib [gidA] (cleanupOK lib)
            crTransientsRemoved r1 `shouldBe` [displaced]
            -- Now make the final unreadable and plant a displaced copy.
            createDirectory displaced
            BS.writeFile (displaced </> "stale") "x"
            let recordA = finalDir root gidA </> entryRecordFileName
            BS.writeFile recordA "torn"
            r2 ← cleanupOK lib
            crTransientsRemoved r2 `shouldBe` []
            crRetainedUnreadable r2 `shouldBe` [finalDir root gidA]
            doesDirectoryExist displaced `shouldReturn` True
            rcWarnings (crReconcile r2) `shouldSatisfy` (not . null)

    it "retains a displaced recovery copy until registry repair is durable" $
        withScratch $ \root → do
            lib ← openOK root
            _ ← publishOK lib gidA payload1
            let displaced = libraryRoot root
                    </> transientDirectoryName DisplacedDir gidA 31 1
            copyEntryDirectory (finalDir root gidA) displaced
            _ ← publishOK lib gidA payload2

            -- A directory at the registry path makes the repair's final
            -- rename fail after its temporary was written and validated.
            removeFile (registryPath root)
            createDirectory (registryPath root)
            failedRepair ← pinned lib [gidA] (cleanupOK lib)
            crTransientsRemoved failedRepair `shouldBe` []
            doesDirectoryExist displaced `shouldReturn` True
            rcWarnings (crReconcile failedRepair)
                `shouldSatisfy` any ("registry write failed" `T.isInfixOf`)
            namesAfterFailure ← rootNames root
            [ n | n ← namesAfterFailure
                , classifyLibraryName n ≡ RegistryTempName ] `shouldBe` []

            removeDirectoryRecursive (registryPath root)
            (_, repaired) ← reconcileOK lib
            rcWarnings repaired `shouldBe` []
            swept ← pinned lib [gidA] (cleanupOK lib)
            crTransientsRemoved swept `shouldBe` [displaced]
            doesDirectoryExist displaced `shouldReturn` False

    it "a replacement whose own registry write fails keeps the displaced copy until a later durable repair" $
        withScratch $ \root → do
            lib ← openOK root
            _ ← publishOK lib gidA payload1
            -- A directory at the registry path makes every registry rename
            -- fail. The replacement itself still commits — the entry is
            -- durable — but its registry state is not, which is exactly the
            -- window the displaced copy exists for.
            removeFile (registryPath root)
            createDirectory (registryPath root)
            replaced ← publishOK lib gidA payload2
            prOutcome replaced `shouldBe` PublishedReplaced
            prWarnings replaced
                `shouldSatisfy` any ("displaced copy retained" `T.isInfixOf`)
            e ← committed lib gidA
            leDigest e `shouldBe` Just (digestOf payload2)
            names ← rootNames root
            let displaced = [ libraryRoot root </> n | n ← names
                            , classifyLibraryName n
                                ≡ TransientName DisplacedDir (tokenOf gidA) ]
            length displaced `shouldBe` 1
            -- Cleanup cannot make the registry durable either, so the old
            -- complete copy stays, run after run.
            stuck ← pinned lib [gidA] (cleanupOK lib)
            crTransientsRemoved stuck `shouldBe` []
            forM_ displaced $ \d → doesDirectoryExist d `shouldReturn` True
            inventory ← listEntries lib ≫= orFail "listEntries"
            liSource inventory `shouldSatisfy` (≢ FromRegistryFile)
            -- Once the registry can be written, the next cleanup proves it
            -- durable and only then sweeps the displaced copy.
            removeDirectoryRecursive (registryPath root)
            swept ← pinned lib [gidA] (cleanupOK lib)
            crTransientsRemoved swept `shouldBe` displaced
            forM_ displaced $ \d → doesDirectoryExist d `shouldReturn` False
            repaired ← listEntries lib ≫= orFail "listEntries"
            liSource repaired `shouldBe` FromRegistryFile
            map rrInventoryDigest (liRows repaired) `shouldBe` [digestOf payload2]

-- Containment ------------------------------------------------------------------------

containmentSpec ∷ Spec
containmentSpec = describe "containment" $ do
    it "refuses a symlinked library root" $
        withScratch $ \root → do
            createDirectoryIfMissing True (root </> "elsewhere")
            createDirectoryLink (root </> "elsewhere") (libraryRoot root)
            opened ← openLibrary (configFor root)
            expectPhase LibUnsafePath opened
            listDirectory (root </> "elsewhere") `shouldReturn` []

    it "refuses a library root whose immediate parent is a symlink" $
        withScratch $ \root → do
            createDirectoryIfMissing True (root </> "real")
            createDirectoryLink (root </> "real") (root </> "link")
            let cfg = (configFor root) { lcRoot = root </> "link" </> libraryDirectory }
            opened ← openLibrary cfg
            expectPhase LibUnsafePath opened
            listDirectory (root </> "real") `shouldReturn` []

    it "an entry directory that is a symlink is unreadable, never read or removed through" $
        withScratch $ \root → do
            lib ← openOK root
            _ ← publishOK lib gidA payload1
            _ ← publishOK lib gidB payload2
            removeDirectoryRecursive (finalDir root gidA)
            createDirectoryLink (finalDir root gidB) (finalDir root gidA)
            found ← lookupEntry lib gidA ≫= orFail "lookupEntry"
            fmap leStatus found `shouldSatisfy` \s → case s of
                Just (EntryUnreadable why) → "symlink" `T.isInfixOf` why
                _                          → False
            report ← pinned lib [gidB] (cleanupOK lib)
            crRetainedUnreadable report `shouldBe` [finalDir root gidA]
            crRemoved report `shouldBe` []
            e ← committed lib gidB
            leDigest e `shouldBe` Just (digestOf payload2)
            doesFileExist (finalDir root gidB </> "coarse.page") `shouldReturn` True

    it "refuses to lock through a symlinked lock file" $
        withScratch $ \root → do
            lib ← openOK root
            BS.writeFile (root </> "victim") ""
            createDirectoryLink (root </> "victim") (libraryRoot root </> lockFileName)
            publishEntry lib gidA payload1 `shouldSatisfyM` failedIn LibUnsafePath
  where
    shouldSatisfyM action p = action ≫= (`shouldSatisfy` p)

-- Coordination ----------------------------------------------------------------------

coordinationSpec ∷ Spec
coordinationSpec = describe "coordination" $ do
    it "cleanup waits for an in-process publication and then retains the pinned entry" $
        withScratch $ \root → do
            lib ← openOK root
            pending ← IORef.newIORef Nothing
            let hooks = noPublishHooks { phAfterStaged = \staging → do
                    a ← async (cleanupLibrary lib HS.empty)
                    IORef.writeIORef pending (Just a)
                    threadDelay 200_000
                    -- Still blocked on the lock: nothing swept, nothing
                    -- returned.
                    poll a `shouldReturn'` Nothing
                    doesDirectoryExist staging `shouldReturn` True }
            report ← pinned lib [gidA] $ do
                _ ← publishEntryWith hooks lib gidA payload1 ≫= orFail "publishEntryWith"
                Just a ← IORef.readIORef pending
                wait a ≫= orFail "cleanupLibrary"
            crRetainedPinned report `shouldBe` [gidA]
            crRemoved report `shouldBe` []
            crTransientsRemoved report `shouldBe` []
            e ← committed lib gidA
            leDigest e `shouldBe` Just (digestOf payload1)

    it "shares pins across handles opened on the same library root" $
        withScratch $ \root → do
            publishingLib ← openOK root
            cleanupLib ← openOK root
            _ ← publishOK publishingLib gidA payload1
            report ← pinned publishingLib [gidA] $ do
                pinnedReferences cleanupLib `shouldReturn` Set.singleton gidA
                cleanupOK cleanupLib
            crRetainedPinned report `shouldBe` [gidA]
            crRemoved report `shouldBe` []

    it "shares pins across handles whose root spellings differ, and normalises the spelling it keeps" $
        withScratch $ \root → do
            let spellings =
                    [ libraryRoot root
                    , libraryRoot root <> "/"
                    , root </> "." </> libraryDirectory
                    , root <> "//" <> libraryDirectory <> "/./"
                    ]
            handles ← forM spellings $ \spelling →
                openLibrary ((configFor root) { lcRoot = spelling }) ≫= orFail "openLibrary"
            -- Every handle carries the one normalised root, so paths in
            -- its reports and failures agree whatever the caller typed.
            forM_ handles $ \h → lcRoot (libraryConfig h) `shouldBe` libraryRoot root
            (pinningLib, others) ← case handles of
                (h : hs) → pure (h, hs)
                []       → do
                    expectationFailure "no handles"
                    error "unreachable"
            _ ← publishOK pinningLib gidA payload1
            pinned pinningLib [gidA] $
                forM_ others $ \other → do
                    pinnedReferences other `shouldReturn` Set.singleton gidA
                    report ← cleanupOK other
                    crRetainedPinned report `shouldBe` [gidA]
                    crRemoved report `shouldBe` []
            doesDirectoryExist (finalDir root gidA) `shouldReturn` True
            -- Released through the first handle, gone through the last.
            report ← cleanupOK (last handles)
            crRemoved report `shouldBe` [gidA]

    it "does not let a pinned action start after cleanup has taken the process mutex" $
        withScratch $ \root → do
            lib ← openOK root
            _ ← publishOK lib gidA payload1
            let quick = (configFor root) { lcLockWaitMicros = 2_000_000 }
            quickLib ← openLibrary quick ≫= orFail "openLibrary"
            let lockPath = libraryRoot root </> lockFileName
                script = unlines
                    [ "import fcntl, sys"
                    , "f = open(sys.argv[1], 'a+')"
                    , "fcntl.lockf(f, fcntl.LOCK_EX)"
                    , "print('locked', flush=True)"
                    , "sys.stdin.readline()" ]
            (Just hin, Just hout, _, ph) ← createProcess
                (proc "python3" ["-c", script, lockPath])
                    { std_in = CreatePipe, std_out = CreatePipe }
            hGetLine hout `shouldReturn` "locked"
            cleanup ← async (cleanupLibrary quickLib HS.empty)
            threadDelay 100_000
            poll cleanup `shouldSatisfyM` isNothing
            pinnedAction ← async (withPinnedReferences lib [gidA] (pure ()))
            threadDelay 100_000
            -- Cleanup owns the process mutex while waiting for the POSIX
            -- lock, so the pin transition and its action cannot slip behind
            -- cleanup's snapshot and run concurrently with deletion.
            poll pinnedAction `shouldSatisfyM` isNothing
            hClose hin
            _ ← waitForProcess ph
            report ← wait cleanup ≫= orFail "cleanupLibrary"
            crRemoved report `shouldBe` [gidA]
            wait pinnedAction ≫= orFail "withPinnedReferences"
            pure ()

    it "a pin held by another process retains the entry; once that process is gone the pin is abandoned and swept" $
        withScratch $ \root → do
            lib ← openOK root
            _ ← publishOK lib gidA payload1
            -- Another process pins gidA the way this library would: a pin
            -- file under the root, held under a record lock.
            let pinPath = libraryRoot root </> pinFileName gidA 424242 1
                script = unlines
                    [ "import fcntl, sys"
                    , "f = open(sys.argv[1], 'w')"
                    , "fcntl.lockf(f, fcntl.LOCK_EX)"
                    , "print('pinned', flush=True)"
                    , "sys.stdin.readline()" ]
            (Just hin, Just hout, _, ph) ← createProcess
                (proc "python3" ["-c", script, pinPath])
                    { std_in = CreatePipe, std_out = CreatePipe }
            hGetLine hout `shouldReturn` "pinned"
            pinnedReferences lib `shouldReturn` Set.empty
            held ← cleanupOK lib
            crRetainedPinned held `shouldBe` [gidA]
            crRemoved held `shouldBe` []
            crTransientsRemoved held `shouldBe` []
            doesFileExist pinPath `shouldReturn` True
            doesDirectoryExist (finalDir root gidA) `shouldReturn` True
            -- The pinning process exits without cleaning up: the lock is
            -- gone, the file is not. That is an abandoned pin.
            hClose hin
            _ ← waitForProcess ph
            gone ← cleanupOK lib
            crTransientsRemoved gone `shouldBe` [pinPath]
            crRemoved gone `shouldBe` [gidA]
            doesFileExist pinPath `shouldReturn` False

    it "a pin taken here is a held pin file another process can see, and is gone after release" $
        withScratch $ \root → do
            lib ← openOK root
            _ ← publishOK lib gidA payload1
            let probe = unlines
                    [ "import fcntl, sys"
                    , "f = open(sys.argv[1], 'r+')"
                    , "try:"
                    , "    fcntl.lockf(f, fcntl.LOCK_EX | fcntl.LOCK_NB)"
                    , "    print('free')"
                    , "except OSError:"
                    , "    print('held')" ]
                pinFilesNow = do
                    names ← rootNames root
                    pure [ libraryRoot root </> n | n ← names
                         , classifyLibraryName n ≡ PinName (tokenOf gidA) ]
            pinFile ← pinned lib [gidA] $ do
                files ← pinFilesNow
                pinFile ← case files of
                    [one] → pure one
                    other → do
                        expectationFailure ("expected one pin file, found " <> show other)
                        error "unreachable"
                readProcess "python3" ["-c", probe, pinFile] "" `shouldReturn` "held\n"
                -- Nested holds keep the one file; only the last release drops it.
                pinned lib [gidA] (pinFilesNow `shouldReturn` [pinFile])
                pinFilesNow `shouldReturn` [pinFile]
                pure pinFile
            doesFileExist pinFile `shouldReturn` False
            pinFilesNow `shouldReturn` []

    it "two same-id publishers leave one complete final entry and a registry matching the winner" $
        withScratch $ \root → do
            lib ← openOK root
            (r1, r2) ← concurrently (publishEntry lib gidA payload1)
                                    (publishEntry lib gidA payload2)
            o1 ← orFail "first publisher" r1
            o2 ← orFail "second publisher" r2
            L.sort (map show [prOutcome o1, prOutcome o2])
                `shouldBe` map show [PublishedNew, PublishedReplaced]
            names ← rootNames root
            names `shouldBe` L.sort [entryDirectoryName gidA, registryFileName, lockFileName]
            e ← committed lib gidA
            leDigest e `shouldSatisfy` (`elem` [Just (digestOf payload1), Just (digestOf payload2)])
            inv ← listEntries lib ≫= orFail "listEntries"
            map (Just . rrInventoryDigest) (liRows inv) `shouldBe` [leDigest e]

    it "a lock held by another process yields a structured busy failure, then succeeds once released" $
        withScratch $ \root → do
            lib ← openOK root
            let quick = (configFor root) { lcLockWaitMicros = 300_000 }
            quickLib ← openLibrary quick ≫= orFail "openLibrary"
            let lockPath = libraryRoot root </> lockFileName
                script = unlines
                    [ "import fcntl, sys"
                    , "f = open(sys.argv[1], 'a+')"
                    , "fcntl.lockf(f, fcntl.LOCK_EX)"
                    , "print('locked', flush=True)"
                    , "sys.stdin.readline()" ]
            (Just hin, Just hout, _, ph) ← createProcess
                (proc "python3" ["-c", script, lockPath])
                    { std_in = CreatePipe, std_out = CreatePipe }
            hGetLine hout `shouldReturn` "locked"
            publishEntry quickLib gidA payload1 `shouldSatisfyM` failedIn LibLock
            cleanupLibrary quickLib HS.empty `shouldSatisfyM` failedIn LibLock
            lookupEntry lib gidA `shouldReturn` Right Nothing
            hClose hin
            _ ← waitForProcess ph
            report ← publishOK quickLib gidA payload1
            prOutcome report `shouldBe` PublishedNew
  where
    shouldSatisfyM action p = action ≫= (`shouldSatisfy` p)
    shouldReturn' action expected = do
        (r ∷ Maybe (Either SomeException (Either LibraryFailure CleanupReport))) ← action
        isNothing r `shouldBe` isNothing expected
