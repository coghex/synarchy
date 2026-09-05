{-# LANGUAGE ScopedTypeVariables #-}
-- | The assembly owner of the "save components" gate (issue #760,
--   split out under #2043): registry-authoritative assembly,
--   decode-once behaviour, page scoping, production envelopes, the
--   optional transfer-orders component, decoded allocator floors,
--   cross-validation, and component-error capping. Pure -- no engine,
--   no IO beyond the deliberate 'System.IO.Unsafe.unsafePerformIO'
--   decode counter below, whose 'Data.IORef.IORef' is created per
--   example.
--
--   Composed by the facade 'Test.Headless.World.Save.Components', which
--   is the only module @test-headless/Spec.hs@ registers.
module Test.Headless.World.Save.Components.Assembly
    (spec) where

import UPrelude
import Test.Hspec
import Control.Exception (evaluate)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import Data.Either (isRight)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.Serialize as S
import qualified Data.Text as T

import qualified Data.HashSet as HS
import World.Save.Envelope
import World.Save.Envelope.Codec
    (encodeEnvelope, decodeEnvelope, DecodedEnvelope(..))
import World.Save.Envelope.Types
    (defaultEnvelopeLimits, ComponentId(..), EnvelopeManifest(..)
    , ComponentDescriptor(..))
import World.Save.Component
import World.Save.Component.Types
import World.Save.Component.Session
import World.Save.Component.Page
import World.Save.Component.Entities
import World.Save.Component.Transfer
    ( validateTransferOrders, TransferOrdersDTO(..)
    , PageTransferOrdersDTO(..), TransferOrderQueueDTO(..)
    , TransferOrderDTO(..), TransferEndpointDTO(..) )
import Unit.Transfer (TransferBatch(..), TransferEndpoint(..))
import Unit.Transfer.Orders
    ( TransferOrderId(..), TransferOrders(..), addTransferOrder
    , transferOrderAllocatorExhausted )
import World.Save.Compat.SessionV90
import World.Save.Integrity (integrityErrorCap)
import World.Save.Reference (SamePageRef(..))
import World.Save.Snapshot
import World.Save.Snapshot.Adapter
    ( SaveRequestMeta(..), snapshotSaveMetadata )
import World.Save.Types
    ( SaveMetadata(..), BuildingSnapshot(..), UnitSnapshot(..) )
import World.Page.Types (WorldPageId(..))
import Structure.Palette (emptyTexPalette, TexPalette(..))
import Item.Ground (emptyGroundItems)
import World.Flora.Harvest (emptyFloraHarvests)
import World.Edit.Types (WorldEdit(..))
import World.Chunk.Types (ChunkCoord(..))
import World.Construct.Attempt (firstConstructAttemptId)
import Craft.Bills (emptyCraftBills, BillId(..), BillMode(..))
import Unit.Transfer.Orders (emptyTransferOrders)
import Power.Types (emptyPowerNodes, PowerNodeId(..), PowerRole(..))
import Building.Types (BuildingId(..))
import Unit.Types (UnitId(..))
import Test.Headless.World.Save.Components.Fixture

spec ∷ Spec
spec = do
    describe "registry is authoritative for assembly (blocker 2, round 6)" $ do
        -- assembleSnapshot is registry-driven: every registered component
        -- prepares a mandatory fold that assembly runs. A full session
        -- populating EVERY component's data must reconstruct EXACTLY, so a
        -- component that were registered but not assembled would drop its
        -- data here.
        it "round-trips a session populating EVERY component's data, \
           \reconstructing the EXACT snapshot" $ do
            let meta  = snapshotSaveMetadata (SaveRequestMeta "s" "t" False) fullSnapshot
                bytes = encodeSessionSnapshot meta fullSnapshot []
            case decodeSessionEnvelope HS.empty HS.empty bytes of
                Left err → expectationFailure (T.unpack err)
                Right (m, snap, _luaComponents, _isMigrated) → do
                    m    `shouldBe` meta
                    snap `shouldBe` fullSnapshot

        it "the registry's ids ARE the reader's known component set (no \
           \registered component is unknown to the reader, and vice versa)" $
            HS.fromList (map rcId saveComponentRegistry)
                `shouldBe` componentKnownIds

        it "every registered component carries an assembly step \
           \(rcPrepare is total over the registry, yielding a fold onto \
           \a snapshot)" $ do
            -- Decode the full envelope, then confirm each registered
            -- component's rcPrepare runs without error against the decoded
            -- payloads — the structural guarantee that registration and
            -- assembly cannot diverge (the fold is a mandatory argument to
            -- registerComponent).
            let meta  = snapshotSaveMetadata (SaveRequestMeta "s" "t" False) fullSnapshot
                bytes = encodeSessionSnapshot meta fullSnapshot []
            case decodeSessionEnvelope HS.empty HS.empty bytes of
                Left err → expectationFailure (T.unpack err)
                Right _  → length saveComponentRegistry `shouldBe`
                               HS.size componentKnownIds

    -- Issue #1919. 'ccDecode' is a PURE function, so the only way to
    -- observe how many times the registry invoked it is a side effect —
    -- hence 'countedDecode''s 'unsafePerformIO'. These drive
    -- 'registerComponent' directly because that is exactly where the
    -- duplication lived: it used to build a @rcDecodeErrors@ and an
    -- @rcApply@ that each ran their own 'decodeComponentValue', so a
    -- successful load decoded every registered payload twice.
    describe "a present component's payload decodes exactly once" $ do
        let bytesFor ∷ Word32 → BS.ByteString
            bytesFor = S.encode

        it "a present REQUIRED component decodes ONCE in rcPrepare, and \
           \running the fold it hands back decodes nothing further" $ do
            ref ← newIORef (0 ∷ Int)
            let rc = registerComponent (countingCodec ref True) rememberValue
            case rcPrepare rc (decodeProbeEnvelope 1 (Just (bytesFor 7))) of
                Left es → expectationFailure ("unexpected failure: " <> show es)
                Right f → do
                    readIORef ref `shouldReturn` 1
                    -- Forcing the fold to WHNF runs its whole body; if it
                    -- decoded again the counter would move.
                    folded ← evaluate (isRight (f fullSnapshot))
                    folded `shouldBe` True
                    readIORef ref `shouldReturn` 1

        it "an ABSENT optional component decodes ZERO times and prepares \
           \to the identity fold" $ do
            ref ← newIORef (0 ∷ Int)
            let rc = registerComponent (countingCodec ref False) rememberValue
            case rcPrepare rc (decodeProbeEnvelope 1 Nothing) of
                Left es → expectationFailure ("unexpected failure: " <> show es)
                Right f → do
                    readIORef ref `shouldReturn` 0
                    f fullSnapshot `shouldBe` Right fullSnapshot
                    readIORef ref `shouldReturn` 0

        it "a PRESENT but malformed optional component decodes ONCE and \
           \fails exactly as a required one would -- absence is a \
           \manifest-level question, not a payload-level one" $ do
            refOpt ← newIORef (0 ∷ Int)
            refReq ← newIORef (0 ∷ Int)
            let junk    = BS.pack [0xff]
                optional = registerComponent (countingCodec refOpt False)
                                             rememberValue
                required = registerComponent (countingCodec refReq True)
                                             rememberValue
                run rc  = rcPrepare rc (decodeProbeEnvelope 1 (Just junk))
            case (run optional, run required) of
                (Left optErrs, Left reqErrs) → do
                    readIORef refOpt `shouldReturn` 1
                    readIORef refReq `shouldReturn` 1
                    map cePhase optErrs `shouldBe` [DecodePhase]
                    optErrs `shouldBe` reqErrs
                other → expectationFailure
                    ("expected both to fail identically, got " <> show
                        (fmap (const ()) (fst other), fmap (const ()) (snd other)))

        it "a present component at an UNSUPPORTED encoded version decodes \
           \once and reports a DecodePhase failure naming that version" $ do
            ref ← newIORef (0 ∷ Int)
            let rc = registerComponent (countingCodec ref True) rememberValue
            case rcPrepare rc (decodeProbeEnvelope 9 (Just (bytesFor 7))) of
                Right _ → expectationFailure "expected an unsupported-version failure"
                Left es → do
                    readIORef ref `shouldReturn` 1
                    map cePhase   es `shouldBe` [DecodePhase]
                    map ceVersion es `shouldBe` [9]

        it "the whole authoritative registry prepares in ONE pass over a \
           \real envelope, and those prepared folds reconstruct the \
           \EXACT snapshot assembleSnapshot produces" $ do
            -- The production registry's codecs cannot be instrumented, so
            -- this pins the other half structurally: every component's
            -- decoded value comes from its single rcPrepare, and folding
            -- those in dependency order is what assembleSnapshot returns.
            let meta  = snapshotSaveMetadata (SaveRequestMeta "s" "t" False) fullSnapshot
                bytes = encodeSessionSnapshot meta fullSnapshot []
            case decodeEnvelope defaultEnvelopeLimits currentEnvelopeVersion
                     (HS.insert metadataComponentId componentKnownIds)
                     (HS.insert metadataComponentId componentRequiredIds)
                     bytes of
                Left err → expectationFailure (show err)
                Right de → do
                    let prepared = [ (rcId rc, rcPrepare rc de)
                                   | rc ← saveComponentRegistry ]
                    [ (cid, es) | (cid, Left es) ← prepared ] `shouldBe` []
                    length [ () | (_, Right _) ← prepared ]
                        `shouldBe` length saveComponentRegistry
                    assembleSnapshot meta de `shouldBe` Right fullSnapshot

    describe "page-scoping (requirement 8)" $ do
        it "rejects a page-scoped slice set missing a page the authority \
           \declares" $ do
            let base = wpBase (basePageSnapshots (WorldPagesDTO [pageCore page1, pageCore page2]))
                bad  = BuildingsDTO
                    [ PageBuildingsDTO page1 HM.empty ]  -- page2 missing
            applyBuildings 1 1 bad base `shouldSatisfy` isLeft

        it "rejects a page-scoped slice for a page the authority does NOT \
           \declare" $ do
            let base = wpBase (basePageSnapshots (WorldPagesDTO [pageCore page1]))
                bad  = BuildingsDTO
                    [ PageBuildingsDTO page1 HM.empty
                    , PageBuildingsDTO page2 HM.empty ]
            applyBuildings 1 1 bad base `shouldSatisfy` isLeft

        it "reports the component's real encoded version (NOT a placeholder \
           \0) on a page-set mismatch (requirement 6)" $ do
            let base = wpBase (basePageSnapshots (WorldPagesDTO [pageCore page1, pageCore page2]))
                bad  = BuildingsDTO [ PageBuildingsDTO page1 HM.empty ]  -- page2 missing
            case applyBuildings 1 10 bad base of
                Left es → do
                    map ceVersion es `shouldSatisfy` all (≡ 1)
                    map ceVersion es `shouldSatisfy` notElem 0
                    map cePhase es `shouldSatisfy` all (≡ AssemblePhase)
                Right _ → expectationFailure "expected a page-mismatch error"

        it "accepts a slice set matching the authority exactly" $ do
            let base = wpBase (basePageSnapshots (WorldPagesDTO [pageCore page1, pageCore page2]))
                ok   = BuildingsDTO
                    [ PageBuildingsDTO page1 HM.empty
                    , PageBuildingsDTO page2 HM.empty ]
            applyBuildings 1 1 ok base `shouldSatisfy` (not . isLeft)

        it "reconstructs the building allocator from the global counter, \
           \not a per-page copy (requirement 9)" $ do
            let base = wpBase (basePageSnapshots (WorldPagesDTO [pageCore page1]))
                ok   = BuildingsDTO [ PageBuildingsDTO page1 HM.empty ]
            case applyBuildings 1 42 ok base of
                Right m  → (bsnNextId . pgsBuildings <$> HM.lookup page1 m)
                             `shouldBe` Just 42
                Left e   → expectationFailure (show e)

    describe "production envelope (encode ↔ decode)" $ do
        it "round-trips a complete multi-page session through \
           \encodeSessionSnapshot / decodeSessionEnvelope, reconstructing \
           \the EXACT snapshot" $
            case decodeSessionEnvelope HS.empty HS.empty encodeRich of
                Left err → expectationFailure (T.unpack err)
                Right (meta, snap, _luaComponents, _isMigrated) → do
                    meta `shouldBe` richMeta
                    snap `shouldBe` richSnapshot

        it "inspects metadata WITHOUT decoding gameplay" $
            decodeSaveEnvelopeMetadata HS.empty encodeRich `shouldBe` Right richMeta

        it "writes exactly the documented component set and NO transitional \
           \monolithic session component" $
            case decodeEnvelopeIds encodeRich of
                Left err → expectationFailure (T.unpack err)
                Right ids → do
                    let expected = [ metadataComponentId, coreSessionComponentId
                                   , worldPagesComponentId, buildingsComponentId
                                   , unitsComponentId, unitSimComponentId
                                   , craftBillsComponentId, powerNodesComponentId
                                   , worldEditsComponentId, worldActivityComponentId
                                   , texPaletteComponentId
                                   , containerKnowledgeComponentId
                                   , transferOrdersComponentId ]
                    ids `shouldMatchList` expected
                    (ComponentId "session" `elem` ids) `shouldBe` False

    -- #1246: the SECOND optional component. Its absent/present-but-broken
    -- split is the same rule #1087's container-knowledge established, so
    -- it is asserted the same way — through the REAL
    -- encodeSessionSnapshot/decodeSessionEnvelope path, never through
    -- 'ccValidate' in isolation, since "absent" is decided at the
    -- MANIFEST level by 'registerComponent' rather than by the codec.
    describe "transfer-orders is OPTIONAL (#1246)" $ do
        let orderedPage = (minimalPage page1)
                { pgsTransferOrders = onePendingOrder }
            orderedSnap = buildSessionSnapshot minimalGlobals [orderedPage]
            orderedMeta = snapshotSaveMetadata (SaveRequestMeta "s" "t" False)
                              orderedSnap
            orderedBytes = encodeSessionSnapshot orderedMeta orderedSnap []
            decodedOrders bytes = case decodeSessionEnvelope HS.empty HS.empty bytes of
                Left err → Left err
                Right (_, snap, _, _) →
                    Right (pgsTransferOrders <$> HM.lookup page1 (snapPages snap))

        it "round-trips a populated order store through the real \
           \production codec" $
            decodedOrders orderedBytes `shouldBe` Right (Just onePendingOrder)

        it "an ABSENT payload decodes to the empty default -- no orders \
           \queued, allocator back at 1 -- which is what lets every save \
           \written before this component existed keep loading" $
            decodedOrders (withoutTransferOrders orderedBytes)
                `shouldBe` Right (Just emptyTransferOrders)

        it "a PRESENT but malformed payload still fails the load exactly \
           \as a required component would -- absent and broken are \
           \different answers" $
            case decodedOrders (withGarbageTransferOrders orderedBytes) of
                Left msg → msg `shouldSatisfy` T.isInfixOf "transfer-orders"
                Right _  → expectationFailure
                    "a garbage transfer-orders payload loaded anyway"

        it "rejects an order id at or above the page's own allocator -- a \
           \decoded store whose next id could collide with a restored \
           \order" $
            validateTransferOrders (ordersDTO 1 (TransferOrderId 1)
                                        (TransferOrderId 1))
                `shouldSatisfy` mentions "not below the page's order allocator"

        it "rejects the reserved id 0, and an allocator of 0 that would \
           \mint it" $ do
            validateTransferOrders (ordersDTO 1 (TransferOrderId 0)
                                        (TransferOrderId 0))
                `shouldSatisfy` mentions "reserved"
            validateTransferOrders (ordersDTO 0 (TransferOrderId 1)
                                        (TransferOrderId 1))
                `shouldSatisfy` mentions "allocator is 0"

        it "rejects a map key that disagrees with the order's own \
           \embedded id -- two copies of one identity that would make \
           \lookup-by-key and lookup-by-field name different orders" $
            validateTransferOrders (ordersDTO 9 (TransferOrderId 1)
                                        (TransferOrderId 2))
                `shouldSatisfy` mentions "map key"

        it "accepts a well-formed store" $
            validateTransferOrders (ordersDTO 2 (TransferOrderId 1)
                                        (TransferOrderId 1))
                `shouldBe` []

        -- Review round 2: 'trosNextId' is a Word32, so incrementing past
        -- maxBound wrapped to 0, the next allocation normalised that back
        -- to 1, and HM.insert then OVERWROTE whatever order already held
        -- id 1 — silent reuse of a durable identity, the one failure a
        -- save format cannot recover from. The allocator now saturates
        -- and refuses instead.
        it "refuses to allocate past the end of the id space rather than \
           \wrapping and overwriting an existing order" $ do
            let exhausted = emptyTransferOrders { trosNextId = maxBound }
            transferOrderAllocatorExhausted exhausted `shouldBe` True
            addTransferOrder (UnitId 1) emptyBatch exhausted
                `shouldBe` Nothing

        it "issues the LAST id below the boundary, and only then reports \
           \exhaustion -- the refusal is off by neither one id nor two" $ do
            let lastFree = emptyTransferOrders { trosNextId = maxBound - 1 }
            transferOrderAllocatorExhausted lastFree `shouldBe` False
            case addTransferOrder (UnitId 1) emptyBatch lastFree of
                Nothing → expectationFailure
                    "refused while one id was still free"
                Just (after, oid) → do
                    oid `shouldBe` TransferOrderId (maxBound - 1)
                    trosNextId after `shouldBe` maxBound
                    transferOrderAllocatorExhausted after `shouldBe` True
                    -- The order it DID issue is still there: saturating
                    -- must not disturb what was already allocated.
                    HM.keys (trosOrders after)
                        `shouldBe` [TransferOrderId (maxBound - 1)]
                    addTransferOrder (UnitId 1) emptyBatch after
                        `shouldBe` Nothing

        it "accepts a SATURATED allocator on decode -- it is the \
           \legitimate terminal state, not corruption: every stored id \
           \is strictly below it and no further id can be issued" $
            validateTransferOrders (ordersDTO maxBound (TransferOrderId 1)
                                        (TransferOrderId 1))
                `shouldBe` []

    -- #1667: every allocator validator compared LIVE IDS with the
    -- cursor and stopped there, so an EMPTY map certified any cursor at
    -- all -- 0 where the id space starts at 1, and a negative value
    -- wherever the wire field is a signed 'Int'. The live allocation
    -- paths then hand that cursor out verbatim as the next real id.
    -- 'validateTransferOrders' (above) already checked its own floor;
    -- these five component validators and the three session-global
    -- cursors now do too, each in its own clause so the map's emptiness
    -- is irrelevant.
    describe "decoded allocators validate their own floor (#1667)" $ do
        let billSlice next = CraftBillsDTO
                [ PageCraftBillsDTO page1 (BillQueueDTO HM.empty next) ]
            nodeSlice next = PowerNodesDTO
                [ PagePowerNodesDTO page1 (NodeRegistryDTO HM.empty next) ]
            activitySlice next = WorldActivityDTO
                [ (emptyPageActivity page1)
                    { padGroundItems = GroundItemsDTO next HM.empty } ]
            paletteWith next = TexPaletteDTO next []

        it "rejects a craft-bill allocator of 0 on an EMPTY bill map, \
           \and accepts the fresh allocator an empty page really has" $ do
            validateCraftBills (billSlice 0)
                `shouldSatisfy` mentions "craft-bill allocator is 0"
            validateCraftBills (billSlice 1) `shouldBe` []

        it "rejects a power-node allocator of 0 on an EMPTY node map, \
           \and accepts the fresh allocator an empty page really has" $ do
            validatePowerNodes (nodeSlice 0)
                `shouldSatisfy` mentions "power-node allocator is 0"
            validatePowerNodes (nodeSlice 1) `shouldBe` []

        it "rejects a NEGATIVE ground-item allocator on an EMPTY item \
           \map, while 0 stays valid -- ground items are the engine's \
           \one zero-based allocator" $ do
            validateWorldActivity (activitySlice (-1))
                `shouldSatisfy` mentions "ground-item allocator is -1"
            validateWorldActivity (activitySlice 0) `shouldBe` []
            validateWorldActivity (activitySlice 5) `shouldBe` []

        it "rejects a NEGATIVE texture-palette allocator on an EMPTY \
           \palette, while 0 stays valid -- the palette is zero-based \
           \too" $ do
            validateTexPalette (paletteWith (-1))
                `shouldSatisfy` mentions "texture palette allocator is -1"
            validateTexPalette (paletteWith 0) `shouldBe` []

        it "the floor is a SEPARATE finding from the per-id comparison, \
           \so a payload violating both reports both" $ do
            let bothWrong = CraftBillsDTO
                    [ PageCraftBillsDTO page1
                        (BillQueueDTO (HM.singleton (BillId 3)
                            sampleBillDTO { bilId = BillId 3 }) 0) ]
            validateCraftBills bothWrong
                `shouldSatisfy` mentions "craft-bill allocator is 0"
            validateCraftBills bothWrong
                `shouldSatisfy` mentions "not below the page's bill allocator"

        it "a legitimate GAP between the highest live id and the cursor \
           \is still accepted -- the floor check tightens nothing that \
           \already passed" $ do
            validateCraftBills (CraftBillsDTO
                [ PageCraftBillsDTO page1
                    (BillQueueDTO (HM.singleton (BillId 1) sampleBillDTO) 99) ])
                `shouldBe` []
            validateWorldActivity (WorldActivityDTO
                [ (emptyPageActivity page1)
                    { padGroundItems = GroundItemsDTO 99 HM.empty } ])
                `shouldBe` []

        -- Requirement 4: a LEGACY component version must gain the
        -- check rather than route around it. 'componentCodec' runs
        -- 'ccValidate' on the canonical MIGRATED value after ANY
        -- accepted decoder, so an older payload reaches the same
        -- clause the current one does -- proved here on the pair
        -- 'decodeComponentValue' itself runs.
        it "a v1 craft-bills payload carrying a 0 allocator is rejected \
           \by the same clause, not waved through by its older decoder" $ do
            let v1With next = S.encode (CraftBillsDTOv1
                    [ PageCraftBillsDTOv1 page1
                        (BillQueueDTOv1 HM.empty next) ])
                decodeThenValidate next =
                    case ccDecode craftBillsCodec 1 (v1With next) of
                        Left e  → [e]
                        Right a → ccValidate craftBillsCodec a
            decodeThenValidate 0
                `shouldSatisfy` mentions "craft-bill allocator is 0"
            decodeThenValidate 1 `shouldBe` []

        -- The three session-global cursors ride on 'CoreSessionDTO' and
        -- are checked by 'validateSessionSnapshot', which BOTH the
        -- modern envelope decode and the legacy v90 bridge funnel
        -- through -- so neither path can bypass the floor. All three are
        -- unsigned on the wire, making 0 the single invalid value.
        it "rejects a session-global item/building/unit allocator of 0 \
           \even with EVERY entity map empty" $ do
            let bare = (minimalPage page1)
                    { pgsBuildings = BuildingSnapshot HM.empty 1
                    , pgsUnits     = UnitSnapshot HM.empty 1
                    , pgsUnitSimStates = HM.empty }
                globals = minimalGlobals
                    { sgNextBuildingId = 1, sgNextUnitId = 1 }
                errsFor g = validateSessionSnapshot
                                (buildSessionSnapshot g [bare])
            errsFor globals { sgNextItemId = 0 }
                `shouldBe` [ItemAllocatorBelowFloor 0]
            errsFor globals { sgNextBuildingId = 0 }
                `shouldBe` [BuildingAllocatorBelowFloor 0]
            errsFor globals { sgNextUnitId = 0 }
                `shouldBe` [UnitAllocatorBelowFloor 0]
            errsFor globals `shouldBe` []

        it "surfaces a rejected session-global floor through the real \
           \envelope decode, attributed to core-session" $ do
            let bare = (minimalPage page1)
                    { pgsBuildings = BuildingSnapshot HM.empty 1
                    , pgsUnits     = UnitSnapshot HM.empty 1
                    , pgsUnitSimStates = HM.empty }
                snap = (buildSessionSnapshot minimalGlobals [bare])
                           { snapNextBuildingId = 0 }
                meta = snapshotSaveMetadata (SaveRequestMeta "s" "t" False) snap
                bytes = encodeSessionSnapshot meta snap []
            case decodeSessionEnvelope HS.empty HS.empty bytes of
                Right _  → expectationFailure
                    "expected a core-session allocator-floor rejection"
                Left msg → do
                    msg `shouldSatisfy` T.isInfixOf "BuildingAllocatorBelowFloor"
                    msg `shouldSatisfy` T.isInfixOf "core-session"

        it "the legacy v90 bridge cannot bypass the floor either -- its \
           \RECONSTRUCTED building/unit cursors and its carried item \
           \cursor all go through the same validator" $ do
            let v90With f = migrateSessionV90 minimalSaveMetadataV90
                                (f minimalSaveDataV90)
                zeroPages g = minimalSaveDataV90
                    { sd90Worlds =
                        [ g (minimalWorldPageSaveV90 (WorldPageId "main_world")) ] }
                rejects r = case r of
                    Left errs → errs `shouldSatisfy` (not . null)
                    Right _   → expectationFailure
                        "expected a v90 allocator-floor rejection"
            rejects (v90With (\sd → sd { sd90NextItemInstanceId = 0 }))
            rejects (migrateSessionV90 minimalSaveMetadataV90
                        (zeroPages (\w → w { wp90Buildings =
                                                BuildingSnapshotV90 HM.empty 0 })))
            rejects (migrateSessionV90 minimalSaveMetadataV90
                        (zeroPages (\w → w { wp90Units =
                                                UnitSnapshotV90 HM.empty 0 })))
            -- The unmodified fixture, whose v90 pages carry the real
            -- convention, still migrates.
            case migrateSessionV90 minimalSaveMetadataV90 minimalSaveDataV90 of
                Left errs → expectationFailure (show errs)
                Right _   → pure ()

    describe "assembly cross-validation (requirement 6/9/12)" $ do
        it "rejects a manifest/gameplay metadata mismatch" $ do
            let wrongMeta = richMeta { smSeed = 999999 }
                bytes = encodeSessionSnapshot wrongMeta richSnapshot []
            case decodeSessionEnvelope HS.empty HS.empty bytes of
                Left msg → msg `shouldSatisfy` T.isInfixOf "seed"
                Right _  → expectationFailure "expected a metadata mismatch rejection"

        it "rejects an orphaned unit sim state (a sim owner with no unit)" $ do
            let orphanPage = (minimalPage page1)
                    { pgsUnitSimStates = HM.singleton (UnitId 77) minimalSimState }
                snap = buildSessionSnapshot minimalGlobals [orphanPage]
                meta = snapshotSaveMetadata
                         (SaveRequestMeta "s" "t" False) snap
                bytes = encodeSessionSnapshot meta snap []
            decodeSessionEnvelope HS.empty HS.empty bytes `shouldSatisfy` isLeft

        it "rejects an allocator collision (a building id at/above the \
           \allocator)" $ do
            let badPage = (minimalPage page1)
                    { pgsBuildings = BuildingSnapshot
                        (HM.singleton (BuildingId 50) (minimalBuildingInstance []))
                        51 }
                snap = buildSessionSnapshot
                         minimalGlobals { sgNextBuildingId = 50 } [badPage]
                meta = snapshotSaveMetadata (SaveRequestMeta "s" "t" False) snap
                bytes = encodeSessionSnapshot meta snap []
            decodeSessionEnvelope HS.empty HS.empty bytes `shouldSatisfy` isLeft

        it "rejects a missing active-page reference" $ do
            let snap = buildSessionSnapshot
                         minimalGlobals { sgActivePage = page2 } [minimalPage page1]
                meta = snapshotSaveMetadata (SaveRequestMeta "s" "t" False) snap
                bytes = encodeSessionSnapshot meta snap []
            decodeSessionEnvelope HS.empty HS.empty bytes `shouldSatisfy` isLeft

        -- #760 round 9 (still-open item 1): the FULL envelope pipeline
        -- (not just the isolated ccValidate calls above) must reject a
        -- craft-bill/power-node whose map key disagrees with its own
        -- embedded id. Built the same way "one malformed component
        -- prevents ANY partial snapshot result" below substitutes a
        -- tampered payload for one real component's real bytes.
        it "rejects a decoded envelope whose craft-bill map key disagrees \
           \with its own embedded id" $ do
            let mismatched = BillQueueDTO
                    { bqBills = HM.singleton (BillId 1) CraftBillDTO
                        { bilId = BillId 2, bilStation = SamePageRef (BuildingId 1)
                        , bilRecipe = "r", bilRemaining = -1, bilClaimant = Nothing
                        , bilClaimedAt = 0, bilProgress = 0, bilSeq = 1
                        , bilPaused = False, bilWorking = False
                        , bilMode = RepeatForever, bilTarget = 0
                        , bilOutputItem = "" }
                    , bqNextId = 5 }
                badDTO = CraftBillsDTO
                    [ PageCraftBillsDTO page1 mismatched
                    , PageCraftBillsDTO page2 (toBillQueueDTO emptyCraftBills) ]
                good = encodeComponentSpecs richSnapshot
                tampered = [ if cid ≡ craftBillsComponentId
                               then (cid, ver, req, S.encode badDTO)
                               else s
                           | s@(cid, ver, req, _) ← good ]
                specs = (metadataComponentId, metadataComponentVersion, True
                        , S.encode richMeta) : tampered
                bytes = case encodeEnvelope defaultEnvelopeLimits
                            currentEnvelopeVersion specs of
                    Right b → b
                    Left e  → error ("test setup: " <> show e)
            case decodeSessionEnvelope HS.empty HS.empty bytes of
                Left msg → msg `shouldSatisfy` T.isInfixOf "map key"
                Right _  → expectationFailure
                    "expected the key/value id mismatch to be rejected"

        it "rejects a decoded envelope whose power-node map key disagrees \
           \with its own embedded id" $ do
            let mismatched = NodeRegistryDTO
                    { regNodes = HM.singleton (PowerNodeId 1) PowerNodeDTO
                        { nodId = PowerNodeId 2, nodBuilding = SamePageRef (BuildingId 1)
                        , nodRole = PowerSource, nodPeakWatts = 400
                        , nodCapacityWh = 0, nodStoredWh = 0 }
                    , regNextId = 5 }
                badDTO = PowerNodesDTO
                    [ PagePowerNodesDTO page1 mismatched
                    , PagePowerNodesDTO page2 (toNodeRegistryDTO emptyPowerNodes) ]
                good = encodeComponentSpecs richSnapshot
                tampered = [ if cid ≡ powerNodesComponentId
                               then (cid, ver, req, S.encode badDTO)
                               else s
                           | s@(cid, ver, req, _) ← good ]
                specs = (metadataComponentId, metadataComponentVersion, True
                        , S.encode richMeta) : tampered
                bytes = case encodeEnvelope defaultEnvelopeLimits
                            currentEnvelopeVersion specs of
                    Right b → b
                    Left e  → error ("test setup: " <> show e)
            case decodeSessionEnvelope HS.empty HS.empty bytes of
                Left msg → msg `shouldSatisfy` T.isInfixOf "map key"
                Right _  → expectationFailure
                    "expected the key/value id mismatch to be rejected"

        -- #760 round 9 (new item 2b): a structure edit's texture/facemap
        -- palette id must resolve in the assembled texture palette -- a
        -- genuine cross-component check (world-edits' pgsEdits against
        -- core-session's snapTexPalette), unlike the tolerated dangling
        -- craft-bill-station/power-node-building references.
        it "rejects a structure edit referencing a texture palette id \
           \absent from the assembled palette" $ do
            let badPage = (minimalPage page1)
                    { pgsEdits = HM.singleton (ChunkCoord 0 0)
                        [ WeSetStructure 1 2 0 5 6 3 ] }
                snap = buildSessionSnapshot
                         minimalGlobals { sgTexPalette = emptyTexPalette } [badPage]
                meta = snapshotSaveMetadata (SaveRequestMeta "s" "t" False) snap
                bytes = encodeSessionSnapshot meta snap []
            decodeSessionEnvelope HS.empty HS.empty bytes `shouldSatisfy` isLeft

        it "accepts a structure edit whose texture/facemap palette ids \
           \both resolve in the assembled palette (does not over-reject a \
           \valid structure edit)" $ do
            let tp = TexPalette
                    { tpPathToId = HM.fromList [("a.png", 5), ("b.png", 6)]
                    , tpIdToPath = HM.fromList [(5, "a.png"), (6, "b.png")]
                    , tpNextId   = 7 }
                goodPage = (minimalPage page1)
                    { pgsEdits = HM.singleton (ChunkCoord 0 0)
                        [ WeSetStructure 1 2 0 5 6 3 ] }
                snap = buildSessionSnapshot
                         minimalGlobals { sgTexPalette = tp } [goodPage]
                meta = snapshotSaveMetadata (SaveRequestMeta "s" "t" False) snap
                bytes = encodeSessionSnapshot meta snap []
            case decodeSessionEnvelope HS.empty HS.empty bytes of
                Left err → expectationFailure (T.unpack err)
                Right (_, snap', _luaComponents, _isMigrated) → snap' `shouldBe` snap

        -- #760 round 9 (new item 2a): the texture-palette component's own
        -- local bijection/allocator invariant, exercised through the FULL
        -- envelope pipeline (not just the isolated ccValidate call).
        it "rejects a decoded envelope whose texture palette has a \
           \duplicate id (non-bijective)" $ do
            let badTP = TexPaletteDTO 2 [("a.png", 0), ("b.png", 0)]
                good = encodeComponentSpecs richSnapshot
                tampered = [ if cid ≡ texPaletteComponentId
                               then (cid, ver, req, S.encode badTP)
                               else s
                           | s@(cid, ver, req, _) ← good ]
                specs = (metadataComponentId, metadataComponentVersion, True
                        , S.encode richMeta) : tampered
                bytes = case encodeEnvelope defaultEnvelopeLimits
                            currentEnvelopeVersion specs of
                    Right b → b
                    Left e  → error ("test setup: " <> show e)
            case decodeSessionEnvelope HS.empty HS.empty bytes of
                Left msg → msg `shouldSatisfy` T.isInfixOf "duplicate"
                Right _  → expectationFailure
                    "expected the non-bijective palette to be rejected"

        it "rejects a decoded envelope whose texture palette carries an id \
           \at/above its own allocator" $ do
            let badTP = TexPaletteDTO 1 [("a.png", 1)]
                good = encodeComponentSpecs richSnapshot
                tampered = [ if cid ≡ texPaletteComponentId
                               then (cid, ver, req, S.encode badTP)
                               else s
                           | s@(cid, ver, req, _) ← good ]
                specs = (metadataComponentId, metadataComponentVersion, True
                        , S.encode richMeta) : tampered
                bytes = case encodeEnvelope defaultEnvelopeLimits
                            currentEnvelopeVersion specs of
                    Right b → b
                    Left e  → error ("test setup: " <> show e)
            decodeSessionEnvelope HS.empty HS.empty bytes `shouldSatisfy` isLeft

        it "one malformed component prevents ANY partial snapshot result \
           \(all-or-nothing)" $ do
            let good = encodeComponentSpecs richSnapshot
                tampered = [ if cid ≡ buildingsComponentId
                               then (cid, ver, req, BS.pack [9,9,9])
                               else s
                           | s@(cid, ver, req, _) ← good ]
                specs = (metadataComponentId, metadataComponentVersion, True
                        , S.encode richMeta) : tampered
                bytes = case encodeEnvelope defaultEnvelopeLimits
                            currentEnvelopeVersion specs of
                    Right b → b
                    Left e  → error ("test setup: " <> show e)
            case decodeSessionEnvelope HS.empty HS.empty bytes of
                Left msg → msg `shouldSatisfy` T.isInfixOf "buildings"
                Right _  → expectationFailure
                    "expected a malformed component to fail the whole decode"

    describe "capComponentErrors (round-3 review, issue #764)" $ do
        it "passes a small list through unchanged, sorted deterministically \
           \-- no trailer note when nothing was actually omitted" $ do
            let errs = [ ComponentError buildingsComponentId 1 ValidatePhase "z"
                       , ComponentError buildingsComponentId 1 ValidatePhase "a" ]
            map ceMessage (capComponentErrors errs) `shouldBe` ["a", "z"]

        it "caps a large synthetic list at integrityErrorCap with a \
           \trailer note reporting the TRUE omitted count -- not a \
           \double-capped undercount from capping twice" $ do
            let n = integrityErrorCap + 137
                errs = [ ComponentError buildingsComponentId 1 ValidatePhase
                            ("msg-" <> tshow4 i)
                       | i ← [1 .. n] ]
                tshow4 i = let s = show (i ∷ Int)
                           in T.pack (replicate (4 - length s) '0' <> s)
                result = capComponentErrors errs
            length result `shouldBe` integrityErrorCap + 1
            ceMessage (last result)
                `shouldBe` "137 additional component finding(s) omitted \
                           \(see World.Save.Integrity.integrityErrorCap)"

        it "an empty list stays empty (no spurious trailer)" $
            capComponentErrors [] `shouldBe` []

-- Helpers -----------------------------------------------------------

-- | The smallest batch an allocator-boundary case needs: what is being
--   measured there is the ID, never the payload.
emptyBatch ∷ TransferBatch
emptyBatch = TransferBatch
    { tbSource      = EndpointUnit (UnitId 1)
    , tbDestination = EndpointBuilding (BuildingId 1)
    , tbEntries     = [] }

-- | A one-page transfer-orders DTO with an explicit allocator, map key
--   and embedded id, so each validator rule can be violated in
--   isolation.
ordersDTO ∷ Word32 → TransferOrderId → TransferOrderId → TransferOrdersDTO
ordersDTO next key embedded = TransferOrdersDTO
    [ PageTransferOrdersDTO page1 TransferOrderQueueDTO
        { toqOrders = HM.singleton key TransferOrderDTO
            { trdId          = embedded
            , trdUnit        = SamePageRef (UnitId 1)
            , trdSource      = TedUnit (SamePageRef (UnitId 1))
            , trdDestination = TedBuilding (SamePageRef (BuildingId 1))
            , trdEntries     = [] }
        , toqNextId = next } ]

-- | #1667: a page's activity slice with every designation map empty and
--   a FRESH ground-item allocator — the shape whose emptiness used to
--   certify any cursor at all.
emptyPageActivity ∷ WorldPageId → PageActivityDTO
emptyPageActivity pid = PageActivityDTO pid HM.empty HM.empty HM.empty
    HM.empty HM.empty emptyFloraHarvests HM.empty
    (toGroundItemsDTO emptyGroundItems) HM.empty HM.empty HM.empty
    firstConstructAttemptId

-- | #1667: one well-formed bill, so a floor test can pair an invalid
--   allocator with a live id and see BOTH findings reported.
sampleBillDTO ∷ CraftBillDTO
sampleBillDTO = CraftBillDTO
    { bilId = BillId 1, bilStation = SamePageRef (BuildingId 1)
    , bilRecipe = "r", bilRemaining = -1, bilClaimant = Nothing
    , bilClaimedAt = 0, bilProgress = 0, bilSeq = 1
    , bilPaused = False, bilWorking = False
    , bilMode = RepeatForever, bilTarget = 0, bilOutputItem = "" }

-- | Drop the transfer-orders component from an encoded envelope
--   entirely: the shape every save written before #1246 has.
withoutTransferOrders ∷ BS.ByteString → BS.ByteString
withoutTransferOrders = rewriteTransferOrders (const Nothing)

-- | Replace its payload with bytes no version of the DTO can decode.
withGarbageTransferOrders ∷ BS.ByteString → BS.ByteString
withGarbageTransferOrders =
    rewriteTransferOrders (const (Just "not-a-transfer-orders-payload"))

rewriteTransferOrders
    ∷ (BS.ByteString → Maybe BS.ByteString) → BS.ByteString → BS.ByteString
rewriteTransferOrders f bytes =
    case decodeEnvelope defaultEnvelopeLimits currentEnvelopeVersion
             (HS.insert metadataComponentId componentKnownIds) HS.empty bytes of
        Left err → error ("rewriteTransferOrders: decode: " <> show err)
        Right de →
            let specs = [ (cdId d, cdVersion d, cdRequired d, payload)
                        | d ← emComponents (deManifest de)
                        , Just raw ← [HM.lookup (cdId d) (dePayloads de)]
                        , Just payload ← [ if cdId d ≡ transferOrdersComponentId
                                             then f raw else Just raw ] ]
            in case encodeEnvelope defaultEnvelopeLimits currentEnvelopeVersion
                        specs of
                Left err  → error ("rewriteTransferOrders: encode: " <> show err)
                Right out → out

-- | The synthetic component id the issue #1919 decode-counting cases
--   register under. Deliberately outside the production registry's id
--   set: these cases exercise 'registerComponent''s contract, never the
--   shipped components.
decodeProbeComponentId ∷ ComponentId
decodeProbeComponentId = ComponentId "decode-probe"

-- | Count one decode of the probe component's payload.
--
--   'unsafePerformIO' is the point, not an accident: 'ccDecode' is a
--   pure function, so a side effect is the only way to observe how many
--   times the registry called it. NOINLINE keeps the shared 'IORef'
--   from being duplicated into separate call sites, and GHC's CSE does
--   not merge 'unsafePerformIO' applications (they carry a @State#@
--   token), so two invocations can never be collapsed into the one this
--   is asserting on.
{-# NOINLINE countedDecode #-}
countedDecode ∷ IORef Int → Word32 → BS.ByteString
              → Either ComponentError Word32
countedDecode ref ver bytes = unsafePerformIO $ do
    modifyIORef' ref (+ 1)
    pure $ if ver ≢ 1
        then Left (ComponentError decodeProbeComponentId ver DecodePhase
                     "unsupported schema version (reader supports v1)")
        else case S.decode bytes of
            Left err → Left (ComponentError decodeProbeComponentId ver DecodePhase
                               ("malformed payload: " <> T.pack err))
            Right w  → Right w

-- | A one-field codec whose every decode is counted.
countingCodec ∷ IORef Int → Bool → ComponentCodec Word32
countingCodec ref required = ComponentCodec
    { ccId        = decodeProbeComponentId
    , ccVersion   = 1
    , ccInputVers = [1]
    , ccRequired  = required
    , ccDeps      = []
    , ccEncode    = const (S.encode (0 ∷ Word32))
    , ccDecode    = countedDecode ref
    , ccValidate  = const []
    }

-- | The probe's assembly fold. It only has to prove it RAN without
--   decoding anything itself, so it leaves the snapshot alone.
rememberValue ∷ Word32 → Word32 → SessionSnapshot
              → Either [ComponentError] SessionSnapshot
rememberValue _ver _value snap = Right snap

-- | A hand-built 'DecodedEnvelope' carrying exactly the probe component,
--   at @ver@, with @payload@ — or declaring it nowhere at all
--   ('Nothing'), which is what an absent optional component looks like.
--   Offsets/lengths/checksums are unread by this path (the envelope
--   codec already verified them upstream), so they are left at zero.
decodeProbeEnvelope ∷ Word32 → Maybe BS.ByteString → DecodedEnvelope
decodeProbeEnvelope ver payload = DecodedEnvelope
    { deVersion  = currentEnvelopeVersion
    , deManifest = EnvelopeManifest
        [ ComponentDescriptor
            { cdId = decodeProbeComponentId, cdVersion = ver, cdRequired = True
            , cdOffset = 0, cdLength = 0, cdChecksum = 0 }
        | Just _ ← [payload] ]
    , dePayloads = HM.fromList [ (decodeProbeComponentId, b) | Just b ← [payload] ]
    }

-- | The component ids actually present in an encoded envelope's
--   manifest — a genuine structural read, so a stray @"session"@
--   component (or a missing gameplay one) would show up.
decodeEnvelopeIds ∷ BS.ByteString → Either Text [ComponentId]
decodeEnvelopeIds bytes =
    let known = HS.insert metadataComponentId componentKnownIds
    in case decodeEnvelope defaultEnvelopeLimits currentEnvelopeVersion
                known known bytes of
        Left err → Left (T.pack (show err))
        Right de → Right (map cdId (emComponents (deManifest de)))
