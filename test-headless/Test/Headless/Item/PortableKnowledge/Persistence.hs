-- | The @"portable-knowledge"@ save component (#2512 requirements 6, 7
--   and 8), through the REAL production codec pair
--   ('encodeSessionSnapshot' / 'decodeSessionEnvelope') rather than
--   through 'ccValidate' in isolation — because "absent" is decided at
--   the MANIFEST level by 'registerComponent', not by the codec, so an
--   isolated validator call cannot see the distinction this component's
--   optionality turns on.
module Test.Headless.Item.PortableKnowledge.Persistence (spec) where

import UPrelude
import Test.Hspec
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Serialize as S
import qualified Data.Text as T
import Engine.Graphics.Camera (CameraFacing(..))
import Item.Knowledge
import Structure.Palette (emptyTexPalette)
import Test.Headless.Harness.GeneratedIds (fixtureGeneratedWorldIdForPage)
import Test.Headless.Item.PortableKnowledge.Fixture
import World.Generate.Types (defaultWorldGenParams, WorldGenParams(..))
import World.Save.Component (componentKnownIds)
import World.Save.Component.Page (blankPageSnapshot)
import World.Save.Component.PortableKnowledge
import World.Save.Component.Types
    ( ComponentCodec(..), ComponentError(..), ComponentPhase(..)
    , metadataComponentId, portableKnowledgeComponentId )
import World.Save.Envelope (encodeSessionSnapshot, decodeSessionEnvelope)
import World.Save.Envelope.Codec
    (DecodedEnvelope(..), decodeEnvelope, encodeEnvelope)
import World.Save.Envelope.Types
    ( ComponentDescriptor(..), EnvelopeManifest(..)
    , defaultEnvelopeLimits )
import World.Save.Snapshot
import World.Save.Snapshot.Adapter (SaveRequestMeta(..), snapshotSaveMetadata)
import World.Save.Types
    (missingPortableItemDefReferences, mpdrDefName, mpdrOwnerId)
import World.Save.Snapshot.Adapter (snapshotToSaveData)
import World.Save.Integrity (IntegrityError(..), KnownEntities(..), luaReferenceErrors)
import World.Save.Payload (LuaRefEdge(..))
import Engine.Scripting.Lua.API.Save.Integrity (knownEntitiesFromSaveData)

-- Fixtures -----------------------------------------------------------

-- | A knowledge map carrying ALL FOUR states at once: never-inspected
--   (absent entirely, plus a present-but-empty record that must read
--   the same way), weight-only, known-empty and known-contents.
--
--   The two timestamps DIFFER on the known-contents record — weighed
--   later than it was opened — so a codec that carried one stamp for
--   both, or copied one into the other, cannot round-trip it.
allFourStates ∷ PortableKnowledge
allFourStates = PortableKnowledge $ HM.fromList
    [ (looseId,   PortableRecord Nothing Nothing)
    , (7001,      PortableRecord (Just (WeightObservation 12.5 900)) Nothing)
    , (7002,      PortableRecord (Just (WeightObservation 6.0 950))
                                 (Just (ContentsObservation [] 950)))
    , (crateId,   PortableRecord (Just (WeightObservation 31.25 1200))
                                 (Just (ContentsObservation [kit] 950.25)))
    ]

-- | A session whose live item allocator sits BELOW the remembered ids
--   and whose live pages carry an item sharing an id with a remembered
--   one — both deliberately, and both illegal for a LIVE entity.
--   Remembered ids are historical observations and must participate in
--   neither the allocator bound nor the duplicate check.
knowledgeSnapshot ∷ SessionSnapshot
knowledgeSnapshot = buildSessionSnapshot globals [page]
  where
    page = (blankPageSnapshot pkPageA (defaultWorldGenParams { wgpSeed = 7 }))
        { pgsGeneratedId = Just (fixtureGeneratedWorldIdForPage pkPageA) }
    globals = SessionGlobals
        { sgGameTime = 1500, sgTexPalette = emptyTexPalette
        , sgNextItemId = 10, sgNextBuildingId = 9, sgNextUnitId = 1
        , sgActivePage = pkPageA, sgVisiblePages = [pkPageA]
        , sgLiveCamera = LiveCameraSnapshot
            { lcsOwnerPage = Just pkPageA, lcsX = 0, lcsY = 0
            , lcsZoom = 1, lcsFacing = FaceSouth }
        , sgPortableKnowledge = allFourStates
        }

encodeFor ∷ SessionSnapshot → BS.ByteString
encodeFor snap = encodeSessionSnapshot meta snap []
  where
    meta = snapshotSaveMetadata
        SaveRequestMeta { srmSlotName = "portable_test"
                        , srmTimestamp = "ts", srmAutosave = False }
        snap

decodeFor ∷ BS.ByteString → Either Text SessionSnapshot
decodeFor bytes =
    (\(_, snap, _, _) → snap)
        <$> decodeSessionEnvelope HS.empty HS.empty bytes

-- | Rewrite an envelope's component set: drop the portable-knowledge
--   component entirely (the pre-#2512 save shape), replace its payload
--   with bytes no version can decode, or re-declare it at a version
--   this reader does not accept.
rewriteComponents
    ∷ (ComponentDescriptor → BS.ByteString
       → Maybe (Word32, BS.ByteString))
    → BS.ByteString → BS.ByteString
rewriteComponents f bytes =
    case decodeEnvelope defaultEnvelopeLimits 1 allIds HS.empty bytes of
        Left err → error ("rewriteComponents: decode: " <> show err)
        Right de →
            let specs = [ (cdId d, ver, cdRequired d, payload)
                        | d ← emComponents (deManifest de)
                        , Just raw ← [HM.lookup (cdId d) (dePayloads de)]
                        , Just (ver, payload) ← [f d raw] ]
            in case encodeEnvelope defaultEnvelopeLimits 1 specs of
                Left err  → error ("rewriteComponents: encode: " <> show err)
                Right out → out
  where allIds = HS.insert metadataComponentId componentKnownIds

onPortable
    ∷ (Word32 → BS.ByteString → Maybe (Word32, BS.ByteString))
    → BS.ByteString → BS.ByteString
onPortable f = rewriteComponents $ \d raw →
    if cdId d ≡ portableKnowledgeComponentId
        then f (cdVersion d) raw
        else Just (cdVersion d, raw)

-- | One decodable payload carrying exactly one record, so a scalar can
--   be set to any bit pattern cereal will happily round-trip. These
--   bytes decode PERFECTLY, so only 'ccValidate' stands between them
--   and the restored map — which is precisely the case a corrupt byte
--   stream cannot produce.
dtoWith ∷ Maybe (Float, Double) → Maybe Double → PortableKnowledgeDTO
dtoWith weight revealedAt = PortableKnowledgeDTO $ HM.singleton crateId
    PortableRecordDTO
        { prdWeight   = (\(w, at) → WeightObservationDTO w at) <$> weight
        , prdContents = ContentsObservationDTO [] <$> revealedAt
        }

faultsFor ∷ Maybe (Float, Double) → Maybe Double → [ComponentError]
faultsFor weight revealedAt =
    ccValidate portableKnowledgeCodec (dtoWith weight revealedAt)

-- | The single error the validator must produce, with its structured
--   identity checked rather than merely "something was rejected".
theFault ∷ Maybe (Float, Double) → Maybe Double → IO ComponentError
theFault weight revealedAt = case faultsFor weight revealedAt of
    [e] → do
        ceComponent e `shouldBe` portableKnowledgeComponentId
        ceVersion   e `shouldBe` 1
        cePhase     e `shouldBe` ValidatePhase
        ceMessage   e `shouldSatisfy` T.isInfixOf "item #41"
        pure e
    other → do
        expectationFailure
            ("expected exactly one validation error, got: " <> show other)
        pure (ComponentError portableKnowledgeComponentId 1 ValidatePhase "")

envelopeWith ∷ Maybe (Float, Double) → Maybe Double → BS.ByteString
envelopeWith weight revealedAt = onPortable
    (\v _ → Just (v, S.encode (dtoWith weight revealedAt)))
    (encodeFor knowledgeSnapshot)

expectRejected ∷ Text → BS.ByteString → IO ()
expectRejected fault bytes = case decodeFor bytes of
    Right _  → expectationFailure
        ("a payload with " <> T.unpack fault <> " loaded anyway")
    Left err → do
        err `shouldSatisfy` T.isInfixOf "[portable-knowledge v1 ValidatePhase]"
        err `shouldSatisfy` T.isInfixOf fault

decodedKnowledge ∷ BS.ByteString → Either Text PortableKnowledge
decodedKnowledge = fmap snapPortableKnowledge ∘ decodeFor

-- Spec ---------------------------------------------------------------

spec ∷ Spec
spec = do
    describe "round-trip through the real production codec" $ do
        it "restores all four states EXACTLY, including two \
           \independently differing observation stamps on one record" $
            decodedKnowledge (encodeFor knowledgeSnapshot)
                `shouldBe` Right allFourStates

        it "keeps a present-but-empty record distinct from an absent \
           \one across the round trip -- both read never-inspected, but \
           \the map still says which is which" $ do
            let restored = decodedKnowledge (encodeFor knowledgeSnapshot)
            (HM.member looseId ∘ pkRecords <$> restored) `shouldBe` Right True
            (portableState looseId <$> restored)
                `shouldBe` Right NeverInspected
            (portableState unlocatableId <$> restored)
                `shouldBe` Right NeverInspected

    describe "the component is OPTIONAL (#2512), on #1087's terms" $ do
        it "an ABSENT payload decodes to the EMPTY map -- no crate ever \
           \hefted or opened -- which is what lets every save written \
           \before this component existed keep loading" $
            decodedKnowledge (onPortable (\_ _ → Nothing)
                                  (encodeFor knowledgeSnapshot))
                `shouldBe` Right emptyPortableKnowledge

        it "an absent payload never back-fills from anything: it does \
           \not read as known-empty, and no record appears at all" $
            case decodedKnowledge (onPortable (\_ _ → Nothing)
                                       (encodeFor knowledgeSnapshot)) of
                Left err → expectationFailure (T.unpack err)
                Right k  → do
                    knownPortableIds k `shouldBe` []
                    portableState crateId k `shouldBe` NeverInspected

        it "a PRESENT but malformed payload still fails the load exactly \
           \as a required component would -- absent and broken are \
           \different answers" $
            case decodeFor (onPortable
                    (\v _ → Just (v, "not-a-portable-knowledge-payload"))
                    (encodeFor knowledgeSnapshot)) of
                Left msg → msg `shouldSatisfy` T.isInfixOf "portable-knowledge"
                Right _  → expectationFailure
                    "a garbage portable-knowledge payload loaded anyway"

        it "a payload declared at an UNSUPPORTED version fails the load: \
           \there is no v2 to migrate from, and a reader must not guess" $
            case decodeFor (onPortable (\_ raw → Just (99, raw))
                                (encodeFor knowledgeSnapshot)) of
                Left msg → msg `shouldSatisfy` T.isInfixOf "portable-knowledge"
                Right _  → expectationFailure
                    "a portable-knowledge payload at v99 loaded anyway"

    describe "malformed remembered scalars are corruption, not gameplay" $ do
        it "accepts a well-formed record" $
            faultsFor (Just (10, 5)) (Just 5) `shouldBe` []

        it "accepts zero for every scalar -- a weightless observation at \
           \game-time 0 is a real thing that can happen" $
            faultsFor (Just (0, 0)) (Just 0) `shouldBe` []

        it "rejects a NEGATIVE remembered weight, naming it as negative" $ do
            e ← theFault (Just (-1, 5)) Nothing
            ceMessage e `shouldSatisfy` T.isInfixOf "a negative remembered weight"

        it "rejects a NaN remembered weight -- every ordered comparison \
           \against it is false, so a bare `< 0` let it through" $ do
            e ← theFault (Just (0 / 0, 5)) Nothing
            ceMessage e `shouldSatisfy`
                T.isInfixOf "a not-a-number remembered weight"

        it "rejects an INFINITE remembered weight in both directions" $ do
            ePos ← theFault (Just (1 / 0, 5)) Nothing
            ceMessage ePos `shouldSatisfy`
                T.isInfixOf "an infinite remembered weight"
            eNeg ← theFault (Just (-1 / 0, 5)) Nothing
            ceMessage eNeg `shouldSatisfy`
                T.isInfixOf "an infinite remembered weight"

        it "rejects a negative, NaN or infinite WEIGH time" $ do
            eNeg ← theFault (Just (1, -1)) Nothing
            ceMessage eNeg `shouldSatisfy` T.isInfixOf "a negative weigh time"
            eNan ← theFault (Just (1, 0 / 0)) Nothing
            ceMessage eNan `shouldSatisfy`
                T.isInfixOf "a not-a-number weigh time"
            eInf ← theFault (Just (1, 1 / 0)) Nothing
            ceMessage eInf `shouldSatisfy` T.isInfixOf "an infinite weigh time"

        it "rejects a negative, NaN or infinite REVEAL time, \
           \independently of the weigh time" $ do
            eNeg ← theFault Nothing (Just (-1))
            ceMessage eNeg `shouldSatisfy` T.isInfixOf "a negative reveal time"
            eNan ← theFault Nothing (Just (0 / 0))
            ceMessage eNan `shouldSatisfy`
                T.isInfixOf "a not-a-number reveal time"
            eInf ← theFault Nothing (Just (1 / 0))
            ceMessage eInf `shouldSatisfy` T.isInfixOf "an infinite reveal time"

        it "refuses the whole load through the REAL decoder for such a \
           \payload, publishing no snapshot at all" $ do
            expectRejected "a negative remembered weight"
                (envelopeWith (Just (-2, 5)) Nothing)
            expectRejected "an infinite reveal time"
                (envelopeWith Nothing (Just (1 / 0)))

        it "does NOT re-derive the remembered weight from the remembered \
           \contents: the weight was measured THEN, against the defs as \
           \they were then, and a crate's own mass is not in its \
           \contents list at all" $
            -- 900 kg of remembered weight against an empty contents
            -- list is perfectly valid history, not a contradiction.
            faultsFor (Just (900, 5)) (Just 5) `shouldBe` []

    describe "remembered ids are historical observations, never live \
             \entities" $ do
        it "never appear in allItemInstanceIds, so they cannot collide \
           \with a live id or a duplicate" $ do
            let ids = allItemInstanceIds knowledgeSnapshot
            ids `shouldBe` []
            (crateId `elem` ids) `shouldBe` False
            (kitId   `elem` ids) `shouldBe` False

        it "may sit ABOVE the session's own item allocator without \
           \failing validation -- the bound governs live entities, and a \
           \memory of a long-destroyed crate outranks it routinely" $ do
            snapNextItemId knowledgeSnapshot `shouldBe` 10
            maximum (knownPortableIds allFourStates)
                `shouldSatisfy` (> snapNextItemId knowledgeSnapshot)
            validateSessionSnapshot knowledgeSnapshot `shouldBe` []

        it "may OVERLAP a live id without producing a duplicate-id \
           \finding: the same crate can be both remembered and present, \
           \which is the ordinary case" $ do
            let live = (blankPageSnapshot pkPageA
                           (defaultWorldGenParams { wgpSeed = 7 }))
                    { pgsGeneratedId = Just (fixtureGeneratedWorldIdForPage pkPageA)
                    , pgsGroundItems = groundWith [crate] }
                snap = knowledgeSnapshot
                    { snapPages = HM.singleton pkPageA live
                    , snapNextItemId = 100 }
            allItemInstanceIds snap
                `shouldMatchList` [crateId, kitId, bandageId]
            validateSessionSnapshot snap `shouldBe` []

    describe "a remembered id is not a live reference (§11)" $ do
        -- The locator answering Nothing is a fact about the LIVE
        -- session; this is the separate fact about the SAVE's own
        -- reference graph, which is what a Lua component's persisted
        -- `item_instance` edge is resolved against.
        --
        -- Both snapshots below carry an allocator ABOVE every id in
        -- play, so a non-resolving edge is reported as a plain
        -- dangling reference rather than as one that outran the
        -- allocator — the two are different findings, and this block is
        -- about liveness, not about the bound.
        let refPage items = (blankPageSnapshot pkPageA
                                (defaultWorldGenParams { wgpSeed = 7 }))
                { pgsGeneratedId = Just (fixtureGeneratedWorldIdForPage pkPageA)
                , pgsGroundItems = groundWith items }
            refSnapshot items = knowledgeSnapshot
                { snapPages = HM.singleton pkPageA (refPage items)
                , snapNextItemId = 100
                , snapPortableKnowledge = PortableKnowledge $
                    HM.singleton crateId (PortableRecord
                        (Just (WeightObservation 31.25 1200))
                        (Just (ContentsObservation [kit] 950.25)))
                }
            -- The kit is REMEMBERED inside the crate's record, and
            -- nothing live carries it.
            rememberedOnly = refSnapshot []
            -- The same kit, now genuinely lying on the ground.
            alsoLive = refSnapshot [kit]
            keOf snap = knownEntitiesFromSaveData
                (snapshotToSaveData
                    (SaveRequestMeta "portable_refs" "ts" False) snap)
            itemEdge iid = LuaRefEdge
                { lreComponent = "unit_ai", lreKind = "item_instance"
                , lreId = fromIntegral iid, lreOwner = Nothing
                , lrePath = "job.carrying", lrePage = Nothing }

        it "a remembered-only instance is absent from the save's \
           \known-entity set, so nothing can resolve against it" $ do
            HS.member (fromIntegral kitId)
                (keItemInstances (keOf rememberedOnly)) `shouldBe` False
            HS.member (fromIntegral bandageId)
                (keItemInstances (keOf rememberedOnly)) `shouldBe` False

        it "a Lua item_instance edge naming a remembered-only id is \
           \reported as a tolerated DANGLING reference -- remembering a \
           \crate must never make a stale live reference look live" $
            case luaReferenceErrors HM.empty (keOf rememberedOnly)
                     [itemEdge kitId] of
                [e] → do
                    ieCode e `shouldBe` "dangling-reference"
                    ieRefValue e `shouldBe` tshow kitId
                other → expectationFailure
                    ("expected exactly one dangling-reference finding, got: "
                     <> show other)

        it "…while the SAME id and the SAME edge resolve once the item \
           \is genuinely live, so the case above is about liveness \
           \rather than about the edge being malformed" $ do
            HS.member (fromIntegral kitId)
                (keItemInstances (keOf alsoLive)) `shouldBe` True
            luaReferenceErrors HM.empty (keOf alsoLive) [itemEdge kitId]
                `shouldBe` []
            -- …and the record is present either way, so what changed is
            -- the live enumeration and nothing else.
            knownPortableIds (snapPortableKnowledge alsoLive)
                `shouldBe` [crateId]
            knownPortableIds (snapPortableKnowledge rememberedOnly)
                `shouldBe` [crateId]

    describe "remembered DEF NAMES are ordinary content references" $ do
        it "names a remembered item whose definition is no longer \
           \registered, recursively through nesting" $ do
            let refs = missingPortableItemDefReferences
                           (HS.fromList ["supply_crate"]) allFourStates
            map mpdrDefName refs
                `shouldMatchList` ["first_aid_kit", "bandage"]
            map mpdrOwnerId refs `shouldMatchList` [crateId, crateId]

        it "reports nothing when every remembered def resolves" $
            missingPortableItemDefReferences
                (HS.fromList ["supply_crate", "first_aid_kit", "bandage"])
                allFourStates
                `shouldBe` []

        it "checks a record whose OWNER is not live either -- the \
           \dangling-owner scrub is a later, tolerated step, so letting \
           \it run first would silently discard the very records this \
           \rejection is about" $ do
            let orphaned = PortableKnowledge $ HM.singleton unlocatableId
                    (PortableRecord Nothing
                        (Just (ContentsObservation [kit] 5)))
                refs = missingPortableItemDefReferences
                           (HS.fromList ["supply_crate"]) orphaned
            map mpdrDefName refs
                `shouldMatchList` ["first_aid_kit", "bandage"]
            map mpdrOwnerId refs `shouldMatchList`
                [unlocatableId, unlocatableId]

    describe "the load-boundary scrub" $ do
        it "keeps a record whose crate is live in the REPLACEMENT \
           \session and drops one whose crate is not, reporting exactly \
           \which ids it dropped" $ do
            let liveIds = HS.fromList [crateId, kitId, bandageId]
            knownPortableIds (retainPortables liveIds allFourStates)
                `shouldBe` [crateId]
            prunedPortableIds liveIds allFourStates
                `shouldMatchList` [looseId, 7001, 7002]

        it "is decided by the incoming session's live items, not by \
           \whether the record looks well-formed: a perfectly valid \
           \known-contents record for an absent crate still goes" $ do
            let one = PortableKnowledge $ HM.singleton crateId
                    (PortableRecord (Just (WeightObservation 1 1))
                                    (Just (ContentsObservation [kit] 1)))
            prunedPortableIds HS.empty one `shouldBe` [crateId]
            knownPortableIds (retainPortables HS.empty one) `shouldBe` []
