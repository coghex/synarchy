-- | The opaque persistent identity of a GENERATED WORLD (#2021, epic
--   #2017 / WML-3): 'World.Page.GeneratedId.GeneratedWorldId'.
--
--   The contract this proves, in one sentence: every generated world
--   page carries exactly one opaque id, assigned once at creation,
--   persisted authoritatively in @world-pages@ v9 and copied into
--   @"metadata"@ v3 for cheap scanning, never derived from anything
--   about the world, and never shared between two independently created
--   worlds even when their seed and parameters are identical.
--
--   The pure half needs no engine: the allocator, the two component
--   versions and every historical one they still decode, the
--   cross-component consistency check and its four failure shapes, and
--   the listing-depth read. The boundary half gets its OWN engine — it
--   creates private w8 pages and saves them, which the shared-worlds
--   engine must not gain, exactly as "Test.Headless.World.Identity" is
--   isolated for the same reason.
module Test.Headless.World.GeneratedIdentity (spec, pureSpec) where

import UPrelude
import Test.Hspec
import Control.Concurrent (threadDelay)
import Control.Exception (finally)
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.List as L
import qualified Data.Serialize as S
import qualified Data.Text as T
import Data.IORef (readIORef, writeIORef)
import Data.List (find)
import System.Directory (doesFileExist, removePathForcibly)
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Log (LoggerState)
import World.Material (MaterialRegistry)
import Test.Headless.Harness (sendWorldCommand, waitForWorldInit)
import Test.Headless.Harness.GeneratedIds (fixtureGeneratedWorldIdForPage)
import World.Types
import World.Page.GeneratedId
    (GeneratedWorldId, newGeneratedWorldId, renderGeneratedWorldId)
import World.Load.Stage (stageSession, renderStageError)
import World.Load.Types (StagedSession(..), StagedPage(..))
import World.Save.Compat.MetadataV1 (SaveMetadataV1(..))
import World.Save.Compat.MetadataV2 (SaveMetadataV2(..))
import World.Save.Component
    (metadataErrors, encodeComponentSpecs, saveComponentRegistry)
import World.Save.Component.Page
import World.Save.Component.Types
    (ComponentError(..), ComponentPhase(..), ComponentCodec(..)
    , RegisteredComponent(..), worldPagesComponentId
    , renderComponentError)
import World.Save.Envelope
    ( decodeSaveEnvelopeMetadata, decodeSessionEnvelope
    , metadataComponentId, metadataComponentVersion
    , legacyMetadataComponentVersion, predecessorMetadataComponentVersion
    , currentEnvelopeVersion, encodeSessionSnapshot )
import World.Save.Envelope.Codec (encodeEnvelope)
import World.Save.Envelope.Types (defaultEnvelopeLimits)
import World.Save.Serialize (loadWorld, listSaves, SaveListing(..))
import World.Save.Snapshot
import World.Save.Snapshot.Adapter
    (SaveRequestMeta(..), snapshotSaveMetadata, snapshotToSaveData)
import World.Render.Zoom.Types (ZoomMapMode(..))
import Structure.Palette (emptyTexPalette)
import Engine.Graphics.Camera (CameraFacing(..))

-- The two pages the engine half creates. Same seed, same world size,
-- same plate count, different page ids: requirement 3's "independently
-- creating a world with the same seed and parameters yields a DIFFERENT
-- id" is only a real claim if the inputs really are identical.
twinPageA, twinPageB ∷ WorldPageId
twinPageA = WorldPageId "gwid_twin_a_w8"
twinPageB = WorldPageId "gwid_twin_b_w8"

twinSeed ∷ Word64
twinSeed = 4242

-- | A display name whose text deliberately contains the seed and both
--   page ids, so "the id is not derivable from the display name" is
--   checked against a name that would make a derived id LOOK plausible.
twinIdentity ∷ WorldIdentity
twinIdentity =
    WorldIdentity "gwid_twin_a_w8 4242 gwid_twin_b_w8" Nothing Nothing Nothing

-- Pure fixtures ------------------------------------------------------

pageOne, pageTwo ∷ WorldPageId
pageOne = WorldPageId "gwid_page1"
pageTwo = WorldPageId "gwid_page2"

idOne, idTwo ∷ GeneratedWorldId
idOne = fixtureGeneratedWorldIdForPage pageOne
idTwo = fixtureGeneratedWorldIdForPage pageTwo

-- | A page core at the CURRENT (v9) wire shape.
coreV9 ∷ WorldPageId → Maybe GeneratedWorldId → PageCoreDTO
coreV9 pid gid = PageCoreDTO
    { pcPageId = pid, pcGenParams = toWorldGenParamsDTO defaultWorldGenParams
    , pcCameraX = 0, pcCameraY = 0, pcTimeHour = 0, pcTimeMinute = 0
    , pcDateYear = 1, pcDateMonth = 1, pcDateDay = 1, pcMapMode = ZMDefault
    , pcIdentity = Nothing, pcGeneratedId = gid }

-- | The same page at the frozen v8 shape — the newest version that has
--   no generated-world id at all.
coreV8 ∷ WorldPageId → PageCoreDTOv8
coreV8 pid = PageCoreDTOv8
    { pc8PageId = pid, pc8GenParams = toWorldGenParamsDTOv7 defaultWorldGenParams
    , pc8CameraX = 0, pc8CameraY = 0, pc8TimeHour = 0, pc8TimeMinute = 0
    , pc8DateYear = 1, pc8DateMonth = 1, pc8DateDay = 1, pc8MapMode = ZMDefault
    , pc8Identity = Nothing }

decodeWorldPages ∷ Word32 → BS.ByteString → Either ComponentError WorldPages
decodeWorldPages = ccDecodeWorldPages
  where ccDecodeWorldPages = ccDecode worldPagesCodec

-- | Decode a @world-pages@ payload at @version@ and then validate it,
--   exactly as 'World.Save.Component.decodeComponentValue' does.
decodeThenValidate
    ∷ Word32 → BS.ByteString → Either ComponentError [ComponentError]
decodeThenValidate v bytes = ccValidate worldPagesCodec <$> decodeWorldPages v bytes

-- | A minimal snapshot whose pages carry the given ids. The first pair
--   is the ACTIVE page, named explicitly rather than taken off the list,
--   so the fixture cannot be handed an empty page set.
snapshotWith
    ∷ (WorldPageId, Maybe GeneratedWorldId)
    → [(WorldPageId, Maybe GeneratedWorldId)] → SessionSnapshot
snapshotWith active rest =
    buildSessionSnapshot globals (map mkPage (active : rest))
  where
    globals = SessionGlobals
        { sgGameTime = 0, sgTexPalette = emptyTexPalette
        , sgNextItemId = 1, sgNextBuildingId = 1, sgNextUnitId = 1
        , sgActivePage = fst active
        , sgVisiblePages = [fst active]
        , sgLiveCamera = LiveCameraSnapshot
            { lcsOwnerPage = Nothing, lcsX = 0, lcsY = 0
            , lcsZoom = 1, lcsFacing = FaceSouth }
        }
    mkPage (pid, gid) =
        (blankPageSnapshot pid defaultWorldGenParams) { pgsGeneratedId = gid }

-- | The metadata a save of @snap@ would carry, with the slot/timestamp
--   a request supplies.
metaFor ∷ SessionSnapshot → SaveMetadata
metaFor = snapshotSaveMetadata SaveRequestMeta
    { srmSlotName = "gwid-slot", srmTimestamp = "2026-09-01T00:00:00.000000Z"
    , srmAutosave = False }

pureSpec ∷ Spec
pureSpec = describe "generated world identity (#2021)" $ do

    describe "the allocator" $ do
        it "never repeats: a thousand consecutive allocations are all \
           \distinct, which is the whole contract a durable library key \
           \rests on" $ do
            ids ← replicateM 1000 newGeneratedWorldId
            length (L.nub ids) `shouldBe` 1000

        it "is not a function of anything: two allocations made with \
           \identical surroundings still differ, so no content digest \
           \could be substituted for it (requirement 3, design decision \
           \D-17)" $ do
            a ← newGeneratedWorldId
            b ← newGeneratedWorldId
            a `shouldNotBe` b

        it "renders as a fixed-width opaque token, stable for the life \
           \of an id" $ do
            gid ← newGeneratedWorldId
            T.length (renderGeneratedWorldId gid) `shouldBe` 32
            renderGeneratedWorldId gid `shouldBe` renderGeneratedWorldId gid

    describe "world-pages v9" $ do
        it "round-trips a page's id byte-exactly through the real codec" $
            case decodeWorldPages 9 (S.encode (WorldPagesDTO [coreV9 pageOne (Just idOne)])) of
                Left e   → expectationFailure (T.unpack (renderComponentError e))
                Right wp → map pgsGeneratedId (HM.elems (wpBase wp))
                               `shouldBe` [Just idOne]

        it "REFUSES a v9 payload whose page carries no id — the writer \
           \had one and did not write it, which is corruption, not the \
           \legacy shape" $
            case decodeThenValidate 9 (S.encode (WorldPagesDTO [coreV9 pageOne Nothing])) of
                Left e   → expectationFailure (T.unpack (renderComponentError e))
                Right [] → expectationFailure
                    "a v9 payload with no generated-world id was ACCEPTED"
                Right es → do
                    map cePhase es `shouldBe` [ValidatePhase]
                    map ceComponent es `shouldBe` [worldPagesComponentId]
                    T.unpack (T.concat (map ceMessage es))
                        `shouldContain` "carries no generated-world id"

        it "refuses two pages naming the SAME generated foundation — no \
           \engine path can produce it, and later slices key durable \
           \artifacts by the id" $
            case decodeThenValidate 9 (S.encode (WorldPagesDTO
                    [coreV9 pageOne (Just idOne), coreV9 pageTwo (Just idOne)])) of
                Left e   → expectationFailure (T.unpack (renderComponentError e))
                Right [] → expectationFailure
                    "two pages sharing one generated-world id were ACCEPTED"
                Right es →
                    T.unpack (T.concat (map ceMessage es))
                        `shouldContain` "duplicate generated-world id"

        it "accepts a MIGRATED v8 payload with no id — absence is that \
           \format's answer, and load staging is what fills it \
           \(requirement 7)" $
            case decodeThenValidate 8 (S.encode (WorldPagesDTOv8 [coreV8 pageOne])) of
                Left e    → expectationFailure (T.unpack (renderComponentError e))
                Right es  → do
                    es `shouldBe` []
                    case decodeWorldPages 8 (S.encode (WorldPagesDTOv8 [coreV8 pageOne])) of
                        Left e   → expectationFailure
                                       (T.unpack (renderComponentError e))
                        Right wp → map pgsGeneratedId (HM.elems (wpBase wp))
                                       `shouldBe` [Nothing]

        it "still accepts every historical version — v1 through v9 all \
           \decode, so the bump added a reader rather than replacing one" $
            worldPagesInputVersions `shouldBe` [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

    describe "metadata v3" $ do
        it "still accepts every historical version — v1 and v2 both \
           \decode through their own frozen mirror" $ do
            legacyMetadataComponentVersion `shouldBe` 1
            predecessorMetadataComponentVersion `shouldBe` 2
            metadataComponentVersion `shouldBe` 3

        it "decodes a v1 payload with an EMPTY inventory rather than an \
           \invented one" $ do
            let meta = metadataV1 { sm1Name = "legacy-v1" }
            case decodeMetaOnly legacyMetadataComponentVersion (S.encode meta) of
                Left err → expectationFailure (T.unpack err)
                Right m  → do
                    smName m `shouldBe` "legacy-v1"
                    smAutosave m `shouldBe` False
                    smGeneratedWorldIds m `shouldBe` []

        it "decodes a v2 payload with an EMPTY inventory, keeping its \
           \autosave classification — proving v2 is read against v2's \
           \own field list, not v1's widened" $ do
            let meta = metadataV2 { sm2Name = "legacy-v2", sm2Autosave = True }
            case decodeMetaOnly predecessorMetadataComponentVersion (S.encode meta) of
                Left err → expectationFailure (T.unpack err)
                Right m  → do
                    smName m `shouldBe` "legacy-v2"
                    smAutosave m `shouldBe` True
                    smGeneratedWorldIds m `shouldBe` []

        it "carries EVERY page's id, not just the active one, in a \
           \canonical duplicate-free order" $ do
            let snap = snapshotWith (pageOne, Just idOne) [(pageTwo, Just idTwo)]
            smGeneratedWorldIds (metaFor snap)
                `shouldBe` L.sort [idOne, idTwo]

    describe "the metadata copy and the authoritative pages agree" $ do
        let snap = snapshotWith (pageOne, Just idOne) [(pageTwo, Just idTwo)]
            meta = metaFor snap

        it "a save this build writes is consistent" $
            metadataErrors meta snap `shouldBe` []

        it "a wholly pre-#2021 save is consistent too — no page has an \
           \id and the inventory is empty" $ do
            let legacy = snapshotWith (pageOne, Nothing) [(pageTwo, Nothing)]
            metadataErrors (metaFor legacy) legacy `shouldBe` []

        it "rejects a MISSING id — the inventory is short by one" $
            metadataErrors meta { smGeneratedWorldIds = [idOne] } snap
                `shouldSatisfy` (not . null)

        it "rejects an EXTRA id naming no page in the save" $ do
            extra ← newGeneratedWorldId
            metadataErrors
                meta { smGeneratedWorldIds = L.sort [idOne, idTwo, extra] } snap
                `shouldSatisfy` (not . null)

        it "rejects a DUPLICATE id in the inventory" $
            metadataErrors meta { smGeneratedWorldIds = [idOne, idOne] } snap
                `shouldSatisfy` (not . null)

        it "rejects a SUBSTITUTED id — same count, wrong value, which a \
           \length check alone would miss" $ do
            other ← newGeneratedWorldId
            metadataErrors
                meta { smGeneratedWorldIds = L.sort [idOne, other] } snap
                `shouldSatisfy` (not . null)

        it "rejects a save where only SOME pages carry an id — no writer \
           \and no migration can produce that shape" $ do
            let mixed = snapshotWith (pageOne, Just idOne) [(pageTwo, Nothing)]
            metadataErrors (metaFor mixed) mixed `shouldSatisfy` (not . null)

    describe "listing-depth reads (requirement 6)" $ do
        let snap = snapshotWith (pageOne, Just idOne) [(pageTwo, Just idTwo)]
            meta = metaFor snap

        it "reports every id from the metadata component alone" $
            case decodeSaveEnvelopeMetadata HS.empty
                     (encodeSessionSnapshot meta snap []) of
                Left err → expectationFailure (T.unpack err)
                Right m  → smGeneratedWorldIds m
                               `shouldBe` L.sort [idOne, idTwo]

        it "still reports them when the world-pages payload is \
           \UNDECODABLE — which is what lets reference-aware cleanup \
           \prove a damaged save still references its foundations" $ do
            let corrupted = withCorruptWorldPages meta snap
            -- The full decode really is broken…
            decodeSessionEnvelope HS.empty HS.empty corrupted
                `shouldSatisfy` isLeftE
            -- …and the listing-depth read is unaffected.
            case decodeSaveEnvelopeMetadata HS.empty corrupted of
                Left err → expectationFailure (T.unpack err)
                Right m  → smGeneratedWorldIds m
                               `shouldBe` L.sort [idOne, idTwo]

    describe "opacity (requirement 1/2)" $
        it "is not equal to, and shares no text with, the display name, \
           \the seed, the page id or the slot name" $ do
            gid ← newGeneratedWorldId
            let token = renderGeneratedWorldId gid
                candidates =
                    [ unWorldPageId pageOne
                    , "gwid-slot"
                    , tshow (wgpSeed defaultWorldGenParams)
                    , "Aldermoor Deep" ]
            forM_ candidates $ \c → do
                token `shouldNotBe` c
                T.unpack c `shouldNotContain` T.unpack token

-- | Every version the @world-pages@ reader advertises, which
--   'componentCodec' derives from the same declarations it dispatches
--   on — so this really is "what still decodes", not a restatement.
worldPagesInputVersions ∷ [Word32]
worldPagesInputVersions =
    case find ((≡ worldPagesComponentId) . rcId) saveComponentRegistry of
        Just c  → rcInputVers c
        Nothing → error "world-pages is not in the component registry"

-- | Decode a @"metadata"@ payload at a chosen version through the real
--   envelope path. The metadata component is spliced into an otherwise
--   ordinary, complete envelope — a listing read must not be able to
--   pass on a structurally short file, so the envelope really does carry
--   every required gameplay component.
decodeMetaOnly ∷ Word32 → BS.ByteString → Either Text SaveMetadata
decodeMetaOnly version payload =
    case encodeEnvelope defaultEnvelopeLimits currentEnvelopeVersion specs of
        Left e      → Left (tshow e)
        Right bytes → decodeSaveEnvelopeMetadata HS.empty bytes
  where
    specs = (metadataComponentId, version, True, payload)
          : encodeComponentSpecs (snapshotWith (pageOne, Just idOne) [])

-- | The same envelope a real save writes, except that the
--   @world-pages@ payload is replaced by bytes no reader can decode.
withCorruptWorldPages ∷ SaveMetadata → SessionSnapshot → BS.ByteString
withCorruptWorldPages meta snap =
    case encodeEnvelope defaultEnvelopeLimits currentEnvelopeVersion specs of
        Right bytes → bytes
        Left e      → error ("test setup: " <> show e)
  where
    specs = (metadataComponentId, metadataComponentVersion, True, S.encode meta)
          : [ if cid ≡ worldPagesComponentId
                then (cid, ver, req, BS.pack [0xFF, 0xFF, 0xFF])
                else (cid, ver, req, payload)
            | (cid, ver, req, payload) ← encodeComponentSpecs snap ]

metadataV1 ∷ SaveMetadataV1
metadataV1 = SaveMetadataV1
    { sm1Name = "legacy", sm1Seed = 42, sm1WorldSize = 8, sm1PlateCount = 3
    , sm1Timestamp = "2026-09-01T00:00:00.000000Z"
    , sm1WorldName = Nothing, sm1WorldGloss = Nothing }

metadataV2 ∷ SaveMetadataV2
metadataV2 = SaveMetadataV2
    { sm2Name = "legacy", sm2Seed = 42, sm2WorldSize = 8, sm2PlateCount = 3
    , sm2Timestamp = "2026-09-01T00:00:00.000000Z"
    , sm2WorldName = Nothing, sm2WorldGloss = Nothing, sm2Autosave = False }

isLeftE ∷ Either a b → Bool
isLeftE (Left _) = True
isLeftE _        = False

-- Engine-backed coverage ---------------------------------------------

spec ∷ SpecWith EngineEnv
spec = describe "generated world identity, at the engine boundary (#2021)" $ do

    it "gives two worlds created from the SAME seed, size and plate \
       \count DIFFERENT ids — there is no deduplication by content \
       \(requirement 3)" $ \env → do
        sendWorldCommand env
            (WorldInit twinPageA twinSeed 8 3 (Just twinIdentity))
        _ ← waitForWorldInit env twinPageA 120
        sendWorldCommand env
            (WorldInit twinPageB twinSeed 8 3 (Just twinIdentity))
        _ ← waitForWorldInit env twinPageB 120
        gidA ← liveGeneratedId env twinPageA
        gidB ← liveGeneratedId env twinPageB
        gidA `shouldNotBe` gidB

    it "is unaffected by the page's display identity, and shares no \
       \text with the name, the seed, the page id or the slot name — \
       \a derived id would collide on inputs this deliberately repeats" $
        \env → do
        _ ← waitForWorldInit env twinPageA 120
        gid ← liveGeneratedId env twinPageA
        let token = renderGeneratedWorldId gid
        forM_ [ wiName twinIdentity, unWorldPageId twinPageA
              , tshow twinSeed, T.pack twinSlot ] $ \c →
            T.unpack c `shouldNotContain` T.unpack token

    it "survives an ordinary save and a load-staging round trip \
       \unchanged, with the metadata copy agreeing with the \
       \authoritative per-page value and the listing reporting both \
       \pages (requirements 4 and 6)" $ \env →
        let cleanup = do
                removePathForcibly ("saves/" <> twinSlot)
                writeIORef (enginePausedRef env) False
        in (`finally` cleanup) $ do
        removePathForcibly ("saves/" <> twinSlot)
        _ ← waitForWorldInit env twinPageA 120
        _ ← waitForWorldInit env twinPageB 120
        liveA ← liveGeneratedId env twinPageA
        liveB ← liveGeneratedId env twinPageB

        sendWorldCommand env
            (WorldSave twinPageA (T.pack twinSlot)
                       "2026-09-01T00:00:00.000000Z" [] [] Nothing)
        waitForFile ("saves/" <> twinSlot <> "/world.synworld")

        logger ← readIORef (loggerRef env)
        (sd, _, _) ← loadWorld logger (T.pack twinSlot) HS.empty HS.empty ⌦ either
            (\(_, e) → expectationFailure (T.unpack e) ≫ error "unreachable")
            pure

        -- Authoritative, per page, byte-exact.
        savedGeneratedId sd twinPageA `shouldBe` Just liveA
        savedGeneratedId sd twinPageB `shouldBe` Just liveB
        -- The metadata copy is the SAME set — every page in the save,
        -- not just the active one.
        smGeneratedWorldIds (sdMetadata sd)
            `shouldContain` [liveA]
        smGeneratedWorldIds (sdMetadata sd)
            `shouldContain` [liveB]
        smGeneratedWorldIds (sdMetadata sd)
            `shouldBe` L.sort (L.nub (savedGeneratedIds sd))

        -- Readable at listing depth, with no gameplay component decoded.
        listings ← listSaves logger HS.empty
        case find ((≡ T.pack twinSlot) . slName) listings of
            Nothing → expectationFailure
                ("listSaves did not report " <> twinSlot)
            Just listed → do
                smGeneratedWorldIds (slMetadata listed)
                    `shouldContain` [liveA]
                smGeneratedWorldIds (slMetadata listed)
                    `shouldContain` [liveB]

        -- Staging restores each page's own saved id.
        matReg ← readIORef (materialRegistryRef env)
        staged ← stageSession env logger sd matReg ⌦ either
            (\e → expectationFailure (T.unpack (renderStageError e))
                    ≫ error "unreachable")
            pure
        stagedGeneratedId staged twinPageA `shouldReturn` Just liveA
        stagedGeneratedId staged twinPageB `shouldReturn` Just liveB

    it "assigns a compatible pre-#2021 page a FRESH id at staging, \
       \twice over, without ever writing one back to the source save \
       \(requirements 7 and 8, design decision D-21)" $ \env →
        let cleanup = removePathForcibly legacySourcePath
        in (`finally` cleanup) $ do
        -- A GENUINE v8 / metadata-v2 envelope, built from this engine's
        -- own live pages: the current save re-encoded with @world-pages@
        -- written through the frozen 'PageCoreDTOv8' and @"metadata"@
        -- through the frozen 'SaveMetadataV2'. That is the newest shape
        -- that predates generated-world identity, and building it from a
        -- real session (rather than reusing a tracked fixture) keeps its
        -- content definitions ones this engine actually has registered.
        _ ← waitForWorldInit env twinPageA 120
        _ ← waitForWorldInit env twinPageB 120
        logger ← readIORef (loggerRef env)
        matReg ← readIORef (materialRegistryRef env)
        live   ← liveSessionSnapshot env [twinPageA, twinPageB]
        BS.writeFile legacySourcePath (asLegacyV8Envelope live)

        before' ← BS.readFile legacySourcePath
        sd ← decodeLegacySave before'
        -- The source really is pre-#2021: it names no generated world,
        -- on either side.
        savedGeneratedIds sd `shouldBe` []
        smGeneratedWorldIds (sdMetadata sd) `shouldBe` []

        firstIds  ← stagedIdsOf env logger matReg sd
        secondIds ← stagedIdsOf env logger matReg sd

        -- Every page got one…
        length firstIds `shouldBe` length (sdWorlds sd)
        firstIds `shouldSatisfy` all isJust
        -- …distinct from each other, so two pages never share a
        -- foundation…
        L.nub firstIds `shouldBe` firstIds
        -- …a DIFFERENT set each time, which is D-21's accepted
        -- behaviour rather than a defect…
        firstIds `shouldNotBe` secondIds
        -- …and the file on disk is byte-identical: loading never
        -- rewrites its source, so the earlier ids simply belong to a
        -- session nobody saved.
        after' ← BS.readFile legacySourcePath
        after' `shouldBe` before'
        sdAfter ← decodeLegacySave after'
        savedGeneratedIds sdAfter `shouldBe` []
        smGeneratedWorldIds (sdMetadata sdAfter) `shouldBe` []

-- | Where the engine half writes its hand-built legacy envelope. Under
--   @saves/@, the directory these specs already write into, and removed
--   again by the example that creates it.
legacySourcePath ∷ FilePath
legacySourcePath = "saves/gwid_legacy_v8_source.bin"

-- | Decode a save envelope back into the transitional 'SaveData' shape
--   the load path consumes, carrying the file's own request metadata.
decodeLegacySave ∷ BS.ByteString → IO SaveData
decodeLegacySave bytes = case decodeSessionEnvelope HS.empty HS.empty bytes of
    Left err → expectationFailure (T.unpack err) ≫ error "unreachable"
    Right (meta, snap, _, _) → pure (snapshotToSaveData (request meta) snap)
  where
    request meta = SaveRequestMeta
        { srmSlotName = smName meta, srmTimestamp = smTimestamp meta
        , srmAutosave = smAutosave meta }

-- | Capture the named live pages into a snapshot, by saving and decoding
--   through the REAL production path — so the legacy envelope built from
--   it is a re-encoding of a genuine session rather than a hand-assembled
--   one.
liveSessionSnapshot
    ∷ EngineEnv → [WorldPageId] → IO (SaveMetadata, SessionSnapshot)
liveSessionSnapshot env pages = case pages of
    []      → expectationFailure "no pages requested" ≫ error "unreachable"
    (p : _) → do
        let slot = "gwid_legacy_capture"
        removePathForcibly ("saves/" <> T.unpack slot)
        sendWorldCommand env
            (WorldSave p slot "2026-09-01T00:00:00.000000Z" [] [] Nothing)
        waitForFile ("saves/" <> T.unpack slot <> "/world.synworld")
        writeIORef (enginePausedRef env) False
        bytes ← BS.readFile ("saves/" <> T.unpack slot <> "/world.synworld")
        removePathForcibly ("saves/" <> T.unpack slot)
        case decodeSessionEnvelope HS.empty HS.empty bytes of
            Left err → expectationFailure (T.unpack err) ≫ error "unreachable"
            Right (meta, snap, _, _) → pure (meta, snap)

-- | Re-encode a captured session as the frozen pre-#2021 shape:
--   @world-pages@ at v8 and @"metadata"@ at v2, every other component
--   exactly as the current build writes it. This is the one place a test
--   deliberately writes a historical version, and it goes through the
--   SAME frozen DTOs the reader migrates from.
asLegacyV8Envelope ∷ (SaveMetadata, SessionSnapshot) → BS.ByteString
asLegacyV8Envelope (meta, snap) =
    case encodeEnvelope defaultEnvelopeLimits currentEnvelopeVersion specs of
        Right bytes → bytes
        Left e      → error ("test setup: " <> show e)
  where
    specs = (metadataComponentId, predecessorMetadataComponentVersion, True
            , S.encode (asMetadataV2 meta))
          : [ if cid ≡ worldPagesComponentId
                then (cid, 8, req, S.encode pagesV8)
                else (cid, ver, req, payload)
            | (cid, ver, req, payload) ← encodeComponentSpecs snap ]
    pagesV8 = WorldPagesDTOv8
        (map toCoreV8 (L.sortOn pgsPageId (HM.elems (snapPages snap))))
    toCoreV8 p = PageCoreDTOv8
        { pc8PageId    = pgsPageId p
        , pc8GenParams = toWorldGenParamsDTOv7 (pgsGenParams p)
        , pc8CameraX   = pgsCameraX p
        , pc8CameraY   = pgsCameraY p
        , pc8TimeHour  = pgsTimeHour p
        , pc8TimeMinute = pgsTimeMinute p
        , pc8DateYear  = pgsDateYear p
        , pc8DateMonth = pgsDateMonth p
        , pc8DateDay   = pgsDateDay p
        , pc8MapMode   = pgsMapMode p
        , pc8Identity  = toWorldIdentityDTO <$> pgsIdentity p
        }

asMetadataV2 ∷ SaveMetadata → SaveMetadataV2
asMetadataV2 m = SaveMetadataV2
    { sm2Name = smName m, sm2Seed = smSeed m, sm2WorldSize = smWorldSize m
    , sm2PlateCount = smPlateCount m, sm2Timestamp = smTimestamp m
    , sm2WorldName = smWorldName m, sm2WorldGloss = smWorldGloss m
    , sm2Autosave = smAutosave m }

-- | The slot the engine half saves into.
twinSlot ∷ String
twinSlot = "gwid_spec_roundtrip"

-- | The id a LIVE page is carrying.
liveGeneratedId ∷ EngineEnv → WorldPageId → IO GeneratedWorldId
liveGeneratedId env pid = do
    mgr ← readIORef (worldManagerRef env)
    case lookup pid (wmWorlds mgr) of
        Nothing → expectationFailure
                      ("no live page " ⧺ T.unpack (unWorldPageId pid))
                  ≫ error "unreachable"
        Just ws → readIORef (wsGeneratedIdRef ws)

-- | The id stored for one saved page.
savedGeneratedId ∷ SaveData → WorldPageId → Maybe GeneratedWorldId
savedGeneratedId sd pid =
    case filter ((≡ pid) . wpsPageId) (sdWorlds sd) of
        (w:_) → wpsGeneratedId w
        []    → Nothing

-- | Every id the save's pages carry, in page order.
savedGeneratedIds ∷ SaveData → [GeneratedWorldId]
savedGeneratedIds sd = [ gid | w ← sdWorlds sd, Just gid ← [wpsGeneratedId w] ]

-- | The id a staged (never published) page carries.
stagedGeneratedId
    ∷ StagedSession → WorldPageId → IO (Maybe GeneratedWorldId)
stagedGeneratedId staged pid =
    case find ((≡ pid) . spPageId) (ssPages staged) of
        Nothing → pure Nothing
        Just p  → Just <$> readIORef (wsGeneratedIdRef (spWorldState p))

-- | Stage a decoded save and report each page's staged id, in the saved
--   pages' own order so two runs are compared like for like.
stagedIdsOf
    ∷ EngineEnv → LoggerState → MaterialRegistry → SaveData
    → IO [Maybe GeneratedWorldId]
stagedIdsOf env logger registry sd = do
    staged ← stageSession env logger sd registry ⌦ either
        (\e → expectationFailure (T.unpack (renderStageError e))
                ≫ error "unreachable")
        pure
    forM (map wpsPageId (sdWorlds sd)) (stagedGeneratedId staged)

-- | Poll until the world thread has written the save file. Fails after
--   ~30 s.
waitForFile ∷ FilePath → IO ()
waitForFile path = go (300 ∷ Int)
  where
    go 0 = expectationFailure $ "save file never appeared: " ⧺ path
    go n = do
        exists ← doesFileExist path
        if exists then pure () else threadDelay 100000 ≫ go (n - 1)
