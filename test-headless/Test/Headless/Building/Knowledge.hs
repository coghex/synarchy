-- | "Container knowledge" (#1087, epic #1013 phase A3): the player's
--   persisted, deliberately-stale view of what each container holds.
--
--   Everything here runs against freshly-made 'IORef's rather than a
--   booted engine — 'Building.Knowledge.Live.ContainerObserver' is the
--   narrow live view every reveal trigger takes, so the REAL trigger
--   code paths are exercised directly, not re-implemented. The
--   persistence half goes through the REAL production codec
--   ('encodeSessionSnapshot' / 'decodeSessionEnvelope'), the same pair
--   "Test.Headless.World.Save.Contract" uses.
--
--   Run just this gate:
--   @cabal test synarchy-test-headless --test-options='--match "Container knowledge"'@
module Test.Headless.Building.Knowledge (spec) where

import UPrelude
import Test.Hspec
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Map.Strict as Map
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef')
import Building.Knowledge
import Building.Knowledge.Live
import Building.Types
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Graphics.Camera (CameraFacing(..))
import Item.Types
    (ItemDef(..), ItemInstance(..), ItemManager(..), ItemContainer(..)
    , itemTotalWeight)
import Structure.Palette (emptyTexPalette)
import Unit.Direction (Direction(..))
import Unit.Faction (Faction(..))
import Unit.Types (UnitId(..), UnitInstance(..), UnitManager(..), emptyUnitManager)
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)
import World.Page.Types (WorldPageId(..))
import World.Save.Component (componentKnownIds)
import World.Save.Component.Page (blankPageSnapshot)
import World.Save.Component.Types (metadataComponentId)
import World.Save.Envelope (encodeSessionSnapshot, decodeSessionEnvelope)
import World.Save.Envelope.Codec
    (DecodedEnvelope(..), decodeEnvelope, encodeEnvelope)
import World.Save.Envelope.Types
    ( ComponentDescriptor(..), ComponentId(..), EnvelopeManifest(..)
    , defaultEnvelopeLimits )
import World.Save.Snapshot
import World.Save.Snapshot.Adapter (SaveRequestMeta(..), snapshotSaveMetadata)
import World.Save.Types (BuildingSnapshot(..), missingItemDefReferences)
import World.Save.Snapshot.Adapter (snapshotToSaveData)
import World.Save.Types (SaveData(..), WorldPageSave(..))
import World.State.Types (WorldManager(..), WorldState(..), emptyWorldState)

-- Fixtures -----------------------------------------------------------

pageA, pageB ∷ WorldPageId
pageA = WorldPageId "knowledge_page_a"
pageB = WorldPageId "knowledge_page_b"

cargoBid, otherBid ∷ BuildingId
cargoBid = BuildingId 1
otherBid = BuildingId 2

scoutUid, wildlifeUid, debugUid ∷ UnitId
scoutUid    = UnitId 1
wildlifeUid = UnitId 2
debugUid    = UnitId 3

-- | A worker-built storage building, exactly the shape every shipped
--   storage def has (@build_work@ > 0, @storage_capacity@ > 0).
cargoDef ∷ BuildingDef
cargoDef = (bareDef "cargo_hold_S")
    { bdBuildWork = 240, bdStorageCapacity = 200 }

-- | Same, with no storage at all — never seeds, never remembers.
shedDef ∷ BuildingDef
shedDef = (bareDef "shed") { bdBuildWork = 60, bdStorageCapacity = 0 }

bareDef ∷ Text → BuildingDef
bareDef name = BuildingDef
    { bdName            = name
    , bdDisplayName     = name
    , bdCategory        = "Test"
    , bdDescription     = ""
    , bdTexture         = TextureHandle 0
    , bdTileW           = 1
    , bdTileH           = 1
    , bdPlacement       = "flat_ground"
    , bdIsStarting      = False
    , bdRace            = "acolyte"
    , bdSpriteAnchor    = "diamond_bottom"
    , bdBuildWork       = 0
    , bdMaterials       = HM.empty
    , bdStorageCapacity = 0
    , bdOperations      = []
    , bdAnimations      = HM.empty
    , bdStateAnims      = HM.empty
    , bdPowerDrain      = 0
    }

mkBuilding ∷ WorldPageId → Text → [ItemInstance] → BuildingInstance
mkBuilding page defName storage = BuildingInstance
    { biDefName = defName, biPage = page, biTexture = TextureHandle 0
    , biAnchorX = 0, biAnchorY = 0, biGridZ = 0, biSpawnedAt = 0
    , biTileW = 1, biTileH = 1, biSpawnRemaining = 0
    , biBuildProgress = 0
    , biMaterialsDelivered = HM.empty
    , biStorage = storage
    }

mkUnit ∷ WorldPageId → Faction → UnitInstance
mkUnit page faction = UnitInstance
    { uiDefName = "acolyte", uiName = "", uiPage = page
    , uiTexture = TextureHandle 0, uiDirSprites = Map.empty
    , uiBaseWidth = 0, uiGridX = 0, uiGridY = 0, uiGridZ = 0
    , uiRealZ = 0, uiFacing = DirS
    , uiCurrentAnim = "", uiAnimStart = 0, uiAnimReverse = False
    , uiActivity = "idle", uiPose = "standing", uiAnimStride = 1
    , uiStats = HM.empty, uiModifiers = HM.empty, uiSkills = HM.empty
    , uiKnowledge = HM.empty, uiInventory = [], uiEquipment = HM.empty
    , uiAccessories = [], uiFactionId = faction, uiWounds = []
    , uiScars = [], uiImmuneResponse = 0, uiImmunities = HM.empty
    , uiBlood = 5.0, uiLastAttackerUid = Nothing, uiLastAttackerAt = 0
    , uiAnimOverride = "", uiFrozen = False, uiForceLoop = False
    , uiClimbDest = Nothing
    , uiTrailState = Nothing
    }

-- | A stored item with every one of 'ItemInstance''s nine fields set to
--   a DISTINCT, non-default value, and a nested content item of its own
--   — so a remembered copy that dropped or defaulted any single field is
--   observably different from the original.
kitAt ∷ Word64 → Float → ItemInstance
kitAt iid condition = ItemInstance
    { iiDefName     = "first_aid_kit"
    , iiCurrentFill = 3
    , iiQuality     = 82
    , iiCondition   = condition
    , iiWeight      = 1.25
    , iiSharpness   = 41
    , iiInstanceId  = iid
    , iiTemp        = Just 21.5
    , iiContents    =
        [ ItemInstance
            { iiDefName = "bandage", iiCurrentFill = 1, iiQuality = 100
            , iiCondition = 100, iiWeight = 0.05, iiSharpness = 0
            , iiInstanceId = iid + 1, iiTemp = Nothing, iiContents = [] } ]
    }

-- | An item registry that gives the kit a real container def, so
--   'itemTotalWeight' has a per-unit fill weight to use and the
--   remembered weight is a genuine recursive measure rather than a
--   coincidence of the 1.0 fallback.
testItems ∷ ItemManager
testItems = ItemManager $ HM.fromList
    [ ("first_aid_kit", (bareItemDef "first_aid_kit" 1.25)
        { idContainer = Just ItemContainer
            { icCapacity = 10, icHolds = "supplies"
            , icFillWeight = 0.5, icDefaultFill = 0 } })
    , ("bandage", bareItemDef "bandage" 0.05)
    ]

bareItemDef ∷ Text → Float → ItemDef
bareItemDef name w = ItemDef
    { idName = name, idDisplayName = name, idTexture = TextureHandle 0
    , idWeight = w, idWeightSpec = Nothing, idKind = "misc"
    , idCategory = "Misc", idMake = "", idMaterial = ""
    , idQualitySpec = Nothing, idQualityTiers = [], idConditionSpec = Nothing
    , idContainer = Nothing, idDefaultContents = [], idFood = Nothing
    , idWeapon = Nothing, idArmor = Nothing, idUnequippable = False
    , idBuffs = [], idInsulation = 0
    }

-- | A live scene: two real pages, a storage container on page B (so
--   nothing can pass by accidentally reaching the FIRST page), a
--   non-storage building on page A, and three units of different
--   factions.
data Scene = Scene
    { scObserver  ∷ ContainerObserver
    , scBuildings ∷ IORef BuildingManager
    , scPageA     ∷ WorldState
    , scPageB     ∷ WorldState
    , scTime      ∷ IORef Double
    }

newScene ∷ IO Scene
newScene = do
    wsA ← emptyWorldState
    wsB ← emptyWorldState
    worldsRef ← newIORef WorldManager
        { wmWorlds = [(pageA, wsA), (pageB, wsB)], wmVisible = [pageA] }
    buildingsRef ← newIORef emptyBuildingManager
        { bmDefs = HM.fromList [ ("cargo_hold_S", cargoDef), ("shed", shedDef) ]
        , bmInstances = HM.fromList
            [ (cargoBid, mkBuilding pageB "cargo_hold_S" [])
            , (otherBid, mkBuilding pageA "shed" []) ]
        , bmNextId = 3 }
    unitsRef ← newIORef emptyUnitManager
        { umInstances = HM.fromList
            [ (scoutUid,    mkUnit pageB FactionPlayer)
            , (wildlifeUid, mkUnit pageB FactionWildlife)
            , (debugUid,    mkUnit pageB FactionDebug) ] }
    itemsRef ← newIORef testItems
    timeRef  ← newIORef 1000.0
    pure Scene
        { scObserver = ContainerObserver
            { coBuildings = buildingsRef, coUnits = unitsRef
            , coWorlds = worldsRef, coItems = itemsRef, coGameTime = timeRef }
        , scBuildings = buildingsRef
        , scPageA = wsA, scPageB = wsB, scTime = timeRef
        }

-- | Overwrite the container's LIVE storage without any reveal — the
--   "contents changed underneath the record" move.
setStorage ∷ Scene → BuildingId → [ItemInstance] → IO ()
setStorage sc bid items = modifyIORef' (scBuildings sc) $ \bm →
    bm { bmInstances = HM.adjust (\i → i { biStorage = items })
                                 bid (bmInstances bm) }

knowledgeOn ∷ WorldState → IO ContainerKnowledge
knowledgeOn = readIORef ∘ wsContainerKnowledgeRef

stateOf ∷ Scene → BuildingId → IO ContainerKnowledgeState
stateOf sc bid = containerState bid <$> knowledgeOn (scPageB sc)

recordOf ∷ Scene → BuildingId → IO (Maybe ContainerRecord)
recordOf sc bid = lookupContainer bid <$> knowledgeOn (scPageB sc)

-- Persistence fixtures -----------------------------------------------

-- | A page carrying all THREE knowledge states at once: a
--   never-inspected container (absent from the map), a known-empty one,
--   and one with remembered contents.
knowledgePage ∷ WorldPageId → PageSnapshot
knowledgePage pid =
    (blankPageSnapshot pid (defaultWorldGenParams { wgpSeed = 7 }))
        { pgsBuildings = BuildingSnapshot
            { bsnInstances = HM.empty, bsnNextId = 9 }
        , pgsUnits = (pgsUnits (blankPageSnapshot pid defaultWorldGenParams))
        , pgsContainerKnowledge = ContainerKnowledge $ HM.fromList
            [ (BuildingId 5, ContainerRecord [] 0 900.0)
            , (BuildingId 6, ContainerRecord [kitAt 4000 61.5] 4.05 950.25) ]
        }

knowledgeSnapshot ∷ SessionSnapshot
knowledgeSnapshot = buildSessionSnapshot globals [knowledgePage pageA]
  where
    globals = SessionGlobals
        { sgGameTime = 1000, sgTexPalette = emptyTexPalette
          -- Deliberately BELOW the remembered instance ids (4000/4001):
          -- a remembered id is a historical observation, not a live
          -- entity, so it must not participate in the allocator bound.
        , sgNextItemId = 10, sgNextBuildingId = 9, sgNextUnitId = 1
        , sgActivePage = pageA, sgVisiblePages = [pageA]
        , sgLiveCamera = LiveCameraSnapshot
            { lcsOwnerPage = Just pageA, lcsX = 0, lcsY = 0
            , lcsZoom = 1, lcsFacing = FaceSouth }
        }

encodeFor ∷ SessionSnapshot → BS.ByteString
encodeFor snap = encodeSessionSnapshot meta snap []
  where
    meta = snapshotSaveMetadata
        SaveRequestMeta { srmSlotName = "knowledge_test"
                        , srmTimestamp = "ts", srmAutosave = False }
        snap

decodeFor ∷ BS.ByteString → Either Text SessionSnapshot
decodeFor bytes =
    (\(_, snap, _, _) → snap)
        <$> decodeSessionEnvelope HS.empty HS.empty bytes

knowledgeComponentId ∷ ComponentId
knowledgeComponentId = ComponentId "container-knowledge"

-- | Rewrite an envelope's component set: drop the container-knowledge
--   component entirely (the pre-A3 save shape) or replace its payload
--   with garbage (the present-but-broken shape).
rewriteComponents
    ∷ (ComponentId → BS.ByteString → Maybe BS.ByteString)
    → BS.ByteString → BS.ByteString
rewriteComponents f bytes =
    case decodeEnvelope defaultEnvelopeLimits 1 allIds HS.empty bytes of
        Left err → error ("rewriteComponents: decode: " <> show err)
        Right de →
            let specs = [ (cdId d, cdVersion d, cdRequired d, payload)
                        | d ← emComponents (deManifest de)
                        , Just raw ← [HM.lookup (cdId d) (dePayloads de)]
                        , Just payload ← [f (cdId d) raw] ]
            in case encodeEnvelope defaultEnvelopeLimits 1 specs of
                Left err  → error ("rewriteComponents: encode: " <> show err)
                Right out → out
  where
    -- Structural pass only: the REAL reader's known-id set (so a
    -- required component isn't rejected as unknown), with no reader
    -- requirements of its own (so a component can be dropped).
    allIds = HS.insert metadataComponentId componentKnownIds

-- Spec ---------------------------------------------------------------

spec ∷ Spec
spec = describe "Container knowledge" $ do

    describe "three distinct states" $ do
        it "a container nobody has interacted with is NEVER-INSPECTED, \
           \which is not the same answer as known-empty" $ do
            sc ← newScene
            stateOf sc cargoBid `shouldReturn` NeverInspected
            recordOf sc cargoBid `shouldReturn` Nothing
            containerKnowledgeStateId NeverInspected `shouldBe` "unknown"

        it "an inspected-and-empty container is KNOWN-EMPTY: a real \
           \record, an empty remembered list, zero weight" $ do
            sc ← newScene
            revealContainer (scObserver sc) cargoBid `shouldReturn` True
            stateOf sc cargoBid `shouldReturn` KnownEmpty
            Just r ← recordOf sc cargoBid
            crItems r `shouldBe` []
            crStoredWeight r `shouldBe` 0
            containerKnowledgeStateId KnownEmpty `shouldBe` "empty"

        it "an inspected container holding something is KNOWN-CONTENTS" $ do
            sc ← newScene
            setStorage sc cargoBid [kitAt 100 70]
            revealContainer (scObserver sc) cargoBid `shouldReturn` True
            stateOf sc cargoBid `shouldReturn` KnownContents
            containerKnowledgeStateId KnownContents `shouldBe` "known"

        it "the three state ids are pairwise distinct, so a consumer can \
           \never render never-inspected as empty" $
            map containerKnowledgeStateId
                [NeverInspected, KnownEmpty, KnownContents]
                `shouldBe` ["unknown", "empty", "known"]

    describe "what reveals contents" $ do
        it "a player-commandable unit's completed storage interaction \
           \refreshes the record (the AI deposit/withdraw path)" $ do
            sc ← newScene
            setStorage sc cargoBid [kitAt 100 70]
            revealContainerForUnit (scObserver sc) scoutUid cargoBid
                `shouldReturn` True
            stateOf sc cargoBid `shouldReturn` KnownContents

        it "a DEBUG-faction unit counts too -- the gate is \
           \isPlayerCommandable, not a hand-rolled player-tag test" $ do
            sc ← newScene
            revealContainerForUnit (scObserver sc) debugUid cargoBid
                `shouldReturn` True
            stateOf sc cargoBid `shouldReturn` KnownEmpty

        it "a NON-commandable unit's otherwise-successful storage \
           \interaction reveals nothing -- the storage really changed, \
           \and the record still does not exist" $ do
            sc ← newScene
            setStorage sc cargoBid [kitAt 100 70]
            revealContainerForUnit (scObserver sc) wildlifeUid cargoBid
                `shouldReturn` False
            stateOf sc cargoBid `shouldReturn` NeverInspected

        it "a non-commandable unit cannot even STALE-EN an existing \
           \record: a wildlife interaction after a real observation \
           \leaves that observation byte-identical" $ do
            sc ← newScene
            setStorage sc cargoBid [kitAt 100 70]
            _ ← revealContainer (scObserver sc) cargoBid
            before ← recordOf sc cargoBid
            setStorage sc cargoBid []
            writeIORef (scTime sc) 9999
            revealContainerForUnit (scObserver sc) wildlifeUid cargoBid
                `shouldReturn` False
            recordOf sc cargoBid `shouldReturn` before

        it "an unknown acting unit reveals nothing (no faction, no \
           \reveal) rather than defaulting to commandable" $ do
            sc ← newScene
            revealContainerForUnit (scObserver sc) (UnitId 99) cargoBid
                `shouldReturn` False
            stateOf sc cargoBid `shouldReturn` NeverInspected

        it "a reveal REPLACES the whole record -- no merge, no \
           \incremental update" $ do
            sc ← newScene
            setStorage sc cargoBid [kitAt 100 70, kitAt 200 70]
            _ ← revealContainer (scObserver sc) cargoBid
            setStorage sc cargoBid [kitAt 300 70]
            writeIORef (scTime sc) 2000
            _ ← revealContainer (scObserver sc) cargoBid
            Just r ← recordOf sc cargoBid
            map iiInstanceId (crItems r) `shouldBe` [300]
            crRevealedAt r `shouldBe` 2000

        it "a completed storage building seeds as KNOWN-EMPTY -- the \
           \player watched it go up" $ do
            sc ← newScene
            seedBuiltContainer (scObserver sc) cargoBid `shouldReturn` True
            stateOf sc cargoBid `shouldReturn` KnownEmpty

        it "seeding never overwrites an existing observation, so a \
           \re-crossed completion threshold cannot erase what the \
           \player already saw" $ do
            sc ← newScene
            setStorage sc cargoBid [kitAt 100 70]
            _ ← revealContainer (scObserver sc) cargoBid
            seedBuiltContainer (scObserver sc) cargoBid `shouldReturn` False
            stateOf sc cargoBid `shouldReturn` KnownContents

        it "a container that is gone (or whose page is) reveals nothing \
           \and reports it, rather than inventing a record" $ do
            sc ← newScene
            revealContainer (scObserver sc) (BuildingId 404)
                `shouldReturn` False
            seedBuiltContainer (scObserver sc) (BuildingId 404)
                `shouldReturn` False
            readContainerKnowledge (scObserver sc) (BuildingId 404)
                `shouldReturn` Nothing

    describe "what does NOT reveal contents" $ do
        it "moving a unit onto the container's tile changes nothing -- \
           \proximity is not an interaction (epic decision 2)" $ do
            sc ← newScene
            setStorage sc cargoBid [kitAt 100 70]
            modifyIORef' (coUnits (scObserver sc)) $ \um → um
                { umInstances = HM.adjust (\u → u { uiGridX = 0, uiGridY = 0 })
                                          scoutUid (umInstances um) }
            stateOf sc cargoBid `shouldReturn` NeverInspected

        it "selecting the container changes nothing" $ do
            sc ← newScene
            modifyIORef' (scBuildings sc) $ \bm →
                bm { bmSelected = Just cargoBid }
            stateOf sc cargoBid `shouldReturn` NeverInspected

        it "delivering construction materials changes nothing -- \
           \biMaterialsDelivered is locked build stock in a different \
           \compartment, not storage" $ do
            sc ← newScene
            modifyIORef' (scBuildings sc) $ \bm → bm
                { bmInstances = HM.adjust
                    (\i → i { biMaterialsDelivered =
                                HM.singleton "steel_bar" [kitAt 700 70] })
                    cargoBid (bmInstances bm) }
            stateOf sc cargoBid `shouldReturn` NeverInspected

        it "contents changing underneath a real record leaves it STALE \
           \-- items, weight and reveal time all unchanged" $ do
            sc ← newScene
            setStorage sc cargoBid [kitAt 100 70]
            _ ← revealContainer (scObserver sc) cargoBid
            before ← recordOf sc cargoBid
            setStorage sc cargoBid [kitAt 500 12, kitAt 600 12]
            writeIORef (scTime sc) 8888
            recordOf sc cargoBid `shouldReturn` before

    describe "what a record remembers" $ do
        it "every one of ItemInstance's nine fields is copied AS OF the \
           \reveal, and survives the live instance changing afterwards" $ do
            sc ← newScene
            let observed = kitAt 100 74.5
            setStorage sc cargoBid [observed]
            _ ← revealContainer (scObserver sc) cargoBid
            -- The live item now differs in every mutable way.
            setStorage sc cargoBid
                [ observed { iiCurrentFill = 0, iiQuality = 1
                           , iiCondition = 2, iiWeight = 99
                           , iiSharpness = 3, iiInstanceId = 12345
                           , iiTemp = Nothing, iiContents = []
                           , iiDefName = "bandage" } ]
            Just r ← recordOf sc cargoBid
            crItems r `shouldBe` [observed]
            case crItems r of
                [remembered] → do
                    iiDefName     remembered `shouldBe` "first_aid_kit"
                    iiCurrentFill remembered `shouldBe` 3
                    iiQuality     remembered `shouldBe` 82
                    iiCondition   remembered `shouldBe` 74.5
                    iiWeight      remembered `shouldBe` 1.25
                    iiSharpness   remembered `shouldBe` 41
                    iiInstanceId  remembered `shouldBe` 100
                    iiTemp        remembered `shouldBe` Just 21.5
                    map iiDefName (iiContents remembered)
                        `shouldBe` ["bandage"]
                other → expectationFailure ("expected one item, got "
                                            <> show (length other))

        it "the remembered weight is the RECURSIVE itemTotalWeight of \
           \the remembered list -- fill and nested contents included" $ do
            sc ← newScene
            let stored = [kitAt 100 70, kitAt 300 70]
            setStorage sc cargoBid stored
            _ ← revealContainer (scObserver sc) cargoBid
            Just r ← recordOf sc cargoBid
            crStoredWeight r
                `shouldBe` sum (map (itemTotalWeight testItems) stored)
            -- Per kit: 1.25 empty + (3 fill x 0.5 kg/unit from its
            -- container def) + the nested bandage's own full weight
            -- (0.05 + 1 x the 1.0 kg/L fallback its non-container def
            -- gets) = 3.80. Two kits = 7.60. Spelled out as a literal
            -- as well as against the measure, so a change to EITHER
            -- the recursion or the fill-weight lookup fails here.
            crStoredWeight r `shouldBe` 7.60

        it "the reveal time is the game-time clock at observation, and \
           \goes stale with the rest of the record" $ do
            sc ← newScene
            writeIORef (scTime sc) 4321.5
            _ ← revealContainer (scObserver sc) cargoBid
            Just r ← recordOf sc cargoBid
            crRevealedAt r `shouldBe` 4321.5
            writeIORef (scTime sc) 99999
            recordOf sc cargoBid `shouldReturn` Just r

        it "capacity is never remembered: enlarging the def leaves the \
           \record untouched, so every consumer reads the LIVE value" $ do
            sc ← newScene
            setStorage sc cargoBid [kitAt 100 70]
            _ ← revealContainer (scObserver sc) cargoBid
            before ← recordOf sc cargoBid
            modifyIORef' (scBuildings sc) $ \bm → bm
                { bmDefs = HM.insert "cargo_hold_S"
                    (cargoDef { bdStorageCapacity = 4000 }) (bmDefs bm) }
            recordOf sc cargoBid `shouldReturn` before
            bm ← readIORef (scBuildings sc)
            (bdStorageCapacity <$> HM.lookup "cargo_hold_S" (bmDefs bm))
                `shouldBe` Just 4000

    describe "page correctness and lifecycle" $ do
        it "a reveal writes to the CONTAINER's own page, not the visible \
           \one" $ do
            sc ← newScene
            _ ← revealContainer (scObserver sc) cargoBid
            (knownContainerIds <$> knowledgeOn (scPageB sc))
                `shouldReturn` [cargoBid]
            (knownContainerIds <$> knowledgeOn (scPageA sc))
                `shouldReturn` []

        it "demolishing a container drops its record on every page" $ do
            sc ← newScene
            _ ← revealContainer (scObserver sc) cargoBid
            forgetContainerEverywhere (coWorlds (scObserver sc)) cargoBid
            stateOf sc cargoBid `shouldReturn` NeverInspected

        it "a rebuilt container reusing the same id inherits nothing" $ do
            sc ← newScene
            setStorage sc cargoBid [kitAt 100 70]
            _ ← revealContainer (scObserver sc) cargoBid
            forgetContainerEverywhere (coWorlds (scObserver sc)) cargoBid
            -- The id is re-registered as a brand-new, empty container.
            modifyIORef' (scBuildings sc) $ \bm → bm
                { bmInstances = HM.insert cargoBid
                    (mkBuilding pageB "cargo_hold_S" []) (bmInstances bm) }
            stateOf sc cargoBid `shouldReturn` NeverInspected

        it "a clear-all teardown empties every page's knowledge" $ do
            sc ← newScene
            _ ← revealContainer (scObserver sc) cargoBid
            forgetAllContainers (coWorlds (scObserver sc))
            stateOf sc cargoBid `shouldReturn` NeverInspected

        it "readContainerKnowledge separates 'no such container' from \
           \'never inspected'" $ do
            sc ← newScene
            readContainerKnowledge (scObserver sc) cargoBid
                `shouldReturn` Just Nothing
            readContainerKnowledge (scObserver sc) (BuildingId 404)
                `shouldReturn` Nothing

    describe "dangling records" $ do
        it "a record whose building is absent is dropped, and REPORTED, \
           \rather than failing anything" $ do
            let live = HS.singleton (BuildingId 5)
                k = ContainerKnowledge $ HM.fromList
                        [ (BuildingId 5, ContainerRecord [] 0 1)
                        , (BuildingId 6, ContainerRecord [] 0 2) ]
            prunedContainerIds live k `shouldBe` [BuildingId 6]
            knownContainerIds (retainContainers live k)
                `shouldBe` [BuildingId 5]

        it "scrubbing keeps every record whose building survived" $ do
            let live = HS.fromList [BuildingId 5, BuildingId 6]
                k = ContainerKnowledge $ HM.fromList
                        [ (BuildingId 5, ContainerRecord [] 0 1)
                        , (BuildingId 6, ContainerRecord [] 0 2) ]
            prunedContainerIds live k `shouldBe` []
            retainContainers live k `shouldBe` k

    describe "persistence" $ do
        it "a save/load round trip through the real codec preserves all \
           \three states" $
            case decodeFor (encodeFor knowledgeSnapshot) of
                Left err → expectationFailure (show err)
                Right snap → do
                    let k = maybe emptyContainerKnowledge pgsContainerKnowledge
                                  (HM.lookup pageA (snapPages snap))
                    containerState (BuildingId 4) k `shouldBe` NeverInspected
                    containerState (BuildingId 5) k `shouldBe` KnownEmpty
                    containerState (BuildingId 6) k `shouldBe` KnownContents
                    k `shouldBe`
                        pgsContainerKnowledge (knowledgePage pageA)

        it "remembered per-instance state survives the round trip \
           \verbatim, nested contents included" $
            case decodeFor (encodeFor knowledgeSnapshot) of
                Left err → expectationFailure (show err)
                Right snap → do
                    let k = maybe emptyContainerKnowledge pgsContainerKnowledge
                                  (HM.lookup pageA (snapPages snap))
                    (crItems <$> lookupContainer (BuildingId 6) k)
                        `shouldBe` Just [kitAt 4000 61.5]
                    (crStoredWeight <$> lookupContainer (BuildingId 6) k)
                        `shouldBe` Just 4.05
                    (crRevealedAt <$> lookupContainer (BuildingId 6) k)
                        `shouldBe` Just 950.25

        it "remembered instance ids are historical observations: they \
           \never enter allItemInstanceIds, so they are exempt from the \
           \allocator bound and the duplicate-live-id check" $ do
            allItemInstanceIds knowledgeSnapshot `shouldBe` []
            validateSessionSnapshot knowledgeSnapshot `shouldBe` []

        it "a save written BEFORE this component existed loads with \
           \every container never-inspected -- never known-empty, and \
           \never with its live contents copied in" $ do
            let stripped = rewriteComponents
                    (\cid raw → if cid ≡ knowledgeComponentId
                                  then Nothing else Just raw)
                    (encodeFor knowledgeSnapshot)
            case decodeFor stripped of
                Left err → expectationFailure (show err)
                Right snap → do
                    let k = maybe (ContainerKnowledge (HM.singleton (BuildingId 0) (ContainerRecord [] 0 0)))
                                  pgsContainerKnowledge
                                  (HM.lookup pageA (snapPages snap))
                    k `shouldBe` emptyContainerKnowledge
                    containerState (BuildingId 5) k `shouldBe` NeverInspected
                    containerState (BuildingId 6) k `shouldBe` NeverInspected

        it "a PRESENT but malformed container-knowledge payload is still \
           \a hard load error -- 'absent' and 'broken' are different \
           \answers" $ do
            let corrupted = rewriteComponents
                    (\cid raw → Just (if cid ≡ knowledgeComponentId
                                        then BS.pack [0xFF, 0xFF, 0xFF]
                                        else raw))
                    (encodeFor knowledgeSnapshot)
            case decodeFor corrupted of
                Left _     → pure ()
                Right _    → expectationFailure
                    "a corrupt container-knowledge payload decoded anyway"

        it "loading REPLACES the whole knowledge owner: a session whose \
           \pages differ carries none of the previous session's records" $ do
            let otherSnap = knowledgeSnapshot
                    { snapActivePage   = pageB
                    , snapVisiblePages = [pageB]
                    , snapLiveCamera   = (snapLiveCamera knowledgeSnapshot)
                        { lcsOwnerPage = Just pageB }
                    , snapPages = HM.singleton pageB
                        (blankPageSnapshot pageB defaultWorldGenParams)
                    }
            case decodeFor (encodeFor otherSnap) of
                Left err → expectationFailure (show err)
                Right snap → do
                    HM.keys (snapPages snap) `shouldBe` [pageB]
                    (pgsContainerKnowledge <$> HM.lookup pageB (snapPages snap))
                        `shouldBe` Just emptyContainerKnowledge

        it "a remembered item's def name is an ordinary content \
           \reference: an unregistered one is reported by the same \
           \missing-item-definition gate live storage uses" $ do
            let sd = snapshotToSaveData
                        SaveRequestMeta { srmSlotName = "k", srmTimestamp = "t"
                                        , srmAutosave = False }
                        knowledgeSnapshot
                pages = [ (wpsPageId w, w) | w ← sdWorlds sd ]
                withAll = missingItemDefReferences
                    (HS.fromList ["first_aid_kit", "bandage"]) pages
                withoutKit = missingItemDefReferences
                    (HS.fromList ["bandage"]) pages
            withAll `shouldBe` []
            length withoutKit `shouldBe` 1
