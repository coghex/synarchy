{-# LANGUAGE ScopedTypeVariables #-}
-- | The "save components" gate (issue #760, save-overhaul B2): the
--   Haskell-owned persistence component split that replaced B1's single
--   transitional @"session"@ payload. Pure — no engine, no IO. Every
--   'SessionSnapshot' below is a synthetic literal, the same pattern
--   'Test.Headless.Save.Snapshot' uses one layer up.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "save components"'@.
module Test.Headless.World.Save.Components (spec) where

import UPrelude
import Test.Hspec
import Control.Exception (ErrorCall(..), evaluate)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import Data.Either (isRight)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.Serialize as S
import qualified Data.Text as T
import Numeric (readHex, showHex)

import qualified Data.HashSet as HS
import World.Save.Envelope
import World.Save.Compat.MetadataV1 (SaveMetadataV1(..))
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
import World.Save.Component.Knowledge
    ( containerKnowledgeCodec, ContainerKnowledgeDTO(..)
    , ContainerKnowledgeDTOv1(..), PageContainerKnowledgeDTOv1(..)
    , PageContainerKnowledgeDTO(..), ContainerRecordDTO(..)
    , toContainerRecordDTOv1 )
import World.Save.Component.Transfer
    ( transferOrdersCodec, validateTransferOrders
    , TransferOrdersDTO(..), PageTransferOrdersDTO(..)
    , TransferOrderQueueDTO(..), TransferOrderDTO(..)
    , TransferEndpointDTO(..) )
import Unit.Transfer
    ( TransferBatch(..), TransferEndpoint(..), TransferItemRef(..)
    , TransferState(..), QueuedTransfer(..) )
import Unit.Transfer.Orders
    ( TransferOrderId(..), TransferOrders(..), addTransferOrder
    , transferOrderAllocatorExhausted )
import World.Save.Compat.SessionV90
import Language.Generated.Types
    (LanguageProvenance(..), LangSeed(..), GeneratorVersion(..))
import World.Save.Integrity (integrityErrorCap, KnownEntities(..))
import Engine.Scripting.Lua.API.Save.Integrity (knownEntitiesFromSaveData)
import World.Save.Reference (SamePageRef(..))
import World.Save.Snapshot
import Test.Headless.Location.Fixture (expectGeometry)
import Location.Types (emptyLocationRegistry)
import Location.Bounds (AbsBounds(..))
import Location.Instance
    ( LocationInstance(..), LocationInstances(..), LocationInstanceId(..)
    , LocationLifecycle(..) )
import World.Save.Snapshot.Adapter
    (SaveRequestMeta(..), snapshotSaveMetadata, snapshotToSaveData)
import World.Save.Types
    ( SaveMetadata(..), BuildingSnapshot(..), BuildingInstanceSnapshot(..)
    , UnitSnapshot(..), UnitInstanceSnapshot(..)
    , MissingDefRef(..), renderMissingDefRef, missingDefReferences
    , MissingItemDefRef(..), missingItemDefReferences
    , MissingRecipeRef(..), missingRecipeReferences
    , MissingBillOutputItemRef(..), missingBillOutputItemReferences
    , MissingConstructDefRef(..)
    , missingConstructDefReferences
    , renderMissingItemDefRef, renderMissingRecipeRef
    , renderMissingBillOutputItemRef, renderMissingConstructDefRef
    , MissingMaterialRef(..), renderMissingMaterialRef
    , MissingFloraRef(..), renderMissingFloraRef
    , MissingLocationRef(..), renderMissingLocationRef
    , MissingInfectionRef(..), renderMissingInfectionRef
    , WorldPageSave(..), SaveData(..), resolveLegacyLocationParams )
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)
import World.River.Naming (RiverNames(..))
import World.Page.Types (WorldPageId(..), WorldIdentity(..))
import World.Render.Zoom.Types (ZoomMapMode(..))
import World.Tool.Types (ToolMode(..))
import Engine.Graphics.Camera (CameraFacing(..))
import Structure.Palette (emptyTexPalette, TexPalette(..))
import Item.Ground (emptyGroundItems, GroundItems(..), GroundItem(..))
import Item.Types (ItemInstance(..), ItemStorage(..))
import World.Spoil.Types (emptySpoilPiles)
import World.Flora.Harvest (emptyFloraHarvests)
import World.Flora.CropPlot (emptyCropPlots)
import World.Edit.Types (emptyWorldEdits, WorldEdit(..))
import World.Chunk.Types (ChunkCoord(..))
import World.Mine.Types (MineDesignation(..))
import World.Construct.Attempt (firstConstructAttemptId)
import World.Construct.Receipt (ConstructPayment(..))
import World.Construct.Types
    ( ConstructDesignation(..), ConstructTarget(..), ConstructStatus(..) )
import World.Time.Types (CalendarConfig(..))
import World.Weather.Types
    ( ClimateState(..), ClimateGrid(..), ClimateCoord(..), RegionClimate(..)
    , SeasonalClimate(..), OceanGrid(..), OceanCell(..), OceanCurrent(..)
    , AtmoGrid(..), PressureSystem(..), PressureType(..), SurfaceType(..)
    , SurfaceBudget(..), initClimateState
    , defaultRegionClimate, emptyOceanGrid, emptyAtmoGrid )
import Craft.Bills
    ( emptyCraftBills, CraftBill(..), CraftBills(..), BillId(..), BillMode(..) )
import Unit.Transfer.Orders (emptyTransferOrders)
import Power.Types
    ( emptyPowerNodes, PowerNode(..), PowerNodes(..), PowerNodeId(..)
    , PowerRole(..) )
import Building.Types (BuildingId(..))
import Unit.Types (UnitId(..))
import Unit.Sim.Types
    (UnitSimState(..), MoveTarget(..), Pose(..), UnitActivity(..)
    , MoveHazardPolicy(..))
import Unit.Direction (Direction(..))
import Building.Knowledge (emptyContainerKnowledge, ContainerRecord(..))
import World.Flora.Identity (firstPlantedFloraCursor)

-- ---------------------------------------------------------------------
-- Fixtures (mirror Test.Headless.Save.Snapshot's minimal* pattern)
-- ---------------------------------------------------------------------

-- | The hazard policy of a decoded sim state's in-flight target, if any
--   (#1217). Spelled once so the migration cases below read as
--   assertions rather than as nested Maybe plumbing.
hazardOf ∷ Maybe UnitSimStateDTO → Maybe MoveHazardPolicy
hazardOf mSim = fmap mvtHazard (simTarget =<< mSim)

page1, page2 ∷ WorldPageId
page1 = WorldPageId "page1"
page2 = WorldPageId "page2"

-- | 'WorldGenParams''s manual cereal instance DERIVES a few nested
--   fields (e.g. the volcanism config's own seed/world-size) from
--   'wgpSeed'/'wgpWorldSize' on decode rather than storing them, so a
--   hand-built default whose nested seeds don't already agree is not a
--   serialize fixpoint. Real gen params (produced by worldgen) always
--   agree; here we reach that fixpoint explicitly by one decode∘encode,
--   so a full-equality snapshot round trip is meaningful. This changes
--   only the in-memory value — the ENCODED bytes are identical either
--   way (the derived fields are never written), so the frozen fixture
--   below stays valid.
canon ∷ WorldGenParams → WorldGenParams
canon gp = case S.decode (S.encode gp) of
    Right gp' → gp'
    Left err  → error ("canon: " <> err)

defaultGP ∷ WorldGenParams
defaultGP = canon defaultWorldGenParams

minimalPage ∷ WorldPageId → PageSnapshot
minimalPage pid = PageSnapshot
    { pgsPageId       = pid
    , pgsGenParams    = defaultGP
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
    -- Per-page bsnNextId/usnNextId equal the global allocator: production
    -- always duplicates the one global counter into every page (see
    -- SessionGlobals' sgNextBuildingId note), and B2's buildings/units
    -- components no longer carry an independent per-page copy — they
    -- refill it from the global on decode. minimalGlobals below uses 10.
    , pgsBuildings    = BuildingSnapshot { bsnInstances = HM.empty, bsnNextId = 10 }
    , pgsUnits        = UnitSnapshot { usnInstances = HM.empty, usnNextId = 10 }
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
    }

-- | A minimal, otherwise-valid frozen v90 (#759 B1-era) page — mirrors
--   'minimalPage' one layer down the wire, built directly from the
--   frozen leaf DTOs "World.Save.Compat.SessionV90" composes (issue
--   #766, save-overhaul C4).
minimalWorldPageSaveV90 ∷ WorldPageId → WorldPageSaveV90
minimalWorldPageSaveV90 pid = WorldPageSaveV90
    { wp90PageId       = pid
    , wp90GenParams    = toWorldGenParamsDTOv1 defaultGP
    , wp90CameraX      = 0
    , wp90CameraY      = 0
    , wp90CameraZoom   = 1
    , wp90CameraFacing = FaceSouth
    , wp90TimeHour     = 12
    , wp90TimeMinute   = 0
    , wp90DateYear     = 1
    , wp90DateMonth    = 1
    , wp90DateDay      = 1
    , wp90TimeScale    = 1
    , wp90MapMode      = ZMDefault
    , wp90ToolMode     = DefaultTool
    , wp90Edits        = HM.empty
    , wp90MineDesignations      = HM.empty
    , wp90ConstructDesignations = HM.empty
    , wp90GroundItems  = GroundItemsDTOv1 0 HM.empty
    , wp90SpoilPiles   = HM.empty
    , wp90Buildings    = BuildingSnapshotV90 HM.empty 1
    , wp90Units        = UnitSnapshotV90 HM.empty 1
    , wp90UnitSimStates = HM.empty
    , wp90FloraHarvests = HM.empty
    , wp90ChopDesignations = HM.empty
    , wp90CraftBills   = BillQueueDTOv1 HM.empty 1
    , wp90PowerNodes   = NodeRegistryDTOv1 HM.empty 1
    , wp90TillDesignations = HM.empty
    , wp90CropPlots    = HM.empty
    , wp90PlantDesignations = HM.empty
    , wp90Identity     = Nothing
    }

-- | Metadata that AGREES with 'minimalSaveDataV90's page (requirement
--   12's manifest/gameplay agreement check applies to a migrated
--   session exactly like a modern one) — seed/size/plates come from
--   'defaultGP', name/gloss from the page's 'Nothing' identity.
minimalSaveMetadataV90 ∷ SaveMetadata
minimalSaveMetadataV90 = SaveMetadata
    { smName = "b1-hand-built", smSeed = wgpSeed defaultGP
    , smWorldSize = wgpWorldSize defaultGP, smPlateCount = wgpPlateCount defaultGP
    , smTimestamp = "2026-07-16T00:00:00.000000Z"
    , smWorldName = Nothing, smWorldGloss = Nothing, smAutosave = False }

-- | The SAME values as 'minimalSaveMetadataV90', but as the frozen
--   'SaveMetadataV90' type (round-17 review) -- the "session" payload's
--   OWN embedded metadata field, distinct from the envelope's
--   separately-decoded @"metadata"@ component 'migrateSessionV90'
--   actually takes as its first argument (still 'SaveMetadata').
minimalFrozenSaveMetadataV90 ∷ SaveMetadataV90
minimalFrozenSaveMetadataV90 = SaveMetadataV90
    { sm90Name = "b1-hand-built", sm90Seed = wgpSeed defaultGP
    , sm90WorldSize = wgpWorldSize defaultGP, sm90PlateCount = wgpPlateCount defaultGP
    , sm90Timestamp = "2026-07-16T00:00:00.000000Z"
    , sm90WorldName = Nothing, sm90WorldGloss = Nothing }

-- | And the SAME values again as the frozen v1 @"metadata"@ component
--   payload (#913). A hand-built B1 envelope must carry METADATA v1 as
--   well as its v90 session: a real B1 file was written while the
--   metadata component was still at v1, and the B1 recognizer pins that
--   historical version deliberately
--   ('World.Save.Envelope.legacyMetadataComponentVersion'), so a
--   metadata bump can never stop this build recognizing its own frozen
--   baseline. Decoding it must yield 'minimalSaveMetadataV90' exactly —
--   including @smAutosave = False@, the documented "legacy saves are
--   manual saves" answer 'migrateSaveMetadataV1' supplies.
minimalSaveMetadataV1 ∷ SaveMetadataV1
minimalSaveMetadataV1 = SaveMetadataV1
    { sm1Name = "b1-hand-built", sm1Seed = wgpSeed defaultGP
    , sm1WorldSize = wgpWorldSize defaultGP, sm1PlateCount = wgpPlateCount defaultGP
    , sm1Timestamp = "2026-07-16T00:00:00.000000Z"
    , sm1WorldName = Nothing, sm1WorldGloss = Nothing }

-- | A minimal, otherwise-valid frozen v90 'SaveDataV90' (issue #766,
--   save-overhaul C4) — the exact shape a real #759-era @"session"@
--   component payload took. Used to prove the ENVELOPE-level dispatch
--   recognizes and migrates a hand-built B1 envelope; the historical
--   byte-for-byte fixture (recovered from git history) lives in
--   "Test.Headless.World.Save.Compat".
minimalSaveDataV90 ∷ SaveDataV90
minimalSaveDataV90 = SaveDataV90
    { sd90Metadata     = minimalFrozenSaveMetadataV90
    , sd90GameTime     = 0
    , sd90EnginePaused = True
    , sd90LuaModules   = HM.empty
    , sd90TexPalette   = toTexPaletteDTO emptyTexPalette
    , sd90NextItemInstanceId = 1
    , sd90ActivePage   = WorldPageId "main_world"
    , sd90VisiblePages = [WorldPageId "main_world"]
    , sd90Worlds       = [minimalWorldPageSaveV90 (WorldPageId "main_world")]
    }

-- | A page carrying a distinctive seed + identity + one building, one
--   unit, one sim state — enough that a round trip that dropped a slice
--   would be observable. Its identity is a GENERATED one (#1092), so a
--   dropped or miswired language provenance is observable here too;
--   the seed is above @2^63-1@ deliberately, to catch a carrier that
--   narrows the range.
richPage ∷ WorldPageId → PageSnapshot
richPage pid = (minimalPage pid)
    { pgsGenParams = canon (defaultWorldGenParams { wgpSeed = 123456 })
    , pgsIdentity  = Just (WorldIdentity "Rich World" (Just "a gloss")
                               (Just (LanguageProvenance
                                          (LangSeed 0xC3A5F00DDEADBEEF)
                                          (GeneratorVersion 1)))
                               Nothing)
    , pgsBuildings = BuildingSnapshot
        { bsnInstances = HM.singleton (BuildingId 1) (minimalBuildingInstance [])
        , bsnNextId = 10 }
    , pgsUnits = UnitSnapshot
        { usnInstances = HM.singleton (UnitId 1) (minimalUnitInstance [])
        , usnNextId = 10 }
    , pgsUnitSimStates = HM.singleton (UnitId 1) minimalSimState
    }

minimalGlobals ∷ SessionGlobals
minimalGlobals = SessionGlobals
    { sgGameTime       = 42
    , sgTexPalette     = emptyTexPalette
    , sgNextItemId     = 1
    , sgNextBuildingId = 10
    , sgNextUnitId     = 10
    , sgActivePage     = page1
    , sgVisiblePages   = [page1]
    , sgLiveCamera     = LiveCameraSnapshot
        { lcsOwnerPage = Just page1
        , lcsX = 7, lcsY = 8, lcsZoom = 3, lcsFacing = FaceEast }
    }

minimalUnitInstance ∷ [ItemInstance] → UnitInstanceSnapshot
minimalUnitInstance inv = UnitInstanceSnapshot
    { uisDefName = "test_unit", uisBaseWidth = 1, uisGridX = 0, uisGridY = 0
    , uisGridZ = 0, uisFacing = DirS, uisCurrentAnim = "", uisAnimStart = 0
    , uisAnimReverse = False, uisActivity = "idle", uisPose = "standing"
    , uisAnimStride = 0, uisStats = HM.empty, uisModifiers = HM.empty
    , uisSkills = HM.empty, uisKnowledge = HM.empty, uisInventory = inv
    , uisEquipped = HM.empty, uisAccessories = [], uisFactionId = ""
    , uisWounds = [], uisScars = [], uisImmuneResponse = 0
    , uisImmunities = HM.empty, uisBlood = 5, uisName = "" }

minimalBuildingInstance ∷ [ItemInstance] → BuildingInstanceSnapshot
minimalBuildingInstance storage = BuildingInstanceSnapshot
    { bisDefName = "test_building", bisAnchorX = 0, bisAnchorY = 0
    , bisGridZ = 0, bisSpawnedAt = 0, bisTileW = 1, bisTileH = 1
    , bisSpawnRemaining = 0, bisBuildProgress = 100
    , bisMaterialsDelivered = HM.empty, bisStorage = storage }

minimalSimState ∷ UnitSimState
minimalSimState = UnitSimState
    { usRealX = 0, usRealY = 0, usGridZ = 0, usRealZ = 0
    , usTarget = Nothing, usPose = Standing, usState = Idle, usFacing = DirS
    , usLocalPath = []
    , usDrinkUntil = Nothing, usEatUntil = Nothing, usPickupUntil = Nothing
    , usTransitionUntil = Nothing, usTransitionStride = 0, usPostTransition = []
    , usClimbFromTile = Nothing, usClimbToTile = Nothing, usClimbStartTime = Nothing
    , usClimbSlipAt = Nothing, usFallFromTile = Nothing, usFallToTile = Nothing
    , usPendingClimbXP = 0, usGetUpAt = Nothing, usPendingFallDrop = Nothing
    , usJumpApex = Nothing, usMoveGrade = 0 }

-- | A sim state with distinctive values in a spread of fields (incl.
--   the nested MoveTarget, an enum, and the climb/fall tuples) so a
--   dropped or mis-mapped field in the DTO conversion would show up.
richSimState ∷ UnitSimState
richSimState = minimalSimState
    { usRealX = 3.5, usRealY = -2.25, usGridZ = 4, usRealZ = 4.5
    , usTarget = Just (MoveTarget 9 10 1.5 FallProhibited)
    , usPose = Climbing, usState = Running, usFacing = DirNE
    , usLocalPath = [(1,2),(3,4)]
    , usDrinkUntil = Just 12.5, usTransitionStride = 2
    , usPostTransition = [Crawling, Standing]
    , usClimbFromTile = Just (1,2,3), usClimbToTile = Just (4,5,6)
    , usPendingClimbXP = 0.75, usPendingFallDrop = Just 2
    , usJumpApex = Just 1.25, usMoveGrade = 0.5 }

richBills ∷ CraftBills
richBills = CraftBills
    { cbsBills = HM.singleton (BillId 3) CraftBill
        { cbId = BillId 3, cbStation = BuildingId 7, cbRecipe = "smelt_steel"
        , cbRemaining = -1, cbClaimant = Just (UnitId 4), cbClaimedAt = 8.5
        , cbProgress = 0.4, cbSeq = 3, cbPaused = False, cbWorking = True
        , cbMode = UntilStock, cbTarget = 12, cbOutputItem = "steel_bar" }
    , cbsNextId = 4 }

richNodes ∷ PowerNodes
richNodes = PowerNodes
    { pnsNodes = HM.singleton (PowerNodeId 2) PowerNode
        { pnId = PowerNodeId 2, pnBuilding = BuildingId 9
        , pnRole = PowerStorage, pnPeakWatts = 0, pnCapacityWh = 5000
        , pnStoredWh = 1234.5 }
    , pnsNextId = 3 }

-- | A first-aid kit holding a bandage holding (absurdly) another kit —
--   exercises 'ItemInstanceDTO''s RECURSIVE 'iiContents' conversion, plus
--   the leaf scalar/Maybe fields, with distinctive values throughout so a
--   dropped or mis-mapped field would show.
richItem ∷ ItemInstance
richItem = ItemInstance
    { iiDefName = "first_aid_kit", iiCurrentFill = 0, iiQuality = 82
    , iiCondition = 74.5, iiWeight = 1.25, iiSharpness = 0
    , iiInstanceId = 900, iiTemp = Just 21.5
      -- #1233: a DIFFERENT physical shape at each of the three nesting
      -- levels (storage + bulk / bulk only / neither), so the recursive
      -- conversion cannot pass by copying one level's answer down.
    , iiBulk = Just 4.25, iiStorage = Just (ItemStorage 8.5 6.75)
    , iiContents =
        [ ItemInstance
            { iiDefName = "bandage", iiCurrentFill = 3, iiQuality = 100
            , iiCondition = 100, iiWeight = 0.05, iiSharpness = 0
            , iiInstanceId = 901, iiTemp = Nothing
            , iiBulk = Just 0.1, iiStorage = Nothing
            , iiContents =
                [ ItemInstance
                    { iiDefName = "mini_kit", iiCurrentFill = 0, iiQuality = 50
                    , iiCondition = 33, iiWeight = 0.2, iiSharpness = 12.5
                    , iiInstanceId = 902, iiTemp = Just (-4.0), iiContents = []
                    , iiBulk = Nothing, iiStorage = Nothing } ] } ] }

-- #1233 pre-#1233-payload helpers ------------------------------------

-- | An empty CURRENT activity slice for a page — the shape
-- 'worldActivityCodec' writes.
toActivity ∷ WorldPageId → PageActivityDTO
toActivity pid = PageActivityDTO
    { padPageId        = pid
    , padConstructNextAttempt = firstConstructAttemptId
    , padMine          = HM.empty
    , padConstruct     = HM.empty
    , padChop          = HM.empty
    , padTill          = HM.empty
    , padPlant         = HM.empty
    , padFloraHarvests = emptyFloraHarvests
    , padCropPlots     = HM.empty
    , padGroundItems   = toGroundItemsDTO emptyGroundItems
    , padSpoilPiles    = HM.empty
    , padPendingChop   = HM.empty
    , padPendingHarvests = HM.empty
    }

-- | The same at the FROZEN pre-#1233 layout (@world-activity@ v1/v2).
toActivityV2 ∷ WorldPageId → PageActivityDTOv2
toActivityV2 pid = PageActivityDTOv2
    { pad2PageId        = pid
    , pad2Mine          = HM.empty
    , pad2Construct     = HM.empty
    , pad2Chop          = HM.empty
    , pad2Till          = HM.empty
    , pad2Plant         = HM.empty
    , pad2FloraHarvests = HM.empty
    , pad2CropPlots     = HM.empty
    , pad2GroundItems   = toGroundItemsDTOv1 emptyGroundItems
    , pad2SpoilPiles    = HM.empty
    }

-- | Every level of a decoded item tree's physical values, root first —
--   so an assertion covers the WHOLE recursion rather than only the item
--   a migration happened to touch at the top.
physicals ∷ ItemInstanceDTO → [(Maybe Float, Maybe ItemStorageDTO)]
physicals d =
    (itdBulk d, itdStorage d) : concatMap physicals (itdContents d)

-- | The same live item with its physical values cleared everywhere — what
--   a migrated pre-#1233 instance must equal, field for field.
stripPhysicals ∷ ItemInstance → ItemInstance
stripPhysicals i = i
    { iiBulk = Nothing, iiStorage = Nothing
    , iiContents = map stripPhysicals (iiContents i) }

-- Item-container coverage fixture (#1090) ----------------------------

-- | A plain item carrying one nested 'iiContents' child, both with
--   distinct ids and def names — @iid@ for the outer, @iid + 1@ for the
--   nested one.
nestedCoverItem ∷ Word64 → Text → ItemInstance
nestedCoverItem iid nm = (coverItem iid nm)
    { iiContents = [coverItem (iid + 1) (nm <> "_nested")] }

coverItem ∷ Word64 → Text → ItemInstance
coverItem iid nm = ItemInstance
    { iiDefName = nm, iiCurrentFill = 0, iiQuality = 0, iiCondition = 100
    , iiWeight = 1, iiSharpness = 0, iiInstanceId = iid, iiTemp = Nothing
    , iiBulk = Just 1, iiStorage = Nothing
    , iiContents = [] }

-- | One page carrying a DISTINCT item id in every one of the six item
--   containers the save system enumerates, each holding a nested
--   'iiContents' child: twelve ids in all (#1090). Every container map
--   is a singleton, so the traversal order is deterministic and can be
--   asserted.
--
--   A container dropped from the shared enumeration is observable here
--   as ids missing from EVERY consumer — which is the point: the three
--   enumerations that preceded #1090 could each silently stop seeing a
--   container with no type error and no test failure.
containerCoveragePage ∷ PageSnapshot
containerCoveragePage = (minimalPage page1)
    { pgsGroundItems = GroundItems 2
        (HM.singleton 1 (GroundItem (nestedCoverItem 100 "ground") 0 0))
    , pgsUnits = UnitSnapshot
        (HM.singleton (UnitId 1)
            (minimalUnitInstance [nestedCoverItem 200 "inventory"])
                { uisEquipped =
                    HM.singleton "head" (nestedCoverItem 300 "equipped")
                , uisAccessories = [nestedCoverItem 400 "accessory"] })
        10
    , pgsBuildings = BuildingSnapshot
        (HM.singleton (BuildingId 1)
            (minimalBuildingInstance [nestedCoverItem 500 "storage"])
                { bisMaterialsDelivered =
                    HM.singleton "wood" [nestedCoverItem 600 "delivered"] })
        10
    }

containerCoverageSnapshot ∷ SessionSnapshot
containerCoverageSnapshot = case captureSessionSnapshot
        minimalGlobals { sgNextItemId = 1000 } [containerCoveragePage] of
    Right s   → s
    Left errs → error ("containerCoverageSnapshot invalid: " <> show errs)

-- | The same fixture seen through the OTHER page shape, via the real
--   production adapter — so one session exercises both the @pgs*@ and
--   @wps*@ projections of the shared enumeration.
containerCoveragePageSave ∷ WorldPageSave
containerCoveragePageSave =
    case sdWorlds (snapshotToSaveData (SaveRequestMeta "s" "t" False)
                       containerCoverageSnapshot) of
        (w : _) → w
        []      → error "containerCoveragePageSave: no pages"

-- | Every item-instance id the fixture plants: six outer items, six
--   nested children.
containerCoverageIds ∷ [Word64]
containerCoverageIds =
    concat [ [n, n + 1] | n ← [100, 200, 300, 400, 500, 600] ]

-- | 'WorldGenParams' with distinctive values planted across the newly
--   FROZEN nested worldgen records (a non-default calendar; a climate
--   state carrying a populated region, ocean cell, named current,
--   pressure system, and surface budget) so a mis-mapped field in ANY
--   of the recursive climate/config DTO conversions is observable.
--   'canon' reaches the manual-Serialize fixpoint (see its note) — the
--   volcano ctx is rebuilt from seed/size/plates, which these edits leave
--   untouched, so the planted climate/calendar values survive it.
richClimate ∷ ClimateState
richClimate = (initClimateState 64)
    { csGlobalCO2  = 1.3, csGlobalTemp = 14.5, csSolarConst = 0.98
    , csClimate = ClimateGrid
        (HM.singleton (ClimateCoord 1 2)
            defaultRegionClimate { rcHumidity = 0.42
                                 , rcAirTemp  = SeasonalClimate 20 5 }) 4
    , csOcean = emptyOceanGrid
        { ogCells = HM.singleton (ClimateCoord 0 1)
            (OceanCell (SeasonalClimate 18 12) 34.5 200 1.1 0.3 0.2 0.05)
        , ogCurrents = [OceanCurrent "Gyre" [ClimateCoord 0 0] True 0.6] }
    , csAtmo = emptyAtmoGrid
        { agSystems = [PressureSystem (ClimateCoord 2 2) HighPressure 3 0.4] }
    , csSurface = HM.singleton (ClimateCoord 1 1)
        (SurfaceBudget SurfDesert 0.35 (-0.2) 0.1 0.0) }

richGenParams ∷ WorldGenParams
richGenParams = canon defaultWorldGenParams
    { wgpSeed        = 777
    , wgpCalender    = CalendarConfig 40 10 20 50
    , wgpClimateState = richClimate }

-- | A page carrying data for the components 'richPage' leaves empty —
--   craft bills, power nodes, ground items, a mine designation, and a
--   world edit — so the full-envelope round trip below observes EVERY
--   registered component's assembly fold (a dropped fold for any of
--   them would lose this data).
fullPage ∷ WorldPageId → PageSnapshot
fullPage pid = (richPage pid)
    { pgsCraftBills  = richBills
      -- #1246: populated here (and NOT on 'richPage') so the pinned
      -- transfer-orders row below is a real order's bytes on one fixture
      -- and the empty default's on the other, rather than the degenerate
      -- encoding on both.
    , pgsTransferOrders = onePendingOrder
    , pgsPowerNodes  = richNodes
    , pgsGroundItems = GroundItems 2 (HM.singleton 1 (GroundItem richItem 3.5 4.5))
    , pgsMineDesignations = HM.singleton (1, 2) (MineDesignation 0 (1,1,1,1) 0.5)
    , pgsEdits       = HM.singleton (ChunkCoord 0 0) [WeDeleteTile 1 2] }

fullSnapshot ∷ SessionSnapshot
fullSnapshot = case captureSessionSnapshot
        minimalGlobals { sgNextItemId = 1000 } [fullPage page1] of
    Right s   → s
    Left errs → error ("fullSnapshot invalid: " <> show errs)

-- A valid, captured multi-page snapshot + its metadata.
richSnapshot ∷ SessionSnapshot
richSnapshot = case captureSessionSnapshot
        minimalGlobals { sgNextItemId = 100 }
        [richPage page1, minimalPage page2] of
    Right s   → s
    Left errs → error ("richSnapshot invalid: " <> show errs)

richMeta ∷ SaveMetadata
richMeta = snapshotSaveMetadata
    (SaveRequestMeta { srmSlotName = "slot", srmTimestamp = "ts", srmAutosave = False }) richSnapshot

encodeRich ∷ BS.ByteString
encodeRich = encodeSessionSnapshot richMeta richSnapshot []

pageCore ∷ WorldPageId → PageCoreDTO
pageCore pid = PageCoreDTO
    { pcPageId = pid, pcGenParams = toWorldGenParamsDTO defaultGP
    , pcCameraX = 0, pcCameraY = 0, pcTimeHour = 0, pcTimeMinute = 0
    , pcDateYear = 1, pcDateMonth = 1, pcDateDay = 1, pcMapMode = ZMDefault
    , pcIdentity = Nothing }

-- | The same page core in the frozen pre-#1092 v2 shape, WITH an identity
--   — the field whose DTO actually differs between v2 and v3, so a v2
--   payload built here is genuinely not a v3 payload.
pageCoreV2 ∷ WorldPageId → PageCoreDTOv2
pageCoreV2 pid = PageCoreDTOv2
    { pc2PageId = pid, pc2GenParams = toWorldGenParamsDTOv2 defaultGP
    , pc2CameraX = 0, pc2CameraY = 0, pc2TimeHour = 0, pc2TimeMinute = 0
    , pc2DateYear = 1, pc2DateMonth = 1, pc2DateDay = 1, pc2MapMode = ZMDefault
    , pc2Identity = Just (WorldIdentityDTOv1 "Old World" Nothing) }

-- | The same page core in the frozen pre-#1101 v3 shape: #1092's
--   three-field identity over the frozen pre-#1101 gen params, whose
--   location-instance table carries no gloss.
pageCoreV4 ∷ WorldPageId → PageCoreDTOv4
pageCoreV4 pid = PageCoreDTOv4
    { pc4PageId = pid, pc4GenParams = toWorldGenParamsDTOv3 defaultGP
    , pc4CameraX = 0, pc4CameraY = 0, pc4TimeHour = 0, pc4TimeMinute = 0
    , pc4DateYear = 1, pc4DateMonth = 1, pc4DateDay = 1, pc4MapMode = ZMDefault
    , pc4Identity = Just (WorldIdentityDTOv2 "Old World" Nothing Nothing) }

pageCoreV3 ∷ WorldPageId → PageCoreDTOv3
pageCoreV3 pid = PageCoreDTOv3
    { pc3PageId = pid, pc3GenParams = toWorldGenParamsDTOv2 defaultGP
    , pc3CameraX = 0, pc3CameraY = 0, pc3TimeHour = 0, pc3TimeMinute = 0
    , pc3DateYear = 1, pc3DateMonth = 1, pc3DateDay = 1, pc3MapMode = ZMDefault
    , pc3Identity = Just (WorldIdentityDTOv2 "Old World" Nothing Nothing) }

-- | A minimal 'WorldPageSave' fixture (all designation/entity maps
--   empty) for the round-8 def-reference validators below, which only
--   ever look at 'wpsBuildings'/'wpsUnits'/'wpsGroundItems'/
--   'wpsCraftBills'/'wpsConstructDesignations'.
minimalWorldPageSave ∷ WorldPageId → WorldPageSave
minimalWorldPageSave pid = WorldPageSave
    { wpsPageId       = pid
    , wpsConstructNextAttempt = firstConstructAttemptId
    , wpsGenParams    = defaultGP
    , wpsCameraX      = 0, wpsCameraY = 0, wpsCameraZoom = 1
    , wpsCameraFacing = FaceSouth
    , wpsTimeHour     = 0, wpsTimeMinute = 0
    , wpsDateYear     = 1, wpsDateMonth = 1, wpsDateDay = 1
    , wpsTimeScale    = 1
    , wpsMapMode      = ZMDefault
    , wpsToolMode     = DefaultTool
    , wpsEdits        = emptyWorldEdits
    , wpsMineDesignations      = HM.empty
    , wpsConstructDesignations = HM.empty
    , wpsGroundItems  = emptyGroundItems
    , wpsSpoilPiles   = emptySpoilPiles
    , wpsBuildings    = BuildingSnapshot HM.empty 10
    , wpsUnits        = UnitSnapshot HM.empty 10
    , wpsUnitSimStates = HM.empty
    , wpsFloraHarvests = emptyFloraHarvests
    , wpsChopDesignations = HM.empty
    , wpsPendingChopMigration = HM.empty
    , wpsPendingFloraHarvests = HM.empty
    , wpsPlantedFloraCursor = firstPlantedFloraCursor
    , wpsCraftBills   = emptyCraftBills
    , wpsTransferOrders = emptyTransferOrders
    , wpsPowerNodes   = emptyPowerNodes
    , wpsTillDesignations = HM.empty
    , wpsCropPlots    = emptyCropPlots
    , wpsPlantDesignations = HM.empty
    , wpsContainerKnowledge = emptyContainerKnowledge
    , wpsIdentity     = Nothing
    }

hexDecode ∷ String → BS.ByteString
hexDecode = BS.pack . go
  where
    go (a:b:rest) = case readHex [a,b] of
        ((v,_):_) → v : go rest
        []        → error ("hexDecode: not a hex byte: " <> [a,b])
    go _          = []

isLeft ∷ Either a b → Bool
isLeft (Left _) = True
isLeft _        = False

componentIdText ∷ ComponentId → Text
componentIdText (ComponentId t) = t

-- | Issue #1275: a synthetic spec whose ONLY interesting content is its
--   declared version table. The DTO is a bare 'Word32' so nothing about
--   any real component's shape can influence what the construction-time
--   check does with the declarations.
versionTableProbe ∷ Word32 → [Word32] → ComponentSpec Word32 Word32
versionTableProbe current older = ComponentSpec
    { csComponent     = probeComponentId
    , csVersion       = current
    , csRequired      = True
    , csDeps          = []
    , csEncode        = const 0
    , csDecode        = id
    , csOlderVersions = [ atVersion v (id ∷ Word32 → Word32) | v ← older ]
    , csValidate      = const []
    }

probeComponentId ∷ ComponentId
probeComponentId = ComponentId "version-table-probe"

-- | An 'ErrorCall' whose message contains every given fragment — used to
--   prove a construction-time rejection actually NAMES the component and
--   the offending version, not merely that something crashed.
errorMentioning ∷ [Text] → Selector ErrorCall
errorMentioning needles (ErrorCall msg) =
    all (`T.isInfixOf` T.pack msg) needles

-- | A payload's byte length plus an FNV-1a-64 fingerprint of its bytes —
--   a compact stand-in for pinning whole encoded payloads inline
--   (issue #1093's byte-identical-encoding requirement). Deliberately its
--   OWN hash rather than the envelope's manifest checksum: this gate must
--   keep meaning exactly "these component bytes are unchanged" even if the
--   envelope's framing checksum is ever changed.
payloadDigest ∷ BS.ByteString → (Int, Text)
payloadDigest bytes = (BS.length bytes, hex16 (BS.foldl' step 0xcbf29ce484222325 bytes))
  where
    step ∷ Word64 → Word8 → Word64
    step h b = (h `xor` fromIntegral b) * 0x100000001b3
    hex16 w = let s = showHex w "" in T.pack (replicate (16 - length s) '0' <> s)

-- | The encoded payload of EVERY registered gameplay component, captured
--   from the code as it stood BEFORE issue #1093 changed how codecs are
--   constructed. That change is entirely about how a 'ComponentCodec' is
--   BUILT, never about what it writes, so every entry here had to survive
--   it untouched — the round-trip and manifest-fixture gates prove
--   decodability and canonical equivalence, but neither would notice a
--   re-encoding that merely round-trips.
--
--   Pinned against BOTH shared snapshots: 'richSnapshot' (two pages,
--   populated entities/edits/climate) and 'fullSnapshot' (the one that
--   also populates craft bills, power nodes, ground items, designations),
--   so no component's row is the degenerate encoding of an empty slice.
--
--   A deliberate schema bump (a new 'csVersion' plus its frozen
--   predecessor in 'csOlderVersions') legitimately moves the affected
--   component's rows — update them in the same commit as the bump, with
--   that component's own compatibility fixture. Any OTHER movement means
--   encoded bytes changed by accident.
goldenRichPayloads ∷ [(Text, (Int, Text))]
goldenRichPayloads =
    [ ("core-session",        (85,   "74d3010096cbbe2b"))
    , ("texture-palette",     (16,   "88201fb960ff6465"))
    , ("world-pages",         (1306, "bbbd554013191bac"))
      -- #1854 re-pinned: @world-edits@ v2 appends the page's
      -- planted-flora allocator cursor to every page slice (and a
      -- FloraInstanceId to every WePlaceFlora entry, of which this
      -- fixture has none), and @world-activity@ v4 appends the two
      -- deferred legacy-migration maps and re-keys Chop/harvest state
      -- onto FloraInstanceId. Every other row is unchanged.
    , ("world-edits",         (66,   "5f4fc96e8f002516"))
      -- #1844 re-pinned again: world-activity v5 appends each
      -- designation's attempt identity and payment record, and the
      -- page's own attempt allocator.
    , ("world-activity",      (242,  "d5f6a72687031136"))
    , ("buildings",           (151,  "3dafc93879ea3b82"))
    , ("units",               (249,  "fc6ed2ffd1c79265"))
    , ("unit-sim",            (123,  "81797b8874157310"))
    , ("craft-bills",         (58,   "beec8f6ff4c58c26"))
    , ("power-nodes",         (58,   "beec8f6ff4c58c26"))
    , ("container-knowledge", (50,   "1ed7627acac89064"))
    , ("transfer-orders",     (58,   "beec8f6ff4c58c26"))
    ]

goldenFullPayloads ∷ [(Text, (Int, Text))]
goldenFullPayloads =
    [ ("core-session",        (85,  "0641eeed95100f9a"))
    , ("texture-palette",     (16,  "88201fb960ff6465"))
    , ("world-pages",         (683, "d30d2ebf9922cf3d"))
      -- #1854 re-pinned, same two components as goldenRichPayloads.
    , ("world-edits",         (78,  "d70f14ce21048a09"))
      -- #1233 re-pinned: this fixture's page carries a ground item, and
      -- world-activity v3 appended the item tree's physical values (an
      -- absent Maybe pair per item, ×3 nesting levels). #1854 re-pinned
      -- it again for v4's two deferred-migration maps. Every other row
      -- is unchanged, because no other fixture slice holds an item.
      -- #1844 re-pinned again for world-activity v5, exactly as
      -- goldenRichPayloads is.
    , ("world-activity",      (378, "401b1ef21412a4ee"))
    , ("buildings",           (130, "2b6c80ab8c216329"))
    , ("units",               (228, "4b3dd9531385aafc"))
    , ("unit-sim",            (102, "2977ea9721e11313"))
    , ("craft-bills",         (125, "687f006dbc839e32"))
    , ("power-nodes",         (58,  "0cadd98f962a6b12"))
    , ("container-knowledge", (29,  "1a075ce50a1643b1"))
    , ("transfer-orders",     (87,  "952016d6f5458b43"))
    ]

encodedPayloadDigests ∷ SessionSnapshot → [(Text, (Int, Text))]
encodedPayloadDigests snap =
    [ (componentIdText (rcId c), payloadDigest (rcEncode c snap))
    | c ← saveComponentRegistry ]

-- | One type-erased view of a concrete codec, enough to probe its
--   version dispatch without knowing what it decodes into.
data CodecProbe = CodecProbe
    { cpId        ∷ ComponentId
    , cpVersion   ∷ Word32
    , cpInputVers ∷ [Word32]
    , cpDecodeErr ∷ Word32 → BS.ByteString → Maybe ComponentError
    }

probeOf ∷ ComponentCodec a → CodecProbe
probeOf cc = CodecProbe
    { cpId        = ccId cc
    , cpVersion   = ccVersion cc
    , cpInputVers = ccInputVers cc
    , cpDecodeErr = decodeErrorOf cc
    }

-- | Every registered gameplay codec, as probes. Kept in
--   'saveComponentRegistry' order and cross-checked against it below, so
--   a component added to the registry without a probe here fails rather
--   than silently escaping the dispatch invariants.
codecProbes ∷ [CodecProbe]
codecProbes =
    [ probeOf coreSessionCodec, probeOf texPaletteCodec
    , probeOf worldPagesCodec, probeOf worldEditsCodec
    , probeOf worldActivityCodec, probeOf buildingsCodec
    , probeOf unitsCodec, probeOf unitSimCodec
    , probeOf craftBillsCodec, probeOf powerNodesCodec
    , probeOf containerKnowledgeCodec
    , probeOf transferOrdersCodec
    ]

decodeErrorOf ∷ ComponentCodec a → Word32 → BS.ByteString → Maybe ComponentError
decodeErrorOf cc v bytes = either Just (const Nothing) (ccDecode cc v bytes)

-- | 'addTransferOrder' refuses on an exhausted allocator (#1246 review
--   round 2), which no fixture here can reach — every one starts from
--   'emptyTransferOrders'. Fail loudly rather than defaulting, so a
--   future change that DID exhaust it surfaces as this error instead of
--   as a silently empty store.
-- | The smallest batch an allocator-boundary case needs: what is being
--   measured there is the ID, never the payload.
emptyBatch ∷ TransferBatch
emptyBatch = TransferBatch
    { tbSource      = EndpointUnit (UnitId 1)
    , tbDestination = EndpointBuilding (BuildingId 1)
    , tbEntries     = [] }

mustAdd ∷ UnitId → TransferBatch → TransferOrders → TransferOrders
mustAdd uid batch orders = case addTransferOrder uid batch orders of
    Just (orders', _) → orders'
    Nothing → error "fixture: addTransferOrder refused a fresh allocator"

-- | #1246: one PENDING transfer order, built through the real creation
--   surface so the fixture also carries the allocator the store issued
--   it from. Deliberately references ids absent from 'minimalPage' --
--   dangling is tolerated (see "World.Save.Integrity"), so this proves
--   the component round-trips without depending on a populated page.
onePendingOrder ∷ TransferOrders
onePendingOrder = mustAdd (UnitId 7) TransferBatch
    { tbSource      = EndpointUnit (UnitId 7)
    , tbDestination = EndpointBuilding (BuildingId 3)
    , tbEntries     =
        [ QueuedTransfer
            { qtItem = TransferItemRef { tirInstanceId = 41
                                       , tirDefName = "bandage" }
            , qtState = TransferQueued } ]
    } emptyTransferOrders

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

mentions ∷ Text → [ComponentError] → Bool
mentions needle = any (T.isInfixOf needle ∘ ceMessage)

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

spec ∷ Spec
spec = do
    describe "registry contract" $ do
        it "the authoritative registry is structurally well-formed \
           \(no duplicate ids, deps resolve, no cycles)" $
            registryStaticErrors `shouldBe` []

        it "topologically orders every component after its dependencies" $
            case dependencyOrder saveComponentRegistry of
                Left cyc → expectationFailure ("unexpected cycle: " <> show cyc)
                Right ordered → do
                    let ids = map rcId ordered
                        before a b = case (elemIndex' a ids, elemIndex' b ids) of
                            (Just i, Just j) → i < j
                            _                → False
                    -- world-pages precedes everything that depends on it
                    before worldPagesComponentId coreSessionComponentId
                        `shouldBe` True
                    before unitsComponentId unitSimComponentId `shouldBe` True
                    before buildingsComponentId craftBillsComponentId
                        `shouldBe` True

        it "rejects a dependency cycle in the registry" $ do
            let a = stubComponent (ComponentId "a") [ComponentId "b"]
                b = stubComponent (ComponentId "b") [ComponentId "a"]
            isLeft (dependencyOrder [a, b]) `shouldBe` True

        it "every gameplay component is required EXCEPT the two \
           \deliberately-optional ones -- requirement 7's rule, plus its \
           \documented exceptions: #1087's container-knowledge (absence \
           \means no container has ever been inspected) and #1246's \
           \transfer-orders (absence means no order is queued). Both \
           \post-date every tracked compatibility baseline and both \
           \absences are TRUE of such a session rather than invented. A \
           \THIRD optional component has to be justified here rather \
           \than slip in unnoticed" $
            [ rcId c | c ← saveComponentRegistry, not (rcRequired c) ]
                `shouldBe` [ containerKnowledgeComponentId
                           , transferOrdersComponentId ]

    describe "per-component codecs" $ do
        it "each component round-trips its own slice of the snapshot at \
           \its OWN CURRENT version" $ do
            -- The decode version is read from the codec rather than
            -- written out, so this cannot drift out of step with the
            -- encoder the way a literal does. It did: these were
            -- hard-coded, and once world-pages went to v7 (#1230) the
            -- v7 bytes were still being dispatched through the v6
            -- migration. It PASSED, because these fixtures carry an
            -- empty location table and the two shapes differ only in a
            -- per-instance field — so the round trip claimed here was
            -- silently never exercising the current decoder at all.
            -- #1233's buildings/units/world-activity bumps left the
            -- same three literals stale for the same reason.
            --
            -- Genuine frozen-shape coverage is not lost by this: every
            -- historical version has real frozen bytes behind it in
            -- "Test.Headless.World.Save.Compat", which encodes each
            -- vN DTO explicitly instead of hoping the current encoder
            -- still happens to emit that layout.
            let check c = case ccDecode c (ccVersion c) (ccEncode c richSnapshot) of
                    Right _  → pure () ∷ IO ()
                    Left e   → expectationFailure (T.unpack (renderComponentError e))
            check coreSessionCodec
            check worldPagesCodec
            check buildingsCodec
            check unitsCodec
            check unitSimCodec
            check craftBillsCodec
            check powerNodesCodec
            check worldEditsCodec
            check worldActivityCodec
            check texPaletteCodec

        it "declares a stable id and current version of 1" $ do
            ccId coreSessionCodec `shouldBe` coreSessionComponentId
            ccVersion coreSessionCodec `shouldBe` 1
            ccVersion worldPagesCodec `shouldBe` 8

        it "rejects a NEWER unsupported version, naming the phase" $
            case ccDecode worldPagesCodec 999 (ccEncode worldPagesCodec richSnapshot) of
                Left e  → do
                    cePhase e `shouldBe` DecodePhase
                    ceVersion e `shouldBe` 999
                Right _ → expectationFailure "expected version rejection"

        it "rejects an OLDER unsupported version" $
            ccDecode buildingsCodec 0 (ccEncode buildingsCodec richSnapshot)
                `shouldSatisfy` isLeftC

        it "rejects a truncated / malformed payload" $
            case ccDecode coreSessionCodec 1 (BS.pack [1,2,3]) of
                Left e  → cePhase e `shouldBe` DecodePhase
                Right _ → expectationFailure "expected malformed-payload rejection"

        it "world-pages self-validates a duplicate page id (component-local \
           \invariant)" $ do
            let dup = basePageSnapshots
                        (WorldPagesDTO [pageCore page1, pageCore page1])
            ccValidate worldPagesCodec dup `shouldSatisfy` (not . null)

        it "world-pages self-validates an empty page set" $
            ccValidate worldPagesCodec (basePageSnapshots (WorldPagesDTO []))
                `shouldSatisfy` (not . null)

        -- #760 round 8: per-page allocator validation for the three
        -- per-page (not global) id counters — craft bills, power nodes,
        -- ground items — mirroring world-pages' own component-local
        -- @ccValidate@ precedent above.
        it "craft-bills self-validates a bill id at/above the page's own \
           \allocator" $ do
            let badQueue = BillQueueDTO
                    { bqBills = HM.singleton (BillId 5) CraftBillDTO
                        { bilId = BillId 5, bilStation = SamePageRef (BuildingId 1)
                        , bilRecipe = "r", bilRemaining = -1, bilClaimant = Nothing
                        , bilClaimedAt = 0, bilProgress = 0, bilSeq = 5
                        , bilPaused = False, bilWorking = False
                        , bilMode = RepeatForever, bilTarget = 0
                        , bilOutputItem = "" }
                    , bqNextId = 5 }
                bad = CraftBillsDTO [ PageCraftBillsDTO page1 badQueue ]
            ccValidate craftBillsCodec bad `shouldSatisfy` (not . null)

        it "craft-bills accepts a queue whose every bill id sits below the \
           \allocator" $
            ccValidate craftBillsCodec
                (CraftBillsDTO [ PageCraftBillsDTO page1
                                    (toBillQueueDTO richBills) ])
                `shouldBe` []

        -- #760 round 9 (still-open item 1): the allocator check alone
        -- doesn't catch a map key that disagrees with the bill's OWN
        -- embedded id -- a hand-crafted envelope could carry
        -- @bqBills = {#1 -> bill{bilId=#2}}@ and slip past the allocator
        -- check (both #1 and #2 sit below it).
        it "craft-bills rejects a bill whose map key disagrees with its \
           \own embedded id" $ do
            let mismatched = BillQueueDTO
                    { bqBills = HM.singleton (BillId 1) CraftBillDTO
                        { bilId = BillId 2, bilStation = SamePageRef (BuildingId 1)
                        , bilRecipe = "r", bilRemaining = -1, bilClaimant = Nothing
                        , bilClaimedAt = 0, bilProgress = 0, bilSeq = 1
                        , bilPaused = False, bilWorking = False
                        , bilMode = RepeatForever, bilTarget = 0
                        , bilOutputItem = "" }
                    , bqNextId = 5 }
                bad = CraftBillsDTO [ PageCraftBillsDTO page1 mismatched ]
            ccValidate craftBillsCodec bad `shouldSatisfy` (not . null)

        it "power-nodes self-validates a node id at/above the page's own \
           \allocator" $ do
            let badReg = NodeRegistryDTO
                    { regNodes = HM.singleton (PowerNodeId 3) PowerNodeDTO
                        { nodId = PowerNodeId 3, nodBuilding = SamePageRef (BuildingId 1)
                        , nodRole = PowerSource, nodPeakWatts = 400
                        , nodCapacityWh = 0, nodStoredWh = 0 }
                    , regNextId = 3 }
                bad = PowerNodesDTO [ PagePowerNodesDTO page1 badReg ]
            ccValidate powerNodesCodec bad `shouldSatisfy` (not . null)

        it "power-nodes accepts a registry whose every node id sits below \
           \the allocator" $
            ccValidate powerNodesCodec
                (PowerNodesDTO [ PagePowerNodesDTO page1
                                    (toNodeRegistryDTO richNodes) ])
                `shouldBe` []

        -- #760 round 9 (still-open item 1): same key/value identity gap
        -- as craft-bills above, for power nodes.
        it "power-nodes rejects a node whose map key disagrees with its \
           \own embedded id" $ do
            let mismatched = NodeRegistryDTO
                    { regNodes = HM.singleton (PowerNodeId 1) PowerNodeDTO
                        { nodId = PowerNodeId 2, nodBuilding = SamePageRef (BuildingId 1)
                        , nodRole = PowerSource, nodPeakWatts = 400
                        , nodCapacityWh = 0, nodStoredWh = 0 }
                    , regNextId = 5 }
                bad = PowerNodesDTO [ PagePowerNodesDTO page1 mismatched ]
            ccValidate powerNodesCodec bad `shouldSatisfy` (not . null)

        it "world-activity self-validates a ground-item id at/above the \
           \page's own allocator" $ do
            let badGround = GroundItemsDTO
                    { gisiNextId = 1
                    , gisiItems = HM.singleton 1
                        (toGroundItemDTO (GroundItem richItem 0 0)) }
                bad = WorldActivityDTO
                    [ PageActivityDTO page1 HM.empty HM.empty HM.empty
                        HM.empty HM.empty emptyFloraHarvests HM.empty
                        badGround HM.empty HM.empty HM.empty
                        firstConstructAttemptId ]
            ccValidate worldActivityCodec bad `shouldSatisfy` (not . null)

        it "world-activity accepts ground items whose ids all sit below \
           \the allocator" $
            ccValidate worldActivityCodec
                (WorldActivityDTO
                    [ PageActivityDTO page1 HM.empty HM.empty HM.empty
                        HM.empty HM.empty emptyFloraHarvests HM.empty
                        (GroundItemsDTO 2
                            (HM.singleton 1 (toGroundItemDTO
                                (GroundItem richItem 0 0))))
                        HM.empty HM.empty HM.empty
                        firstConstructAttemptId ])
                `shouldBe` []

    -- #1668: the stored footprint of a persisted location instance is
    -- durable spatial authority (#911/#777), and the save decode path
    -- is the ONE 'AbsBounds' construction site that does not sit
    -- downstream of the YAML loader's inverted-bounds gate --
    -- 'fromAbsBoundsDTO' copies four unrestricted 'Int's off the wire.
    -- These cases drive the real decode+validate boundary
    -- ('decodeComponentValue' 's own @ccDecode@ then @ccValidate@
    -- sequence) at EVERY carrier shape, so no historical version
    -- routes around the check: the current 'LocationInstanceDTO' rides
    -- @world-pages@ v8, frozen 'LocationInstanceDTOv4' rides v7,
    -- 'LocationInstanceDTOv3' rides v6,
    -- 'LocationInstanceDTOv2' rides v4/v5 and 'LocationInstanceDTOv1'
    -- rides v2/v3 (one version per identical carrier shape suffices).
    -- @world-pages@ v1 predates persisted instances and carries no
    -- 'AbsBoundsDTO' at all.
    describe "location-instance stored bounds (#1668)" $ do
        let gpWith b = defaultGP
                { wgpLocationInstances = LocationInstances
                    { lisNextId        = 2
                    , lisById          = HM.singleton (LocationInstanceId 1)
                        LocationInstance
                            { liId              = LocationInstanceId 1
                            , liDefId           = "ruin"
                            , liChunk           = ChunkCoord 0 0
                            , liAnchor          = (8, 8)
                            , liBounds          = b
                            , liDisplayName     = "Small Ruin"
                            , liGloss           = Nothing
                            , liEtymology       = Nothing
                            , liLifecycle       = LifecycleUnknown
                            , liContentsSpawned = False
                            , liEncounter       = Nothing }
                    , lisPendingLegacy = Nothing } }
            -- One box per carrier, all inverted on x, so a failure names
            -- which version leaked rather than which coordinate did.
            invertedX = AbsBounds 10 6 6 10
            invertedY = AbsBounds 6 10 10 6
            invertedXY = AbsBounds 10 10 6 6
            degenerate = AbsBounds 6 6 6 6

            bytesAt ∷ Word32 → AbsBounds → BS.ByteString
            bytesAt 8 b = S.encode (WorldPagesDTO
                [ (pageCore page1) { pcGenParams = toWorldGenParamsDTO (gpWith b) } ])
            bytesAt 7 b = S.encode (WorldPagesDTOv7
                [ PageCoreDTOv7
                    { pc7PageId = page1
                    , pc7GenParams = toWorldGenParamsDTOv6 (gpWith b)
                    , pc7CameraX = 0, pc7CameraY = 0
                    , pc7TimeHour = 0, pc7TimeMinute = 0
                    , pc7DateYear = 1, pc7DateMonth = 1, pc7DateDay = 1
                    , pc7MapMode = ZMDefault, pc7Identity = Nothing } ])
            bytesAt 6 b = S.encode (WorldPagesDTOv6
                [ PageCoreDTOv6
                    { pc6PageId = page1
                    , pc6GenParams = toWorldGenParamsDTOv5 (gpWith b)
                    , pc6CameraX = 0, pc6CameraY = 0
                    , pc6TimeHour = 0, pc6TimeMinute = 0
                    , pc6DateYear = 1, pc6DateMonth = 1, pc6DateDay = 1
                    , pc6MapMode = ZMDefault, pc6Identity = Nothing } ])
            bytesAt 5 b = S.encode (WorldPagesDTOv5
                [ PageCoreDTOv5
                    { pc5PageId = page1
                    , pc5GenParams = toWorldGenParamsDTOv4 (gpWith b)
                    , pc5CameraX = 0, pc5CameraY = 0
                    , pc5TimeHour = 0, pc5TimeMinute = 0
                    , pc5DateYear = 1, pc5DateMonth = 1, pc5DateDay = 1
                    , pc5MapMode = ZMDefault, pc5Identity = Nothing } ])
            bytesAt 3 b = S.encode (WorldPagesDTOv3
                [ (pageCoreV3 page1)
                    { pc3GenParams = toWorldGenParamsDTOv2 (gpWith b) } ])
            bytesAt v _ = error ("bytesAt: unsupported version " <> show v)

            -- Exactly what 'decodeComponentValue' does: decode at the
            -- descriptor's version, then validate the canonical value.
            decodeThenValidate v b =
                case ccDecode worldPagesCodec v (bytesAt v b) of
                    Left e   → Left e
                    Right wp → Right (ccValidate worldPagesCodec wp)

            carriers ∷ [(String, Word32)]
            carriers = [ ("v8 / LocationInstanceDTO",   8)
                       , ("v7 / LocationInstanceDTOv4", 7)
                       , ("v6 / LocationInstanceDTOv3", 6)
                       , ("v5 / LocationInstanceDTOv2", 5)
                       , ("v3 / LocationInstanceDTOv1", 3) ]

            expectErrors label v b check =
                case decodeThenValidate v b of
                    Left e → expectationFailure
                        (label <> ": decode failed -- "
                         <> T.unpack (renderComponentError e))
                    Right [] → expectationFailure
                        (label <> ": an inverted stored box was ACCEPTED")
                    Right es → check es

        it "rejects an x-inverted stored box at EVERY carrier shape, in \
           \ValidatePhase, naming the component, the page, the instance \
           \and the axis" $
            forM_ carriers $ \(label, v) →
                expectErrors label v invertedX $ \es → do
                    map cePhase es `shouldBe` [ValidatePhase]
                    map ceComponent es `shouldBe` [worldPagesComponentId]
                    es `shouldSatisfy` mentions "page1"
                    es `shouldSatisfy` mentions "location instance #1"
                    es `shouldSatisfy` mentions "x axis"
                    es `shouldSatisfy` mentions "minX 10"
                    es `shouldSatisfy` mentions "maxX 6"

        it "rejects a y-inverted stored box at every carrier shape" $
            forM_ carriers $ \(label, v) →
                expectErrors label v invertedY $ \es → do
                    es `shouldSatisfy` mentions "y axis"
                    es `shouldNotSatisfy` mentions "x axis"

        it "names BOTH axes when a stored box is inverted on both -- a \
           \single unspecified inversion would not say what is wrong" $
            forM_ carriers $ \(label, v) →
                expectErrors label v invertedXY $ \es → do
                    length es `shouldBe` 2
                    es `shouldSatisfy` mentions "x axis"
                    es `shouldSatisfy` mentions "y axis"

        it "ACCEPTS a degenerate single-tile stored box at every carrier \
           \shape -- inclusive bounds make min ≡ max a real 1x1 \
           \footprint, not corruption" $
            forM_ carriers $ \(label, v) →
                case decodeThenValidate v degenerate of
                    Left e → expectationFailure
                        (label <> ": decode failed -- "
                         <> T.unpack (renderComponentError e))
                    Right es → (label, es) `shouldBe` (label, [])

        it "leaves an accepted stored box AUTHORITATIVE -- the decoded \
           \footprint is the one on the wire, never rederived (#911)" $
            forM_ carriers $ \(label, v) →
                case ccDecode worldPagesCodec v (bytesAt v degenerate) of
                    Left e → expectationFailure
                        (label <> ": decode failed -- "
                         <> T.unpack (renderComponentError e))
                    Right wp →
                        ( label
                        , map liBounds
                            (concatMap (HM.elems ∘ lisById
                                          ∘ wgpLocationInstances
                                          ∘ pgsGenParams)
                                       (HM.elems (wpBase wp))) )
                            `shouldBe` (label, [degenerate])

        it "converts snapshot ↔ DTO with no live-state reads: the world \
           \seed survives the round trip (a meaningful seed stays present, \
           \requirement 10)" $
            case ccDecode worldPagesCodec (ccVersion worldPagesCodec)
                          (ccEncode worldPagesCodec richSnapshot) of
                Right wp →
                    [ wgpSeed (pgsGenParams p)
                    | p ← maybeToList (HM.lookup page1 (wpBase wp)) ]
                        `shouldBe` [123456]
                Left e → expectationFailure (T.unpack (renderComponentError e))

    -- Issue #1093: every codec is now built through ONE shared
    -- construction that takes named arguments and can decode more than
    -- one encoded version, each through its own frozen DTO. These are the
    -- contracts that refactor had to keep exactly: the bytes it writes,
    -- the errors it reports, and the fact that its advertised
    -- accepted-version set IS what it dispatches on.
    describe "shared codec construction (issue #1093)" $ do
        it "encodes every registered gameplay component to byte-identical \
           \payloads (pinned length + fingerprint captured from the code \
           \BEFORE the construction changed)" $ do
            encodedPayloadDigests richSnapshot `shouldBe` goldenRichPayloads
            encodedPayloadDigests fullSnapshot `shouldBe` goldenFullPayloads

        it "probes EVERY registered component -- a new codec cannot escape \
           \the dispatch invariants below by simply not being listed" $
            map cpId codecProbes `shouldBe` map rcId saveComponentRegistry

        it "advertises exactly the versions it dispatches on: ccInputVers is \
           \strictly ascending, ends at ccVersion, every listed version \
           \reaches a real decoder, and nothing outside it does" $
            forM_ codecProbes $ \p → do
                let vers = cpInputVers p
                    label extra = T.unpack (componentIdText (cpId p)) <> ": " <> extra
                vers `shouldSatisfy` \vs → and (zipWith (<) vs (drop 1 vs))
                unless (not (null vers) ∧ last vers ≡ cpVersion p) $
                    expectationFailure
                        (label "ccInputVers must end at ccVersion, got "
                         <> show vers <> " for v" <> show (cpVersion p))
                -- An ACCEPTED version reaches its own cereal decoder, so
                -- empty bytes fail as a malformed payload…
                forM_ vers $ \v → case cpDecodeErr p v BS.empty of
                    Just e → do
                        cePhase e `shouldBe` DecodePhase
                        ceVersion e `shouldBe` v
                        ceComponent e `shouldBe` cpId p
                        unless ("malformed payload: " `T.isPrefixOf` ceMessage e) $
                            expectationFailure
                                (label ("v" <> show v <> " is advertised as \
                                        \accepted but did not reach a decoder: ")
                                 <> T.unpack (ceMessage e))
                    Nothing → expectationFailure
                        (label ("v" <> show v <> " decoded EMPTY bytes"))
                -- …while anything outside the set is rejected as an
                -- unsupported version, naming every version that IS
                -- accepted.
                let expected = "unsupported schema version (reader supports "
                             <> T.intercalate ", " [ "v" <> T.pack (show v)
                                                   | v ← vers ] <> ")"
                forM_ [0, cpVersion p + 1] $ \v →
                    cpDecodeErr p v BS.empty
                        `shouldBe` Just (ComponentError (cpId p) v DecodePhase expected)

        it "reports an unsupported version identically for a SINGLETON \
           \reader (component, version, phase, and the full message)" $
            decodeErrorOf coreSessionCodec 2
                    (ccEncode coreSessionCodec richSnapshot)
                `shouldBe` Just (ComponentError coreSessionComponentId 2
                    DecodePhase
                    "unsupported schema version (reader supports v1)")

        it "reports an unsupported version identically for a TWO-version \
           \reader -- the existing 'reader supports v1, v2' rendering" $ do
            decodeErrorOf craftBillsCodec 3
                    (ccEncode craftBillsCodec richSnapshot)
                `shouldBe` Just (ComponentError craftBillsComponentId 3
                    DecodePhase
                    "unsupported schema version (reader supports v1, v2)")
            decodeErrorOf powerNodesCodec 7 BS.empty
                `shouldBe` Just (ComponentError powerNodesComponentId 7
                    DecodePhase
                    "unsupported schema version (reader supports v1, v2)")

        -- unit-sim gained a third version with #1217's per-request hazard
        -- policy; it is the reader that exercises the rendering between
        -- the two- and seven-version cases either side of it.
        it "reports an unsupported version identically for a THREE-version \
           \reader" $
            decodeErrorOf unitSimCodec 0 BS.empty
                `shouldBe` Just (ComponentError unitSimComponentId 0
                    DecodePhase
                    "unsupported schema version (reader supports v1, v2, v3)")

        it "reports an unsupported version identically for an EIGHT-version \
           \reader" $
            decodeErrorOf worldPagesCodec 9 BS.empty
                `shouldBe` Just (ComponentError worldPagesComponentId 9
                    DecodePhase
                    "unsupported schema version \
                    \(reader supports v1, v2, v3, v4, v5, v6, v7, v8)")

        it "reports a malformed payload identically -- same component, \
           \supplied version, DecodePhase, and cereal-derived message -- at \
           \a singleton reader's only version and at BOTH a multi-version \
           \reader's current and historical versions" $ do
            let truncated = BS.pack [1, 2, 3]
                cerealMsg = "malformed payload: too few bytes\n\
                            \From:\tdemandInput\n\n"
            decodeErrorOf coreSessionCodec 1 truncated
                `shouldBe` Just (ComponentError coreSessionComponentId 1
                                   DecodePhase cerealMsg)
            decodeErrorOf craftBillsCodec 2 truncated
                `shouldBe` Just (ComponentError craftBillsComponentId 2
                                   DecodePhase cerealMsg)
            decodeErrorOf craftBillsCodec 1 truncated
                `shouldBe` Just (ComponentError craftBillsComponentId 1
                                   DecodePhase cerealMsg)

        -- The point a widened ccInputVers alone could never reach: ONE
        -- byte string means different things at different versions,
        -- because each version owns a different frozen DTO type. A
        -- pre-#1092 world-pages page core ends in the two-field
        -- 'WorldIdentityDTOv1', where v3's ends in the three-field
        -- 'WorldIdentityDTO' — so a v2 payload carrying an identity is
        -- genuinely shorter than any v3 payload, and reading it with the
        -- current DTO must fail rather than half-parse.
        it "reads each accepted version through its OWN frozen DTO -- the \
           \same v2 world-pages bytes decode at v2 and are REJECTED at v3" $ do
            let v2Bytes = S.encode (WorldPagesDTOv2 [pageCoreV2 page1])
            case ccDecode worldPagesCodec 2 v2Bytes of
                Right wp → map (fmap wiName . pgsIdentity)
                               (HM.elems (wpBase wp))
                    `shouldBe` [Just "Old World"]
                Left e → expectationFailure (T.unpack (renderComponentError e))
            decodeErrorOf worldPagesCodec 3 v2Bytes
                `shouldSatisfy` maybe False ((≡ DecodePhase) . cePhase)
            decodeErrorOf worldPagesCodec 4 v2Bytes
                `shouldSatisfy` maybe False ((≡ DecodePhase) . cePhase)
            decodeErrorOf worldPagesCodec 5 v2Bytes
                `shouldSatisfy` maybe False ((≡ DecodePhase) . cePhase)

        it "a v3 world-pages payload reaches the v3 decoder (#1101) rather \
           \than the current one" $ do
            let v3Bytes = S.encode (WorldPagesDTOv3 [pageCoreV3 page1])
            case ccDecode worldPagesCodec 3 v3Bytes of
                Right wp → map (fmap wiName . pgsIdentity)
                               (HM.elems (wpBase wp))
                    `shouldBe` [Just "Old World"]
                Left e → expectationFailure (T.unpack (renderComponentError e))

        it "a v4 world-pages payload reaches the v4 decoder (#1102) rather \
           \than the current one, and its page comes back with NO river \
           \names -- a save written before rivers were named never \
           \acquires them" $ do
            let v4Bytes = S.encode (WorldPagesDTOv4 [pageCoreV4 page1])
            case ccDecode worldPagesCodec 4 v4Bytes of
                Right wp → do
                    map (fmap wiName . pgsIdentity) (HM.elems (wpBase wp))
                        `shouldBe` [Just "Old World"]
                    map (rvnById . wgpRiverNames . pgsGenParams)
                        (HM.elems (wpBase wp))
                        `shouldBe` [HM.empty]
                Left e → expectationFailure (T.unpack (renderComponentError e))

        it "a v1 craft-bills payload reaches the v1 decoder and comes back \
           \MIGRATED (bare ids wrapped as same-page references), not \
           \reinterpreted as the current DTO" $ do
            let v1Bytes = S.encode (CraftBillsDTOv1
                    [ PageCraftBillsDTOv1 page1 (BillQueueDTOv1
                        { bq1NextId = 2
                        , bq1Bills  = HM.singleton (BillId 1) CraftBillDTOv1
                            { bil1Id         = BillId 1
                            , bil1Station    = BuildingId 1
                            , bil1Recipe     = "forge_steel_dagger"
                            , bil1Remaining  = 1
                            , bil1Claimant   = Nothing
                            , bil1ClaimedAt  = 0
                            , bil1Progress   = 0
                            , bil1Seq        = 1
                            , bil1Paused     = False
                            , bil1Working    = False
                            , bil1Mode       = FixedCount
                            , bil1Target     = 0
                            , bil1OutputItem = "steel_dagger"
                            } }) ])
            case ccDecode craftBillsCodec 1 v1Bytes of
                Right (CraftBillsDTO [slice]) →
                    map bilStation (HM.elems (bqBills (pcbBills slice)))
                        `shouldBe` [SamePageRef (BuildingId 1)]
                Right other → expectationFailure
                    ("expected exactly one migrated page slice, got "
                     <> show other)
                Left e → expectationFailure (T.unpack (renderComponentError e))

    -- Issue #1275: 'csOlderVersions' promised every entry was OLDER than
    -- the current version, and nothing enforced it. Because
    -- 'componentCodec' sorts the current version together with the
    -- declared older ones and dispatches by first-match 'lookup', a
    -- malformed table degrades SILENTLY rather than failing: a repeated
    -- version leaves its second decoder unreachable, the current version
    -- listed as older is shadowed by the real current decoder, and a
    -- future version is advertised and accepted as though it were
    -- history. The version is an ordinary 'Word32' argument, so the type
    -- checker sees nothing wrong with any of them.
    --
    -- 'componentCodec' is the AUTHORITATIVE boundary for that contract:
    -- it rejects the table BEFORE a 'ComponentCodec' exists, so a
    -- malformed declaration cannot reach a live dispatch table at all.
    -- The registered-codec invariants above ("advertises exactly the
    -- versions it dispatches on") and 'tools/save_compat_audit.py' both
    -- still observe the same rule over the real components — deliberately
    -- kept as defense-in-depth, since each catches it through a
    -- different mechanism (runtime probing / source parsing).
    describe "csOlderVersions table validity (issue #1275)" $ do
        it "rejects a version declared TWICE, naming the component and the \
           \repeated version -- the dispatch table's lookup would only ever \
           \reach the first of the two decoders" $
            evaluate (componentCodec (versionTableProbe 4 [3, 2, 3]))
                `shouldThrow` errorMentioning
                    ["version-table-probe", "v3", "more than once"]

        it "rejects the CURRENT version declared as older -- sortOn is \
           \stable and the current decoder is prepended, so the entry's own \
           \frozen DTO would never be reached" $
            evaluate (componentCodec (versionTableProbe 4 [4, 1]))
                `shouldThrow` errorMentioning
                    ["version-table-probe", "v4", "CURRENT version"]

        it "rejects a version NEWER than csVersion -- the reader would \
           \advertise and accept a version no writer has ever produced" $
            evaluate (componentCodec (versionTableProbe 4 [5, 1]))
                `shouldThrow` errorMentioning
                    ["version-table-probe", "v5", "NEWER"]

        it "reports the FIRST offending entry in declaration order, so the \
           \diagnostic points at a real line rather than at whichever entry \
           \a sort happened to surface" $ do
            evaluate (componentCodec (versionTableProbe 4 [9, 2, 2]))
                `shouldThrow` errorMentioning ["v9"]
            evaluate (componentCodec (versionTableProbe 4 [2, 2, 9]))
                `shouldThrow` errorMentioning ["v2"]

        it "leaves every WELL-FORMED table alone: descending, ascending, \
           \single-entry, and empty declarations all build and advertise \
           \the same ascending accepted set they always did" $ do
            let versOf = ccInputVers . componentCodec
            versOf (versionTableProbe 4 [3, 2, 1]) `shouldBe` [1, 2, 3, 4]
            versOf (versionTableProbe 4 [1, 2, 3]) `shouldBe` [1, 2, 3, 4]
            versOf (versionTableProbe 4 [1])       `shouldBe` [1, 4]
            versOf (versionTableProbe 4 [])        `shouldBe` [4]

        it "the pure check agrees exactly with what construction does -- \
           \Nothing for a well-formed table, and a message naming the \
           \component and the offending version otherwise" $ do
            olderVersionTableError probeComponentId 4 [3, 2, 1]
                `shouldBe` Nothing
            olderVersionTableError probeComponentId 4 []
                `shouldBe` Nothing
            olderVersionTableError probeComponentId 4 [2, 2]
                `shouldSatisfy` maybe False
                    (\m → "version-table-probe" `T.isInfixOf` m
                          ∧ "v2" `T.isInfixOf` m)

        it "every REAL registered codec still constructs -- forcing the \
           \whole authoritative registry runs this check over every shipped \
           \declaration, so a malformed one would fail HERE rather than \
           \being reported after the fact" $
            map (T.null . componentIdText . rcId) saveComponentRegistry
                `shouldSatisfy` all not

    -- #1233 appended physical values (external bulk + optional internal
    -- storage capacities) to the recursive item tree, which FOUR
    -- independently versioned components carry. Each therefore bumped and
    -- froze its previous tree, and each must still decode a real
    -- NON-EMPTY historical payload: a component whose migration were
    -- wired to the wrong DTO, or forgotten, would only show up on a
    -- payload that actually holds items.
    --
    -- These drive each component's REAL 'ccDecode' at the OLD version
    -- over bytes encoded from the frozen v1 tree, rather than calling the
    -- migration function directly — so the version dispatch, the frozen
    -- layout and the migration are all exercised together, exactly as a
    -- shipped save exercises them.
    describe "pre-#1233 item payloads still decode (all four components)" $ do
        let expectDecode label = either
                (\e → do expectationFailure
                             (label ⧺ ": " ⧺ T.unpack (renderComponentError e))
                         pure Nothing)
                (pure ∘ Just)

        it "world-activity accepts v1 and v2, and a ground item's whole \
           \contents tree decodes with its physical values absent" $ do
            let legacy = WorldActivityDTOv2
                    [ (toActivityV2 page1)
                        { pad2GroundItems = toGroundItemsDTOv1
                            (GroundItems 2 (HM.singleton 1
                                (GroundItem richItem 3.5 4.5))) } ]
                bytes = S.encode legacy
            ccInputVers worldActivityCodec `shouldBe` [1, 2, 3, 4, 5]
            forM_ [1, 2] $ \ver → do
                mv ← expectDecode ("v" ⧺ show ver)
                          (ccDecode worldActivityCodec ver bytes)
                case mv of
                    Nothing → pure ()
                    Just (WorldActivityDTO slices) → do
                        let gs = concatMap
                                   (HM.elems ∘ gisiItems ∘ padGroundItems)
                                   slices
                        map giiX gs `shouldBe` [3.5]
                        -- Every level of the recursive tree, not just the
                        -- root: three items in richItem's kit-in-kit.
                        concatMap (physicals ∘ giiInst) gs `shouldBe`
                            replicate 3 (Nothing, Nothing)
                        -- The rest of the item survives untouched.
                        map (fromItemInstanceDTO ∘ giiInst) gs `shouldBe`
                            [stripPhysicals richItem]

        it "buildings accepts v1, migrating BOTH a delivered-materials \
           \item and a loose-storage item" $ do
            let inst = (minimalBuildingInstance [richItem])
                    { bisMaterialsDelivered =
                        HM.singleton "steel_bar" [richItem] }
                bytes = S.encode (BuildingsDTOv1
                    [ PageBuildingsDTOv1 page1
                        (HM.singleton (BuildingId 1)
                            (toBuildingInstanceDTOv1 inst)) ])
            ccInputVers buildingsCodec `shouldBe` [1, 2]
            mv ← expectDecode "buildings v1"
                     (ccDecode buildingsCodec 1 bytes)
            case mv of
                Nothing → pure ()
                Just (BuildingsDTO slices) → do
                    let insts = concatMap (HM.elems ∘ pbInstances) slices
                    concatMap (concatMap physicals ∘ bidStorage) insts
                        `shouldBe` replicate 3 (Nothing, Nothing)
                    concatMap (concatMap physicals ∘ concat ∘ HM.elems
                               ∘ bidMaterialsDelivered) insts
                        `shouldBe` replicate 3 (Nothing, Nothing)
                    map fromBuildingInstanceDTO insts `shouldBe`
                        [ inst { bisStorage = [stripPhysicals richItem]
                               , bisMaterialsDelivered = HM.singleton
                                   "steel_bar" [stripPhysicals richItem] } ]

        it "units accepts v1, migrating inventory, equipment AND \
           \accessories" $ do
            let inst = (minimalUnitInstance [richItem])
                    { uisEquipped = HM.singleton "head" richItem
                    , uisAccessories = [richItem] }
                bytes = S.encode (UnitsDTOv1
                    [ PageUnitsDTOv1 page1
                        (HM.singleton (UnitId 1)
                            (toUnitInstanceDTOv1 inst)) ])
            ccInputVers unitsCodec `shouldBe` [1, 2]
            mv ← expectDecode "units v1" (ccDecode unitsCodec 1 bytes)
            case mv of
                Nothing → pure ()
                Just (UnitsDTO slices) → do
                    let insts = concatMap (HM.elems ∘ puInstances) slices
                        allItems u = uidInventory u
                                       ⧺ HM.elems (uidEquipped u)
                                       ⧺ uidAccessories u
                    -- Three containers × three nesting levels.
                    concatMap (concatMap physicals ∘ allItems) insts
                        `shouldBe` replicate 9 (Nothing, Nothing)
                    map fromUnitInstanceDTO insts `shouldBe`
                        [ inst { uisInventory = [stripPhysicals richItem]
                               , uisEquipped = HM.singleton "head"
                                   (stripPhysicals richItem)
                               , uisAccessories =
                                   [stripPhysicals richItem] } ]

        it "container-knowledge accepts v1 — being OPTIONAL governs \
           \ABSENCE, not migration of a payload that IS present" $ do
            let rec' = ContainerRecord
                    { crItems = [richItem], crStoredWeight = 12.5
                    , crRevealedAt = 99.25 }
                bytes = S.encode (ContainerKnowledgeDTOv1
                    [ PageContainerKnowledgeDTOv1 page1
                        (HM.singleton (BuildingId 1)
                            (toContainerRecordDTOv1 rec')) ])
            ccInputVers containerKnowledgeCodec `shouldBe` [1, 2]
            mv ← expectDecode "container-knowledge v1"
                     (ccDecode containerKnowledgeCodec 1 bytes)
            case mv of
                Nothing → pure ()
                Just (ContainerKnowledgeDTO slices) → do
                    let recs = concatMap (HM.elems ∘ pckRecords) slices
                    concatMap (concatMap physicals ∘ crdItems) recs
                        `shouldBe` replicate 3 (Nothing, Nothing)
                    -- The remembered scalars are observations and must
                    -- not be re-derived.
                    map crdStoredWeight recs `shouldBe` [12.5]
                    map crdRevealedAt recs `shouldBe` [99.25]

        it "a CURRENT-version payload round-trips the physical values \
           \for real, so absence is the migration's answer and not the \
           \codec's" $ do
            -- The counterpart to the four cases above: if the current
            -- shape silently dropped bulk/storage, every one of them
            -- would still pass.
            let bytes = S.encode (WorldActivityDTO
                    [ (toActivity page1)
                        { padGroundItems = toGroundItemsDTO
                            (GroundItems 2 (HM.singleton 1
                                (GroundItem richItem 3.5 4.5))) } ])
            mv ← expectDecode "world-activity v3"
                     (ccDecode worldActivityCodec 3 bytes)
            case mv of
                Nothing → pure ()
                Just (WorldActivityDTO slices) →
                    map (fromItemInstanceDTO ∘ giiInst)
                        (concatMap (HM.elems ∘ gisiItems ∘ padGroundItems)
                                   slices)
                        `shouldBe` [richItem]

    describe "frozen entity DTOs (requirement 4)" $ do
        -- The mutable runtime STATE records (UnitSimState, CraftBill,
        -- PowerNode) are never embedded directly; each has a distinct,
        -- component-owned DTO with an explicit field-by-field conversion.
        -- These prove the conversion is lossless (identity) on a
        -- non-trivial value, so a change to the live record surfaces as a
        -- compile error in the conversion, never as silent v1 byte drift.
        it "UnitSimStateDTO round-trips a non-default sim state" $
            fromUnitSimStateDTO (toUnitSimStateDTO richSimState)
                `shouldBe` richSimState

        it "BillQueueDTO round-trips a non-empty craft-bill queue" $
            fromBillQueueDTO (toBillQueueDTO richBills) `shouldBe` richBills

        it "NodeRegistryDTO round-trips a non-empty power-node registry" $
            fromNodeRegistryDTO (toNodeRegistryDTO richNodes) `shouldBe` richNodes

        it "migrates an unambiguous v1 unit-sim page slice into the typed \
           \current shape (issue #764 round-3 review: psSim's map KEY is a \
           \same-page cross-component reference to its owning unit, typed \
           \the same way as a bill's station or a node's host building)" $ do
            let v1 = PageSimDTOv1
                    { ps1PageId = page1
                    , ps1Sim = HM.singleton (UnitId 5)
                        (toUnitSimStateDTOv1 richSimState) }
                v2 = migratePageSimDTOv1 v1
            psPageId v2 `shouldBe` page1
            HM.keys (psSim v2) `shouldBe` [SamePageRef (UnitId 5)]
            HM.lookup (SamePageRef (UnitId 5)) (psSim v2)
                `shouldBe` Just (migrateUnitSimStateDTOv1
                                    (toUnitSimStateDTOv1 richSimState))

        it "migrateUnitSimDTOv1 migrates every page slice in a v1 payload" $
            let v1 = UnitSimDTOv1
                    [ PageSimDTOv1 page1 (HM.singleton (UnitId 5)
                        (toUnitSimStateDTOv1 richSimState))
                    , PageSimDTOv1 page2 HM.empty ]
                UnitSimDTO v2Pages = migrateUnitSimDTOv1 v1
            in map psPageId v2Pages `shouldBe` [page1, page2]

        -- #1217: the unit-sim component is at v3, and BOTH older shapes
        -- must land on the fall-permitted default a pre-#1217 target was
        -- written under. richSimState's own target is FallProhibited, so
        -- a migration that simply carried the current policy through
        -- (rather than defaulting) would pass the round-trip test above
        -- and fail these.
        it "migrates a v1 unit-sim target to the fall-permitted default" $
            let v1 = PageSimDTOv1 page1 (HM.singleton (UnitId 5)
                        (toUnitSimStateDTOv1 richSimState))
                migrated = HM.lookup (SamePageRef (UnitId 5))
                                     (psSim (migratePageSimDTOv1 v1))
            in hazardOf migrated `shouldBe` Just FallPermitted

        it "migrates a v2 unit-sim page slice, defaulting its target's \
           \hazard policy to fall-permitted" $ do
            let v2 = PageSimDTOv2
                    { ps2PageId = page1
                    , ps2Sim = HM.singleton (SamePageRef (UnitId 5))
                        (toUnitSimStateDTOv1 richSimState) }
                v3 = migratePageSimDTOv2 v2
            psPageId v3 `shouldBe` page1
            HM.keys (psSim v3) `shouldBe` [SamePageRef (UnitId 5)]
            let migrated = HM.lookup (SamePageRef (UnitId 5)) (psSim v3)
            hazardOf migrated `shouldBe` Just FallPermitted
            -- Everything else carries across untouched.
            fmap simLocalPath migrated `shouldBe` Just (usLocalPath richSimState)

        it "migrateUnitSimDTOv2 migrates every page slice in a v2 payload" $
            let v2 = UnitSimDTOv2
                    [ PageSimDTOv2 page1 (HM.singleton (SamePageRef (UnitId 5))
                        (toUnitSimStateDTOv1 richSimState))
                    , PageSimDTOv2 page2 HM.empty ]
                UnitSimDTO v3Pages = migrateUnitSimDTOv2 v2
            in map psPageId v3Pages `shouldBe` [page1, page2]

        it "a v1 move target loses nothing but its (absent) policy" $
            let current = toUnitSimStateDTO richSimState
                back    = migrateUnitSimStateDTOv1 (toUnitSimStateDTOv1 richSimState)
                permitted = fmap (\t → t { mvtHazard = FallPermitted })
                                 (simTarget current)
            in back `shouldBe` current { simTarget = permitted }

    describe "frozen worldgen + item DTOs (boundary rule, review round 6)" $ do
        -- The nested worldgen config/state records and the recursive
        -- ItemInstance are no longer embedded live: each has a frozen DTO
        -- with an explicit conversion. These prove those conversions are
        -- lossless (identity) on non-trivial values, so a live-record field
        -- change surfaces as a compile error in a conversion rather than as
        -- silent v1 byte drift.
        it "ItemInstanceDTO round-trips a recursive (kit-in-kit) item" $
            fromItemInstanceDTO (toItemInstanceDTO richItem) `shouldBe` richItem

        it "GroundItemDTO round-trips a ground item carrying a recursive item" $
            let g = GroundItem richItem 3.5 4.5
            in fromGroundItemDTO (toGroundItemDTO g) `shouldBe` g

        it "WorldGenParamsDTO round-trips a populated worldgen config/climate \
           \tree (frozen nested records, no live embedding)" $
            fromWorldGenParamsDTO (toWorldGenParamsDTO richGenParams)
                `shouldBe` richGenParams

        it "a planted nested climate/calendar value survives the DTO round \
           \trip (recursion is lossless, not merely structural)" $ do
            let gp' = fromWorldGenParamsDTO (toWorldGenParamsDTO richGenParams)
            ccDaysPerMonth (wgpCalender gp') `shouldBe` 40
            csGlobalTemp (wgpClimateState gp') `shouldBe` 14.5
            (rcHumidity <$> HM.lookup (ClimateCoord 1 2)
                (cgRegions (csClimate (wgpClimateState gp'))))
                `shouldBe` Just 0.42

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

    describe "frozen tracked fixture" $
        it "decodes a frozen, tracked multi-component byte fixture -- not \
           \merely this test's own encoder output -- proving the component \
           \envelope round-trips from real stored bytes" $ do
            let bytes = hexDecode trackedComponentFixtureHex
            decodeSaveEnvelopeMetadata HS.empty bytes `shouldBe` Right richMeta
            case decodeSessionEnvelope HS.empty HS.empty bytes of
                Left err → expectationFailure (T.unpack err)
                Right (meta, snap, _luaComponents, isMigrated) → do
                    meta `shouldBe` richMeta
                    -- These are pre-#911 bytes (@world-pages@ v1), so
                    -- their pages come back with the old per-chunk
                    -- location flags PENDING rather than an instance
                    -- table -- exactly the state the load path then
                    -- resolves against the location registry before
                    -- publication. Applying that same resolution here
                    -- (this fixture places no locations, so the registry
                    -- is empty) is what makes the comparison against the
                    -- current snapshot shape meaningful rather than
                    -- vacuous.
                    -- They also predate #1092, so their identities come
                    -- back with language provenance ABSENT, never
                    -- inferred -- which is why the expectation strips it
                    -- from the current in-memory snapshot rather than the
                    -- fixture being re-cut. This is requirement 3 proven
                    -- against REAL historical bytes.
                    resolveFixturePages snap
                        `shouldBe` withoutLanguageProvenance richSnapshot
                    languageProvenanceOf snap page1 `shouldBe` Nothing
                    isMigrated `shouldBe` False

    -- | Issue #766 (save-overhaul C4) completes what #760's acceptance
    --   explicitly deferred: "preserve a fixture for the transitional
    --   payload only if it is deliberately supported by an explicit
    --   migration; otherwise document and test its intentional
    --   incompatibility." A real B1-era envelope (metadata + a single
    --   required @"session"@ component, no gameplay components at all)
    --   is now RECOGNIZED and migrated, both for full decode
    --   ('decodeSessionEnvelope') and metadata-only listing
    --   ('decodeSaveEnvelopeMetadata', what 'World.Save.Serialize.listSaves'
    --   calls) — a B1-era save is listable and loadable again, reporting
    --   its true, then-authoritative metadata unchanged.
    --   See "Test.Headless.World.Save.Compat" for the frozen fixture
    --   coverage (a real B1 envelope recovered from git history).
    describe "B1 -> current migration (issue #766, save-overhaul C4)" $ do
        it "a hand-built B1-shaped envelope (metadata + a required \
           \'session' component) migrates and lists under the current \
           \build rather than being rejected as unknown" $ do
            let b1Meta = minimalSaveMetadataV90
                b1Specs =
                    [ (metadataComponentId, legacyMetadataComponentVersion, True
                      , S.encode minimalSaveMetadataV1)
                    , (ComponentId "session", 90, True
                      , S.encode minimalSaveDataV90) ]
                bytes = case encodeEnvelope defaultEnvelopeLimits
                            currentEnvelopeVersion b1Specs of
                    Right b → b
                    Left e  → error ("test setup: " <> show e)
            decodeSaveEnvelopeMetadata HS.empty bytes `shouldBe` Right b1Meta
            case decodeSessionEnvelope HS.empty HS.empty bytes of
                Left err → expectationFailure (T.unpack err)
                Right (meta, snap, luaComponents, isMigrated) → do
                    meta `shouldBe` b1Meta
                    luaComponents `shouldBe` []
                    isMigrated `shouldBe` True
                    snapActivePage snap `shouldBe` WorldPageId "main_world"

        it "refuses to migrate a B1 session carrying non-empty legacy Lua \
           \module state, rather than silently discarding it (requirement \
           \7: the pre-#761 Lua deserializer that could interpret it is \
           \gone, so there is no honest translation left)" $ do
            let luaSd = minimalSaveDataV90
                    { sd90LuaModules = HM.singleton "unit_ai" "opaque legacy blob" }
            case migrateSessionV90 minimalSaveMetadataV90 luaSd of
                Right _   → expectationFailure
                    "expected non-empty legacy Lua state to be rejected"
                Left errs → do
                    errs `shouldSatisfy` (not . null)
                    map cePhase errs `shouldSatisfy` all (≡ MigratePhase)

    -- #760 requirement 9: a saved building/unit whose content DEFINITION
    -- is no longer registered must be a LOAD-VALIDATION FAILURE (the
    -- complete load is rejected before any live state is published), not
    -- the silent per-entity pruning fromBuildingSnapshot/fromUnitSnapshot
    -- fall back to. 'missingDefReferences' is the pure detector the load
    -- boundary (Engine.Scripting.Lua.API.Save.loadSaveFn) runs against the
    -- live managers' registered def key-sets before it touches anything;
    -- the real engine boundary itself needs a full engine (covered by the
    -- multiworld save probe's round-trip), but the decision logic is here.
    describe "missing gameplay definition rejection (#760 requirement 9)" $ do
        let knownB = HS.fromList ["test_building"]
            knownU = HS.fromList ["test_unit"]
            withB defName = BuildingSnapshot
                (HM.singleton (BuildingId 1)
                    ((minimalBuildingInstance []) { bisDefName = defName })) 10
            withU defName = UnitSnapshot
                (HM.singleton (UnitId 1)
                    ((minimalUnitInstance []) { uisDefName = defName })) 10
            emptyB = BuildingSnapshot HM.empty 10
            emptyU = UnitSnapshot HM.empty 10

        it "accepts a save whose every building/unit def resolves" $
            missingDefReferences knownB knownU
                [(page1, withB "test_building", withU "test_unit")]
                `shouldBe` []

        it "flags (does not silently drop) a building whose def is \
           \unregistered" $ do
            let miss = missingDefReferences knownB knownU
                          [(page1, withB "ghost_building", emptyU)]
            map mdrKind miss `shouldBe` ["building"]
            map mdrDefName miss `shouldBe` ["ghost_building"]
            map mdrPage miss `shouldBe` [page1]

        it "flags a unit whose def is unregistered" $ do
            let miss = missingDefReferences knownB knownU
                          [(page1, emptyB, withU "ghost_unit")]
            map mdrKind miss `shouldBe` ["unit"]
            map mdrDefName miss `shouldBe` ["ghost_unit"]

        it "reports EVERY missing reference across pages, not just the \
           \first (whole-session rejection)" $
            length (missingDefReferences knownB knownU
                        [ (page1, withB "ghost_building", emptyU)
                        , (page2, emptyB, withU "ghost_unit") ])
                `shouldBe` 2

        it "renders a reference naming the kind, page, and unresolved def" $
            case missingDefReferences knownB knownU
                     [(page1, withB "ghost_building", emptyU)] of
                [m] → do
                    renderMissingDefRef m `shouldSatisfy` T.isInfixOf "ghost_building"
                    renderMissingDefRef m `shouldSatisfy` T.isInfixOf "building"
                    renderMissingDefRef m `shouldSatisfy` T.isInfixOf "page1"
                other → expectationFailure
                    ("expected exactly one missing ref, got " <> show other)

    -- #760 round 8: recursive item-instance id validation. The previous
    -- 'allItemInstanceIds' only ever looked at a container's OUTER id,
    -- so a nested item's id colliding with the allocator (or with
    -- another item elsewhere) went undetected.
    describe "recursive item-instance id validation (#760 round 8)" $ do
        it "rejects a NESTED item id at/above the item allocator, not just \
           \an outer container's id" $ do
            let nestedTooHigh = richItem
                    { iiInstanceId = 5
                    , iiContents = case iiContents richItem of
                        (b : _) → [ b { iiInstanceId = 999999 } ]
                        []      → []
                    }
                badPage = (minimalPage page1)
                    { pgsBuildings = BuildingSnapshot
                        (HM.singleton (BuildingId 1)
                            ((minimalBuildingInstance [nestedTooHigh])))
                        10 }
                snap = buildSessionSnapshot
                         minimalGlobals { sgNextItemId = 1000 } [badPage]
            validateSessionSnapshot snap
                `shouldSatisfy` any (\e → case e of
                    ItemInstanceIdNotBelowAllocator 999999 → True
                    _                                      → False)

        it "rejects a NESTED item id duplicating another item's id \
           \elsewhere in the session" $ do
            let dupNested = richItem
                    { iiInstanceId = 5
                    , iiContents = case iiContents richItem of
                        (b : _) → [ b { iiInstanceId = 5 } ]
                        []      → []
                    }
                badPage = (minimalPage page1)
                    { pgsBuildings = BuildingSnapshot
                        (HM.singleton (BuildingId 1)
                            (minimalBuildingInstance [dupNested]))
                        10 }
                snap = buildSessionSnapshot
                         minimalGlobals { sgNextItemId = 1000 } [badPage]
            validateSessionSnapshot snap
                `shouldSatisfy` any (\e → case e of
                    DuplicateItemInstanceId 5 → True
                    _                         → False)

        it "accepts a session whose nested item ids are all distinct and \
           \below the allocator (the recursive check does not over-reject \
           \a valid recursive item)" $
            captureSessionSnapshot minimalGlobals { sgNextItemId = 1000 }
                [fullPage page1] `shouldSatisfy` (\r → case r of
                    Right _ → True
                    Left _  → False)

    -- #760 round 8: item def-name validation, including recursively
    -- through 'iiContents'.
    describe "missing item definition rejection (#760 round 8)" $ do
        let knownItems = HS.fromList ["first_aid_kit", "bandage", "mini_kit"]
            pageWith w = [(page1, w)]

        it "accepts a page whose every item (incl. nested contents) \
           \resolves" $
            missingItemDefReferences knownItems
                (pageWith (minimalWorldPageSave page1)
                    { wpsBuildings = BuildingSnapshot
                        (HM.singleton (BuildingId 1)
                            (minimalBuildingInstance [richItem])) 10 })
                `shouldBe` []

        it "flags a NESTED item (inside a kit-in-kit) whose def is \
           \unregistered, not just the outer container" $ do
            let missing = missingItemDefReferences
                    (HS.fromList ["first_aid_kit", "bandage"])
                    (pageWith (minimalWorldPageSave page1)
                        { wpsBuildings = BuildingSnapshot
                            (HM.singleton (BuildingId 1)
                                (minimalBuildingInstance [richItem])) 10 })
            map midrDefName missing `shouldBe` ["mini_kit"]

        it "flags an unregistered ground item" $ do
            let ground = emptyGroundItems
                    { gisNextId = 1
                    , gisItems = HM.singleton 0
                        (GroundItem (richItem { iiContents = [] }) 1 1) }
                missing = missingItemDefReferences (HS.fromList ["bandage"])
                    (pageWith (minimalWorldPageSave page1)
                        { wpsGroundItems = ground })
            map midrDefName missing `shouldBe` ["first_aid_kit"]

        it "flags an unregistered item in unit inventory/equipped/\
           \accessories" $ do
            let u = (minimalUnitInstance [richItem { iiContents = [] }])
                    { uisEquipped = HM.singleton "head"
                        (richItem { iiContents = [], iiInstanceId = 5000
                                  , iiDefName = "ghost_helmet" }) }
                missing = missingItemDefReferences (HS.fromList ["bandage"])
                    (pageWith (minimalWorldPageSave page1)
                        { wpsUnits = UnitSnapshot
                            (HM.singleton (UnitId 1) u) 10 })
            HS.fromList (map midrDefName missing)
                `shouldBe` HS.fromList ["first_aid_kit", "ghost_helmet"]

    -- #1090: the three item enumerations became one. These pin what
    -- unification is FOR — that every consumer observes every
    -- container — against an explicit expected id set rather than
    -- against each other, since pairwise agreement would be satisfied
    -- by a container the shared enumeration drops for all of them.
    describe "shared item enumeration (#1090)" $ do
        let coverPages = [(page1, containerCoveragePageSave)]
            -- No item def is registered, so every planted item is
            -- reported and the source labels cover all six containers.
            coverMissing = missingItemDefReferences HS.empty coverPages

        it "the allocator/duplicate walk observes every container" $
            HS.fromList (allItemInstanceIds containerCoverageSnapshot)
                `shouldBe` HS.fromList containerCoverageIds

        it "the allocator/duplicate walk reports each id exactly once" $
            length (allItemInstanceIds containerCoverageSnapshot)
                `shouldBe` length containerCoverageIds

        it "the load-time known-entity set observes every container" $
            keItemInstances
                (knownEntitiesFromSaveData
                    (snapshotToSaveData (SaveRequestMeta "s" "t" False)
                         containerCoverageSnapshot))
                `shouldBe` HS.fromList (map fromIntegral containerCoverageIds)

        it "the missing-item-def validator observes every container" $
            HS.fromList (map midrItemId coverMissing)
                `shouldBe` HS.fromList containerCoverageIds

        it "reports the right source label for every container's OUTER \
           \and NESTED item" $
            HM.fromList [ (midrItemId r, midrSource r) | r ← coverMissing ]
                `shouldBe` HM.fromList
                    [ (100, "ground item"),      (101, "ground item")
                    , (200, "unit inventory"),   (201, "unit inventory")
                    , (300, "unit equipped"),    (301, "unit equipped")
                    , (400, "unit accessories"), (401, "unit accessories")
                    , (500, "building storage"), (501, "building storage")
                    , (600, "building materials delivered")
                    , (601, "building materials delivered") ]

        -- Requirement 4: unifying the enumeration must not renumber
        -- either consumer's output. The two were written with opposite
        -- conventions and both orders are observable, so the shared
        -- walk keeps both.
        it "preserves the id walk's ground-first container order" $
            allItemInstanceIds containerCoverageSnapshot
                `shouldBe` [ 100, 101      -- ground items
                           , 200, 201      -- unit inventory
                           , 300, 301      -- unit equipped
                           , 400, 401      -- unit accessories
                           , 600, 601      -- building materials delivered
                           , 500, 501 ]    -- building storage

        it "preserves the missing-item-def validator's buildings-first \
           \container order" $
            map midrItemId coverMissing
                `shouldBe` [ 500, 501      -- building storage
                           , 600, 601      -- building materials delivered
                           , 200, 201      -- unit inventory
                           , 300, 301      -- unit equipped
                           , 400, 401      -- unit accessories
                           , 100, 101 ]    -- ground items

    -- #760 round 8: craft-bill recipe validation.
    describe "missing recipe definition rejection (#760 round 8)" $ do
        it "accepts a page whose every bill's recipe resolves" $
            missingRecipeReferences (HS.fromList ["smelt_steel"])
                [(page1, (minimalWorldPageSave page1)
                    { wpsCraftBills = richBills })]
                `shouldBe` []

        it "flags a bill whose recipe is no longer registered" $ do
            let missing = missingRecipeReferences (HS.fromList ["other_recipe"])
                    [(page1, (minimalWorldPageSave page1)
                        { wpsCraftBills = richBills })]
            map mrrRecipe missing `shouldBe` ["smelt_steel"]
            map mrrPage missing `shouldBe` [page1]

    -- #760 round 9 (opposite-brand review): UntilStock craft-bill output-
    -- item validation, the same content-definition load-rejection contract
    -- as recipe/item/construct-target references above. 'richBills' is
    -- already an UntilStock bill with 'cbOutputItem = "steel_bar"'.
    describe "missing craft-bill output-item definition rejection \
             \(#760 round 9)" $ do
        it "accepts a page whose UntilStock bill's output item resolves" $
            missingBillOutputItemReferences (HS.fromList ["steel_bar"])
                [(page1, (minimalWorldPageSave page1)
                    { wpsCraftBills = richBills })]
                `shouldBe` []

        it "flags an UntilStock bill whose output item is no longer \
           \registered" $ do
            let missing = missingBillOutputItemReferences
                    (HS.fromList ["other_item"])
                    [(page1, (minimalWorldPageSave page1)
                        { wpsCraftBills = richBills })]
            map mbirDefName missing `shouldBe` ["steel_bar"]
            map mbirPage missing `shouldBe` [page1]

        it "does not flag a FixedCount/RepeatForever bill, whose \
           \cbOutputItem is always empty" $ do
            let plainBill = (cbsBills richBills HM.! BillId 3)
                    { cbMode = RepeatForever, cbOutputItem = "" }
                bills = richBills { cbsBills = HM.singleton (BillId 3) plainBill }
            missingBillOutputItemReferences HS.empty
                [(page1, (minimalWorldPageSave page1)
                    { wpsCraftBills = bills })]
                `shouldBe` []

    -- #760 round 8: construct-designation building-def-name reference
    -- validation.
    describe "missing construct-target building definition rejection \
             \(#760 round 8)" $ do
        let designation defName = HM.singleton (1, 2) ConstructDesignation
                { cdZ = 0, cdTarget = CtBuilding defName, cdStatus = CsPending
                , cdProgress = 0, cdAttempt = firstConstructAttemptId
                , cdPayment = CpUnpaid }

        it "accepts a construct designation whose building target resolves" $
            missingConstructDefReferences (HS.fromList ["cargo_hold_S"])
                [(page1, (minimalWorldPageSave page1)
                    { wpsConstructDesignations = designation "cargo_hold_S" })]
                `shouldBe` []

        it "flags a construct designation whose building target is \
           \unregistered" $ do
            let missing = missingConstructDefReferences HS.empty
                    [(page1, (minimalWorldPageSave page1)
                        { wpsConstructDesignations = designation "ghost_bldg" })]
            map mcdDefName missing `shouldBe` ["ghost_bldg"]
            map mcdTile missing `shouldBe` [(1, 2)]

    -- #760 round 8: the "texture-palette" component no longer rides on
    -- TexPalette's own live Serialize instance.
    describe "texture-palette frozen DTO (#760 round 8)" $
        it "round-trips a non-empty palette through the component codec" $ do
            let tp = TexPalette
                    { tpPathToId = HM.fromList [("a.png", 0), ("b.png", 1)]
                    , tpIdToPath = HM.fromList [(0, "a.png"), (1, "b.png")]
                    , tpNextId   = 2 }
                snap = richSnapshot { snapTexPalette = tp }
            case ccDecode texPaletteCodec 1 (ccEncode texPaletteCodec snap) of
                Left e  → expectationFailure (T.unpack (renderComponentError e))
                Right d → fromTexPaletteDTO d `shouldBe` tp

    -- #1091: 'WorldPageId' grew a field label so the nine hand-written
    -- @where unWorldPageId (WorldPageId t) = t@ clauses in
    -- 'World.Save.Types' (plus the two named accessors) could go. That
    -- is a pure cleanup ONLY if three observable things are unchanged:
    -- every render string, the stock 'Show' representation the save
    -- validation diagnostics and 'Sim.Thread' print through 'tshow',
    -- and the wire bytes 'deriving newtype Serialize' produces.
    describe "WorldPageId accessor cleanup (#1091)" $ do
        -- The expected strings below are written out in full rather
        -- than rebuilt from the record's own fields, so a change to
        -- either side of a render function fails this gate.
        it "renderMissingDefRef is unchanged" $
            renderMissingDefRef MissingDefRef
                { mdrKind = "building", mdrPage = page1
                , mdrEntity = 7, mdrDefName = "ghost_building" }
                `shouldBe`
                "building #7 on page 'page1' references unknown \
                \definition 'ghost_building'"

        it "renderMissingItemDefRef is unchanged" $
            renderMissingItemDefRef MissingItemDefRef
                { midrSource = "unit inventory", midrPage = page1
                , midrItemId = 42, midrDefName = "ghost_item" }
                `shouldBe`
                "unit inventory item #42 on page 'page1' references \
                \unknown item definition 'ghost_item'"

        it "renderMissingRecipeRef is unchanged" $
            renderMissingRecipeRef MissingRecipeRef
                { mrrPage = page2, mrrBillId = 3, mrrRecipe = "ghost_recipe" }
                `shouldBe`
                "craft bill #3 on page 'page2' references unknown \
                \recipe 'ghost_recipe'"

        it "renderMissingBillOutputItemRef is unchanged" $
            renderMissingBillOutputItemRef MissingBillOutputItemRef
                { mbirPage = page1, mbirBillId = 9, mbirDefName = "ghost_output" }
                `shouldBe`
                "craft bill #9 on page 'page1' references unknown \
                \output item definition 'ghost_output'"

        it "renderMissingConstructDefRef is unchanged" $
            renderMissingConstructDefRef MissingConstructDefRef
                { mcdPage = page1, mcdTile = (1, 2), mcdDefName = "ghost_bldg" }
                `shouldBe`
                "construct designation at (1,2) on page 'page1' \
                \references unknown building definition 'ghost_bldg'"

        it "renderMissingMaterialRef is unchanged" $
            renderMissingMaterialRef MissingMaterialRef
                { mmrSource = "edit log", mmrPage = page2
                , mmrCoord = (-3, 4), mmrMatId = 200 }
                `shouldBe`
                "edit log at (-3,4) on page 'page2' references unknown \
                \material id 200"

        it "renderMissingFloraRef is unchanged" $
            renderMissingFloraRef MissingFloraRef
                { mfrSource = "crop plot", mfrPage = page1
                , mfrCoord = (5, -6), mfrFloraId = 77 }
                `shouldBe`
                "crop plot at (5,-6) on page 'page1' references unknown \
                \flora id 77"

        it "renderMissingLocationRef is unchanged" $
            renderMissingLocationRef MissingLocationRef
                { mlrPage = page2, mlrCoord = (0, 1), mlrLocId = "ghost_loc" }
                `shouldBe`
                "location overlay chunk (0,1) on page 'page2' references \
                \unknown location id 'ghost_loc'"

        it "renderMissingInfectionRef is unchanged" $
            renderMissingInfectionRef MissingInfectionRef
                { mirPage = page1, mirUnitId = 12
                , mirWoundPart = "left_arm", mirInfType = "ghost_rot" }
                `shouldBe`
                "unit #12 wound (left_arm) on page 'page1' references \
                \unknown infection id 'ghost_rot'"

        -- The field label is deliberately NOT reflected in 'Show': the
        -- record-syntax derivation would print
        -- @WorldPageId {unWorldPageId = "page1"}@, and page ids reach
        -- diagnostics through 'tshow'.
        it "Show still prints the unlabelled constructor application" $ do
            show page1 `shouldBe` "WorldPageId \"page1\""
            showsPrec 11 page1 "" `shouldBe` "(WorldPageId \"page1\")"

        it "the accessor reads back exactly what the constructor wrapped" $
            unWorldPageId (WorldPageId "main_world") `shouldBe` "main_world"

        -- Requirement 4: a field label changes nothing about what
        -- @deriving newtype Serialize@ derives, which is why no save
        -- version moved with this cleanup.
        it "encodes byte-identically to the underlying Text" $
            S.encode (WorldPageId "main_world")
                `shouldBe` S.encode ("main_world" ∷ Text)

-- Helpers -----------------------------------------------------------

-- | The load path's own pre-#911 location resolution
--   ('World.Save.Types.resolveLegacyLocations'), applied to every page
--   of a decoded snapshot.
resolveFixturePages ∷ SessionSnapshot → SessionSnapshot
resolveFixturePages snap = snap
    { snapPages = HM.map resolvePage (snapPages snap) }
  where
    resolvePage p = p
        { pgsGenParams = expectGeometry
            (resolveLegacyLocationParams emptyLocationRegistry
                                         (pgsGenParams p)) }

-- | The expectation for a fixture whose bytes predate #1092: identical
--   to the live snapshot except that no identity carries language
--   provenance. Written as a transformation of the SAME expected value
--   (rather than a second hand-maintained fixture) so a drop anywhere
--   ELSE in the page still fails the comparison.
withoutLanguageProvenance ∷ SessionSnapshot → SessionSnapshot
withoutLanguageProvenance snap = snap
    { snapPages = HM.map stripPage (snapPages snap) }
  where
    stripPage p = p
        { pgsIdentity = (\i → i { wiLanguage = Nothing }) <$> pgsIdentity p }

languageProvenanceOf
    ∷ SessionSnapshot → WorldPageId → Maybe LanguageProvenance
languageProvenanceOf snap pid =
    wiLanguage =≪ (pgsIdentity =≪ HM.lookup pid (snapPages snap))

isLeftC ∷ Either ComponentError a → Bool
isLeftC (Left _) = True
isLeftC _        = False

elemIndex' ∷ Eq a ⇒ a → [a] → Maybe Int
elemIndex' x = go 0
  where go _ [] = Nothing
        go i (y:ys) | x ≡ y = Just i
                    | otherwise = go (i+1) ys

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

-- | A dummy registered component for cycle testing (its codec bodies are
--   never exercised — dependencyOrder only reads id + deps).
stubComponent ∷ ComponentId → [ComponentId] → RegisteredComponent
stubComponent cid deps = RegisteredComponent
    { rcId = cid, rcVersion = 1, rcInputVers = [1], rcRequired = True
    , rcDeps = deps, rcEncode = const BS.empty
    , rcPrepare = const (Right (\s → Right s)) }

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

trackedComponentFixtureHex ∷ String
trackedComponentFixtureHex =
    "53595241000000010000000000000212000000000000000b000000000000\
    \00096275696c64696e677300000001010000000000000000000000000000\
    \00973dafc93879ea3b82000000000000000c636f72652d73657373696f6e\
    \00000001010000000000000097000000000000005574d3010096cbbe2b00\
    \0000000000000b63726166742d62696c6c73000000010100000000000000\
    \ec000000000000003abeec8f6ff4c58c2600000000000000086d65746164\
    \617461000000010100000000000001260000000000000051cda064806651\
    \992f000000000000000b706f7765722d6e6f646573000000010100000000\
    \00000177000000000000003abeec8f6ff4c58c26000000000000000f7465\
    \78747572652d70616c65747465000000010100000000000001b100000000\
    \0000001088201fb960ff64650000000000000008756e69742d73696d0000\
    \00010100000000000001c1000000000000007b81797b8874157310000000\
    \0000000005756e6974730000000101000000000000023c00000000000000\
    \f9fc6ed2ffd1c79265000000000000000e776f726c642d61637469766974\
    \790000000101000000000000033500000000000000c2251087e70708d624\
    \000000000000000b776f726c642d65646974730000000101000000000000\
    \03f700000000000000321ed7627acac89064000000000000000b776f726c\
    \642d70616765730000000101000000000000042900000000000004f8b7df\
    \1cb66e09260ff8327e7c13afca0300000000000000020000000000000005\
    \7061676531000000000000000100000001000000000000000d746573745f\
    \6275696c64696e6700000000000000000000000000000000000000000000\
    \000000000000000000000000000000000001000000000000000100000000\
    \0000000042c8000000000000000000000000000000000000000000000000\
    \000570616765320000000000000000404500000000000000000000000000\
    \640000000a0000000a000000000000000570616765310000000000000001\
    \00000000000000057061676531010000000000000005706167653140e000\
    \004100000040400000030000000000000002000000000000000570616765\
    \310000000000000000000000010000000000000005706167653200000000\
    \00000000000000010000000000000004736c6f74000000000001e2400000\
    \000000000080000000000000000a00000000000000027473010000000000\
    \00000a5269636820576f726c640100000000000000076120676c6f737300\
    \000000000000020000000000000005706167653100000000000000000000\
    \000100000000000000057061676532000000000000000000000001000000\
    \000000000000000000000000000000000000000002000000000000000570\
    \616765310000000000000001000000010000000000000000000000000000\
    \000000000000000000000000000000000000000000000000000000000000\
    \000000000000000000000000000000000000000000000000000000000000\
    \000005706167653200000000000000000000000000000002000000000000\
    \000570616765310000000000000001000000010000000000000009746573\
    \745f756e69743f8000000000000000000000000000000000000000000000\
    \0000000000000000000000000000000000000000000469646c6500000000\
    \000000087374616e64696e67000000000000000000000000000000000000\
    \000000000000000000000000000000000000000000000000000000000000\
    \000000000000000000000000000000000000000000000000000000000000\
    \0000000000000000000000000000000000000000000040a0000000000000\
    \000000000000000000000005706167653200000000000000000000000000\
    \000002000000000000000570616765310000000000000000000000000000\
    \000000000000000000000000000000000000000000000000000000000000\
    \000000000000000000000000000000000000000000000000000000000000\
    \000000000000000000000000000570616765320000000000000000000000\
    \000000000000000000000000000000000000000000000000000000000000\
    \000000000000000000000000000000000000000000000000000000000000\
    \000000000000000000000000000000000200000000000000057061676531\
    \000000000000000000000000000000057061676532000000000000000000\
    \0000000000000200000000000000057061676531000000000001e2400000\
    \000000000080000000000000000a0000000000000000000000000000001e\
    \000000000000000c0000000000000018000000000000003c3ecccccd3f00\
    \0000000000000000001c0000000000000000000000000000000000000080\
    \000000000000000000000000000000000000000000000000000000000000\
    \000000000000000000010000000000000000000000000000000000000000\
    \000000000000000000000000000000000000000000000000000000000000\
    \000000000000000000000000000000000000000000000000000000000000\
    \000000000000000000000000000000000000000000000000000000000000\
    \000000000000000000000000000000000000000000000000000000000000\
    \0000000000000000000000323f8000003e99999a3f3333333fc000003f80\
    \00003f0000003f8333330000000000000000000000000000002000000000\
    \000000000000000000000000000000000000000000000000000000000000\
    \000000000000000000000000000000000000000000000000000000000000\
    \3f800000000000003f8000003f3333333fa0000000000000000000060000\
    \000000000016000000000000000c3f8000003f8000003f80000000000000\
    \000000010000000000000002000000000000000100000000000000030000\
    \000000000001000000000000000300000000000000010000000000000003\
    \000000000000000000000000000000000000000000000000000000000000\
    \00000000000000000000000000000000000c000000000000000000000000\
    \00000001000000000000000100000000000000010001000000000000000a\
    \5269636820576f726c640100000000000000076120676c6f737300000000\
    \000000057061676532000000000000002a00000000000000800000000000\
    \00000a0000000000000000000000000000001e000000000000000c000000\
    \0000000018000000000000003c3ecccccd3f000000000000000000001c00\
    \000000000000000000000000000000000000800000000000000000000000\
    \000000000000000000000000000000000000000000000000000000000100\
    \000000000000000000000000000000000000000000000000000000000000\
    \000000000000000000000000000000000000000000000000000000000000\
    \000000000000000000000000000000000000000000000000000000000000\
    \000000000000000000000000000000000000000000000000000000000000\
    \000000000000000000000000000000000000000000000000000000000000\
    \323f8000003e99999a3f3333333fc000003f8000003f0000003f83333300\
    \000000000000000000000000000020000000000000000000000000000000\
    \000000000000000000000000000000000000000000000000000000000000\
    \000000000000000000000000000000000000003f800000000000003f8000\
    \003f3333333fa00000000000000000000600000000000000160000000000\
    \00000c3f8000003f8000003f800000000000000000000100000000000000\
    \020000000000000001000000000000000300000000000000010000000000\
    \000003000000000000000100000000000000030000000000000000000000\
    \000000000000000000000000000000000000000000000000000000000000\
    \0000000000000c0000000000000000000000000000000100000000000000\
    \0100000000000000010000"
