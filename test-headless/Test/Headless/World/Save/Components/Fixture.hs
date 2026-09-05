{-# LANGUAGE ScopedTypeVariables #-}
-- | Synthetic fixture truth shared by more than one child of the
--   "save components" gate (issue #2043). Every value here is a
--   top-level CAF built from literals: this module boots no engine,
--   performs no IO and touches no filesystem, exactly like the facade
--   'Test.Headless.World.Save.Components' it serves.
--
--   Requirement 6 of #2043 makes this the SINGLE definition site for
--   the fixtures below -- no child may re-derive 'minimalPage',
--   'richSnapshot', a payload fingerprint or a frozen byte string of
--   its own. A fixture used by exactly one owner belongs in that
--   owner's child module instead, not here.
module Test.Headless.World.Save.Components.Fixture
    ( -- * Page and session snapshots
      page1, page2, canon, defaultGP, minimalPage, fullPage, fullSnapshot
    , richSnapshot, richMeta, encodeRich, minimalGlobals
      -- * Entity, building and item fixtures
    , minimalUnitInstance, minimalBuildingInstance, minimalSimState
    , richBills, richNodes, richItem, significantOwner
      -- * The transfer order 'fullPage' plants
    , onePendingOrder
      -- * Frozen v90 metadata and save shapes
    , minimalWorldPageSaveV90, minimalSaveMetadataV90, minimalSaveDataV90
      -- * Component DTO constructors
    , pageCore
      -- * Assertion helpers
    , isLeft, mentions
    ) where

import UPrelude
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.Serialize as S
import qualified Data.Text as T

import World.Save.Envelope
import World.Save.Component.Types
import World.Save.Component.Session
import World.Save.Component.Page
import World.Save.Component.Entities
import Unit.Transfer
    ( TransferBatch(..), TransferEndpoint(..), TransferItemRef(..)
    , TransferState(..), QueuedTransfer(..) )
import Unit.Transfer.Orders (TransferOrders(..), addTransferOrder)
import World.Save.Compat.SessionV90
import Language.Generated.Types
    (LanguageProvenance(..), LangSeed(..), GeneratorVersion(..))
import World.Save.Snapshot
import Location.Bounds (AbsBounds(..))
import Location.Instance
    ( LocationInstance(..), LocationInstanceId(..), LocationLifecycle(..)
    , LocationSignificantItem(..) )
import World.Save.Snapshot.Adapter
    ( SaveRequestMeta(..), snapshotSaveMetadata )
import World.Save.Types
    ( SaveMetadata(..), BuildingSnapshot(..), BuildingInstanceSnapshot(..)
    , UnitSnapshot(..), UnitInstanceSnapshot(..) )
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)
import World.Page.Types (WorldPageId(..), WorldIdentity(..))
import World.Render.Zoom.Types (ZoomMapMode(..))
import World.Tool.Types (ToolMode(..))
import Engine.Graphics.Camera (CameraFacing(..))
import Structure.Palette (emptyTexPalette)
import Item.Ground (emptyGroundItems, GroundItems(..), GroundItem(..))
import Item.Types (ItemInstance(..), ItemStorage(..))
import World.Spoil.Types (emptySpoilPiles)
import World.Flora.Harvest (emptyFloraHarvests)
import World.Edit.Types (emptyWorldEdits, WorldEdit(..))
import World.Chunk.Types (ChunkCoord(..))
import World.Mine.Types (MineDesignation(..))
import World.Construct.Attempt (firstConstructAttemptId)
import Craft.Bills
    ( emptyCraftBills, CraftBill(..), CraftBills(..), BillId(..), BillMode(..) )
import Unit.Transfer.Orders (emptyTransferOrders)
import Power.Types
    ( emptyPowerNodes, PowerNode(..), PowerNodes(..), PowerNodeId(..)
    , PowerRole(..) )
import Building.Types (BuildingId(..))
import Unit.Types (UnitId(..))
import Unit.Sim.Types (UnitSimState(..), Pose(..), UnitActivity(..))
import Unit.Direction (Direction(..))
import Building.Knowledge (emptyContainerKnowledge)
import World.Flora.Identity (firstPlantedFloraCursor)
import Test.Headless.Harness.GeneratedIds (fixtureGeneratedWorldIdForPage)

-- ---------------------------------------------------------------------
-- Fixtures (mirror Test.Headless.Save.Snapshot's minimal* pattern)
-- ---------------------------------------------------------------------

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
    , pgsCropPlots    = HM.empty
    , pgsPlantDesignations = HM.empty
    , pgsContainerKnowledge = emptyContainerKnowledge
    , pgsIdentity     = Nothing
    , pgsGeneratedId  = Just (fixtureGeneratedWorldIdForPage pid)
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
--
--   #2021: no generated-world ids either. A v90 payload migrates through
--   @world-pages@ v1, which predates them, so its pages carry none — and
--   an all-absent save is exactly the legacy shape
--   'World.Save.Component.metadataErrors' accepts.
minimalSaveMetadataV90 ∷ SaveMetadata
minimalSaveMetadataV90 = SaveMetadata
    { smName = "b1-hand-built", smSeed = wgpSeed defaultGP
    , smWorldSize = wgpWorldSize defaultGP, smPlateCount = wgpPlateCount defaultGP
    , smTimestamp = "2026-07-16T00:00:00.000000Z"
    , smWorldName = Nothing, smWorldGloss = Nothing, smAutosave = False
    , smGeneratedWorldIds = [] }

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

-- | A minimal, otherwise-valid frozen v90 'SaveDataV90' (issue #766,
--   save-overhaul C4) — the exact shape a real #759-era @"session"@
--   component payload took. Used to prove the ENVELOPE-level dispatch
--   recognizes and migrates a hand-built B1 envelope; the historical
--   byte-for-byte fixture (recovered from git history) lives in
--   "Test.Headless.World.Save.Compat.B1Fixture".
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

-- | A placed location owing @entries@ (#917), with every other field
--   at a value this check never reads.
significantOwner ∷ [LocationSignificantItem] → LocationInstance
significantOwner entries = LocationInstance
    { liId              = LocationInstanceId 1
    , liDefId           = "ruin_small"
    , liChunk           = ChunkCoord 0 0
    , liAnchor          = (8, 8)
    , liBounds          = AbsBounds 6 6 10 10
    , liDisplayName     = "Small Ruin"
    , liGloss           = Nothing
    , liEtymology       = Nothing
    , liLifecycle       = LifecycleDiscovered
    , liContentsSpawned = False
    , liEncounter       = Nothing
    , liSignificant     = entries
    , liClearEventEmitted = False
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
    , pcIdentity = Nothing
    , pcGeneratedId = Just (fixtureGeneratedWorldIdForPage pid) }

isLeft ∷ Either a b → Bool
isLeft (Left _) = True
isLeft _        = False

-- | 'addTransferOrder' refuses on an exhausted allocator (#1246 review
--   round 2), which no fixture here can reach — every one starts from
--   'emptyTransferOrders'. Fail loudly rather than defaulting, so a
--   future change that DID exhaust it surfaces as this error instead of
--   as a silently empty store.

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

mentions ∷ Text → [ComponentError] → Bool
mentions needle = any (T.isInfixOf needle ∘ ceMessage)
