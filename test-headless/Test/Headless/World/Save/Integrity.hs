-- | The "persistence reference integrity" gate (issue #764,
--   save-overhaul C3): typed persistent references
--   ("World.Save.Reference") and the shared save/load integrity graph
--   ("World.Save.Integrity"). Pure — no engine, no IO. Fixtures mirror
--   "Test.Headless.World.Save.Components"'s minimal* pattern.
--
--   Coverage this file does NOT duplicate, because it already exists
--   elsewhere and duplicating it would only add regression risk for no
--   gain (see "World.Save.Integrity"'s module haddock for why):
--   duplicate page/unit/building/item ids and allocator-too-low
--   ("Test.Headless.Save.Snapshot"), duplicate/mismatched craft-bill and
--   power-node ids ("Test.Headless.World.Save.Components"), missing
--   gameplay content definitions
--   ("Test.Headless.World.Save.Components"'s "missing gameplay
--   definition rejection" group), and component-registry dependency-
--   cycle rejection (both "Test.Headless.World.Save.Components"'
--   "registry contract" group and "Test.Headless.Lua.SaveModules"'
--   dependency-ordering coverage — this is what "prohibited ownership/
--   dependency cycles" cashes out to in this codebase; there is no
--   separate generic cycle-detecting graph walk here to test).
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "persistence reference integrity"'@.
module Test.Headless.World.Save.Integrity (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.List as L
import qualified Data.Text as T

import World.Save.Reference
import World.Save.Integrity
import World.Save.Snapshot
import World.Save.Component.Types
    ( craftBillsComponentId, powerNodesComponentId, transferOrdersComponentId
    , worldPagesComponentId )
import World.Save.Envelope.Types (ComponentId(..))
import World.Save.Component.Entities
    ( CraftBillDTO(..), CraftBillDTOv1(..), migrateCraftBillDTOv1
    , PowerNodeDTO(..), PowerNodeDTOv1(..), migratePowerNodeDTOv1
    , toCraftBillDTO )
import World.Save.Types
    ( BuildingSnapshot(..), BuildingInstanceSnapshot(..)
    , UnitSnapshot(..), UnitInstanceSnapshot(..) )
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)
import Location.Bounds (AbsBounds(..))
import Location.Instance
    ( LocationEncounter(..), LocationEncounterOccupant(..)
    , LocationInstance(..), LocationInstances(..), LocationInstanceId(..)
    , LocationLifecycle(..) )
import World.Chunk.Types (ChunkCoord(..))
import World.Page.Types (WorldPageId(..))
import World.Render.Zoom.Types (ZoomMapMode(..))
import Engine.Graphics.Camera (CameraFacing(..))
import Structure.Palette (emptyTexPalette)
import Item.Ground (emptyGroundItems)
import World.Spoil.Types (emptySpoilPiles)
import World.Flora.Harvest (emptyFloraHarvests)
import World.Flora.CropPlot (emptyCropPlots)
import World.Edit.Types (emptyWorldEdits)
import Craft.Bills
    (emptyCraftBills, CraftBill(..), CraftBills(..), BillId(..), BillMode(..))
import Unit.Transfer
    ( TransferBatch(..), TransferEndpoint(..), TransferItemRef(..)
    , TransferState(..), QueuedTransfer(..) )
import Unit.Transfer.Orders
    (TransferOrders, addTransferOrder, emptyTransferOrders)
import Item.Types (ItemInstance(..))
import Data.Int (Int64)
import Power.Types
    (emptyPowerNodes, PowerNode(..), PowerNodes(..), PowerNodeId(..), PowerRole(..))
import Building.Types (BuildingId(..))
import Unit.Types (UnitId(..))
import Unit.Direction (Direction(..))
import Building.Knowledge (emptyContainerKnowledge)
import World.Construct.Attempt (firstConstructAttemptId)
import World.Flora.Identity (firstPlantedFloraCursor)

page1, page2 ∷ WorldPageId
page1 = WorldPageId "page1"
page2 = WorldPageId "page2"

minimalPage ∷ WorldPageId → PageSnapshot
minimalPage pid = PageSnapshot
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
    , pgsBuildings    = BuildingSnapshot { bsnInstances = HM.empty, bsnNextId = 100 }
    , pgsUnits        = UnitSnapshot { usnInstances = HM.empty, usnNextId = 100 }
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

minimalBuilding ∷ BuildingInstanceSnapshot
minimalBuilding = BuildingInstanceSnapshot
    { bisDefName = "test_building", bisAnchorX = 0, bisAnchorY = 0
    , bisGridZ = 0, bisSpawnedAt = 0, bisTileW = 1, bisTileH = 1
    , bisSpawnRemaining = 0, bisBuildProgress = 100
    , bisMaterialsDelivered = HM.empty, bisStorage = [] }

minimalUnit ∷ UnitInstanceSnapshot
minimalUnit = UnitInstanceSnapshot
    { uisDefName = "test_unit", uisBaseWidth = 1, uisGridX = 0, uisGridY = 0
    , uisGridZ = 0, uisFacing = DirS, uisCurrentAnim = ""
    , uisAnimStart = 0, uisAnimReverse = False, uisActivity = "idle"
    , uisPose = "standing", uisAnimStride = 0, uisStats = HM.empty
    , uisModifiers = HM.empty, uisSkills = HM.empty, uisKnowledge = HM.empty
    , uisInventory = [], uisEquipped = HM.empty, uisAccessories = []
    , uisFactionId = "", uisWounds = [], uisScars = [], uisImmuneResponse = 0
    , uisImmunities = HM.empty, uisBlood = 5, uisName = "" }

minimalGlobals ∷ WorldPageId → SessionGlobals
minimalGlobals active = SessionGlobals
    { sgGameTime       = 0
    , sgTexPalette     = emptyTexPalette
    , sgNextItemId     = 1000
    , sgNextBuildingId = 100
    , sgNextUnitId     = 100
    , sgActivePage     = active
    , sgVisiblePages   = [active]
    , sgLiveCamera     = LiveCameraSnapshot
        { lcsOwnerPage = Just active, lcsX = 0, lcsY = 0, lcsZoom = 1
        , lcsFacing = FaceSouth }
    }

-- | A bill on @page1@ whose station is a building that exists ONLY on
--   @page2@ — a genuine wrong-page violation (distinguishable from
--   "absent everywhere", which stays tolerated).
billWithStation ∷ BillId → BuildingId → CraftBills
billWithStation bid station = CraftBills
    { cbsBills = HM.singleton bid CraftBill
        { cbId = bid, cbStation = station, cbRecipe = "r"
        , cbRemaining = -1, cbClaimant = Nothing, cbClaimedAt = 0
        , cbProgress = 0, cbSeq = 1, cbPaused = False, cbWorking = False
        , cbMode = RepeatForever, cbTarget = 0, cbOutputItem = "" }
    , cbsNextId = 100 }

billWithClaimant ∷ BillId → UnitId → CraftBills
billWithClaimant bid claimant = CraftBills
    { cbsBills = HM.singleton bid CraftBill
        { cbId = bid, cbStation = BuildingId 1, cbRecipe = "r"
        , cbRemaining = -1, cbClaimant = Just claimant, cbClaimedAt = 0
        , cbProgress = 0, cbSeq = 1, cbPaused = False, cbWorking = False
        , cbMode = RepeatForever, cbTarget = 0, cbOutputItem = "" }
    , cbsNextId = 100 }

nodeWithBuilding ∷ PowerNodeId → BuildingId → PowerNodes
nodeWithBuilding nid building = PowerNodes
    { pnsNodes = HM.singleton nid PowerNode
        { pnId = nid, pnBuilding = building, pnRole = PowerSource
        , pnPeakWatts = 100, pnCapacityWh = 0, pnStoredWh = 0 }
    , pnsNextId = 100 }

-- | 'addTransferOrder' refuses on an exhausted allocator (#1246 review
--   round 2), which no fixture here can reach — every one starts from
--   'emptyTransferOrders'. Fail loudly rather than defaulting, so a
--   future change that DID exhaust it surfaces as this error instead of
--   as a silently empty store.
mustAdd ∷ UnitId → TransferBatch → TransferOrders → TransferOrders
mustAdd uid batch orders = case addTransferOrder uid batch orders of
    Just (orders', _) → orders'
    Nothing → error "fixture: addTransferOrder refused a fresh allocator"

-- | #1246: one transfer order whose acting unit, source endpoint,
--   destination endpoint and single requested item are each supplied
--   independently, so any ONE of the four can be pointed at a
--   wrong-page or absent target while the other three resolve.
orderWith ∷ UnitId → TransferEndpoint → TransferEndpoint → Int64
          → TransferOrders
orderWith acting source dest iid = mustAdd acting
    TransferBatch
        { tbSource      = source
        , tbDestination = dest
        , tbEntries     =
            [ QueuedTransfer
                { qtItem = TransferItemRef { tirInstanceId = iid
                                           , tirDefName = "bandage" }
                , qtState = TransferQueued } ] }
    emptyTransferOrders

-- | A unit carrying one item instance, so an order's item reference has
--   something real to resolve against.
unitHolding ∷ Word64 → UnitInstanceSnapshot
unitHolding iid = minimalUnit
    { uisInventory = [ ItemInstance
        { iiDefName = "bandage", iiCurrentFill = 1, iiQuality = 100
        , iiCondition = 100, iiWeight = 0.05, iiSharpness = 0
        , iiInstanceId = iid, iiTemp = Nothing, iiContents = []
        , iiBulk = Just 0.1, iiStorage = Nothing } ] }

-- | The ordinary, fully-resolving arrangement: unit 1 (holding item 500)
--   moving it into building 1, both on the SAME page as the order.
wellFormedOrderPage ∷ WorldPageId → PageSnapshot
wellFormedOrderPage pid = (minimalPage pid)
    { pgsUnits = UnitSnapshot (HM.singleton (UnitId 1) (unitHolding 500)) 100
    , pgsBuildings = BuildingSnapshot
        (HM.singleton (BuildingId 1) minimalBuilding) 100
    , pgsTransferOrders = orderWith (UnitId 1) (EndpointUnit (UnitId 1))
                              (EndpointBuilding (BuildingId 1)) 500
    }

pageWithEncounter ∷ WorldPageId → UnitId → PageSnapshot
pageWithEncounter pid uid = (minimalPage pid)
    { pgsGenParams = defaultWorldGenParams
        { wgpLocationInstances = LocationInstances
            { lisNextId = 2
            , lisById = HM.singleton (LocationInstanceId 1) LocationInstance
                { liId = LocationInstanceId 1
                , liDefId = "ruin_small"
                , liChunk = ChunkCoord 0 0
                , liAnchor = (8, 8)
                , liBounds = AbsBounds 6 6 10 10
                , liDisplayName = "Small Ruin"
                , liGloss = Nothing
                , liEtymology = Nothing
                , liLifecycle = LifecycleActive
                , liContentsSpawned = True
                , liEncounter = Just LocationEncounter
                    { leRolledCount = 1
                    , leOccupants =
                        [ LocationEncounterOccupant uid (8, 8)
                            True False ]
                    , leRosterComplete = True
                    , leDeathOnlyClearance = True
                    , leActivated = True
                    , leEpisodeActive = True
                    , leAggressionAnnounced = True
                    , leDisengageAnnounced = False
                    , leCleared = False
                    , leClearEventEmitted = False
                    }
                }
            , lisPendingLegacy = Nothing
            }
        }
    }

buildSnap ∷ WorldPageId → [PageSnapshot] → SessionSnapshot
buildSnap active pages = buildSessionSnapshot (minimalGlobals active) pages

spec ∷ Spec
spec = do
    describe "reference-codec (requirement 2/3)" $ do
        it "SamePageRef round-trips a runtime id (Serialize instance is \
           \wire-transparent)" $
            (unSamePageRef (SamePageRef (BuildingId 7)) ∷ BuildingId)
                `shouldBe` BuildingId 7

        it "renders every RefKind constructor to a distinct, stable string" $ do
            let kinds = [ RefPage, RefUnit, RefBuilding, RefItemInstance
                        , RefBill, RefPowerNode, RefGroundItem
                        , RefContent ContentUnit, RefLocationInstance ]
                texts = map refKindText kinds
            L.nub texts `shouldBe` texts   -- every kind renders distinctly
            -- The Lua `kind` vocabulary and this enum must render
            -- identically or a diagnostic naming one would not match a
            -- diagnostic naming the other (#915's memories are reported
            -- from Lua, resolved in Haskell).
            refKindText RefLocationInstance `shouldBe` "location_instance"

        it "renders every ContentKind constructor to a distinct string" $ do
            let kinds = [ ContentUnit, ContentItem, ContentBuilding
                        , ContentMaterial, ContentRecipe, ContentFlora
                        , ContentConstruct, ContentLocation, ContentInfection ]
                texts = map contentKindText kinds
            L.nub texts `shouldBe` texts

        it "an optional same-page reference round-trips both Nothing and \
           \Just (requirement 2's optional/required semantics)" $ do
            let withClaimant = toCraftBillDTO (CraftBill
                    { cbId = BillId 1, cbStation = BuildingId 1, cbRecipe = "r"
                    , cbRemaining = -1, cbClaimant = Just (UnitId 5)
                    , cbClaimedAt = 0, cbProgress = 0, cbSeq = 1
                    , cbPaused = False, cbWorking = False, cbMode = RepeatForever
                    , cbTarget = 0, cbOutputItem = "" })
                withoutClaimant = toCraftBillDTO (CraftBill
                    { cbId = BillId 2, cbStation = BuildingId 1, cbRecipe = "r"
                    , cbRemaining = -1, cbClaimant = Nothing
                    , cbClaimedAt = 0, cbProgress = 0, cbSeq = 1
                    , cbPaused = False, cbWorking = False, cbMode = RepeatForever
                    , cbTarget = 0, cbOutputItem = "" })
            (unSamePageRef ⊚ bilClaimant withClaimant) `shouldBe` Just (UnitId 5)
            bilClaimant withoutClaimant `shouldBe` Nothing

        it "a wrong-kind id (same numeric value, different kind) cannot \
           \resolve — kind-specific sets never cross-match" $ do
            let ke = KnownEntities
                    { keUnits = HS.singleton 5, keBuildings = HS.empty
                    , keBillsByPage = HM.empty, keItemInstances = HS.empty
                    , keGroundItemsByPage = HM.empty
                    , keLocationsByPage = HM.empty, keUnitPage = HM.empty
                    , keNextUnitId = 100
                    , keNextBuildingId = 100, keNextItemId = 100 }
                unitEdge     = LuaRefEdge "test" "unit" 5 Nothing "" Nothing
                buildingEdge = LuaRefEdge "test" "building" 5 Nothing "" Nothing
            luaReferenceErrors HM.empty ke [unitEdge] `shouldBe` []
            length (luaReferenceErrors HM.empty ke [buildingEdge]) `shouldBe` 1

        it "explicitly permitted cross-page references are accepted \
           \(requirement 4)" $
            refEdgeError craftBillsComponentId 2 "test.path" RefBuilding
                ScopeCrossPage page1 [page2] "1"
                `shouldBe` Nothing

        it "forbidden (same-page-only) cross-page references are rejected \
           \(requirement 4)" $
            refEdgeError craftBillsComponentId 2 "test.path" RefBuilding
                ScopeSamePage page1 [page2] "1"
                `shouldSatisfy` (≢ Nothing)

        it "a global-scope reference is accepted regardless of which page \
           \it resolves on" $
            refEdgeError craftBillsComponentId 2 "test.path" RefUnit
                ScopeGlobal page1 [page2] "1"
                `shouldBe` Nothing

        it "a reference absent from every page is tolerated, not rejected \
           \(the #758-established dangling-reference contract)" $
            refEdgeError craftBillsComponentId 2 "test.path" RefBuilding
                ScopeSamePage page1 [] "1"
                `shouldBe` Nothing

        it "a reference resolving on MORE THAN ONE page never fires its \
           \own wrong-page verdict -- that ambiguity is a duplicate-identity \
           \violation reported elsewhere, not guessed at here (requirement 8)" $
            refEdgeError craftBillsComponentId 2 "test.path" RefBuilding
                ScopeSamePage page1 [page1, page2] "1"
                `shouldBe` Nothing

    describe "Haskell migration (requirement 12/14)" $ do
        it "migrates an unambiguous v1 craft-bill into the typed v2 shape" $ do
            let v1 = CraftBillDTOv1
                    { bil1Id = BillId 3, bil1Station = BuildingId 7
                    , bil1Recipe = "smelt_steel", bil1Remaining = -1
                    , bil1Claimant = Just (UnitId 4), bil1ClaimedAt = 8.5
                    , bil1Progress = 0.4, bil1Seq = 3, bil1Paused = False
                    , bil1Working = True, bil1Mode = UntilStock
                    , bil1Target = 12, bil1OutputItem = "steel_bar" }
                v2 = migrateCraftBillDTOv1 v1
            bilId v2 `shouldBe` BillId 3
            unSamePageRef (bilStation v2) `shouldBe` BuildingId 7
            (unSamePageRef ⊚ bilClaimant v2) `shouldBe` Just (UnitId 4)
            bilRecipe v2 `shouldBe` "smelt_steel"

        it "migrates an unambiguous v1 power node into the typed v2 shape" $ do
            let v1 = PowerNodeDTOv1
                    { nod1Id = PowerNodeId 2, nod1Building = BuildingId 9
                    , nod1Role = PowerSource, nod1PeakWatts = 400
                    , nod1CapacityWh = 0, nod1StoredWh = 0 }
                v2 = migratePowerNodeDTOv1 v1
            nodId v2 `shouldBe` PowerNodeId 2
            unSamePageRef (nodBuilding v2) `shouldBe` BuildingId 9

        it "migrated state passes the same graph validator as a newly \
           \written snapshot (no station on any page — tolerated either way)" $ do
            let page = (minimalPage page1)
                    { pgsCraftBills = billWithStation (BillId 1) (BuildingId 99) }
                snap = buildSnap page1 [page]
            -- BuildingId 99 exists nowhere in this snapshot: tolerated
            -- (dangling), not a hard error — same outcome whether the
            -- bill DTO arrived via a v1 migration or a fresh v2 write,
            -- since both produce the identical in-memory 'CraftBills'.
            sessionIntegrityErrors snap `shouldBe` []

    describe "integrity graph — valid session (requirement 6)" $
        it "a fully valid multi-page session produces no findings" $ do
            let p1 = (minimalPage page1)
                    { pgsBuildings = BuildingSnapshot
                        (HM.singleton (BuildingId 1) minimalBuilding) 100
                    , pgsCraftBills = billWithStation (BillId 1) (BuildingId 1) }
                p2 = minimalPage page2
                snap = buildSnap page1 [p1, p2]
            sessionIntegrityErrors snap `shouldBe` []

    describe "integrity graph — duplicate global identities (requirement 8)" $ do
        it "rejects the SAME BuildingId existing on two different pages \
           \(BuildingId is a GLOBAL allocator, unlike per-page BillId/ \
           \PowerNodeId)" $ do
            let p1 = (minimalPage page1)
                    { pgsBuildings = BuildingSnapshot
                        (HM.singleton (BuildingId 1) minimalBuilding) 100 }
                p2 = (minimalPage page2)
                    { pgsBuildings = BuildingSnapshot
                        (HM.singleton (BuildingId 1) minimalBuilding) 100 }
                snap = buildSnap page1 [p1, p2]
                errs = sessionIntegrityErrors snap
            length (filter ((≡ "duplicate-identity") ∘ ieCode) errs)
                `shouldBe` 1
            map ieRefKind (filter ((≡ "duplicate-identity") ∘ ieCode) errs)
                `shouldBe` [RefBuilding]

        it "rejects the SAME UnitId existing on two different pages" $ do
            let p1 = (minimalPage page1)
                    { pgsUnits = UnitSnapshot
                        (HM.singleton (UnitId 1) minimalUnit) 100 }
                p2 = (minimalPage page2)
                    { pgsUnits = UnitSnapshot
                        (HM.singleton (UnitId 1) minimalUnit) 100 }
                snap = buildSnap page1 [p1, p2]
                errs = sessionIntegrityErrors snap
            length (filter ((≡ "duplicate-identity") ∘ ieCode) errs)
                `shouldBe` 1

        it "does NOT reject the SAME BillId/PowerNodeId existing on two \
           \different pages -- both are legitimately PER-PAGE allocators" $ do
            let p1 = (minimalPage page1)
                    { pgsBuildings = BuildingSnapshot
                        (HM.singleton (BuildingId 1) minimalBuilding) 100
                    , pgsCraftBills = billWithStation (BillId 1) (BuildingId 1)
                    , pgsPowerNodes = nodeWithBuilding (PowerNodeId 1) (BuildingId 1) }
                p2 = (minimalPage page2)
                    { pgsBuildings = BuildingSnapshot
                        (HM.singleton (BuildingId 2) minimalBuilding) 100
                    , pgsCraftBills = billWithStation (BillId 1) (BuildingId 2)
                    , pgsPowerNodes = nodeWithBuilding (PowerNodeId 1) (BuildingId 2) }
                snap = buildSnap page1 [p1, p2]
            sessionIntegrityErrors snap `shouldBe` []

    describe "integrity graph — wrong-page violations (requirement 8)" $ do
        it "rejects a craft bill whose station resolves on a DIFFERENT page \
           \(a genuine wrong-page violation, not a tolerated absence)" $ do
            let p1 = (minimalPage page1)
                    { pgsCraftBills = billWithStation (BillId 1) (BuildingId 5) }
                p2 = (minimalPage page2)
                    { pgsBuildings = BuildingSnapshot
                        (HM.singleton (BuildingId 5) minimalBuilding) 100 }
                snap = buildSnap page1 [p1, p2]
            case sessionIntegrityErrors snap of
                [e] → do
                    ieCode e `shouldBe` "wrong-page"
                    ieComponent e `shouldBe` craftBillsComponentId
                other → expectationFailure ("expected one finding, got " <> show other)

        it "rejects a craft bill whose claimant resolves on a DIFFERENT page" $ do
            let p1 = (minimalPage page1)
                    { pgsCraftBills = billWithClaimant (BillId 1) (UnitId 5) }
                p2 = (minimalPage page2)
                    { pgsUnits = UnitSnapshot
                        (HM.singleton (UnitId 5) minimalUnit) 100 }
                snap = buildSnap page1 [p1, p2]
            case sessionIntegrityErrors snap of
                [e] → do
                    ieCode e `shouldBe` "wrong-page"
                    ieRefKind e `shouldBe` RefUnit
                other → expectationFailure ("expected one finding, got " <> show other)

        it "rejects a power node whose host building resolves on a \
           \DIFFERENT page, and (round-2 review) names the offending \
           \node id in its path -- not just the page" $ do
            let p1 = (minimalPage page1)
                    { pgsPowerNodes = nodeWithBuilding (PowerNodeId 1) (BuildingId 5) }
                p2 = (minimalPage page2)
                    { pgsBuildings = BuildingSnapshot
                        (HM.singleton (BuildingId 5) minimalBuilding) 100 }
                snap = buildSnap page1 [p1, p2]
            case sessionIntegrityErrors snap of
                [e] → do
                    ieCode e `shouldBe` "wrong-page"
                    ieComponent e `shouldBe` powerNodesComponentId
                    T.isInfixOf "node=1" (iePath e) `shouldBe` True
                other → expectationFailure ("expected one finding, got " <> show other)

        it "does NOT reject a craft-bill station absent from the WHOLE \
           \session (the #758-established tolerated gap)" $ do
            let p1 = (minimalPage page1)
                    { pgsCraftBills = billWithStation (BillId 1) (BuildingId 999) }
                snap = buildSnap page1 [p1]
            sessionIntegrityErrors snap `shouldBe` []

        it "does NOT reject a bill/node whose station/building is on its \
           \OWN page (the ordinary, correct case)" $ do
            let p1 = (minimalPage page1)
                    { pgsBuildings = BuildingSnapshot
                        (HM.singleton (BuildingId 1) minimalBuilding) 100
                    , pgsCraftBills = billWithStation (BillId 1) (BuildingId 1)
                    , pgsPowerNodes = nodeWithBuilding (PowerNodeId 1) (BuildingId 1) }
                snap = buildSnap page1 [p1]
            sessionIntegrityErrors snap `shouldBe` []

    describe "integrity graph — placed ruin encounter occupants (#916)" $ do
        it "accepts a roster UID that resolves on the encounter's own page" $ do
            let uid = UnitId 5
                p1 = (pageWithEncounter page1 uid)
                    { pgsUnits = UnitSnapshot
                        (HM.singleton uid minimalUnit) 100 }
                snap = buildSnap page1 [p1]
            sessionIntegrityErrors snap `shouldBe` []
            sessionIntegrityWarnings snap `shouldBe` []

        it "hard-fails a roster UID that resolves only on another page, \
           \naming the world-pages v8 occupant path" $ do
            let uid = UnitId 5
                p1 = pageWithEncounter page1 uid
                p2 = (minimalPage page2)
                    { pgsUnits = UnitSnapshot
                        (HM.singleton uid minimalUnit) 100 }
                snap = buildSnap page1 [p1, p2]
            case sessionIntegrityErrors snap of
                [e] → do
                    ieCode e `shouldBe` "wrong-page"
                    ieComponent e `shouldBe` worldPagesComponentId
                    ieVersion e `shouldBe` 8
                    ieRefKind e `shouldBe` RefUnit
                    iePath e `shouldSatisfy` T.isInfixOf
                        "locations[1].encounter.occupants[0].unit"
                other → expectationFailure
                    ("expected one encounter wrong-page finding, got " <> show other)

        it "retains an absent roster UID as a tolerated dangling reference \
           \that is reported but never promoted to a load error" $ do
            let snap = buildSnap page1 [pageWithEncounter page1 (UnitId 999)]
            sessionIntegrityErrors snap `shouldBe` []
            case sessionIntegrityWarnings snap of
                [d] → do
                    ieCode d `shouldBe` "dangling-reference"
                    ieComponent d `shouldBe` worldPagesComponentId
                    ieRefValue d `shouldBe` "999"
                    ieMessage d `shouldSatisfy` T.isInfixOf
                        "roster membership is retained"
                other → expectationFailure
                    ("expected one encounter dangling warning, got " <> show other)

    -- #1246: a transfer order puts FOUR durable references into the
    -- graph -- the acting unit, both endpoints, and every requested item
    -- instance -- and they split the same way a craft bill's do: a
    -- wrong-page target is fatal, an absent one is a tolerated
    -- diagnostic. The two halves are separate functions
    -- ('sessionIntegrityErrors' vs 'sessionIntegrityWarnings') precisely
    -- because everything the first returns aborts a save or a load.
    describe "integrity graph — transfer orders (#1246)" $ do
        it "accepts an order whose acting unit, endpoints and item all \
           \resolve on its OWN page" $
            sessionIntegrityErrors (buildSnap page1 [wellFormedOrderPage page1])
                `shouldBe` []

        it "reports NO warning for that order either -- nothing dangles" $
            sessionIntegrityWarnings
                (buildSnap page1 [wellFormedOrderPage page1]) `shouldBe` []

        it "rejects an order whose ACTING UNIT resolves on a different \
           \page" $ do
            let p1 = (minimalPage page1)
                    { pgsTransferOrders = orderWith (UnitId 5)
                        (EndpointBuilding (BuildingId 1))
                        (EndpointBuilding (BuildingId 1)) 500
                    , pgsBuildings = BuildingSnapshot
                        (HM.singleton (BuildingId 1) minimalBuilding) 100
                    , pgsUnits = UnitSnapshot
                        (HM.singleton (UnitId 9) (unitHolding 500)) 100 }
                p2 = (minimalPage page2)
                    { pgsUnits = UnitSnapshot
                        (HM.singleton (UnitId 5) minimalUnit) 100 }
            case sessionIntegrityErrors (buildSnap page1 [p1, p2]) of
                [e] → do
                    ieCode e `shouldBe` "wrong-page"
                    ieComponent e `shouldBe` transferOrdersComponentId
                    ieRefKind e `shouldBe` RefUnit
                    T.isInfixOf "order=1" (iePath e) `shouldBe` True
                    T.isInfixOf ".unit" (iePath e) `shouldBe` True
                other → expectationFailure
                    ("expected one finding, got " <> show other)

        it "rejects an order whose DESTINATION building resolves on a \
           \different page (both endpoints are checked, not just the \
           \source)" $ do
            let p1 = (minimalPage page1)
                    { pgsUnits = UnitSnapshot
                        (HM.singleton (UnitId 1) (unitHolding 500)) 100
                    , pgsTransferOrders = orderWith (UnitId 1)
                        (EndpointUnit (UnitId 1))
                        (EndpointBuilding (BuildingId 5)) 500 }
                p2 = (minimalPage page2)
                    { pgsBuildings = BuildingSnapshot
                        (HM.singleton (BuildingId 5) minimalBuilding) 100 }
            case sessionIntegrityErrors (buildSnap page1 [p1, p2]) of
                [e] → do
                    ieCode e `shouldBe` "wrong-page"
                    ieRefKind e `shouldBe` RefBuilding
                    T.isInfixOf ".destination" (iePath e) `shouldBe` True
                other → expectationFailure
                    ("expected one finding, got " <> show other)

        it "rejects an order whose requested ITEM INSTANCE lives on a \
           \different page" $ do
            let p1 = (minimalPage page1)
                    { pgsUnits = UnitSnapshot
                        (HM.singleton (UnitId 1) minimalUnit) 100
                    , pgsBuildings = BuildingSnapshot
                        (HM.singleton (BuildingId 1) minimalBuilding) 100
                    , pgsTransferOrders = orderWith (UnitId 1)
                        (EndpointUnit (UnitId 1))
                        (EndpointBuilding (BuildingId 1)) 500 }
                p2 = (minimalPage page2)
                    { pgsUnits = UnitSnapshot
                        (HM.singleton (UnitId 2) (unitHolding 500)) 100 }
            case sessionIntegrityErrors (buildSnap page1 [p1, p2]) of
                [e] → do
                    ieCode e `shouldBe` "wrong-page"
                    ieRefKind e `shouldBe` RefItemInstance
                    T.isInfixOf "entries[0].instance" (iePath e)
                        `shouldBe` True
                other → expectationFailure
                    ("expected one finding, got " <> show other)

        it "TOLERATES a dangling destination (a demolished building) -- \
           \no hard error, and the order itself is untouched in the \
           \snapshot" $ do
            let p1 = (minimalPage page1)
                    { pgsUnits = UnitSnapshot
                        (HM.singleton (UnitId 1) (unitHolding 500)) 100
                    , pgsTransferOrders = orderWith (UnitId 1)
                        (EndpointUnit (UnitId 1))
                        (EndpointBuilding (BuildingId 999)) 500 }
                snap = buildSnap page1 [p1]
            sessionIntegrityErrors snap `shouldBe` []
            (pgsTransferOrders <$> HM.lookup page1 (snapPages snap))
                `shouldBe` Just (pgsTransferOrders p1)

        it "REPORTS that dangling destination as a non-blocking \
           \diagnostic naming the order, the kind and the value" $
            case sessionIntegrityWarnings (buildSnap page1
                    [ (minimalPage page1)
                        { pgsUnits = UnitSnapshot
                            (HM.singleton (UnitId 1) (unitHolding 500)) 100
                        , pgsTransferOrders = orderWith (UnitId 1)
                            (EndpointUnit (UnitId 1))
                            (EndpointBuilding (BuildingId 999)) 500 } ]) of
                [e] → do
                    ieCode e `shouldBe` "dangling-reference"
                    ieComponent e `shouldBe` transferOrdersComponentId
                    ieRefKind e `shouldBe` RefBuilding
                    ieRefValue e `shouldBe` "999"
                    T.isInfixOf "tolerated" (ieMessage e) `shouldBe` True
                other → expectationFailure
                    ("expected one warning, got " <> show other)

        it "reports a dead carrier and a consumed item the same tolerated \
           \way -- one diagnostic each, still no hard error" $ do
            let p1 = (minimalPage page1)
                    { pgsBuildings = BuildingSnapshot
                        (HM.singleton (BuildingId 1) minimalBuilding) 100
                    , pgsTransferOrders = orderWith (UnitId 42)
                        (EndpointBuilding (BuildingId 1))
                        (EndpointBuilding (BuildingId 1)) 777 }
                snap = buildSnap page1 [p1]
            sessionIntegrityErrors snap `shouldBe` []
            map (\e → (ieRefKind e, ieRefValue e))
                (sessionIntegrityWarnings snap)
                `shouldMatchList` [(RefUnit, "42"), (RefItemInstance, "777")]

    describe "integrity graph — Lua AI references (requirement 8, unit_ai/ \
              \building_spawn)" $ do
        -- unit #1 lives on page1, unit #2 on page2 -- page1's bill/
        -- ground-item #1 must NOT resolve a reference owned by unit #2
        -- (see the page-scoped tests below).
        let ke = KnownEntities
                { keUnits = HS.fromList [1, 2], keBuildings = HS.fromList [10]
                , keBillsByPage = HM.fromList [ (page1, HS.fromList [1]) ]
                , keItemInstances = HS.fromList [500]
                , keGroundItemsByPage = HM.fromList [ (page1, HS.fromList [1]) ]
                -- #915: page1 carries location instances 1 and 2; page2
                -- carries only instance 1 -- the same NUMBER on both
                -- pages, which is exactly the aliasing a page-scoped
                -- resolution has to refuse.
                , keLocationsByPage = HM.fromList
                    [ (page1, HS.fromList [1, 2]), (page2, HS.fromList [1]) ]
                , keUnitPage = HM.fromList [ (1, page1), (2, page2) ]
                , keNextUnitId = 50
                , keNextBuildingId = 50, keNextItemId = 1000 }

        it "a Lua reference that resolves produces no diagnostic" $
            luaReferenceErrors HM.empty ke [LuaRefEdge "unit_ai" "unit" 1 Nothing "" Nothing]
                `shouldBe` []

        it "a dangling Lua reference (target legitimately gone) is a \
           \non-blocking diagnostic, coded distinctly from an allocator \
           \violation" $
            -- id 30 sits BELOW keNextUnitId (50) -- a unit that could
            -- legitimately have existed and died, unlike 999 below
            -- (which could never have been allocated at all).
            case luaReferenceErrors HM.empty ke [LuaRefEdge "unit_ai" "unit" 30 Nothing "" Nothing] of
                [d]   → ieCode d `shouldBe` "dangling-reference"
                other → expectationFailure ("expected one finding, got " <> show other)

        it "an id at/above the allocator is coded as an allocator-reuse \
           \hazard, not an ordinary dangling reference (requirement 8)" $
            case luaReferenceErrors HM.empty ke [LuaRefEdge "unit_ai" "unit" 999 Nothing "" Nothing] of
                [d]   → ieCode d `shouldBe` "ref-exceeds-allocator"
                other → expectationFailure ("expected one finding, got " <> show other)

        it "a dangling Lua reference's finding carries the REAL source \
           \field path (round-2 review, issue #764), not a synthetic \
           \'kind#id' string, and falls back to the synthetic form only \
           \when the edge itself carries no path" $ do
            case luaReferenceErrors HM.empty ke
                    [LuaRefEdge "unit_ai" "unit" 30 Nothing "unit[7].attackTargetUid" Nothing] of
                [d]   → iePath d `shouldBe` "unit[7].attackTargetUid"
                other → expectationFailure ("expected one finding, got " <> show other)
            case luaReferenceErrors HM.empty ke [LuaRefEdge "unit_ai" "unit" 30 Nothing "" Nothing] of
                [d]   → iePath d `shouldBe` "unit#30"
                other → expectationFailure ("expected one finding, got " <> show other)

        it "a dangling Lua reference's finding carries the version its \
           \OWNING component was actually collected against (round-2 \
           \review), not a hardcoded placeholder — an unknown component \
           \id defaults to version 0 rather than crashing" $ do
            let versions = HM.fromList [("unit_ai", 2)]
            case luaReferenceErrors versions ke
                    [LuaRefEdge "unit_ai" "unit" 30 Nothing "" Nothing] of
                [d]   → ieVersion d `shouldBe` 2
                other → expectationFailure ("expected one finding, got " <> show other)
            case luaReferenceErrors versions ke
                    [LuaRefEdge "some_other_component" "unit" 30 Nothing "" Nothing] of
                [d]   → ieVersion d `shouldBe` 0
                other → expectationFailure ("expected one finding, got " <> show other)

        it "a dangling reference's expected-scope text distinguishes a \
           \page-scoped (per-page-allocator) kind from a globally-scoped \
           \one (round-2 review) — never claims 'global' for craft_bill/ \
           \ground_item" $ do
            case luaReferenceErrors HM.empty ke
                    [LuaRefEdge "unit_ai" "craft_bill" 1 Nothing "" Nothing] of
                [d]   → T.unpack (ieExpectedScope d) `shouldContain` "per-page"
                other → expectationFailure ("expected one finding, got " <> show other)
            case luaReferenceErrors HM.empty ke
                    [LuaRefEdge "unit_ai" "unit" 30 Nothing "" Nothing] of
                [d]   → T.unpack (ieExpectedScope d) `shouldContain` "global"
                other → expectationFailure ("expected one finding, got " <> show other)

        it "permitted gameplay cycles (mutual combat/AI targets) never \
           \produce a finding — existence-only checking has no cycle \
           \concept to reject (requirement 9)" $ do
            let mutual = [ LuaRefEdge "unit_ai" "unit" 1 Nothing "" Nothing
                         , LuaRefEdge "unit_ai" "unit" 2 Nothing "" Nothing ]
                         -- 1 targets 2, 2 targets 1
            luaReferenceErrors HM.empty ke mutual `shouldBe` []

        it "an unknown kind string never manufactures a false positive \
           \(a registration-time vocabulary mismatch is the audit's job, \
           \not this validator's)" $
            luaReferenceErrors HM.empty ke [LuaRefEdge "unit_ai" "not_a_real_kind" 1 Nothing "" Nothing]
                `shouldBe` []

        it "a craft_bill/ground_item reference resolves when the OWNING \
           \unit's page has a matching id (requirement 8's page-scoped \
           \per-page-allocator resolution)" $ do
            luaReferenceErrors HM.empty ke [LuaRefEdge "unit_ai" "craft_bill" 1 (Just 1) "" Nothing]
                `shouldBe` []
            luaReferenceErrors HM.empty ke [LuaRefEdge "unit_ai" "ground_item" 1 (Just 1) "" Nothing]
                `shouldBe` []

        it "a craft_bill/ground_item reference does NOT resolve against a \
           \same-numbered entity on a DIFFERENT page than its owning unit \
           \-- session-wide matching would mask a genuine dangling \
           \reference (requirement 8)" $ do
            -- unit #2 lives on page2, which has no bill/ground-item #1 --
            -- only page1 does. Must NOT falsely resolve.
            length (luaReferenceErrors HM.empty ke [LuaRefEdge "unit_ai" "craft_bill" 1 (Just 2) "" Nothing])
                `shouldBe` 1
            length (luaReferenceErrors HM.empty ke [LuaRefEdge "unit_ai" "ground_item" 1 (Just 2) "" Nothing])
                `shouldBe` 1

        it "a craft_bill/ground_item reference with no owner (or an \
           \unresolvable owner) never resolves -- there is no session-wide \
           \fallback for a per-page-allocated kind" $ do
            length (luaReferenceErrors HM.empty ke [LuaRefEdge "unit_ai" "craft_bill" 1 Nothing "" Nothing])
                `shouldBe` 1
            length (luaReferenceErrors HM.empty ke [LuaRefEdge "unit_ai" "craft_bill" 1 (Just 999) "" Nothing])
                `shouldBe` 1

    describe "integrity graph — per-unit location memory (#915)" $ do
        -- Same fixture as above: page1 has location instances 1 and 2,
        -- page2 has only instance 1.
        let ke = KnownEntities
                { keUnits = HS.fromList [1, 2], keBuildings = HS.empty
                , keBillsByPage = HM.empty, keItemInstances = HS.empty
                , keGroundItemsByPage = HM.empty
                , keLocationsByPage = HM.fromList
                    [ (page1, HS.fromList [1, 2]), (page2, HS.fromList [1]) ]
                , keUnitPage = HM.fromList [ (1, page1), (2, page2) ]
                , keNextUnitId = 50, keNextBuildingId = 50, keNextItemId = 50 }
            locEdge i page path =
                LuaRefEdge "unit_ai" "location_instance" i (Just 1) path page

        it "resolves against the page the EDGE declares" $
            luaReferenceErrors HM.empty ke
                [ locEdge 1 (Just "page1") "unit[1].knownLocations[1]"
                , locEdge 2 (Just "page1") "unit[1].knownLocations[2]" ]
                `shouldBe` []

        it "does NOT resolve a same-numbered instance from another page \
           \-- equal instance ids on two pages must never alias" $
            -- Instance 2 exists on page-one only. A memory declaring
            -- page-two must report, even though the OWNING unit (#1)
            -- lives on page-one where 2 does exist: the page travels
            -- with the memory, not with the unit.
            case luaReferenceErrors HM.empty ke
                    [ locEdge 2 (Just "page2") "unit[1].knownLocations[1]" ] of
                [d]   → ieCode d `shouldBe` "dangling-reference"
                other → expectationFailure ("expected one finding, got " <> show other)

        it "an unknown page never falls back to a session-wide match" $
            length (luaReferenceErrors HM.empty ke
                        [ locEdge 1 (Just "no-such-page") "p" ]) `shouldBe` 1

        it "an edge with no declared page never resolves — a per-page id \
           \names nothing on its own" $
            length (luaReferenceErrors HM.empty ke [ locEdge 1 Nothing "p" ])
                `shouldBe` 1

        it "a missing memory's diagnostic names its component, field \
           \path, page AND id, and the location_instance kind — a bare \
           \id would identify nothing" $
            case luaReferenceErrors (HM.fromList [("unit_ai", 4)]) ke
                    [ locEdge 7 (Just "page1") "unit[1].knownLocations[2]" ] of
                [d] → do
                    ieComponent d `shouldBe` ComponentId "lua.unit_ai"
                    ieVersion d `shouldBe` 4
                    iePath d `shouldBe` "unit[1].knownLocations[2]"
                    ieRefKind d `shouldBe` RefLocationInstance
                    ieRefValue d `shouldBe` "page=page1,id=7"
                    T.unpack (ieExpectedScope d) `shouldContain` "page1"
                    T.unpack (ieMessage d) `shouldContain` "location_instance"
                    T.unpack (ieMessage d) `shouldContain` "unit_ai"
                    ieCode d `shouldBe` "dangling-reference"
                other → expectationFailure ("expected one finding, got " <> show other)

        it "a dangling memory never suppresses its resolving siblings — \
           \exactly one finding, for the one entry that is gone" $
            case luaReferenceErrors HM.empty ke
                    [ locEdge 1 (Just "page1") "unit[1].knownLocations[1]"
                    , locEdge 9 (Just "page1") "unit[1].knownLocations[2]"
                    , locEdge 2 (Just "page1") "unit[1].knownLocations[3]" ] of
                [d]   → iePath d `shouldBe` "unit[1].knownLocations[2]"
                other → expectationFailure ("expected one finding, got " <> show other)

    describe "deterministic ordering + truncation (requirement 10)" $ do
        it "sorts findings deterministically by (component, path, value, code)" $ do
            let mk n = IntegrityError
                    { ieComponent = craftBillsComponentId, ieVersion = 2
                    , iePath = "p" <> tshow n, ieRefKind = RefBuilding
                    , ieRefValue = tshow n, ieExpectedScope = "same-page"
                    , ieActual = "elsewhere", ieCode = "wrong-page"
                    , ieMessage = "x" }
                errs = map mk [5, 1, 3, 2, 4 ∷ Int]
                report = capIntegrityErrors errs
            map iePath (irErrors report) `shouldBe` ["p1", "p2", "p3", "p4", "p5"]
            irOmitted report `shouldBe` 0
            irTotal report `shouldBe` 5

        it "caps the reported list and reports how many were omitted, \
           \never silently truncating without saying so" $ do
            let mk n = IntegrityError
                    { ieComponent = craftBillsComponentId, ieVersion = 2
                    , iePath = "p" <> tshow n, ieRefKind = RefBuilding
                    , ieRefValue = tshow n, ieExpectedScope = "same-page"
                    , ieActual = "elsewhere", ieCode = "wrong-page"
                    , ieMessage = "x" }
                total = integrityErrorCap + 37
                errs = map mk [1 .. total]
                report = capIntegrityErrors errs
            length (irErrors report) `shouldBe` integrityErrorCap
            irTotal report `shouldBe` total
            irOmitted report `shouldBe` 37

    describe "rendering" $
        it "renderIntegrityError names the component, path, and code" $ do
            let e = IntegrityError
                    { ieComponent = craftBillsComponentId, ieVersion = 2
                    , iePath = "craft-bills[page=page1,bill=1].station"
                    , ieRefKind = RefBuilding, ieRefValue = "5"
                    , ieExpectedScope = "same page ('page1')"
                    , ieActual = "found on page 'page2'"
                    , ieCode = "wrong-page", ieMessage = "building 5 resolves \
                                                          \on page 'page2'" }
                rendered = renderIntegrityError e
            T.isInfixOf "craft-bills" rendered `shouldBe` True
            T.isInfixOf "wrong-page" rendered `shouldBe` True
