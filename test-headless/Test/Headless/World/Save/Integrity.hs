{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
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
import World.Save.Component.Types (craftBillsComponentId, powerNodesComponentId)
import World.Save.Component.Entities
    ( CraftBillDTO(..), CraftBillDTOv1(..), migrateCraftBillDTOv1
    , PowerNodeDTO(..), PowerNodeDTOv1(..), migratePowerNodeDTOv1
    , toCraftBillDTO )
import World.Save.Types
    ( BuildingSnapshot(..), BuildingInstanceSnapshot(..)
    , UnitSnapshot(..), UnitInstanceSnapshot(..) )
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
import Craft.Bills
    (emptyCraftBills, CraftBill(..), CraftBills(..), BillId(..), BillMode(..))
import Power.Types
    (emptyPowerNodes, PowerNode(..), PowerNodes(..), PowerNodeId(..), PowerRole(..))
import Building.Types (BuildingId(..))
import Unit.Types (UnitId(..))
import Unit.Direction (Direction(..))

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
    , pgsGroundItems  = emptyGroundItems
    , pgsSpoilPiles   = emptySpoilPiles
    , pgsBuildings    = BuildingSnapshot { bsnInstances = HM.empty, bsnNextId = 100 }
    , pgsUnits        = UnitSnapshot { usnInstances = HM.empty, usnNextId = 100 }
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
                        , RefContent ContentUnit ]
                texts = map refKindText kinds
            L.nub texts `shouldBe` texts   -- every kind renders distinctly

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
                    , keBills = HS.empty, keItemInstances = HS.empty
                    , keGroundItems = HS.empty, keNextUnitId = 100
                    , keNextBuildingId = 100, keNextItemId = 100 }
                unitEdge     = LuaRefEdge "test" "unit" 5
                buildingEdge = LuaRefEdge "test" "building" 5
            luaReferenceErrors ke [unitEdge] `shouldBe` []
            length (luaReferenceErrors ke [buildingEdge]) `shouldBe` 1

        it "explicitly permitted cross-page references are accepted \
           \(requirement 4)" $
            refEdgeError craftBillsComponentId 2 "test.path" RefBuilding
                ScopeCrossPage page1 (Just page2) "1"
                `shouldBe` Nothing

        it "forbidden (same-page-only) cross-page references are rejected \
           \(requirement 4)" $
            refEdgeError craftBillsComponentId 2 "test.path" RefBuilding
                ScopeSamePage page1 (Just page2) "1"
                `shouldSatisfy` (≢ Nothing)

        it "a global-scope reference is accepted regardless of which page \
           \it resolves on" $
            refEdgeError craftBillsComponentId 2 "test.path" RefUnit
                ScopeGlobal page1 (Just page2) "1"
                `shouldBe` Nothing

        it "a reference absent from every page is tolerated, not rejected \
           \(the #758-established dangling-reference contract)" $
            refEdgeError craftBillsComponentId 2 "test.path" RefBuilding
                ScopeSamePage page1 Nothing "1"
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
           \DIFFERENT page" $ do
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

    describe "integrity graph — Lua AI references (requirement 8, unit_ai/ \
              \building_spawn)" $ do
        let ke = KnownEntities
                { keUnits = HS.fromList [1, 2], keBuildings = HS.fromList [10]
                , keBills = HS.fromList [1], keItemInstances = HS.fromList [500]
                , keGroundItems = HS.fromList [1], keNextUnitId = 50
                , keNextBuildingId = 50, keNextItemId = 1000 }

        it "a Lua reference that resolves produces no diagnostic" $
            luaReferenceErrors ke [LuaRefEdge "unit_ai" "unit" 1] `shouldBe` []

        it "a dangling Lua reference (target legitimately gone) is a \
           \non-blocking diagnostic, coded distinctly from an allocator \
           \violation" $
            -- id 30 sits BELOW keNextUnitId (50) -- a unit that could
            -- legitimately have existed and died, unlike 999 below
            -- (which could never have been allocated at all).
            case luaReferenceErrors ke [LuaRefEdge "unit_ai" "unit" 30] of
                [d]   → ieCode d `shouldBe` "dangling-reference"
                other → expectationFailure ("expected one finding, got " <> show other)

        it "an id at/above the allocator is coded as an allocator-reuse \
           \hazard, not an ordinary dangling reference (requirement 8)" $
            case luaReferenceErrors ke [LuaRefEdge "unit_ai" "unit" 999] of
                [d]   → ieCode d `shouldBe` "ref-exceeds-allocator"
                other → expectationFailure ("expected one finding, got " <> show other)

        it "permitted gameplay cycles (mutual combat/AI targets) never \
           \produce a finding — existence-only checking has no cycle \
           \concept to reject (requirement 9)" $ do
            let mutual = [ LuaRefEdge "unit_ai" "unit" 1
                         , LuaRefEdge "unit_ai" "unit" 2 ]  -- 1 targets 2, 2 targets 1
            luaReferenceErrors ke mutual `shouldBe` []

        it "an unknown kind string never manufactures a false positive \
           \(a registration-time vocabulary mismatch is the audit's job, \
           \not this validator's)" $
            luaReferenceErrors ke [LuaRefEdge "unit_ai" "not_a_real_kind" 1]
                `shouldBe` []

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
  where
    tshow ∷ Show a ⇒ a → Text
    tshow = T.pack . show

