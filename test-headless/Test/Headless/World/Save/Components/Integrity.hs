{-# LANGUAGE ScopedTypeVariables #-}
-- | The integrity owner of the "save components" gate (issue #760,
--   split out under #2043): missing gameplay, item, significant-item,
--   recipe, bill-output and construct-target definitions, recursive
--   item identities, shared item enumeration, the acquired-immunity
--   scrub, and texture-palette validation. Pure -- no engine, no IO;
--   every 'World.Save.Component.Session.SessionSnapshot' here is a
--   synthetic literal.
--
--   Composed by the facade 'Test.Headless.World.Save.Components', which
--   is the only module @test-headless/Spec.hs@ registers.
module Test.Headless.World.Save.Components.Integrity
    (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

import qualified Data.HashSet as HS
import World.Save.Component.Types
import World.Save.Component.Session
import World.Save.Integrity (KnownEntities(..))
import Engine.Scripting.Lua.API.Save.Integrity (knownEntitiesFromSaveData)
import World.Save.Snapshot
import Location.Instance
    ( LocationInstances(..), LocationInstanceId(..)
    , LocationSignificantItem(..) )
import World.Save.Snapshot.Adapter (SaveRequestMeta(..), snapshotToSaveData)
import World.Save.Types
    ( BuildingSnapshot(..), BuildingInstanceSnapshot(..), UnitSnapshot(..)
    , UnitInstanceSnapshot(..), MissingDefRef(..), renderMissingDefRef
    , missingDefReferences, MissingItemDefRef(..)
    , missingItemDefReferences, MissingSignificantItemRef(..)
    , missingSignificantItemReferences, renderMissingSignificantItemRef
    , MissingRecipeRef(..), missingRecipeReferences
    , MissingBillOutputItemRef(..), missingBillOutputItemReferences
    , MissingConstructDefRef(..), missingConstructDefReferences
    , MissingInfectionRef(..), missingInfectionReferences
    , ImmunityScrub(..), emptyImmunityScrub, renderImmunityScrub
    , fromUnitSnapshot, toUnitSnapshot, WorldPageSave(..), SaveData(..) )
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)
import World.Page.Types (WorldPageId(..))
import World.Render.Zoom.Types (ZoomMapMode(..))
import World.Tool.Types (ToolMode(..))
import Engine.Graphics.Camera (CameraFacing(..))
import Structure.Palette (TexPalette(..))
import Item.Ground (emptyGroundItems, GroundItems(..), GroundItem(..))
import Item.Types (ItemInstance(..), emptyItemManager)
import Equipment.Types (emptyEquipmentClassManager)
import World.Spoil.Types (emptySpoilPiles)
import World.Flora.Harvest (emptyFloraHarvests)
import World.Edit.Types (emptyWorldEdits)
import World.Construct.Attempt (firstConstructAttemptId)
import World.Construct.Receipt (ConstructPayment(..))
import World.Construct.Types
    ( ConstructDesignation(..), ConstructTarget(..), ConstructStatus(..) )
import Craft.Bills
    ( emptyCraftBills, CraftBill(..), CraftBills(..), BillId(..), BillMode(..) )
import Unit.Transfer.Orders (emptyTransferOrders)
import Power.Types (emptyPowerNodes)
import Building.Types (BuildingId(..))
import Engine.Asset.Handle (TextureHandle(..))
import Infection.Types
    (InfectionDef(..), InfectionManager(..), emptyInfectionManager)
import qualified Data.Map.Strict as Map
import Unit.Types
    ( BodyPart(..), UnitDef(..), UnitId(..), UnitInstance(..)
    , UnitManager(..), Wound(..), defaultNaturalResistance )
import Building.Knowledge (emptyContainerKnowledge)
import World.Flora.Identity (firstPlantedFloraCursor)
import Test.Headless.Harness.GeneratedIds (fixtureGeneratedWorldIdForPage)
import Test.Headless.World.Save.Components.Fixture

spec ∷ Spec
spec = do
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

    -- #917: an UNSPAWNED significant obligation names the item the next
    -- chunk load will try to spawn. A save written before that spawn,
    -- loaded against a build whose item set has moved on, would
    -- otherwise publish into a state where the spawn fails forever and
    -- the location can never clear — the load-path counterpart of the
    -- authoring-time rejection in
    -- 'Engine.Asset.YamlLocations.significantItemErrors'.
    describe "missing significant-obligation item rejection (#917)" $ do
        let pageOwing entries = [(page1, (minimalWorldPageSave page1)
                { wpsGenParams = defaultWorldGenParams
                    { wgpLocationInstances = LocationInstances
                        { lisNextId = 2
                        , lisById = HM.singleton (LocationInstanceId 1)
                            (significantOwner entries)
                        , lisPendingLegacy = Nothing } } })]

        it "accepts an obligation whose stored item def still resolves" $
            missingSignificantItemReferences
                (HS.singleton "processing_unit")
                (pageOwing [ LocationSignificantItem 1 "processing_unit"
                                 Nothing False ])
                `shouldBe` []

        it "flags an UNSPAWNED obligation whose stored item def is gone, \
           \naming the page, the location, the slot and the def" $
            case missingSignificantItemReferences HS.empty
                     (pageOwing [ LocationSignificantItem 3 "ghost_core"
                                      Nothing False ]) of
                [r] → do
                    msirPage r     `shouldBe` page1
                    msirInstance r `shouldBe` 1
                    msirSlot r     `shouldBe` 3
                    msirDefName r  `shouldBe` "ghost_core"
                    renderMissingSignificantItemRef r `shouldSatisfy`
                        \m → all (`T.isInfixOf` m)
                            ["location #1", "slot 3", "ghost_core"]
                other → expectationFailure
                    ("expected one finding, got " <> show other)

        it "IGNORES an obligation that already names a spawned item, \
           \however its def has since fared -- nothing re-spawns a bound \
           \slot, so its def name is a historical record and the item \
           \may legitimately have been consumed or destroyed" $ do
            missingSignificantItemReferences HS.empty
                (pageOwing [ LocationSignificantItem 1 "ghost_core"
                                 (Just 900) False ])
                `shouldBe` []
            missingSignificantItemReferences HS.empty
                (pageOwing [ LocationSignificantItem 1 "ghost_core"
                                 (Just 900) True ])
                `shouldBe` []

        it "reports every unspawned offender on the page, and only those" $
            map msirSlot (missingSignificantItemReferences
                (HS.singleton "processing_unit")
                (pageOwing
                    [ LocationSignificantItem 1 "processing_unit" Nothing False
                    , LocationSignificantItem 2 "ghost_core" Nothing False
                    , LocationSignificantItem 3 "ghost_core" (Just 901) True
                    , LocationSignificantItem 4 "other_ghost" Nothing False
                    ]))
                `shouldBe` [2, 4]

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

    -- #2305: 'uiImmunities' is acquired immunity keyed by infection
    -- DEFINITION id, and until this it round-tripped a save verbatim. An
    -- entry whose definition has since been removed from
    -- data/infections/ has no legitimate surface —
    -- 'Engine.Scripting.Lua.API.Units.Combat.unitGetImmunitiesFn' prints
    -- the raw key where a disease name belongs, and 'Combat.Wounds.Tick'
    -- would resume honouring it for whatever content later reclaimed
    -- that id — so 'fromUnitSnapshot' drops it while the session is
    -- still being staged, on #1087's container-knowledge terms:
    -- diagnosed with a count and the ids, never a load failure.
    --
    -- The scrub lives INSIDE that one pure restore rather than beside
    -- it, so these exercise the production path directly and resnapshot
    -- its output, which is what requirement 4 ("a session loaded this
    -- way saves back clean") actually asks.
    describe "acquired-immunity scrub (#2305)" $ do
        let uid1 = UnitId 1
            immUnit imm = (minimalUnitInstance [])
                { uisImmunities = HM.fromList imm }
            woundedUnit imm wounds = (immUnit imm) { uisWounds = wounds }
            snapshotOf us = UnitSnapshot (HM.fromList us) 99
            restore infMgr snap =
                fromUnitSnapshot page1 immunityUnitDefs infMgr
                    emptyEquipmentClassManager emptyItemManager snap
            restored infMgr snap uid =
                let (um, _, _, _, _) = restore infMgr snap
                in uiImmunities <$> HM.lookup uid (umInstances um)
            scrubOf infMgr snap =
                let (_, _, _, sc, _) = restore infMgr snap in sc
            -- Restore, then take the save the resulting session would
            -- write, through the same two adapters 'World.Load.Stage'
            -- and 'World.Thread.Command.Save.WriteWorld' use.
            resnapshotted infMgr snap uid =
                let (um, _, _, _, _) = restore infMgr snap
                in uisImmunities
                     <$> HM.lookup uid (usnInstances (toUnitSnapshot page1 um))
            pageWith us = (minimalWorldPageSave page1) { wpsUnits = us }
            mixed = snapshotOf
                [ (uid1, immUnit [("staph", 0.75), ("ghost_rot", 0.9)]) ]

        it "drops an immunity key whose infection definition is gone" $
            (HM.member "ghost_rot" <$> restored liveInfections mixed uid1)
                `shouldBe` Just False

        it "preserves a resolving key's EXACT level, untouched by the \
           \scrub beside it" $
            (HM.lookup "staph" =≪ restored liveInfections mixed uid1)
                `shouldBe` Just 0.75

        it "leaves an all-resolving map completely alone" $
            restored liveInfections
                (snapshotOf [(uid1, immUnit [("staph", 0.2), ("gas_gangrene", 1)])])
                uid1
                `shouldBe` Just (HM.fromList [("staph", 0.2), ("gas_gangrene", 1)])

        it "drops EVERY entry when nothing is registered, and keeps the \
           \unit itself" $
            restored emptyInfectionManager mixed uid1 `shouldBe` Just HM.empty

        it "the scrubbed key is gone from the save this session writes \
           \back, so the orphan cannot reappear in a later generation \
           \(requirement 4)" $ do
            resnapshotted liveInfections mixed uid1
                `shouldBe` Just (HM.singleton "staph" 0.75)
            case resnapshotted liveInfections mixed uid1 of
                Nothing   → expectationFailure "unit vanished across the round trip"
                Just imm2 → scrubOf liveInfections
                                (snapshotOf [(uid1, immUnit (HM.toList imm2))])
                                `shouldBe` emptyImmunityScrub

        it "the diagnostic is empty, and renders nothing, when every key \
           \resolves" $ do
            let sc = scrubOf liveInfections
                        (snapshotOf [(uid1, immUnit [("staph", 0.5)])])
            sc `shouldBe` emptyImmunityScrub
            renderImmunityScrub page1 sc `shouldBe` Nothing

        it "counts ENTRIES removed, not distinct ids: four units carrying \
           \one dead id report 4 removals and a single id" $ do
            let sc = scrubOf liveInfections $ snapshotOf
                        [ (UnitId n, immUnit [("ghost_rot", 0.5), ("staph", 0.5)])
                        | n ← [1 .. 4] ]
            iscRemoved sc `shouldBe` 4
            iscIds sc `shouldBe` ["ghost_rot"]

        it "names every DISTINCT unresolved id, sorted, however many \
           \units carried each" $ do
            let sc = scrubOf liveInfections $ snapshotOf
                        [ (UnitId 1, immUnit [("zeta_pox", 0.1), ("ghost_rot", 0.2)])
                        , (UnitId 2, immUnit [("ghost_rot", 0.3), ("staph", 0.4)]) ]
            iscRemoved sc `shouldBe` 3
            iscIds sc `shouldBe` ["ghost_rot", "zeta_pox"]

        it "renders the count and the ids for the page it happened on" $
            renderImmunityScrub page1 (ImmunityScrub 3 ["ghost_rot", "zeta_pox"])
                `shouldBe` Just
                    "dropping 3 acquired-immunity entries on page 'page1' \
                    \whose infection definition no longer exists \
                    \(ghost_rot, zeta_pox)"

        it "renders the singular for exactly one removal" $
            renderImmunityScrub page1 (ImmunityScrub 1 ["ghost_rot"])
                `shouldBe` Just
                    "dropping 1 acquired-immunity entry on page 'page1' \
                    \whose infection definition no longer exists \
                    \(ghost_rot)"

        it "ignores a unit dropped as a DEF orphan: its whole instance \
           \goes, so the scrub removed nothing" $
            scrubOf liveInfections (snapshotOf
                [ (uid1, (immUnit [("ghost_rot", 0.5)])
                            { uisDefName = "ghost_unit" }) ])
                `shouldBe` emptyImmunityScrub

        it "scrubs a DEAD unit, whose wound tick would never decay the \
           \entry (review correction)" $ do
            let snap = snapshotOf
                    [ (uid1, (immUnit [("ghost_rot", 1.0)]) { uisPose = "dead" }) ]
            iscRemoved (scrubOf liveInfections snap) `shouldBe` 1
            restored liveInfections snap uid1 `shouldBe` Just HM.empty

        -- Requirement 6: the OTHER half of an infection reference stays
        -- a hard load rejection, and the two halves must not bleed into
        -- each other. A wound reference names an infection a unit is
        -- CURRENTLY carrying; an immunity key is a memory of content it
        -- already survived, and inventorying it here would refuse every
        -- save in which any surviving unit still holds un-decayed
        -- immunity to a removed definition.
        it "missingInfectionReferences does NOT report an immunity-only \
           \orphan — that one belongs to the staging scrub" $
            missingInfectionReferences liveInfections
                [ (page1, pageWith (snapshotOf
                      [(uid1, immUnit [("ghost_rot", 0.9)])])) ]
                `shouldBe` []

        it "missingInfectionReferences still reports an unresolved \
           \woundInfectionType, which stays a load rejection" $ do
            let refs = missingInfectionReferences liveInfections
                    [ (page1, pageWith (snapshotOf
                          [ (UnitId 7
                            , woundedUnit [] [infectedWound "ghost_rot"]) ])) ]
            map mirInfType refs `shouldBe` ["ghost_rot"]
            map mirUnitId refs `shouldBe` [7]

        it "reports the wound and NOT the immunity when one unit carries \
           \both halves of the same dead id" $ do
            let refs = missingInfectionReferences liveInfections
                    [ (page1, pageWith (snapshotOf
                          [ (UnitId 7
                            , woundedUnit [("ghost_rot", 0.9)]
                                          [infectedWound "ghost_rot"]) ])) ]
            length refs `shouldBe` 1
            map mirWoundPart refs `shouldBe` ["torso"]

        it "missingInfectionReferences still ignores the empty-string \
           \no-infection sentinel" $
            missingInfectionReferences liveInfections
                [ (page1, pageWith (snapshotOf
                      [(uid1, woundedUnit [] [infectedWound ""])])) ]
                `shouldBe` []

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

-- Helpers -----------------------------------------------------------

-- | #2305 fixtures. Two registered infections and nothing else:
--   @ghost_rot@ and @zeta_pox@ are the ids every case uses for content
--   that has been REMOVED, so neither may ever appear here.
liveInfections ∷ InfectionManager
liveInfections = InfectionManager $ HM.fromList
    [ (i, immunityInfectionDef i) | i ← ["staph", "gas_gangrene"] ]

immunityInfectionDef ∷ Text → InfectionDef
immunityInfectionDef iid = InfectionDef
    { infId = iid, infName = "Display " <> iid, infIcon = ""
    , infCategory = "bacterial", infSites = ["surface"], infBaseWeight = 1
    , infTempMin = 0, infTempMax = 40, infMoistMin = 0, infMoistMax = 1
    , infAggressiveness = 1, infInfectability = 1
    , infCurableBy = ["antibiotics"], infCureRate = 1
    , infWoundInfectable = True, infEffects = []
    , infTransmissibility = 0, infTransmission = [] }

-- | The def set 'minimalUnitInstance' resolves against. Mirrors
--   'Test.Headless.Unit.Faction.minimalDef' — only the fields
--   'fromUnitSnapshot' re-resolves carry any weight.
immunityUnitDefs ∷ HM.HashMap Text UnitDef
immunityUnitDefs = HM.singleton "test_unit" UnitDef
    { udName = "test_unit", udNamePool = Nothing, udDisplayName = Nothing
    , udTexture = TextureHandle 0, udPortrait = Nothing
    , udDirSprites = Map.empty
    , udBaseWidth = 0, udMaxSpeed = 1.0, udRunThreshold = 0.6
    , udAnimations = HM.empty, udStateAnims = HM.empty, udEagerStats = False
    , udStatTemplates = HM.empty, udBodyTemplates = HM.empty
    , udSkillTemplates = HM.empty, udKnowledgeTemplates = HM.empty
    , udStartingInventory = []
    , udEquipmentClass = Nothing, udStartingEquipment = HM.empty
    , udStartingAccessories = []
    , udBodyParts =
        [ BodyPart
            { bpId = "torso", bpName = "torso", bpParent = Nothing
            , bpVital = False, bpAreaWeight = 1.0, bpTacticalValue = 0.5
            , bpBleedFactor = 1.0, bpHeightLow = 0, bpHeightHigh = 1
            , bpLayers = [], bpTargetable = True, bpDepth = 0.0
            , bpAffectsLocomotion = False, bpAffectsBalance = False } ]
    , udNaturalResistance = defaultNaturalResistance
    , udNaturalWeapon = Nothing, udModifiers = [] }

-- | A wound whose infection type is @iid@ — the reference
--   'missingInfectionReferences' does inventory.
infectedWound ∷ Text → Wound
infectedWound iid = Wound
    { woundPart = "torso", woundKind = "slash", woundSeverity = 0.6
    , woundAt = 0, woundBandage = 0, woundClot = 0, woundHeal = 0
    , woundDressing = "", woundInfection = 0.4, woundClean = False
    , woundInfectionType = iid, woundNecrosis = 0 }

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

-- | A minimal 'WorldPageSave' fixture (all designation/entity maps
--   empty) for the round-8 def-reference validators below, which only
--   ever look at 'wpsBuildings'/'wpsUnits'/'wpsGroundItems'/
--   'wpsCraftBills'/'wpsConstructDesignations'.
minimalWorldPageSave ∷ WorldPageId → WorldPageSave
minimalWorldPageSave pid = WorldPageSave
    { wpsPageId       = pid
    , wpsGeneratedId  = Just (fixtureGeneratedWorldIdForPage pid)
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
    , wpsCropPlots    = HM.empty
    , wpsPlantDesignations = HM.empty
    , wpsContainerKnowledge = emptyContainerKnowledge
    , wpsIdentity     = Nothing
    }
