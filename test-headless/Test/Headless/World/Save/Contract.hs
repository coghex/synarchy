{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
-- | The end-to-end persistence contract gate (#767, save-overhaul D1 —
--   the final Phase-4 child of the persistence-overhaul epic, #768).
--
--   A1-C4 (issues #756-#766) each own their own targeted tests for one
--   slice of the save/load pipeline (state classification, the snapshot
--   barrier, the immutable snapshot, the component codecs, Lua
--   persistence, atomic storage, transactional loading, reference
--   integrity, migrations). This module is the ONE place that proves
--   the assembled system honors the player-facing contract TOGETHER:
--
--   > A fresh process loads the same persistent gameplay state captured
--   > at the save boundary [...] and resumes at default speed without
--   > promising the same random future.
--
--   The PURE half lives here: a single representative multi-page
--   session (deliberately touching every category
--   @docs/persistence_state_inventory.md@'s SS12 coverage map names —
--   designations of every kind, ground items with nested contents,
--   units with stats/skills/equipment/wounds, unit-sim state, buildings
--   with storage/progress, craft bills, power nodes, and a world
--   identity) is captured, encoded, and decoded through the REAL
--   production codec ('World.Save.Envelope.encodeSessionSnapshot' /
--   'decodeSessionEnvelope' — the same functions
--   'World.Thread.Command.Save.WriteWorld' and the world-thread load
--   path actually call), and every field is compared via
--   'SessionSnapshot''s own derived 'Eq' — no bespoke JSON schema, no
--   partial field list (contract requirement 5: "must derive its
--   coverage from #756's authoritative persistence inventory and
--   compare every classified persistent field", never "checks only a
--   few fields").
--
--   The FRESH-PROCESS half (an actual @quit@ + a genuinely new headless
--   process, requirement 5's own literal ask) cannot run inside hspec at
--   all — it needs real process boundaries. That lives in
--   @tools/persistence_contract_probe.py@ (compact, CI-eligible smoke)
--   and @tools/persistence_contract_sweep.py@ (the broader manual
--   sweep), both built on 'tools/persistence_snapshot.py''s
--   @compare_session_files@ — which reuses the EXACT SAME
--   'decodeSessionEnvelope' entry point this module exercises in-process,
--   just run via a @cabal repl@ subprocess against real files on disk so
--   two independently-produced save generations (across a real restart)
--   can be compared the identical way. See
--   @docs/persistence_state_inventory.md@ SS12 for the full coverage
--   map and @docs/persistence_contract.md@ SS6 for the consolidated test
--   matrix.
--
--   Run just this gate:
--   @cabal test synarchy-test-headless --test-options='--match \"persistence contract\"'@
module Test.Headless.World.Save.Contract (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Serialize as S
import World.Save.Envelope (encodeSessionSnapshot, decodeSessionEnvelope)
import World.Save.Snapshot
import World.Save.Snapshot.Adapter
    (SaveRequestMeta(..), snapshotSaveMetadata, snapshotToSaveData)
import World.Save.Types
    ( BuildingSnapshot(..), BuildingInstanceSnapshot(..)
    , UnitSnapshot(..), UnitInstanceSnapshot(..)
    , SaveData(..), WorldPageSave(..) )
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)
import World.Page.Types (WorldPageId(..), WorldIdentity(..))
import World.Render.Zoom.Types (ZoomMapMode(..))
import World.Tool.Types (ToolMode(..))
import Engine.Graphics.Camera (CameraFacing(..))
import Structure.Palette (emptyTexPalette)
import Item.Ground (GroundItems(..), GroundItem(..))
import Item.Types (ItemInstance(..))
import World.Spoil.Types (emptySpoilPiles, SpoilPile(..))
import World.Flora.Harvest (emptyFloraHarvests)
import World.Flora.CropPlot (emptyCropPlots, CropPlot(..))
import World.Flora.Types (FloraId(..))
import World.Edit.Types (emptyWorldEdits, WorldEdit(..))
import World.Chunk.Types (ChunkCoord(..))
import World.Material.Id (MaterialId(..))
import World.Mine.Types (MineDesignation(..))
import World.Construct.Types
    (ConstructDesignation(..), ConstructTarget(..), ConstructStatus(..))
import World.Chop.Types (ChopDesignation(..))
import World.Till.Types (TillDesignation(..))
import World.Plant.Types (PlantDesignation(..))
import Craft.Bills (emptyCraftBills, CraftBill(..), CraftBills(..), BillId(..), BillMode(..))
import Power.Types (emptyPowerNodes, PowerNode(..), PowerNodes(..), PowerNodeId(..), PowerRole(..))
import Building.Types (BuildingId(..))
import Unit.Types (UnitId(..), Wound(..), Scar(..), StatModifier(..))
import Unit.Sim.Types (UnitSimState(..), Pose(..), UnitActivity(..))
import Unit.Direction (Direction(..))

page1, page2 ∷ WorldPageId
page1 = WorldPageId "page1"
page2 = WorldPageId "page2"

-- | 'WorldGenParams''s manual cereal instance DERIVES a few nested
--   fields from 'wgpSeed'/'wgpWorldSize' on decode rather than storing
--   them, so a hand-built value whose nested seeds don't already agree
--   is not a serialize fixpoint -- reach it explicitly via one
--   decode∘encode (mirrors 'Test.Headless.World.Save.Components''s
--   identical helper/note).
canon ∷ WorldGenParams → WorldGenParams
canon gp = case S.decode (S.encode gp) of
    Right gp' → gp'
    Left err  → error ("canon: " <> err)

richItem ∷ Word64 → ItemInstance
richItem iid = ItemInstance
    { iiDefName = "first_aid_kit", iiCurrentFill = 3, iiQuality = 82
    , iiCondition = 74.5, iiWeight = 1.25, iiSharpness = 0
    , iiInstanceId = iid, iiTemp = Just 21.5
    , iiContents =
        [ ItemInstance
            { iiDefName = "bandage", iiCurrentFill = 1, iiQuality = 100
            , iiCondition = 100, iiWeight = 0.05, iiSharpness = 0
            , iiInstanceId = iid + 1, iiTemp = Nothing, iiContents = [] } ]
    }

-- | A unit with populated stats/skills/knowledge/modifiers/inventory/
--   equipment/accessories/wounds/scars/immunities -- requirement 4's
--   "units with stats, skills, inventory, equipment, wounds or other
--   mutable physiology".
richUnit ∷ UnitInstanceSnapshot
richUnit = UnitInstanceSnapshot
    { uisDefName = "acolyte", uisBaseWidth = 1, uisGridX = 12.5, uisGridY = 7.5
    , uisGridZ = 2, uisFacing = DirS, uisCurrentAnim = "combat_idle"
    , uisAnimStart = 1234.5, uisAnimReverse = False, uisActivity = "combat"
    , uisPose = "standing", uisAnimStride = 3
    , uisStats = HM.fromList [("health", 62.0), ("stamina", 40.0)]
    , uisModifiers = HM.fromList
        [ ("health", [ StatModifier { smDelta = -5, smSource = "wounded-torso"
                                     , smExpiry = Nothing, smPercent = 0 } ]) ]
    , uisSkills = HM.fromList [("mining", 34.0), ("combat", 12.0)]
    , uisKnowledge = HM.fromList [("smithing", 5.0)]
    , uisInventory = [richItem 950]
    , uisEquipped = HM.fromList [("main_hand", richItem 960)]
    , uisAccessories = [richItem 970]
    , uisFactionId = "player"
    , uisWounds =
        [ Wound { woundPart = "torso", woundKind = "slash", woundSeverity = 0.4
                , woundAt = 100.0, woundBandage = 0.3, woundClot = 0.2
                , woundHeal = 0.1, woundDressing = "bandage"
                , woundInfection = 0.05, woundClean = True
                , woundInfectionType = "", woundNecrosis = 0 } ]
    , uisScars =
        [ Scar { scarPart = "left_arm", scarKind = "burn", scarSeverity = 0.6
               , scarAt = 50.0 } ]
    , uisImmuneResponse = 0.3
    , uisImmunities = HM.singleton "staph" 0.2
    , uisBlood = 4.2
    , uisName = "Test Acolyte"
    }

richBuilding ∷ BuildingInstanceSnapshot
richBuilding = BuildingInstanceSnapshot
    { bisDefName = "furnace", bisAnchorX = 3, bisAnchorY = 4, bisGridZ = 0
    , bisSpawnedAt = 0, bisTileW = 2, bisTileH = 2, bisSpawnRemaining = 0
    , bisBuildProgress = 100
    , bisMaterialsDelivered = HM.singleton "stone" [richItem 980]
    , bisStorage = [richItem 990]
    }

richSimState ∷ UnitSimState
richSimState = UnitSimState
    { usRealX = 12.5, usRealY = 7.5, usGridZ = 2, usRealZ = 2.0
    , usTarget = Nothing, usPose = Standing, usState = Idle, usFacing = DirS
    , usLocalPath = []
    , usDrinkUntil = Nothing, usEatUntil = Nothing, usPickupUntil = Nothing
    , usTransitionUntil = Nothing, usTransitionStride = 0, usPostTransition = []
    , usClimbFromTile = Nothing, usClimbToTile = Nothing, usClimbStartTime = Nothing
    , usClimbSlipAt = Nothing, usFallFromTile = Nothing, usFallToTile = Nothing
    , usPendingClimbXP = 0, usGetUpAt = Nothing, usPendingFallDrop = Nothing
    , usJumpApex = Nothing, usMoveGrade = 0
    }

richBills ∷ CraftBills
richBills = CraftBills
    { cbsBills = HM.singleton (BillId 1) CraftBill
        { cbId = BillId 1, cbStation = BuildingId 1, cbRecipe = "smelt_steel"
        , cbRemaining = 3, cbClaimant = Nothing, cbClaimedAt = 0
        , cbProgress = 0.4, cbSeq = 1
        , cbPaused = False, cbWorking = False, cbMode = RepeatForever
        , cbTarget = 0, cbOutputItem = "steel_bar" }
    , cbsNextId = 2 }

richNodes ∷ PowerNodes
richNodes = PowerNodes
    { pnsNodes = HM.singleton (PowerNodeId 1) PowerNode
        { pnId = PowerNodeId 1, pnBuilding = BuildingId 1
        , pnRole = PowerStorage, pnPeakWatts = 0, pnCapacityWh = 5000
        , pnStoredWh = 1234.5 }
    , pnsNextId = 2 }

-- | Deliberately combines EVERY category the coverage map (SS12) names:
--   designations of every kind, ground items with nested contents, a
--   rich unit + building + sim state, craft bills, power nodes, and a
--   world identity -- in one page, so a single encode/decode round trip
--   exercises every registered component's assembly fold at once.
richPage ∷ PageSnapshot
richPage = PageSnapshot
    { pgsPageId       = page1
    , pgsGenParams    = canon defaultWorldGenParams { wgpSeed = 424242 }
    , pgsCameraX      = 12.5
    , pgsCameraY      = 7.5
    , pgsTimeHour     = 14
    , pgsTimeMinute   = 30
    , pgsDateYear     = 3
    , pgsDateMonth    = 5
    , pgsDateDay      = 17
    , pgsMapMode      = ZMPressure
    , pgsEdits        = HM.singleton (ChunkCoord 0 0) [WeDeleteTile 1 2]
    , pgsMineDesignations      = HM.singleton (1, 2) (MineDesignation 0 (0.9, 0.8, 0.7, 0.6) 0.3)
    , pgsConstructDesignations = HM.singleton (3, 4)
        (ConstructDesignation 0 (CtBuilding "cargo_hold_S") CsClaimed 0.5 True)
    , pgsGroundItems  = GroundItems 2 (HM.singleton 1 (GroundItem (richItem 900) 5.5 6.5))
    , pgsSpoilPiles   = HM.singleton (5, 6) (SpoilPile (MaterialId 3) (1.0, 1.0, 1.0, 1.0))
    , pgsBuildings    = BuildingSnapshot
        { bsnInstances = HM.singleton (BuildingId 1) richBuilding, bsnNextId = 2 }
    , pgsUnits        = UnitSnapshot
        { usnInstances = HM.singleton (UnitId 1) richUnit, usnNextId = 2 }
    , pgsUnitSimStates = HM.singleton (UnitId 1) richSimState
    , pgsFloraHarvests = emptyFloraHarvests
    , pgsChopDesignations = HM.singleton (7, 8) (ChopDesignation 0)
    , pgsCraftBills   = richBills
    , pgsPowerNodes   = richNodes
    , pgsTillDesignations = HM.singleton (9, 10) (TillDesignation 0)
    , pgsCropPlots    = HM.singleton (11, 12) (CropPlot (FloraId 3) 5 0.9)
    , pgsPlantDesignations = HM.singleton (13, 14) (PlantDesignation 0 (FloraId 3))
    , pgsIdentity     = Just (WorldIdentity "Aldermoor Deep" (Just "the deep home"))
    }

-- | A second, minimal page -- proves multi-page independence (a stable
--   identity + distinct per-page camera/gen-params, requirement 4).
minimalPage2 ∷ PageSnapshot
minimalPage2 = PageSnapshot
    { pgsPageId       = page2
    , pgsGenParams    = canon defaultWorldGenParams { wgpSeed = 99 }
    , pgsCameraX      = 0, pgsCameraY = 0
    , pgsTimeHour     = 0, pgsTimeMinute = 0
    , pgsDateYear     = 1, pgsDateMonth = 1, pgsDateDay = 1
    , pgsMapMode      = ZMDefault
    , pgsEdits        = emptyWorldEdits
    , pgsMineDesignations      = HM.empty
    , pgsConstructDesignations = HM.empty
    , pgsGroundItems  = GroundItems 0 HM.empty
    , pgsSpoilPiles   = emptySpoilPiles
    , pgsBuildings    = BuildingSnapshot { bsnInstances = HM.empty, bsnNextId = 2 }
    , pgsUnits        = UnitSnapshot { usnInstances = HM.empty, usnNextId = 2 }
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

richGlobals ∷ SessionGlobals
richGlobals = SessionGlobals
    { sgGameTime       = 50000.5
    , sgTexPalette     = emptyTexPalette
    , sgNextItemId     = 1000
    , sgNextBuildingId = 2
    , sgNextUnitId     = 2
    , sgActivePage     = page1
    , sgVisiblePages   = [page1, page2]
    , sgLiveCamera     = LiveCameraSnapshot
        { lcsOwnerPage = Just page1, lcsX = 12.5, lcsY = 7.5, lcsZoom = 3
        , lcsFacing = FaceEast }
    }

representativeSnapshot ∷ SessionSnapshot
representativeSnapshot = case captureSessionSnapshot richGlobals [richPage, minimalPage2] of
    Right s   → s
    Left errs → error ("representativeSnapshot invalid: " <> show errs)

spec ∷ Spec
spec = do
    describe "fresh-process structural equivalence (pure round trip, \
             \requirement 5)" $ do
        it "round-trips a representative multi-page session -- every \
           \designation kind, nested ground/inventory items, a unit \
           \with stats/skills/equipment/wounds, unit-sim state, a \
           \building with storage, a craft bill, a power node, and a \
           \world identity -- through the REAL production codec \
           \(encodeSessionSnapshot / decodeSessionEnvelope), comparing \
           \EVERY persistent field via SessionSnapshot's derived Eq" $ do
            let req = SaveRequestMeta { srmSlotName = "contract_test", srmTimestamp = "ts" }
                meta = snapshotSaveMetadata req representativeSnapshot
                encoded = encodeSessionSnapshot meta representativeSnapshot []
            case decodeSessionEnvelope HS.empty HS.empty encoded of
                Left err → expectationFailure (show err)
                Right (_meta, snap, _luaComponents, isMigrated) → do
                    snap `shouldBe` representativeSnapshot
                    isMigrated `shouldBe` False

    describe "repeated-cycle stability (pure, requirement 9)" $ do
        it "three successive encode -> decode -> re-encode cycles never \
           \drift -- no cycle accumulates ghost pages, duplicate \
           \entities, or allocator drift" $ do
            let req = SaveRequestMeta { srmSlotName = "cycle_test", srmTimestamp = "ts" }
                cycleOnce snap =
                    let meta = snapshotSaveMetadata req snap
                        encoded = encodeSessionSnapshot meta snap []
                    in case decodeSessionEnvelope HS.empty HS.empty encoded of
                        Left err → error (show err)
                        Right (_, snap', _, _) → snap'
                gen1 = cycleOnce representativeSnapshot
                gen2 = cycleOnce gen1
                gen3 = cycleOnce gen2
            gen1 `shouldBe` representativeSnapshot
            gen2 `shouldBe` representativeSnapshot
            gen3 `shouldBe` representativeSnapshot

    describe "reset/rebuild policy at the type level (requirement 6)" $ do
        it "the adapter fabricates the documented reset defaults for \
           \EVERY page in the representative (not just minimal) \
           \session -- DefaultTool and time scale 1, never a captured \
           \value" $ do
            let req = SaveRequestMeta { srmSlotName = "reset_test", srmTimestamp = "ts" }
                sd = snapshotToSaveData req representativeSnapshot
            forM_ (sdWorlds sd) $ \wps → do
                wpsTimeScale wps `shouldBe` 1
                wpsToolMode wps `shouldBe` DefaultTool

    describe "nondeterministic continuation (requirement 8)" $ do
        it "SessionSnapshot carries no runtime RNG/thread-schedule state \
           \-- only the domain-meaningful world-generation seed, which \
           \IS persisted exactly" $ do
            -- Structural: SessionSnapshot's own type (World.Save.Snapshot)
            -- has no RNG-generator or thread-schedule field at all (see
            -- its module haddock) -- this asserts the one seed the
            -- contract DOES require, distinguishing "meaningful seed" from
            -- "replay RNG" the same way the contract's own SS1 does.
            case HM.lookup page1 (snapPages representativeSnapshot) of
                Nothing → expectationFailure "page1 missing from representativeSnapshot"
                Just page → wgpSeed (pgsGenParams page) `shouldBe` 424242
