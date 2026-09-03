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
import qualified Data.Text as T
import World.Save.Envelope
    ( encodeSessionSnapshot, decodeSessionEnvelope, LuaComponentSpec(..)
    , metadataComponentId, currentEnvelopeVersion )
import World.Save.Envelope.Codec (DecodedEnvelope(..), decodeEnvelope)
import World.Save.Envelope.Types
    ( ComponentId(..), ComponentDescriptor(..), EnvelopeManifest(..)
    , defaultEnvelopeLimits )
import World.Save.Component (componentKnownIds, componentRequiredIds)
import World.Save.Snapshot
import World.Save.Snapshot.Adapter
    (SaveRequestMeta(..), snapshotSaveMetadata, snapshotToSaveData)
import World.Save.Types
    ( BuildingSnapshot(..), BuildingInstanceSnapshot(..)
    , UnitSnapshot(..), UnitInstanceSnapshot(..)
    , SaveData(..), WorldPageSave(..) )
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)
import World.Base (GeoFeatureId(..))
import World.River.Naming (RiverName(..), RiverNames(..))
import Language.Etymology.Source (EtymologySource(..))
import Language.Semantic.Types
    (ConceptId(..), GramNumber(..), NameExpr(..))
import World.Page.Types (WorldPageId(..), WorldIdentity(..), mkWorldIdentity)
import Language.Generated.Types
    (LanguageProvenance(..), LangSeed(..), GeneratorVersion(..))
import Location.Bounds (AbsBounds(..))
import Location.Instance
    ( LocationInstance(..), LocationInstances(..), LocationInstanceId(..)
    , LocationLifecycle(..), LocationSignificantItem(..) )
import World.Render.Zoom.Types (ZoomMapMode(..))
import World.Tool.Types (ToolMode(..))
import Engine.Graphics.Camera (CameraFacing(..))
import Structure.Palette (TexPalette(..))
import Item.Ground (GroundItems(..), GroundItem(..))
import Item.Types (ItemInstance(..), ItemStorage(..))
import World.Spoil.Types (emptySpoilPiles, SpoilPile(..))
import World.Flora.Harvest (emptyFloraHarvests)
import World.Flora.CropPlot (CropPlotOf(..))
import World.Edit.Types (emptyWorldEdits, WorldEdit(..))
import qualified Data.List as L
import World.Flora.Reference (FloraRef(..), renderFloraRef)
import World.Chunk.Types (ChunkCoord(..))
import World.Material.Id (MaterialId(..))
import World.Mine.Types (MineDesignation(..))
import World.Construct.Attempt (ConstructAttemptId(..), firstConstructAttemptId)
import World.Construct.Receipt (ConstructPayment(..), mkMaterialReceipt)
import World.Construct.Types
    (ConstructDesignation(..), ConstructTarget(..), ConstructStatus(..))
import World.Chop.Types (ChopDesignation(..))
import World.Till.Types (TillDesignation(..))
import World.Plant.Types (PlantDesignationOf(..))
import Craft.Bills (emptyCraftBills, CraftBill(..), CraftBills(..), BillId(..), BillMode(..))
import Unit.Transfer
    ( TransferBatch(..), TransferEndpoint(..), TransferItemRef(..)
    , TransferReason(..), TransferState(..), QueuedTransfer(..)
    , requestFailure, staleFailure )
import Unit.Transfer.Orders
    ( TransferOrder(..), TransferOrderId(..), TransferOrders(..)
    , addTransferOrder, emptyTransferOrders, transferOrderList )
import Power.Types (emptyPowerNodes, PowerNode(..), PowerNodes(..), PowerNodeId(..), PowerRole(..))
import Building.Types (BuildingId(..))
import Building.Knowledge
    (ContainerKnowledge(..), ContainerRecord(..), emptyContainerKnowledge)
import Unit.Types (UnitId(..), Wound(..), Scar(..), StatModifier(..))
import Unit.Sim.Types
    (UnitSimState(..), Pose(..), UnitActivity(..), MoveTarget(..)
    , MoveHazardPolicy(..))
import Unit.Direction (Direction(..))
import World.Flora.Identity
    ( firstPlantedFloraCursor, generatedFloraInstanceId
    , plantedFloraInstanceId )
import Test.Headless.Harness.GeneratedIds (fixtureGeneratedWorldIdForPage)

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
      -- #1233: distinctive physical values, and a kit that really
      -- offers storage, so a dropped/mis-mapped bulk or capacity shows
      -- up in the whole-session round trip. The nested bandage carries
      -- a bulk but NO storage, so the optional half is exercised in
      -- both directions inside one recursive tree.
    , iiBulk = Just 4.25, iiStorage = Just (ItemStorage 8.5 6.75)
    , iiContents =
        [ ItemInstance
            { iiDefName = "bandage", iiCurrentFill = 1, iiQuality = 100
            , iiCondition = 100, iiWeight = 0.05, iiSharpness = 0
            , iiInstanceId = iid + 1, iiTemp = Nothing, iiContents = []
            , iiBulk = Just 0.1, iiStorage = Nothing } ]
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

-- | A SECOND building on the rich page (#1246). It exists so the
--   populated transfer-order fixture below can carry a genuine
--   building-to-building order (epic decision D-10) whose acting unit is
--   neither endpoint — a same-building "pair" would prove nothing about
--   whether both endpoint identities really survive independently.
richBuilding2 ∷ BuildingInstanceSnapshot
richBuilding2 = BuildingInstanceSnapshot
    { bisDefName = "cargo_hold_S", bisAnchorX = 9, bisAnchorY = 11
    , bisGridZ = 0
    , bisSpawnedAt = 0, bisTileW = 1, bisTileH = 1, bisSpawnRemaining = 0
    , bisBuildProgress = 100
    , bisMaterialsDelivered = HM.singleton "stone" [richItem 930]
    , bisStorage = [richItem 940]
    }

-- | 'addTransferOrder' refuses on an exhausted allocator (#1246 review
--   round 2), which no fixture here can reach — every one starts from
--   'emptyTransferOrders'. Fail loudly rather than defaulting, so a
--   future change that DID exhaust it surfaces as this error instead of
--   as a silently empty store.
mustAdd ∷ UnitId → TransferBatch → TransferOrders → TransferOrders
mustAdd uid batch orders = case addTransferOrder uid batch orders of
    Just (orders', _) → orders'
    Nothing → error "fixture: addTransferOrder refused a fresh allocator"

-- | #1246: a POPULATED transfer-order store, built through the REAL
--   creation surface ('addTransferOrder') rather than by hand, so the
--   fixture also pins that ids start at 1, advance, and leave the
--   allocator above every order it issued.
--
--   Between the two orders the entries cover ALL SIX
--   'Unit.Transfer.TransferState' shapes, and 'TransferFailed' appears
--   twice — once from 'requestFailure' (no cause) and once from
--   'staleFailure' (a cause present) — because a fixture carrying only
--   one of those could not tell a persisted @Nothing@ apart from a
--   dropped @Just@. Every referenced unit, building and item instance
--   really exists on this page, so the round trip exercises the
--   RESOLVING path; the dangling and wrong-page paths have their own
--   coverage in "Test.Headless.World.Save.Integrity".
richTransferOrders ∷ TransferOrders
richTransferOrders =
    let afterFirst  = mustAdd (UnitId 1) unitToBuilding emptyTransferOrders
        afterSecond = mustAdd (UnitId 1) buildingToBuilding afterFirst
    in afterSecond
  where
    -- Request order is meaningful and is asserted positionally below.
    unitToBuilding = TransferBatch
        { tbSource      = EndpointUnit (UnitId 1)
        , tbDestination = EndpointBuilding (BuildingId 1)
        , tbEntries =
            [ entry 950 "first_aid_kit" TransferQueued
            , entry 951 "bandage"       TransferInTransit
            , entry 960 "first_aid_kit" TransferReadyToCommit
            , entry 961 "bandage"       TransferCompleted
            ]
        }
    -- D-10: both ends are buildings and the acting unit is neither.
    buildingToBuilding = TransferBatch
        { tbSource      = EndpointBuilding (BuildingId 1)
        , tbDestination = EndpointBuilding (BuildingId 2)
        , tbEntries =
            [ entry 990 "first_aid_kit" TransferCancelled
            , entry 991 "bandage"
                (TransferFailed (requestFailure ReasonReceiverFull))
            , entry 980 "first_aid_kit"
                (TransferFailed (staleFailure ReasonInstanceMissing))
            ]
        }
    entry iid nm st = QueuedTransfer
        { qtItem = TransferItemRef { tirInstanceId = iid, tirDefName = nm }
        , qtState = st }

-- | Populated with a real move target, a multi-step local path, a
--   non-Idle activity, a pending drink timer, and an in-progress pose
--   transition -- round-2 review: an all-Nothing/all-empty fixture's Eq
--   comparison can't detect a dropped or mis-mapped POPULATED field.
richSimState ∷ UnitSimState
richSimState = UnitSimState
    { usRealX = 12.5, usRealY = 7.5, usGridZ = 2, usRealZ = 2.0
      -- FallProhibited deliberately (#1217): the representative session
      -- must carry a NON-default hazard policy, so a codec that dropped
      -- it — or defaulted it back on load — fails this contract rather
      -- than round-tripping vacuously.
    , usTarget = Just (MoveTarget 20.0 15.0 1.5 FallProhibited)
    , usPose = Standing, usState = Walking, usFacing = DirS
    , usLocalPath = [(13.0, 7.5), (14.0, 8.0), (15.0, 8.5)]
    , usDrinkUntil = Just 54321.0, usEatUntil = Nothing, usPickupUntil = Nothing
    , usTransitionUntil = Just 500.0, usTransitionStride = 2
    , usPostTransition = [Crouching, Standing]
    , usClimbFromTile = Nothing, usClimbToTile = Nothing, usClimbStartTime = Nothing
    , usClimbSlipAt = Nothing, usFallFromTile = Nothing, usFallToTile = Nothing
    , usPendingClimbXP = 0.4, usGetUpAt = Nothing, usPendingFallDrop = Nothing
    , usJumpApex = Nothing, usMoveGrade = 0.75
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

-- | #1087: a POPULATED container-knowledge record, so this session's
--   round trip proves the remembered view really persists rather than
--   only proving an empty map survives. The remembered instance ids are
--   deliberately DISTINCT from every live one in this session (900/950/
--   960/970/980/990 are the live ones) — that is what a historical
--   observation looks like once the real item has moved on, and it also
--   demonstrates that these ids are exempt from the session's
--   duplicate-live-id and allocator checks by construction.
richKnowledge ∷ ContainerKnowledge
richKnowledge = ContainerKnowledge $ HM.singleton (BuildingId 1)
    ContainerRecord
        { crItems        = [richItem 800]
        , crStoredWeight = 1.35
        , crRevealedAt   = 49000.25
        }

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
    , pgsGeneratedId  = Just (fixtureGeneratedWorldIdForPage page1)
    , pgsGenParams    = canon defaultWorldGenParams
                          { wgpSeed = 424242
                          -- #1101: a placed location named in this
                          -- page's own language, gloss and all. An
                          -- empty instance table could not tell a
                          -- persisted gloss from a dropped one.
                          , wgpLocationInstances = richLocationInstances
                          -- #1102: the same for rivers — a named one
                          -- with its gloss, keyed by the feature id the
                          -- timeline allocated.
                          , wgpRiverNames = richRiverNames }
    , pgsCameraX      = 12.5
    , pgsCameraY      = 7.5
    , pgsTimeHour     = 14
    , pgsTimeMinute   = 30
    , pgsDateYear     = 3
    , pgsDateMonth    = 5
    , pgsDateDay      = 17
    , pgsMapMode      = ZMPressure
      -- #2243: the first of the three durable species references the
      -- round trip below reads back BY NAME. Its planted id is 2, below
      -- 'pgsPlantedFloraCursor' (3) so 'validateWorldEdits' is satisfied.
    , pgsEdits        = HM.singleton (ChunkCoord 0 0)
        [ WeDeleteTile 1 2
        , WePlaceFloraRef 15 16 (FloraByName "wheat") 4 0.5
              (plantedFloraInstanceId 2) ]
    , pgsMineDesignations      = HM.singleton (1, 2) (MineDesignation 0 (0.9, 0.8, 0.7, 0.6) 0.3)
    , pgsConstructDesignations = HM.singleton (3, 4)
        (ConstructDesignation 0 (CtBuilding "cargo_hold_S") CsClaimed 0.5
            (ConstructAttemptId 7)
            (CpPaid (mkMaterialReceipt [("steel_plate", 2), ("wood_log", 1)])))
    , pgsConstructNextAttempt = ConstructAttemptId 8
    , pgsGroundItems  = GroundItems 2 (HM.singleton 1 (GroundItem (richItem 900) 5.5 6.5))
    , pgsSpoilPiles   = HM.singleton (5, 6) (SpoilPile (MaterialId 3) (1.0, 1.0, 1.0, 1.0))
    , pgsBuildings    = BuildingSnapshot
        { bsnInstances = HM.fromList [ (BuildingId 1, richBuilding)
                                     , (BuildingId 2, richBuilding2) ]
        , bsnNextId = 3 }
    , pgsUnits        = UnitSnapshot
        { usnInstances = HM.singleton (UnitId 1) richUnit, usnNextId = 2 }
    , pgsUnitSimStates = HM.singleton (UnitId 1) richSimState
      -- #1854: both maps are keyed by flora INSTANCE now, and the
      -- fixture uses one id from each namespace so the disjointness and
      -- both wire encodings ride the representative session.
    , pgsFloraHarvests = HM.singleton
          (generatedFloraInstanceId "page1" 15 16 "probe_berry" 0) 1234.5
    , pgsChopDesignations = HM.singleton
          (plantedFloraInstanceId 2) (ChopDesignation 0 7 8)
      -- #1854: the two deferred legacy-migration maps and the planted
      -- allocator, populated so a round trip proves an unresolved
      -- pre-identity entry survives repeated save/load rather than being
      -- silently dropped.
    , pgsPendingChopMigration = HM.singleton (21, 22) (ChopDesignation 4 21 22)
    , pgsPendingFloraHarvests = HM.singleton (23, 24) 77.5
    , pgsPlantedFloraCursor = 3
    , pgsCraftBills   = richBills
    , pgsTransferOrders = richTransferOrders
    , pgsPowerNodes   = richNodes
    , pgsTillDesignations = HM.singleton (9, 10) (TillDesignation 0)
      -- #2243: a captured page names its species. The edit log's own
      -- planted crop is 'richEdits' below; these two are the other two
      -- durable reference sites, all three naming the same species so a
      -- round trip that mixed them up is visible.
    , pgsCropPlots    = HM.singleton (11, 12)
                            (CropPlot (FloraByName "wheat") 5 0.9)
    , pgsPlantDesignations = HM.singleton (13, 14)
                            (PlantDesignation 0 (FloraByName "wheat"))
    , pgsContainerKnowledge = richKnowledge
    , pgsIdentity     = Just (WorldIdentity "Aldermoor Deep"
                                  (Just "the deep home")
                                  (Just richProvenance)
                                  (Just richWorldEtymology))
    }

-- | The rich page's own etymology source (#1104): the expression its
--   name was rendered from, plus the language that rendered it. An
--   @Of@ with an explicit PLURAL is chosen deliberately — it is the
--   only form carrying a 'GramNumber', so a carrier that dropped or
--   defaulted the number would show up here rather than survive as a
--   silently-singular round trip.
richWorldEtymology ∷ EtymologySource
richWorldEtymology = EtymologySource
    { esExpr     = Of (ConceptId "DEEP") Plural (ConceptId "HOME")
    , esLanguage = richProvenance
    }

-- | The rich page's language provenance (#1092): a GENERATED identity,
--   so this session's round trip carries a NON-absent provenance. An
--   all-@Nothing@ session could not tell a correctly-persisted
--   provenance from a dropped one. The seed is above @2^63-1@
--   deliberately — a carrier that narrowed the range would show up here
--   rather than in a value that happens to fit in 63 bits.
richProvenance ∷ LanguageProvenance
richProvenance = LanguageProvenance
    { lpSeed = LangSeed 0x8FEEDFACECAFEB0B, lpVersion = GeneratorVersion 1 }

-- | The rich page's placed locations (#1101): one named in the page's
--   own language WITH a gloss, and one 'ldLabel' fallback with none —
--   so the round trip proves the gloss is carried when present AND left
--   absent when it is not, rather than defaulted either way. #1104's
--   etymology source rides the same split, on the same two instances,
--   for the same reason.
richLocationInstances ∷ LocationInstances
richLocationInstances = LocationInstances
    { lisNextId        = 3
    , lisById          = HM.fromList
        [ (LocationInstanceId 1, LocationInstance
            { liId              = LocationInstanceId 1
            , liDefId           = "ruin_small"
            , liChunk           = ChunkCoord 2 3
            , liAnchor          = (80, 112)
            , liBounds          = AbsBounds 78 110 82 114
            , liDisplayName     = "Vashenkoro"
            , liGloss           = Just "Ashen Keep"
            , liEtymology       = Just EtymologySource
                { esExpr     = Modifier (ConceptId "ASH") (ConceptId "KEEP")
                , esLanguage = richProvenance }
            , liLifecycle       = LifecycleDiscovered
            , liContentsSpawned = True
            , liEncounter       = Nothing
            -- #917: two obligations in DIFFERENT states, so a round
            -- trip that collapsed the list, dropped the optional
            -- physical id, or lost one latch cannot pass — slot 1
            -- spawned and taken, slot 2 spawned and still lying there.
            , liSignificant =
                [ LocationSignificantItem
                    { lsiSlot        = 1
                    , lsiItemDefName = "processing_unit"
                    , lsiInstanceId  = Just 8801
                    , lsiTaken       = True }
                , LocationSignificantItem
                    { lsiSlot        = 2
                    , lsiItemDefName = "processing_unit"
                    , lsiInstanceId  = Just 8802
                    , lsiTaken       = False }
                ]
            , liClearEventEmitted = False
            })
        , (LocationInstanceId 2, LocationInstance
            { liId              = LocationInstanceId 2
            , liDefId           = "ruin_small"
            , liChunk           = ChunkCoord 5 5
            , liAnchor          = (176, 176)
            , liBounds          = AbsBounds 174 174 178 178
            , liDisplayName     = "Small Ruin"
            , liGloss           = Nothing
            , liEtymology       = Nothing
            , liLifecycle       = LifecycleUnknown
            , liContentsSpawned = False
            , liEncounter       = Nothing
            -- The other side of the split: an obligation whose item has
            -- not been spawned yet, so its physical id is ABSENT — the
            -- state that must survive as absence rather than as a zero.
            , liSignificant =
                [ LocationSignificantItem
                    { lsiSlot        = 1
                    , lsiItemDefName = "processing_unit"
                    , lsiInstanceId  = Nothing
                    , lsiTaken       = False }
                ]
            , liClearEventEmitted = False
            })
        ]
    , lisPendingLegacy = Nothing
    }

-- | The rich page's river names (#1102): two rivers named in the page's
--   own language, each with its English gloss, keyed by 'GeoFeatureId'.
--   Two rather than one so a round trip that collapsed the table to a
--   single entry — or lost the keying — could not pass.
richRiverNames ∷ RiverNames
richRiverNames = RiverNames $ HM.fromList
    [ (GeoFeatureId 3, RiverName "Vashendral" (Just "Ashen River")
        (Just EtymologySource
            { esExpr     = Modifier (ConceptId "ASH") (ConceptId "RIVER")
            , esLanguage = richProvenance }))
    -- The second river deliberately carries NO source (#1104), so the
    -- round trip proves absence survives beside presence in the same
    -- table rather than being defaulted one way for every entry.
    , (GeoFeatureId 11, RiverName "Koromvash" (Just "Iron Ford") Nothing)
    ]

-- | A second, minimal page -- proves multi-page independence (a stable
--   identity + distinct per-page camera/gen-params, requirement 4),
--   and (#1092) the CUSTOM-name case beside the rich page's generated
--   one: a player-entered name with NO language, which must come back
--   with its provenance still absent rather than acquiring an inferred
--   one. Built through 'mkWorldIdentity' itself, so the case really is
--   the production custom-name path.
minimalPage2 ∷ PageSnapshot
minimalPage2 = PageSnapshot
    { pgsPageId       = page2
    , pgsGeneratedId  = Just (fixtureGeneratedWorldIdForPage page2)
    , pgsGenParams    = canon defaultWorldGenParams { wgpSeed = 99 }
    , pgsCameraX      = 0, pgsCameraY = 0
    , pgsTimeHour     = 0, pgsTimeMinute = 0
    , pgsDateYear     = 1, pgsDateMonth = 1, pgsDateDay = 1
    , pgsMapMode      = ZMDefault
    , pgsEdits        = emptyWorldEdits
    , pgsMineDesignations      = HM.empty
    , pgsConstructDesignations = HM.empty
    , pgsConstructNextAttempt = firstConstructAttemptId
    , pgsGroundItems  = GroundItems 0 HM.empty
    , pgsSpoilPiles   = emptySpoilPiles
      -- bsnNextId mirrors the session-global building allocator
      -- (sgNextBuildingId), which every page's slice is rewritten to on
      -- assembly -- see 'World.Save.Snapshot.Adapter'.
    , pgsBuildings    = BuildingSnapshot { bsnInstances = HM.empty, bsnNextId = 3 }
    , pgsUnits        = UnitSnapshot { usnInstances = HM.empty, usnNextId = 2 }
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
    , pgsIdentity     = customIdentity
    }

customIdentity ∷ Maybe WorldIdentity
customIdentity = mkWorldIdentity (Just "Player's Own Name") Nothing

pageIdentityOf ∷ SessionSnapshot → WorldPageId → Maybe WorldIdentity
pageIdentityOf snap pid = pgsIdentity =≪ HM.lookup pid (snapPages snap)

identityLanguage ∷ SessionSnapshot → WorldPageId → Maybe LanguageProvenance
identityLanguage snap pid = wiLanguage =≪ pageIdentityOf snap pid

-- | #1246: one page's restored transfer-order store.
ordersOf ∷ SessionSnapshot → WorldPageId → TransferOrders
ordersOf snap pid =
    maybe emptyTransferOrders pgsTransferOrders (HM.lookup pid (snapPages snap))

endpointsOf ∷ TransferBatch → (TransferEndpoint, TransferEndpoint)
endpointsOf b = (tbSource b, tbDestination b)

riverNamesOf ∷ SessionSnapshot → WorldPageId → HM.HashMap GeoFeatureId RiverName
riverNamesOf snap pid =
    maybe HM.empty (rvnById ∘ wgpRiverNames ∘ pgsGenParams)
          (HM.lookup pid (snapPages snap))

richGlobals ∷ SessionGlobals
richGlobals = SessionGlobals
    { sgGameTime       = 50000.5
    , sgTexPalette     = TexPalette
        { tpPathToId = HM.singleton "structures/test_wall.png" 1
        , tpIdToPath = HM.singleton 1 "structures/test_wall.png"
        , tpNextId   = 2 }
    -- Above every item id this session carries, the #917
    -- significant obligations (8801/8802) included: a bound
    -- obligation id at or above the cursor is one the monotonic
    -- allocator could never have minted, and
    -- 'significantProvenanceErrors' hard-fails it.
    , sgNextItemId     = 9000
    , sgNextBuildingId = 3
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

-- | Synthetic, opaque Lua component payloads -- standing in for the
--   REAL @scripts/lib/data_codec.lua@-encoded bytes a live Lua VM would
--   produce (this module is pure, no HsLua/engine). The envelope layer
--   never interprets a Lua component's internal bytes at all (that's
--   entirely the registered Lua module's own `decode`/`apply`), so an
--   opaque marker string is exactly as meaningful a test of "does the
--   envelope carry a named Lua component through encode/decode
--   byte-identically" as real canonical bytes would be -- proving the
--   ENVELOPE half of Lua persistence here, complementing the REAL,
--   live-engine `unitAi.getState`/`building.getSpawnRemaining`
--   round-trip checks in `tools/persistence_contract_probe.py`/
--   `_sweep.py` (which a pure, engine-less module cannot perform).
syntheticLuaComponents ∷ [LuaComponentSpec]
syntheticLuaComponents =
    [ LuaComponentSpec { lcsId = "unit_ai", lcsVersion = 3
                       , lcsRequired = True
                       , lcsPayload = "synthetic-unit_ai-payload" }
    , LuaComponentSpec { lcsId = "building_spawn", lcsVersion = 3
                       , lcsRequired = True
                       , lcsPayload = "synthetic-building_spawn-payload" }
    ]

-- | The encoding-pin fixture (issue #1103), deliberately separate from
--   'syntheticLuaComponents': every field differs between the two specs
--   (versions 4 vs 2, required True vs False, distinct payloads) and
--   they are listed in NON-canonical id order, so the manifest
--   assertions below genuinely pin which value each envelope descriptor
--   field received and what order the codec laid them out in — a fixture
--   whose entries agreed on a field could not.
pinnedLuaComponents ∷ [LuaComponentSpec]
pinnedLuaComponents =
    [ LuaComponentSpec { lcsId = "unit_ai", lcsVersion = 4
                       , lcsRequired = True
                       , lcsPayload = "pinned-unit_ai-payload" }
    , LuaComponentSpec { lcsId = "building_spawn", lcsVersion = 2
                       , lcsRequired = False
                       , lcsPayload = "pinned-building_spawn-bytes" }
    ]

-- | The reserved @lua.@ namespace "World.Save.Envelope" prefixes a bare
--   registry id into on the way to the manifest.
luaCid ∷ Text → ComponentId
luaCid name = ComponentId ("lua." <> name)

-- | The real Lua persistence registry's module names (mirrors
--   'save_compat_audit.GHCI_DUMP_SUMMARY_TEMPLATE'/
--   'save_compat_migration_probe.py''s identical @luaNames@) -- decode
--   must be told which component ids are known/required the same way
--   encode declared them, or an unrecognized-but-required component id
--   fails decode outright (the same "unknown required component" gate
--   requirement 11 exercises deliberately elsewhere).
luaNames ∷ HS.HashSet Text
luaNames = HS.fromList ["unit_ai", "building_spawn"]

-- | Every durable flora-species reference one decoded page carries,
--   labelled by which of the three sites it came from and sorted so the
--   comparison is order-independent (#2243). Reading the three sites
--   through ONE accessor is what makes a site that lost its reference
--   visible as an absent row rather than as a silently shorter list.
speciesRefsOf ∷ SessionSnapshot → WorldPageId → [(Text, FloraRef)]
speciesRefsOf snap pid = case HM.lookup pid (snapPages snap) of
    Nothing → []
    Just p  → L.sortOn (fmap renderFloraRef) $
        [ ("edit log", ref)
        | es ← HM.elems (pgsEdits p)
        , WePlaceFloraRef _ _ ref _ _ _ ← es ]
        ⧺ [ ("crop plot", cpSpecies cp) | cp ← HM.elems (pgsCropPlots p) ]
        ⧺ [ ("plant designation", ptCrop pd)
          | pd ← HM.elems (pgsPlantDesignations p) ]

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
           \EVERY persistent field via SessionSnapshot's derived Eq, \
           \PLUS every lua.<module> component payload byte-for-byte" $ do
            let req = SaveRequestMeta { srmSlotName = "contract_test", srmTimestamp = "ts", srmAutosave = False }
                meta = snapshotSaveMetadata req representativeSnapshot
                encoded = encodeSessionSnapshot meta representativeSnapshot
                              syntheticLuaComponents
            case decodeSessionEnvelope luaNames luaNames encoded of
                Left err → expectationFailure (show err)
                Right (_meta, snap, luaComponents, isMigrated) → do
                    snap `shouldBe` representativeSnapshot
                    isMigrated `shouldBe` False
                    luaComponents `shouldMatchList` syntheticLuaComponents
                    -- Stated explicitly as well as through the derived
                    -- Eq above (#1092): the generated page's language
                    -- provenance comes back intact, while the
                    -- custom-named page's stays absent. Naming both
                    -- here keeps the two cases legible if the
                    -- fixtures ever drift.
                    identityLanguage snap page1
                        `shouldBe` Just richProvenance
                    identityLanguage snap page2 `shouldBe` Nothing
                    -- #1102, stated explicitly for the same reason:
                    -- the generated page's river names AND glosses come
                    -- back keyed by the same feature ids, while the
                    -- custom-named page's table stays empty rather than
                    -- acquiring inferred names.
                    riverNamesOf snap page1
                        `shouldBe` rvnById richRiverNames
                    riverNamesOf snap page2 `shouldBe` HM.empty
                    (wiName <$> pageIdentityOf snap page2)
                        `shouldBe` (wiName <$> customIdentity)
                    -- #1246, stated explicitly for the same reason as
                    -- the two above: the derived Eq already covers the
                    -- order store, but a positional read of what came
                    -- back is what makes a dropped state, a reordered
                    -- entry list, or a flattened endpoint pair legible
                    -- when it breaks.
                    ordersOf snap page1 `shouldBe` richTransferOrders
                    ordersOf snap page2 `shouldBe` emptyTransferOrders
                    map troId (transferOrderList (ordersOf snap page1))
                        `shouldBe` [TransferOrderId 1, TransferOrderId 2]
                    trosNextId (ordersOf snap page1) `shouldBe` 3
                    -- All six lifecycle shapes survive, in request
                    -- order, with TransferFailed's optional cause both
                    -- present and absent.
                    concatMap (map qtState ∘ tbEntries ∘ troBatch)
                              (transferOrderList (ordersOf snap page1))
                        `shouldBe`
                        [ TransferQueued, TransferInTransit
                        , TransferReadyToCommit, TransferCompleted
                        , TransferCancelled
                        , TransferFailed (requestFailure ReasonReceiverFull)
                        , TransferFailed (staleFailure ReasonInstanceMissing) ]
                    concatMap (map (tirInstanceId ∘ qtItem) ∘ tbEntries
                                   ∘ troBatch)
                              (transferOrderList (ordersOf snap page1))
                        `shouldBe` [950, 951, 960, 961, 990, 991, 980]
                    -- D-10: the building-to-building order keeps BOTH
                    -- endpoints and an acting unit that is neither.
                    map (endpointsOf ∘ troBatch)
                        (transferOrderList (ordersOf snap page1))
                        `shouldBe`
                        [ (EndpointUnit (UnitId 1)
                          , EndpointBuilding (BuildingId 1))
                        , (EndpointBuilding (BuildingId 1)
                          , EndpointBuilding (BuildingId 2)) ]
                    map troUnit (transferOrderList (ordersOf snap page1))
                        `shouldBe` [UnitId 1, UnitId 1]
                    -- #2243, stated explicitly for the same reason as
                    -- the four above: the derived Eq already covers all
                    -- three durable species references, but reading each
                    -- back BY NAME is what makes a codec that quietly
                    -- reverted to persisting a numeric ordinal — or one
                    -- that crossed a crop plot's species with a plant
                    -- designation's — legible when it breaks. All three
                    -- name the same species, so a swap shows up as the
                    -- WRONG SITE losing its reference, not as a
                    -- different name.
                    speciesRefsOf snap page1 `shouldBe`
                        [ ("crop plot", FloraByName "wheat")
                        , ("edit log", FloraByName "wheat")
                        , ("plant designation", FloraByName "wheat") ]
                    speciesRefsOf snap page2 `shouldBe` []

    describe "Lua component encoding pin (issue #1103)" $ do
        it "encodeSessionSnapshot puts each LuaComponentSpec's OWN id, \
           \version, required flag and payload on the wire, in the \
           \codec's canonical id order -- read back through the \
           \Codec-level envelope decoder rather than the symmetric \
           \encode/extractLuaComponents pair a round trip exercises, so \
           \a swap made consistently on both sides still fails here" $ do
            let req = SaveRequestMeta { srmSlotName  = "lua_pin_test"
                                      , srmTimestamp = "ts"
                                      , srmAutosave  = False }
                meta = snapshotSaveMetadata req representativeSnapshot
                encoded = encodeSessionSnapshot meta representativeSnapshot
                              pinnedLuaComponents
                knownIds = HS.insert metadataComponentId componentKnownIds
                    `HS.union`
                        HS.fromList (map (luaCid . lcsId) pinnedLuaComponents)
                requiredIds =
                    HS.insert metadataComponentId componentRequiredIds
            case decodeEnvelope defaultEnvelopeLimits currentEnvelopeVersion
                                knownIds requiredIds encoded of
                Left err → expectationFailure (show err)
                Right de → do
                    let isLua (ComponentId t) = "lua." `T.isPrefixOf` t
                        luaDescs = filter (isLua . cdId)
                                          (emComponents (deManifest de))
                    -- Canonical layout order is component-id ascending,
                    -- which reverses the input order above.
                    map cdId luaDescs `shouldBe`
                        [luaCid "building_spawn", luaCid "unit_ai"]
                    map cdVersion luaDescs `shouldBe` [2, 4]
                    map cdRequired luaDescs `shouldBe` [False, True]
                    map (\d → HM.lookup (cdId d) (dePayloads de)) luaDescs
                        `shouldBe` [ Just "pinned-building_spawn-bytes"
                                   , Just "pinned-unit_ai-payload" ]

    describe "repeated-cycle stability (pure, requirement 9)" $ do
        it "three successive encode -> decode -> re-encode cycles never \
           \drift -- no cycle accumulates ghost pages, duplicate \
           \entities, or allocator drift, INCLUDING the lua.<module> \
           \component payloads" $ do
            let req = SaveRequestMeta { srmSlotName = "cycle_test", srmTimestamp = "ts", srmAutosave = False }
                cycleOnce (snap, lua) =
                    let meta = snapshotSaveMetadata req snap
                        encoded = encodeSessionSnapshot meta snap lua
                    in case decodeSessionEnvelope luaNames luaNames encoded of
                        Left err → error (show err)
                        Right (_, snap', lua', _) → (snap', lua')
                gen1 = cycleOnce (representativeSnapshot, syntheticLuaComponents)
                gen2 = cycleOnce gen1
                gen3 = cycleOnce gen2
            forM_ [gen1, gen2, gen3] $ \(snap, lua) → do
                snap `shouldBe` representativeSnapshot
                lua `shouldMatchList` syntheticLuaComponents

    describe "reset/rebuild policy at the type level (requirement 6)" $ do
        it "the adapter fabricates the documented reset defaults for \
           \EVERY page in the representative (not just minimal) \
           \session -- DefaultTool and time scale 1, never a captured \
           \value" $ do
            let req = SaveRequestMeta { srmSlotName = "reset_test", srmTimestamp = "ts", srmAutosave = False }
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
