-- | Power-network connectivity + energy balance tests (#360), plus
--   #361's requires_power consumer folding and #590's combineConsumers
--   union. No engine/Lua needed — the connectivity + brownout math
--   (incl. consumer drain) is fully exercised here with synthetic
--   nodes/consumers. activeCraftConsumersOn itself (#590 — job-
--   dependent recipe drain) needs a real BuildingManager/RecipeManager/
--   CraftBills to exercise meaningfully; that's covered end-to-end by
--   tools/power_workshop_probe.py instead, matching how every other
--   Building/Craft-manager-shaped consumer here (consumersOn included)
--   is engine-probe-tested rather than hand-built in hspec.
module Test.Headless.Power.Network (spec) where

import UPrelude
import Test.Hspec
import Data.List (find)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Power.Types
import Power.Network
import Building.Types (BuildingId(..))
import Structure.Types (StructureSlot(..), emptyChunkStructures)
import World.Chunk.Types (ChunkCoord(..), ColumnTiles(..), LoadedChunk(..),
                           chunkSize)
import World.Edit.Apply (replayEdits)
import World.Edit.Types (WorldEdit(..), WorldEdits)
import World.Flora.Types (emptyFloraChunkData)
import World.Fluid.Types (emptyIceMap)
import World.Generate.Coordinates (canonicalTile)
import World.Tile.Types (WorldTileData(..))

panel, battery, battery2, farPanel, workshop, workshop2 ∷ BuildingId
panel     = BuildingId 1
battery   = BuildingId 2
battery2  = BuildingId 3
farPanel  = BuildingId 4
workshop  = BuildingId 5
workshop2 = BuildingId 6

-- | Seed a battery's stored charge directly (bypassing the tick), for
--   scenarios that start mid-charge/mid-discharge.
seedStored ∷ PowerNodeId → Float → PowerNodes → PowerNodes
seedStored nid wh nodes =
    nodes { pnsNodes = HM.adjust (\n → n { pnStoredWh = wh }) nid (pnsNodes nodes) }

noon, midnight, dawn, dusk ∷ Float
noon     = 0.5
midnight = 0.0
dawn     = 0.25
dusk     = 0.75

-- | World size (chunks) passed to every 'computeSnapshots'/'tickPowerNodes'
--   call in this file (#794 threaded per-source local phasing through
--   both). Circumference = 4 * 'World.Chunk.Types.chunkSize' (16) = 64
--   tiles — the #794-specific tests below build their scenarios around
--   that number directly; every OTHER (pre-#794) test's positions keep
--   their source nodes at u = gx - gy = 0, so their exact-value
--   assertions hold for ANY worldSize (local angle == global angle
--   exactly when u = 0), independent of this particular choice.
testWorldSize ∷ Int
testWorldSize = 4

-- Residency-independent wire topology (#1207) -------------------------
--
-- Wire is persistent: every placement/clear writes the loaded chunk's
-- structure overlay AND the per-chunk edit log, and chunks evict by
-- camera distance. So a page's wire set has to read the same whether or
-- not its chunks are resident. The fixtures below express EXACTLY that
-- pairing: one ordered edit log, viewed twice — once with its chunk
-- resident (built by the real 'replayEdits', the same function chunk
-- regeneration uses, so the overlay can't be hand-tuned into agreement)
-- and once with nothing resident at all.

wireTag, floorTag ∷ Word8
wireTag  = fromIntegral (fromEnum SWire)
floorTag = fromIntegral (fromEnum SFloor)

-- | A wire placement. @palette@ stands in for the texture/facemap ids
--   scripts/wire.lua bakes in per autotile variant — a recap re-places
--   the same tile with DIFFERENT ids, which must still be one wire tile.
setWire ∷ Int → (Int, Int) → WorldEdit
setWire palette (gx, gy) = WeSetStructure gx gy wireTag palette palette 0

clearWire ∷ (Int, Int) → WorldEdit
clearWire (gx, gy) = WeClearStructure gx gy wireTag

-- | One contiguous wire run STRADDLING a chunk boundary, so a partially-
--   resident page is testable: x = 13..15 lives in chunk (0,0), x =
--   16..18 in chunk (1,0), and the whole run is a single network.
homeChunk, farChunk ∷ ChunkCoord
homeChunk = ChunkCoord 0 0
farChunk  = ChunkCoord 1 0

homeRun, farRun ∷ [(Int, Int)]
homeRun = [(13, 5), (14, 5), (15, 5)]
farRun  = [(chunkSize, 5), (chunkSize + 1, 5), (chunkSize + 2, 5)]

-- | A blank generated chunk — what regeneration hands 'replayEdits'.
--   Columns are 20 cells deep so a slope/veg edit sharing the log lands
--   in range instead of no-oping for the wrong reason.
blankChunk ∷ ChunkCoord → LoadedChunk
blankChunk coord =
    let area = chunkSize * chunkSize
        col  = ColumnTiles { ctStartZ = 0
                           , ctMats   = VU.replicate 20 1
                           , ctSlopes = VU.replicate 20 0
                           , ctVeg    = VU.replicate 20 0
                           }
    in LoadedChunk
        { lcCoord = coord
        , lcTiles = V.replicate area col
        , lcSurfaceMap = VU.replicate area 19
        , lcTerrainSurfaceMap = VU.replicate area 19
        , lcFluidMap = V.replicate area Nothing
        , lcIceMap = emptyIceMap, lcFlora = emptyFloraChunkData
        , lcSideDeco = VU.empty, lcWaterTableMap = VU.empty
        , lcMagma = Nothing, lcStructures = emptyChunkStructures
        }

-- | The page with @resident@ chunks loaded (each one regenerated blank
--   and replayed, exactly as 'World.Thread.ChunkLoading' does) and every
--   other chunk in @log'@ evicted.
pageWith ∷ [ChunkCoord] → WorldEdits → (WorldTileData, WorldEdits)
pageWith resident log' =
    ( WorldTileData
        { wtdChunks = HM.fromList
            [ (coord, replayEdits log' (blankChunk coord)) | coord ← resident ]
        , wtdMaxChunks = 200 }
    , log' )

-- | The same wire set seen with @resident@ chunks loaded.
wireWith ∷ [ChunkCoord] → WorldEdits → HS.HashSet (Int, Int)
wireWith resident = uncurry pageWireTiles . pageWith resident

-- | Node membership for a wire set — the connectivity question
--   requirement 1 actually asks, not just the tile set behind it.
--   The source sits beside the run's near end (in the home chunk), the
--   battery beside its far end (in the far chunk), so they share a
--   network only while the whole run is visible to topology.
membershipFor ∷ HS.HashSet (Int, Int) → [HS.HashSet PowerNodeId]
membershipFor wire =
    let (n1, srcId) = addPowerNode panel PowerSource 400 emptyPowerNodes
        (n2, batId) = addPowerNode battery PowerStorage 5000 n1
        positions   = HM.fromList [ (srcId, (13, 4))
                                  , (batId, (chunkSize + 2, 4)) ]
    in map (HS.fromList . pnwNodeIds)
           (computeSnapshots testWorldSize noon HM.empty wire n2 positions HM.empty)

-- Cylindrical-seam fixtures (#2634) ------------------------------------
--
-- 'testWorldSize' = 4 chunks wraps u = gx - gy with a period of
-- 4 * 'chunkSize' = 64 tiles, so canonical chunk-u lives in [-2, 2) and
-- the seam falls between tile (31, 0) and its EASTERN neighbour, whose
-- canonical name is (0, 32) — a whole world away in raw coordinates.
-- Every seam coordinate below is DERIVED through 'canonicalTile' rather
-- than written out, so the fixtures state the physical layout (take one
-- cardinal step) and let the shared coordinate rules name the result;
-- a change to the wrap can't leave a hand-copied literal behind.

-- | A synthetic wire tile named in the CANONICAL storage frame, from
--   the raw coordinate that states where it physically sits.
--
--   Every wire set in this file goes through this, because that is the
--   frame 'pageWireTiles' hands the real topology: a size-4 world stores
--   chunk-u in [-2, 2), so a hand-written tile at u = 32 or beyond names
--   an ALIAS rather than the key production would use. NODE positions
--   are deliberately left raw — 'localSunAngle' reads u directly and an
--   alias names the same longitude, so the #794 fixtures keep saying
--   "one circumference apart" in the frame that makes that legible.
canonWire ∷ (Int, Int) → (Int, Int)
canonWire (gx, gy) = canonicalTile testWorldSize gx gy

-- | The canonical name of the tile one step EAST of @(gx, gy)@.
eastOf ∷ (Int, Int) → (Int, Int)
eastOf (gx, gy) = canonicalTile testWorldSize (gx + 1) gy

-- | The canonical name of the tile one step SOUTH of @(gx, gy)@.
southOf ∷ (Int, Int) → (Int, Int)
southOf (gx, gy) = canonicalTile testWorldSize gx (gy + 1)

-- | A world that does not wrap at all: 'canonicalTile' is the identity
--   here, so the seam frame degenerates to exactly the raw cardinal
--   offsets that shipped before #2634. Every seam-CROSSING claim below
--   is restated at this size, so the wrap — and not some incidental
--   coordinate arithmetic — is demonstrably what changed the answer.
nonWrappingWorldSize ∷ Int
nonWrappingWorldSize = 0

-- | The global sun angle at which @(gx, gy)@ reads LOCAL noon.
--
--   A seam fixture cannot put its source at u = 0 the way the
--   pre-#794 tests do — it has to stand beside the seam, at u ≈ ±32 —
--   so the global clock is solved for instead. Phasing the SOURCE
--   (never the battery, whose tile has no bearing on generation) keeps
--   every seam scenario at full output, and the away-from-seam control
--   is phased through this same helper for its own u, so the two
--   differ only in where they sit. All the values involved are exact
--   multiples of 1/64, so this is not an approximation.
globalAngleForLocalNoon ∷ (Int, Int) → Float
globalAngleForLocalNoon (gx, gy) =
    let circumference = fromIntegral (testWorldSize * chunkSize) ∷ Float
        raw = noon - fromIntegral (gx - gy) / circumference
    in raw - fromIntegral (floor raw ∷ Int)

-- | Stored charge to within a tolerance — generation rides on a cosine
--   of a solved-for angle, so an exact literal would be asserting the
--   float arithmetic rather than the connectivity.
shouldBeStored ∷ Maybe Float → Float → Expectation
shouldBeStored actual expected = case actual of
    Just got | abs (got - expected) < 1.0e-3 → pure ()
    _ → expectationFailure
            ("expected stored charge of about " <> show expected
              <> ", got " <> show actual)

spec ∷ Spec
spec = do
    describe "solarIntensity" $ do
        it "peaks at 1 at noon" $
            solarIntensity noon `shouldBe` 1.0

        it "is 0 at midnight" $
            solarIntensity midnight `shouldBe` 0.0

        it "is 0 at dawn and dusk" $ do
            solarIntensity dawn `shouldSatisfy` (< 1.0e-6)
            solarIntensity dusk `shouldSatisfy` (< 1.0e-6)

    describe "wireComponents" $ do
        it "an empty tile set has no components" $
            wireComponents testWorldSize HS.empty `shouldBe` []

        it "a straight run of wire is one component" $
            wireComponents testWorldSize (HS.fromList [(0,0), (1,0), (2,0)])
                `shouldBe` [HS.fromList [(0,0), (1,0), (2,0)]]

        it "two disjoint runs are two components" $
            length (wireComponents testWorldSize
                        (HS.fromList [(0,0), (1,0), (10,10), (11,10)]))
                `shouldBe` 2

        it "diagonal-only tiles do NOT connect (4-dir adjacency only)" $
            length (wireComponents testWorldSize (HS.fromList [(0,0), (1,1)]))
                `shouldBe` 2

    describe "computeSnapshots — connectivity" $ do
        it "a source and a battery joined by ONE wire tile share a network" $ do
            let (n1, srcId) = addPowerNode panel PowerSource 400 emptyPowerNodes
                (n2, batId) = addPowerNode battery PowerStorage 5000 n1
                positions   = HM.fromList [(srcId, (0, 0)), (batId, (2, 0))]
                wire        = HS.singleton (1, 0)
                nets        = computeSnapshots testWorldSize noon HM.empty wire n2 positions HM.empty
            case nets of
                [net] → HS.fromList (pnwNodeIds net) `shouldBe` HS.fromList [srcId, batId]
                _     → expectationFailure ("expected exactly one network, got " <> show nets)

        it "two nodes with no wire at all attach to no network" $ do
            let (n1, srcId) = addPowerNode panel PowerSource 400 emptyPowerNodes
                (n2, batId) = addPowerNode battery PowerStorage 5000 n1
                positions   = HM.fromList [(srcId, (0, 0)), (batId, (1, 0))]
                nets        = computeSnapshots testWorldSize noon HM.empty HS.empty n2 positions HM.empty
            nets `shouldBe` []
              -- adjacent tiles, but NO wire tile between them => not networked

        it "a node with no wire adjacent to it isn't networked, even if another node is" $ do
            let (n1, srcId) = addPowerNode panel PowerSource 400 emptyPowerNodes
                (n2, farId) = addPowerNode farPanel PowerSource 400 n1
                positions   = HM.fromList [(srcId, (0, 0)), (farId, (50, 50))]
                wire        = HS.singleton (1, 0)
                nets        = computeSnapshots testWorldSize noon HM.empty wire n2 positions HM.empty
            case nets of
                [net] → pnwNodeIds net `shouldBe` [srcId]
                _     → expectationFailure ("expected exactly one network, got " <> show nets)

        it "a node bridging two otherwise-disconnected wire stubs merges them into one network" $ do
            -- Two 1-tile wire stubs, (4,5) and (6,5), NOT adjacent to each
            -- other (2 apart in x) — wireComponents sees them as separate
            -- components. A battery at (5,5) is orthogonally adjacent to
            -- BOTH: it must bridge them into one network, not attach to
            -- each independently.
            let (n1, srcAId) = addPowerNode panel PowerSource 100 emptyPowerNodes
                (n2, srcBId) = addPowerNode farPanel PowerSource 100 n1
                (n3, batId)  = addPowerNode battery PowerStorage 5000 n2
                positions = HM.fromList [ (srcAId, (4, 4))
                                        , (batId,  (5, 5))
                                        , (srcBId, (6, 6)) ]
                wire = HS.fromList [(4, 5), (6, 5)]
                nets = computeSnapshots testWorldSize noon HM.empty wire n3 positions HM.empty
            case nets of
                [net] → HS.fromList (pnwNodeIds net)
                            `shouldBe` HS.fromList [srcAId, batId, srcBId]
                _     → expectationFailure
                            ("expected exactly one merged network, got " <> show nets)

        it "the bridged network's battery is charged by BOTH sources, not overwritten" $ do
            let (n1, srcAId) = addPowerNode panel PowerSource 100 emptyPowerNodes
                (n2, srcBId) = addPowerNode farPanel PowerSource 100 n1
                (n3, batId)  = addPowerNode battery PowerStorage 5000 n2
                positions = HM.fromList [ (srcAId, (4, 4))
                                        , (batId,  (5, 5))
                                        , (srcBId, (6, 6)) ]
                wire = HS.fromList [(4, 5), (6, 5)]
                -- noon (full intensity), 1 hour: 100W + 100W = 200 Wh into
                -- the one shared battery. A regression here (the battery
                -- ending up at 100, or split across two spurious networks)
                -- would mean one source's contribution was silently lost.
                ticked = tickPowerNodes testWorldSize noon HM.empty 3600 wire positions HM.empty n3
            pnStoredWh ⊚ lookupPowerNode batId ticked `shouldBe` Just 200

    describe "computeSnapshots — instantaneous status" $ do
        it "generation covering drain reads Powered even with no storage" $ do
            let (n1, srcId) = addPowerNode panel PowerSource 400 emptyPowerNodes
                positions   = HM.singleton srcId (0, 0)
                wire        = HS.singleton (1, 0)
                drain       = HM.singleton srcId 100
                nets        = computeSnapshots testWorldSize noon drain wire n1 positions HM.empty
            map pnwStatus nets `shouldBe` [Powered]

        it "drain exceeding generation with no storage reads Brownout" $ do
            let (n1, srcId) = addPowerNode panel PowerSource 400 emptyPowerNodes
                positions   = HM.singleton srcId (0, 0)
                wire        = HS.singleton (1, 0)
                drain       = HM.singleton srcId 500
                nets        = computeSnapshots testWorldSize noon drain wire n1 positions HM.empty
            map pnwStatus nets `shouldBe` [Brownout]

        it "a deficit is Powered as long as the battery still holds charge" $ do
            let (n1, srcId) = addPowerNode panel PowerSource 100 emptyPowerNodes
                (n2, batId) = addPowerNode battery PowerStorage 5000 n1
                seeded      = seedStored batId 10 n2
                positions   = HM.fromList [(srcId, (0, 0)), (batId, (2, 0))]
                wire        = HS.singleton (1, 0)
                drain       = HM.singleton srcId 1000
                nets        = computeSnapshots testWorldSize midnight drain wire seeded positions HM.empty
            map pnwStatus nets `shouldBe` [Powered]

    describe "tickPowerNodes — charging" $ do
        it "a full hour at noon charges the battery by peakWatts Wh" $ do
            let (n1, srcId) = addPowerNode panel PowerSource 400 emptyPowerNodes
                (n2, batId) = addPowerNode battery PowerStorage 5000 n1
                positions   = HM.fromList [(srcId, (0, 0)), (batId, (2, 0))]
                wire        = HS.singleton (1, 0)
                n3          = tickPowerNodes testWorldSize noon HM.empty 3600 wire positions HM.empty n2
            pnStoredWh ⊚ lookupPowerNode batId n3 `shouldBe` Just 400

        it "charging never exceeds capacity — surplus is curtailed" $ do
            let (n1, srcId) = addPowerNode panel PowerSource 400 emptyPowerNodes
                (n2, batId) = addPowerNode battery PowerStorage 500 n1
                seeded      = seedStored batId 400 n2
                positions   = HM.fromList [(srcId, (0, 0)), (batId, (2, 0))]
                wire        = HS.singleton (1, 0)
                n3          = tickPowerNodes testWorldSize noon HM.empty 3600 wire positions HM.empty seeded
            pnStoredWh ⊚ lookupPowerNode batId n3 `shouldBe` Just 500

        it "no time passing (dtGameSeconds <= 0) is a no-op" $ do
            let (n1, srcId) = addPowerNode panel PowerSource 400 emptyPowerNodes
                (n2, batId) = addPowerNode battery PowerStorage 5000 n1
                positions   = HM.fromList [(srcId, (0, 0)), (batId, (2, 0))]
                wire        = HS.singleton (1, 0)
                n3          = tickPowerNodes testWorldSize noon HM.empty 0 wire positions HM.empty n2
            pnStoredWh ⊚ lookupPowerNode batId n3 `shouldBe` Just 0

    describe "tickPowerNodes — discharging + brownout" $ do
        it "a synthetic drain at night discharges the battery" $ do
            let (n1, srcId) = addPowerNode panel PowerSource 400 emptyPowerNodes
                (n2, batId) = addPowerNode battery PowerStorage 5000 n1
                seeded      = seedStored batId 50 n2
                positions   = HM.fromList [(srcId, (0, 0)), (batId, (2, 0))]
                wire        = HS.singleton (1, 0)
                drain       = HM.singleton srcId 100
                -- midnight: 0 generation, 100W drain, 1h => -100Wh from
                -- 50Wh stored => clamped to 0
                n3          = tickPowerNodes testWorldSize midnight drain 3600 wire positions HM.empty seeded
            pnStoredWh ⊚ lookupPowerNode batId n3 `shouldBe` Just 0

        it "depleting the battery reports Brownout on the next query" $ do
            let (n1, srcId) = addPowerNode panel PowerSource 400 emptyPowerNodes
                (n2, batId) = addPowerNode battery PowerStorage 5000 n1
                positions   = HM.fromList [(srcId, (0, 0)), (batId, (2, 0))]
                wire        = HS.singleton (1, 0)
                drain       = HM.singleton srcId 100
                nets        = computeSnapshots testWorldSize midnight drain wire n2 positions HM.empty
            map pnwStatus nets `shouldBe` [Brownout]

        it "two batteries discharge proportionally to their own charge" $ do
            let (n1, srcId)  = addPowerNode panel PowerSource 0 emptyPowerNodes
                (n2, batId1) = addPowerNode battery PowerStorage 5000 n1
                (n3, batId2) = addPowerNode battery2 PowerStorage 5000 n2
                seeded = seedStored batId1 100 (seedStored batId2 200 n3)
                positions = HM.fromList [ (srcId, (0, 0))
                                        , (batId1, (2, 0)), (batId2, (2, 1)) ]
                wire  = HS.fromList [(1, 0), (1, 1)]
                drain = HM.singleton srcId 300
                -- 300W drain for 1h = 300Wh demand, split 1:2 by current
                -- charge (100 vs 200) => battery1 loses 100 (empty),
                -- battery2 loses 200 (empty) — both exactly drained.
                ticked = tickPowerNodes testWorldSize midnight drain 3600 wire positions HM.empty seeded
            pnStoredWh ⊚ lookupPowerNode batId1 ticked `shouldBe` Just 0
            pnStoredWh ⊚ lookupPowerNode batId2 ticked `shouldBe` Just 0

    describe "consumer drain (#361 — requires_power buildings)" $ do
        it "a consumer touching the same wire as a source folds its drain into drainW" $ do
            let (n1, srcId) = addPowerNode panel PowerSource 400 emptyPowerNodes
                positions   = HM.singleton srcId (0, 0)
                wire        = HS.singleton (1, 0)
                consumers   = HM.singleton workshop ((2, 0), 150)
                nets        = computeSnapshots testWorldSize noon HM.empty wire n1 positions consumers
            case nets of
                [net] → do
                    pnwDrainW net `shouldBe` 150
                    pnwConsumerIds net `shouldBe` [workshop]
                _     → expectationFailure ("expected exactly one network, got " <> show nets)

        it "two consumers on one network sum their drain" $ do
            let (n1, srcId) = addPowerNode panel PowerSource 400 emptyPowerNodes
                positions   = HM.singleton srcId (0, 0)
                wire        = HS.singleton (1, 0)
                consumers   = HM.fromList [ (workshop,  ((2, 0), 150))
                                          , (workshop2, ((1, 1), 75)) ]
                nets        = computeSnapshots testWorldSize noon HM.empty wire n1 positions consumers
            map pnwDrainW nets `shouldBe` [225]

        it "a consumer not adjacent to any wire is dropped — no network, no drain" $ do
            let (n1, srcId) = addPowerNode panel PowerSource 400 emptyPowerNodes
                positions   = HM.singleton srcId (0, 0)
                wire        = HS.singleton (1, 0)
                consumers   = HM.singleton workshop ((50, 50), 150)
                nets        = computeSnapshots testWorldSize noon HM.empty wire n1 positions consumers
            case nets of
                [net] → do
                    pnwDrainW net `shouldBe` 0
                    pnwConsumerIds net `shouldBe` []
                _     → expectationFailure ("expected exactly one network, got " <> show nets)

        it "a consumer with no node network anywhere produces no snapshot at all" $ do
            -- No PowerNode exists on the whole page — groupByComponent has
            -- nothing to build a network around, so the consumer's drain
            -- goes uncounted. Vacuously correct: with no source/storage
            -- ever, the consumer could never be Powered anyway.
            let wire      = HS.singleton (1, 0)
                consumers = HM.singleton workshop ((0, 0), 150)
                nets      = computeSnapshots testWorldSize noon HM.empty wire emptyPowerNodes HM.empty consumers
            nets `shouldBe` []

        it "consumer drain actually discharges a battery over time (day/night balance)" $ do
            let (n1, batId) = addPowerNode battery PowerStorage 5000 emptyPowerNodes
                seeded      = seedStored batId 500 n1
                positions   = HM.singleton batId (0, 0)
                wire        = HS.singleton (1, 0)
                consumers   = HM.singleton workshop ((2, 0), 100)
                -- midnight: 0 generation, 100W drain, 1h => -100Wh
                ticked = tickPowerNodes testWorldSize midnight HM.empty 3600 wire positions consumers seeded
            pnStoredWh ⊚ lookupPowerNode batId ticked `shouldBe` Just 400

        it "a consumer-only network with an empty battery reads Brownout" $ do
            let (n1, batId) = addPowerNode battery PowerStorage 5000 emptyPowerNodes
                positions   = HM.singleton batId (0, 0)
                wire        = HS.singleton (1, 0)
                consumers   = HM.singleton workshop ((2, 0), 100)
                nets        = computeSnapshots testWorldSize midnight HM.empty wire n1 positions consumers
            map pnwStatus nets `shouldBe` [Brownout]

        it "a bridging node still lets a consumer on either stub join the SAME network" $ do
            -- Mirrors the node-bridging connectivity test above, but with
            -- a consumer sitting on one of the two stubs the panel joins.
            let (n1, srcId) = addPowerNode panel PowerSource 400 emptyPowerNodes
                positions   = HM.singleton srcId (5, 5)
                wire        = HS.fromList [(4, 5), (6, 5)]
                -- workshop at (7,5) is adjacent to the (6,5) stub only —
                -- the panel at (5,5) bridges (4,5) and (6,5) into one
                -- network, so the workshop's drain must land in it.
                consumers   = HM.singleton workshop ((7, 5), 150)
                nets        = computeSnapshots testWorldSize noon HM.empty wire n1 positions consumers
            case nets of
                [net] → do
                    pnwConsumerIds net `shouldBe` [workshop]
                    pnwDrainW net `shouldBe` 150
                _     → expectationFailure ("expected exactly one network, got " <> show nets)

    describe "combineConsumers (#590 — always-on + active-job union)" $ do
        it "sums drain for a building present on both sides" $ do
            let always = HM.singleton workshop ((2, 0), 50)
                active  = HM.singleton workshop ((2, 0), 150)
            combineConsumers always active
                `shouldBe` HM.singleton workshop ((2, 0), 200)

        it "keeps entries that only appear on one side untouched" $ do
            let always = HM.singleton workshop ((2, 0), 50)
                active  = HM.singleton workshop2 ((1, 1), 150)
            combineConsumers always active
                `shouldBe` HM.fromList [ (workshop,  ((2, 0), 50))
                                       , (workshop2, ((1, 1), 150)) ]

        it "is a no-op when one side is empty" $ do
            let active = HM.singleton workshop ((2, 0), 150)
            combineConsumers HM.empty active `shouldBe` active
            combineConsumers active HM.empty `shouldBe` active

    describe "pageWireTiles — residency independence (#1207)" $ do
        let fullRun  = homeRun ⧺ farRun
            placeAll = HM.fromList
                [ (homeChunk, map (setWire 0) homeRun)
                , (farChunk,  map (setWire 0) farRun) ]
            bothChunks = [homeChunk, farChunk]

        it "a resident chunk contributes EXACTLY its lcStructures wire set" $ do
            -- Requirement 4: no second authority. With everything
            -- resident the page view must not add, drop, or relabel a
            -- single tile relative to the overlay topology used to read.
            let (td, edits) = pageWith bothChunks placeAll
            pageWireTiles td edits `shouldBe` wireTilesOn td
            wireTilesOn td `shouldBe` HS.fromList fullRun

        it "an evicted page reports the same wire set as a fully resident one" $
            wireWith [] placeAll `shouldBe` wireWith bothChunks placeAll

        it "a partially resident page reports the same wire set as either extreme" $ do
            wireWith [homeChunk] placeAll `shouldBe` HS.fromList fullRun
            wireWith [farChunk]  placeAll `shouldBe` HS.fromList fullRun

        it "network MEMBERSHIP is identical loaded, half-loaded and evicted" $ do
            let loadedNets = membershipFor (wireWith bothChunks placeAll)
            length loadedNets `shouldBe` 1
            membershipFor (wireWith [homeChunk] placeAll) `shouldBe` loadedNets
            membershipFor (wireWith [farChunk]  placeAll) `shouldBe` loadedNets
            membershipFor (wireWith []          placeAll) `shouldBe` loadedNets

        it "repeated sets of one tile (autotile recapping) stay one wire tile" $ do
            -- scripts/wire.lua re-places a neighbour to update its
            -- connection variant, so the log carries several sets of the
            -- same tile with DIFFERENT palette ids.
            let recapped = HM.fromList
                    [ (homeChunk, map (setWire 0) homeRun
                                    ⧺ map (setWire 7) homeRun
                                    ⧺ [setWire 9 (14, 5)]) ]
            wireWith []          recapped `shouldBe` HS.fromList homeRun
            wireWith [homeChunk] recapped `shouldBe` HS.fromList homeRun

        it "a cleared tile stays cleared once its chunk evicts" $ do
            -- Requirement 3: the ordered set/clear semantics decide, so
            -- the earlier set can never resurrect connectivity.
            let cleared = HM.fromList
                    [ (homeChunk, map (setWire 0) homeRun ⧺ [clearWire (14, 5)]) ]
                expected = HS.fromList [(13, 5), (15, 5)]
            wireWith []          cleared `shouldBe` expected
            wireWith [homeChunk] cleared `shouldBe` expected

        it "a set AFTER a clear puts the tile back, evicted or resident" $ do
            let reset = HM.fromList
                    [ (homeChunk, map (setWire 0) homeRun
                                    ⧺ [clearWire (14, 5), setWire 3 (14, 5)]) ]
            wireWith []          reset `shouldBe` HS.fromList homeRun
            wireWith [homeChunk] reset `shouldBe` HS.fromList homeRun

        it "clearing the whole run disconnects the network while evicted" $ do
            let wiped = HM.fromList
                    [ (homeChunk, map (setWire 0) homeRun ⧺ map clearWire homeRun)
                    , (farChunk,  map (setWire 0) farRun  ⧺ map clearWire farRun) ]
            wireWith [] wiped `shouldBe` HS.empty
            membershipFor (wireWith [] wiped) `shouldBe` []

        it "a log stripped of its structure edits (clearAll) yields no wire" $ do
            -- structure.clearAll empties every loaded overlay AND strips
            -- WeSetStructure/WeClearStructure from the log. What's left
            -- must leave nothing derivable — connectivity cannot come
            -- back from the surrounding terrain edits.
            let strippedLog = HM.fromList
                    [ (homeChunk, [WeSetSlope 13 5 19 1, WeSetVeg 14 5 19 3]) ]
            wireWith []          strippedLog `shouldBe` HS.empty
            wireWith [homeChunk] strippedLog `shouldBe` HS.empty

        it "non-wire structure pieces never read as wire, on either side" $ do
            -- A floor laid over the exact same tiles is a different slot
            -- tag; the filter is by SLOT, not by tile.
            let floors = HM.fromList
                    [ (homeChunk, [ WeSetStructure gx gy floorTag 0 0 0
                                  | (gx, gy) ← homeRun ]) ]
            wireWith []          floors `shouldBe` HS.empty
            wireWith [homeChunk] floors `shouldBe` HS.empty

        it "an unrelated chunk's edits contribute nothing to another's wire" $ do
            -- A page whose only evicted history is non-structure edits
            -- adds nothing to the resident chunk's wire.
            let mixed = HM.fromList
                    [ (homeChunk, map (setWire 0) homeRun)
                    , (farChunk,  [WeSetSlope (chunkSize + 1) 5 19 2]) ]
            wireWith [homeChunk] mixed `shouldBe` HS.fromList homeRun
            wireWith []          mixed `shouldBe` HS.fromList homeRun

    describe "local solar phasing (#794)" $ do
        it "two otherwise-identical panels read local noon vs. local midnight under the same global clock" $ do
            -- testWorldSize's circumference is 64 tiles; noonId sits
            -- exactly half of it away (u=32) from midnightId (u=0), so
            -- under a global clock reading midnight, noonId's OWN local
            -- angle is noon while midnightId's stays at the global angle.
            let (n1, noonId)     = addPowerNode panel    PowerSource 400 emptyPowerNodes
                (n2, midnightId) = addPowerNode farPanel PowerSource 400 n1
                positions = HM.fromList [ (noonId, (32, 0)), (midnightId, (0, 0)) ]
                -- Two disjoint single-node networks, in the storage
                -- frame: (33, 0) is a u-alias at this world size and is
                -- stored as (1, 32).
                wire      = HS.fromList (map canonWire [(33, 0), (1, 0)])
                nets = computeSnapshots testWorldSize midnight HM.empty wire n2 positions HM.empty
            case ( find (elem noonId . pnwNodeIds) nets
                 , find (elem midnightId . pnwNodeIds) nets ) of
                (Just noonNet, Just midnightNet) → do
                    pnwGenerationW noonNet `shouldBe` 400
                    pnwGenerationW midnightNet `shouldBe` 0
                _ → expectationFailure
                        ("expected two separate single-node networks, got " <> show nets)

        it "sums independently-phased sources at different longitudes, one exactly opposite the others on the cylinder" $ do
            -- u=0 (global midnight => local midnight => 0), u=16 (a
            -- quarter circumference away => local dawn => 0), u=32 (HALF
            -- the circumference away — the antipodal point on the
            -- cylinder => local noon => full output). The old bug
            -- (applying one shared GLOBAL intensity to every source)
            -- would report a total of 0 here, since the global angle IS
            -- midnight — not 200.
            let (n1, midId)  = addPowerNode panel    PowerSource 100 emptyPowerNodes
                (n2, dawnId) = addPowerNode farPanel  PowerSource 150 n1
                (n3, noonId) = addPowerNode battery   PowerSource 200 n2
                positions = HM.fromList [ (midId, (0, 0)), (dawnId, (16, 0)), (noonId, (32, 0)) ]
                -- One contiguous run touching all 3, canonicalised the
                -- way the page would store it — its last tile crosses
                -- the seam, so (32, 1) is stored as (0, 33).
                wire = HS.fromList [ canonWire (x, 1) | x ← [0 .. 32] ]
                nets = computeSnapshots testWorldSize midnight HM.empty wire n3 positions HM.empty
            case nets of
                [net] → pnwGenerationW net `shouldBe` 200
                _     → expectationFailure ("expected exactly one network, got " <> show nets)

        it "positions exactly one circumference apart (seam-aliased) agree" $ do
            let (n1, aId) = addPowerNode panel    PowerSource 300 emptyPowerNodes
                (n2, bId) = addPowerNode farPanel PowerSource 300 n1
                positions = HM.fromList [ (aId, (5, 0)), (bId, (5 + 64, 0)) ]
                wire        = HS.fromList (map canonWire [(6, 0), (6 + 64, 0)])
                globalAngle = 0.375  -- an arbitrary non-breakpoint angle
                nets = computeSnapshots testWorldSize globalAngle HM.empty wire n2 positions HM.empty
            case (find (elem aId . pnwNodeIds) nets, find (elem bId . pnwNodeIds) nets) of
                (Just netA, Just netB) → pnwGenerationW netA `shouldBe` pnwGenerationW netB
                _ → expectationFailure
                        ("expected two separate seam-aliased networks, got " <> show nets)

        it "only the locally-illuminated panel's battery actually charges" $ do
            let (n1, noonSrc) = addPowerNode panel    PowerSource 100 emptyPowerNodes
                (n2, noonBat) = addPowerNode battery  PowerStorage 5000 n1
                (n3, midSrc)  = addPowerNode farPanel PowerSource 100 n2
                (n4, midBat)  = addPowerNode battery2 PowerStorage 5000 n3
                positions = HM.fromList [ (noonSrc, (32, 0)), (noonBat, (32, 2))
                                        , (midSrc,  (0, 0)),  (midBat,  (0, 2)) ]
                wire   = HS.fromList (map canonWire [(32, 1), (0, 1)])
                ticked = tickPowerNodes testWorldSize midnight HM.empty 3600 wire positions HM.empty n4
            pnStoredWh ⊚ lookupPowerNode noonBat ticked `shouldBe` Just 100
            pnStoredWh ⊚ lookupPowerNode midBat  ticked `shouldBe` Just 0

    describe "cylindrical seam connectivity (#2634)" $ do
        -- The seam pair every scenario below is built from: (31, 0) and
        -- the tile one step EAST of it, whose canonical name is a whole
        -- world away. Before #2634 power connectivity compared raw
        -- (x + 1, y) keys, so these two were never neighbours and a wire
        -- run, node, or consumer crossing here split in half — while
        -- placement and autotiling, which already resolved the same
        -- neighbours canonically, drew it as one continuous run.
        let seamWest = (31, 0) ∷ (Int, Int)
            seamEast = eastOf seamWest

        it "the two tiles the seam separates really do have distant canonical keys" $ do
            -- Guards every fixture below: if these ever became ordinary
            -- (x, y)/(x + 1, y) neighbours, the rest of this group would
            -- pass without exercising the seam at all.
            seamEast `shouldSatisfy` (\(ex, ey) → abs (ex - 31) > 1 ∨ ey ≢ 0)
            canonicalTile testWorldSize 31 0 `shouldBe` seamWest

        it "a wire run crossing the seam is ONE component" $
            length (wireComponents testWorldSize (HS.fromList [seamWest, seamEast]))
                `shouldBe` 1

        it "the same two tiles are TWO components in a non-wrapping world" $
            -- The control for the assertion above: with no wrap the
            -- canonical step is the identity, so the distant keys stay
            -- distant and the pre-#2634 answer is the right one.
            length (wireComponents nonWrappingWorldSize
                        (HS.fromList [seamWest, seamEast]))
                `shouldBe` 2

        it "a wire run crossing the seam along the OTHER axis is one component too" $ do
            -- The wrap is on u = gx - gy, so a southward step crosses it
            -- at a different place than an eastward one; cardinal
            -- adjacency has to hold on both axes.
            let vWest = (0, 47) ∷ (Int, Int)
                vEast = southOf vWest
            vEast `shouldSatisfy` (\(ex, ey) → ex ≢ 0 ∨ abs (ey - 47) > 1)
            length (wireComponents testWorldSize (HS.fromList [vWest, vEast]))
                `shouldBe` 1
            length (wireComponents nonWrappingWorldSize
                        (HS.fromList [vWest, vEast]))
                `shouldBe` 2

        it "diagonal-only tiles still do NOT connect across the seam" $
            -- Seam-awareness widens WHICH keys count as cardinal
            -- neighbours; it must not widen the 4-dir shape itself.
            length (wireComponents testWorldSize
                        (HS.fromList [seamWest, southOf seamEast]))
                `shouldBe` 2

        it "a source and battery on opposite sides of a seam-crossing run share a network, and it charges" $ do
            let source    = (30, 0)
                batteryAt = eastOf seamEast
                (n1, srcId) = addPowerNode panel PowerSource 100 emptyPowerNodes
                (n2, batId) = addPowerNode battery PowerStorage 5000 n1
                positions   = HM.fromList [(srcId, source), (batId, batteryAt)]
                wire        = HS.fromList [seamWest, seamEast]
                angle       = globalAngleForLocalNoon source
                nets = computeSnapshots testWorldSize angle HM.empty wire n2 positions HM.empty
                ticked = tickPowerNodes testWorldSize angle HM.empty 3600 wire positions HM.empty n2
            case nets of
                [net] → HS.fromList (pnwNodeIds net) `shouldBe` HS.fromList [srcId, batId]
                _     → expectationFailure
                            ("expected exactly one network across the seam, got " <> show nets)
            -- 100 W at local noon for one hour. Before #2634 the battery
            -- sat on its own sourceless component and stayed at 0.
            (pnStoredWh ⊚ lookupPowerNode batId ticked) `shouldBeStored` 100
            -- The non-wrapping control: the identical layout in a world
            -- with no seam leaves the battery on its own component, so
            -- the wrap is what joined them.
            length (computeSnapshots nonWrappingWorldSize angle HM.empty
                        wire n2 positions HM.empty)
                `shouldBe` 2

        it "the same layout away from the seam charges identically" $ do
            -- The control the seam scenario is measured against: same
            -- shape, same wattage, same one-hour tick, ordinary interior
            -- coordinates, and its source phased to its OWN local noon
            -- (u differs, so the global angle must).
            let source      = (1, 0)
                (n1, srcId) = addPowerNode panel PowerSource 100 emptyPowerNodes
                (n2, batId) = addPowerNode battery PowerStorage 5000 n1
                positions   = HM.fromList [(srcId, source), (batId, (4, 0))]
                wire        = HS.fromList [(2, 0), (3, 0)]
                angle       = globalAngleForLocalNoon source
                ticked = tickPowerNodes testWorldSize angle HM.empty 3600 wire positions HM.empty n2
            (pnStoredWh ⊚ lookupPowerNode batId ticked) `shouldBeStored` 100

        it "a node attaches to a wire tile that is only its neighbour across the seam" $ do
            -- One wire tile, no run: the battery's attachment is the
            -- whole question, and the source stands on the other side of
            -- that same tile away from the seam.
            let source      = (30, 0)
                (n1, srcId) = addPowerNode panel PowerSource 100 emptyPowerNodes
                (n2, batId) = addPowerNode battery PowerStorage 5000 n1
                positions   = HM.fromList [(srcId, source), (batId, seamEast)]
                wire        = HS.singleton seamWest
                angle       = globalAngleForLocalNoon source
                nets = computeSnapshots testWorldSize angle HM.empty wire n2 positions HM.empty
                ticked = tickPowerNodes testWorldSize angle HM.empty 3600 wire positions HM.empty n2
            case nets of
                [net] → HS.fromList (pnwNodeIds net) `shouldBe` HS.fromList [srcId, batId]
                _     → expectationFailure
                            ("expected one network holding both nodes, got " <> show nets)
            (pnStoredWh ⊚ lookupPowerNode batId ticked) `shouldBeStored` 100
            -- The non-wrapping control: with no seam the battery reaches
            -- no wire at all, so only the source is on a network.
            map pnwNodeIds (computeSnapshots nonWrappingWorldSize angle
                                HM.empty wire n2 positions HM.empty)
                `shouldBe` [[srcId]]

        it "a node SHARING a seam-canonical wire tile is still attached" $ do
            -- The tile-itself branch of attachment, exercised at the
            -- seam: canonicalising the neighbours must not cost a node
            -- the overlay it is standing on.
            let (n1, srcId) = addPowerNode panel PowerSource 100 emptyPowerNodes
                positions   = HM.singleton srcId seamEast
                wire        = HS.singleton seamEast
                nets = computeSnapshots testWorldSize noon HM.empty wire n1 positions HM.empty
            map pnwNodeIds nets `shouldBe` [[srcId]]

        it "a node bridges two wire stubs that only the seam frame puts on either side of it" $ do
            -- (30, 0) and the tile two steps east of it are separate
            -- components; the battery between them touches one by an
            -- ordinary step and the other only across the seam, and must
            -- merge them into ONE network rather than attach twice.
            let stubWest = (30, 0)
                stubEast = eastOf seamWest
                (n1, srcAId) = addPowerNode panel PowerSource 100 emptyPowerNodes
                (n2, srcBId) = addPowerNode farPanel PowerSource 100 n1
                (n3, batId)  = addPowerNode battery PowerStorage 5000 n2
                positions = HM.fromList [ (srcAId, (29, 0))
                                        , (batId,  seamWest)
                                        , (srcBId, eastOf stubEast) ]
                wire = HS.fromList [stubWest, stubEast]
                nets = computeSnapshots testWorldSize noon HM.empty wire n3 positions HM.empty
            length (wireComponents testWorldSize wire) `shouldBe` 2
            case nets of
                [net] → HS.fromList (pnwNodeIds net)
                            `shouldBe` HS.fromList [srcAId, batId, srcBId]
                _     → expectationFailure
                            ("expected one seam-bridged network, got " <> show nets)
            -- The non-wrapping control: with no seam the battery reaches
            -- only the western stub, so there is nothing to bridge and
            -- the two stubs stay two networks.
            length (computeSnapshots nonWrappingWorldSize noon HM.empty
                        wire n3 positions HM.empty)
                `shouldBe` 2

        it "a consumer touching both stubs across the seam joins one of them and merges neither" $ do
            -- The paired CONTROL for the bridging case above, at the
            -- identical geometry with a workshop in the bridging tile
            -- instead of a node. A consumer is a passive tap, so the two
            -- stubs must stay two networks and its drain be counted
            -- exactly once — an answer seam-awareness must NOT change,
            -- which is the point: it holds before and after #2634, while
            -- the node in the same tile only merges afterwards.
            let stubWest = (30, 0)
                stubEast = eastOf seamWest
                (n1, srcAId) = addPowerNode panel PowerSource 100 emptyPowerNodes
                (n2, srcBId) = addPowerNode farPanel PowerSource 100 n1
                positions = HM.fromList [ (srcAId, (29, 0))
                                        , (srcBId, eastOf stubEast) ]
                wire      = HS.fromList [stubWest, stubEast]
                consumers = HM.singleton workshop (seamWest, 40)
                nets = computeSnapshots testWorldSize noon HM.empty wire n2 positions consumers
            length nets `shouldBe` 2
            concatMap pnwConsumerIds nets `shouldBe` [workshop]
            sum (map pnwDrainW nets) `shouldBe` 40
            HS.fromList (concatMap pnwNodeIds nets)
                `shouldBe` HS.fromList [srcAId, srcBId]

        it "a consumer attaches across the seam: its drain lands in the snapshot AND in the battery" $ do
            -- Requirement 2 for consumers, stated so that omitting the
            -- consumer could not also pass: the snapshot must NAME it and
            -- carry its drain, and the same layout with and without it
            -- must charge the battery by exactly that drain's worth less
            -- under otherwise identical surplus generation.
            let source      = (30, 0)
                (n1, srcId) = addPowerNode panel PowerSource 100 emptyPowerNodes
                (n2, batId) = addPowerNode battery PowerStorage 5000 n1
                positions   = HM.fromList [(srcId, source), (batId, southOf seamWest)]
                wire        = HS.singleton seamWest
                consumers   = HM.singleton workshop (seamEast, 40)
                angle       = globalAngleForLocalNoon source
                nets = computeSnapshots testWorldSize angle HM.empty wire n2 positions consumers
                withConsumer = tickPowerNodes testWorldSize angle HM.empty 3600
                                              wire positions consumers n2
                without = tickPowerNodes testWorldSize angle HM.empty 3600
                                         wire positions HM.empty n2
            case nets of
                [net] → do
                    pnwConsumerIds net `shouldBe` [workshop]
                    pnwDrainW net `shouldBe` 40
                    pnwGenerationW net `shouldBe` 100
                _ → expectationFailure
                        ("expected exactly one network, got " <> show nets)
            -- 100 W generated, 40 W drawn, one hour => 60 Wh stored, a
            -- full 40 Wh below the 100 Wh the consumer-free control
            -- stores over the very same tick.
            (pnStoredWh ⊚ lookupPowerNode batId withConsumer) `shouldBeStored` 60
            (pnStoredWh ⊚ lookupPowerNode batId without) `shouldBeStored` 100
            -- The non-wrapping control: with no seam the workshop
            -- touches no wire, so it is dropped and the network draws
            -- nothing.
            map (\net → (pnwConsumerIds net, pnwDrainW net))
                (computeSnapshots nonWrappingWorldSize angle HM.empty
                    wire n2 positions consumers)
                `shouldBe` [([], 0)]
