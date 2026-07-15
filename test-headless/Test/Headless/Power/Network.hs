{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
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
import Power.Types
import Power.Network
import Building.Types (BuildingId(..))

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
            wireComponents HS.empty `shouldBe` []

        it "a straight run of wire is one component" $
            wireComponents (HS.fromList [(0,0), (1,0), (2,0)])
                `shouldBe` [HS.fromList [(0,0), (1,0), (2,0)]]

        it "two disjoint runs are two components" $
            length (wireComponents (HS.fromList [(0,0), (1,0), (10,10), (11,10)]))
                `shouldBe` 2

        it "diagonal-only tiles do NOT connect (4-dir adjacency only)" $
            length (wireComponents (HS.fromList [(0,0), (1,1)])) `shouldBe` 2

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

    describe "local solar phasing (#794)" $ do
        it "two otherwise-identical panels read local noon vs. local midnight under the same global clock" $ do
            -- testWorldSize's circumference is 64 tiles; noonId sits
            -- exactly half of it away (u=32) from midnightId (u=0), so
            -- under a global clock reading midnight, noonId's OWN local
            -- angle is noon while midnightId's stays at the global angle.
            let (n1, noonId)     = addPowerNode panel    PowerSource 400 emptyPowerNodes
                (n2, midnightId) = addPowerNode farPanel PowerSource 400 n1
                positions = HM.fromList [ (noonId, (32, 0)), (midnightId, (0, 0)) ]
                wire      = HS.fromList [(33, 0), (1, 0)]  -- two disjoint single-node networks
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
                wire = HS.fromList [ (x, 1) | x ← [0 .. 32] ]  -- one contiguous run touching all 3
                nets = computeSnapshots testWorldSize midnight HM.empty wire n3 positions HM.empty
            case nets of
                [net] → pnwGenerationW net `shouldBe` 200
                _     → expectationFailure ("expected exactly one network, got " <> show nets)

        it "positions exactly one circumference apart (seam-aliased) agree" $ do
            let (n1, aId) = addPowerNode panel    PowerSource 300 emptyPowerNodes
                (n2, bId) = addPowerNode farPanel PowerSource 300 n1
                positions = HM.fromList [ (aId, (5, 0)), (bId, (5 + 64, 0)) ]
                wire        = HS.fromList [(6, 0), (6 + 64, 0)]
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
                wire   = HS.fromList [(32, 1), (0, 1)]
                ticked = tickPowerNodes testWorldSize midnight HM.empty 3600 wire positions HM.empty n4
            pnStoredWh ⊚ lookupPowerNode noonBat ticked `shouldBe` Just 100
            pnStoredWh ⊚ lookupPowerNode midBat  ticked `shouldBe` Just 0
