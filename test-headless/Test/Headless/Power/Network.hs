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
                nets        = computeSnapshots noon HM.empty wire n2 positions HM.empty
            case nets of
                [net] → HS.fromList (pnwNodeIds net) `shouldBe` HS.fromList [srcId, batId]
                _     → expectationFailure ("expected exactly one network, got " <> show nets)

        it "two nodes with no wire at all attach to no network" $ do
            let (n1, srcId) = addPowerNode panel PowerSource 400 emptyPowerNodes
                (n2, batId) = addPowerNode battery PowerStorage 5000 n1
                positions   = HM.fromList [(srcId, (0, 0)), (batId, (1, 0))]
                nets        = computeSnapshots noon HM.empty HS.empty n2 positions HM.empty
            nets `shouldBe` []
              -- adjacent tiles, but NO wire tile between them => not networked

        it "a node with no wire adjacent to it isn't networked, even if another node is" $ do
            let (n1, srcId) = addPowerNode panel PowerSource 400 emptyPowerNodes
                (n2, farId) = addPowerNode farPanel PowerSource 400 n1
                positions   = HM.fromList [(srcId, (0, 0)), (farId, (50, 50))]
                wire        = HS.singleton (1, 0)
                nets        = computeSnapshots noon HM.empty wire n2 positions HM.empty
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
                                        , (srcBId, (7, 5)) ]
                wire = HS.fromList [(4, 5), (6, 5)]
                nets = computeSnapshots noon HM.empty wire n3 positions HM.empty
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
                                        , (srcBId, (7, 5)) ]
                wire = HS.fromList [(4, 5), (6, 5)]
                -- noon (full intensity), 1 hour: 100W + 100W = 200 Wh into
                -- the one shared battery. A regression here (the battery
                -- ending up at 100, or split across two spurious networks)
                -- would mean one source's contribution was silently lost.
                ticked = tickPowerNodes noon HM.empty 3600 wire positions HM.empty n3
            pnStoredWh ⊚ lookupPowerNode batId ticked `shouldBe` Just 200

    describe "computeSnapshots — instantaneous status" $ do
        it "generation covering drain reads Powered even with no storage" $ do
            let (n1, srcId) = addPowerNode panel PowerSource 400 emptyPowerNodes
                positions   = HM.singleton srcId (0, 0)
                wire        = HS.singleton (1, 0)
                drain       = HM.singleton srcId 100
                nets        = computeSnapshots noon drain wire n1 positions HM.empty
            map pnwStatus nets `shouldBe` [Powered]

        it "drain exceeding generation with no storage reads Brownout" $ do
            let (n1, srcId) = addPowerNode panel PowerSource 400 emptyPowerNodes
                positions   = HM.singleton srcId (0, 0)
                wire        = HS.singleton (1, 0)
                drain       = HM.singleton srcId 500
                nets        = computeSnapshots noon drain wire n1 positions HM.empty
            map pnwStatus nets `shouldBe` [Brownout]

        it "a deficit is Powered as long as the battery still holds charge" $ do
            let (n1, srcId) = addPowerNode panel PowerSource 100 emptyPowerNodes
                (n2, batId) = addPowerNode battery PowerStorage 5000 n1
                seeded      = seedStored batId 10 n2
                positions   = HM.fromList [(srcId, (0, 0)), (batId, (2, 0))]
                wire        = HS.singleton (1, 0)
                drain       = HM.singleton srcId 1000
                nets        = computeSnapshots midnight drain wire seeded positions HM.empty
            map pnwStatus nets `shouldBe` [Powered]

    describe "tickPowerNodes — charging" $ do
        it "a full hour at noon charges the battery by peakWatts Wh" $ do
            let (n1, srcId) = addPowerNode panel PowerSource 400 emptyPowerNodes
                (n2, batId) = addPowerNode battery PowerStorage 5000 n1
                positions   = HM.fromList [(srcId, (0, 0)), (batId, (2, 0))]
                wire        = HS.singleton (1, 0)
                n3          = tickPowerNodes noon HM.empty 3600 wire positions HM.empty n2
            pnStoredWh ⊚ lookupPowerNode batId n3 `shouldBe` Just 400

        it "charging never exceeds capacity — surplus is curtailed" $ do
            let (n1, srcId) = addPowerNode panel PowerSource 400 emptyPowerNodes
                (n2, batId) = addPowerNode battery PowerStorage 500 n1
                seeded      = seedStored batId 400 n2
                positions   = HM.fromList [(srcId, (0, 0)), (batId, (2, 0))]
                wire        = HS.singleton (1, 0)
                n3          = tickPowerNodes noon HM.empty 3600 wire positions HM.empty seeded
            pnStoredWh ⊚ lookupPowerNode batId n3 `shouldBe` Just 500

        it "no time passing (dtGameSeconds <= 0) is a no-op" $ do
            let (n1, srcId) = addPowerNode panel PowerSource 400 emptyPowerNodes
                (n2, batId) = addPowerNode battery PowerStorage 5000 n1
                positions   = HM.fromList [(srcId, (0, 0)), (batId, (2, 0))]
                wire        = HS.singleton (1, 0)
                n3          = tickPowerNodes noon HM.empty 0 wire positions HM.empty n2
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
                n3          = tickPowerNodes midnight drain 3600 wire positions HM.empty seeded
            pnStoredWh ⊚ lookupPowerNode batId n3 `shouldBe` Just 0

        it "depleting the battery reports Brownout on the next query" $ do
            let (n1, srcId) = addPowerNode panel PowerSource 400 emptyPowerNodes
                (n2, batId) = addPowerNode battery PowerStorage 5000 n1
                positions   = HM.fromList [(srcId, (0, 0)), (batId, (2, 0))]
                wire        = HS.singleton (1, 0)
                drain       = HM.singleton srcId 100
                nets        = computeSnapshots midnight drain wire n2 positions HM.empty
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
                ticked = tickPowerNodes midnight drain 3600 wire positions HM.empty seeded
            pnStoredWh ⊚ lookupPowerNode batId1 ticked `shouldBe` Just 0
            pnStoredWh ⊚ lookupPowerNode batId2 ticked `shouldBe` Just 0

    describe "consumer drain (#361 — requires_power buildings)" $ do
        it "a consumer touching the same wire as a source folds its drain into drainW" $ do
            let (n1, srcId) = addPowerNode panel PowerSource 400 emptyPowerNodes
                positions   = HM.singleton srcId (0, 0)
                wire        = HS.singleton (1, 0)
                consumers   = HM.singleton workshop ((2, 0), 150)
                nets        = computeSnapshots noon HM.empty wire n1 positions consumers
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
                nets        = computeSnapshots noon HM.empty wire n1 positions consumers
            map pnwDrainW nets `shouldBe` [225]

        it "a consumer not adjacent to any wire is dropped — no network, no drain" $ do
            let (n1, srcId) = addPowerNode panel PowerSource 400 emptyPowerNodes
                positions   = HM.singleton srcId (0, 0)
                wire        = HS.singleton (1, 0)
                consumers   = HM.singleton workshop ((50, 50), 150)
                nets        = computeSnapshots noon HM.empty wire n1 positions consumers
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
                nets      = computeSnapshots noon HM.empty wire emptyPowerNodes HM.empty consumers
            nets `shouldBe` []

        it "consumer drain actually discharges a battery over time (day/night balance)" $ do
            let (n1, batId) = addPowerNode battery PowerStorage 5000 emptyPowerNodes
                seeded      = seedStored batId 500 n1
                positions   = HM.singleton batId (0, 0)
                wire        = HS.singleton (1, 0)
                consumers   = HM.singleton workshop ((2, 0), 100)
                -- midnight: 0 generation, 100W drain, 1h => -100Wh
                ticked = tickPowerNodes midnight HM.empty 3600 wire positions consumers seeded
            pnStoredWh ⊚ lookupPowerNode batId ticked `shouldBe` Just 400

        it "a consumer-only network with an empty battery reads Brownout" $ do
            let (n1, batId) = addPowerNode battery PowerStorage 5000 emptyPowerNodes
                positions   = HM.singleton batId (0, 0)
                wire        = HS.singleton (1, 0)
                consumers   = HM.singleton workshop ((2, 0), 100)
                nets        = computeSnapshots midnight HM.empty wire n1 positions consumers
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
                nets        = computeSnapshots noon HM.empty wire n1 positions consumers
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
