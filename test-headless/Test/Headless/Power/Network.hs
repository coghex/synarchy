{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
-- | Power-network connectivity + energy balance tests (#360): the pure
--   flood-fill + tick math in Power.Network. No engine/Lua needed — the
--   connectivity + brownout math is fully exercised here with synthetic
--   drain, since #361's real consumers don't exist yet (see
--   Power.Network's module haddock).
module Test.Headless.Power.Network (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Power.Types
import Power.Network
import Building.Types (BuildingId(..))

panel, battery, battery2, farPanel ∷ BuildingId
panel    = BuildingId 1
battery  = BuildingId 2
battery2 = BuildingId 3
farPanel = BuildingId 4

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
                nets        = computeSnapshots noon HM.empty wire n2 positions
            case nets of
                [net] → HS.fromList (pnwNodeIds net) `shouldBe` HS.fromList [srcId, batId]
                _     → expectationFailure ("expected exactly one network, got " <> show nets)

        it "two nodes with no wire at all attach to no network" $ do
            let (n1, srcId) = addPowerNode panel PowerSource 400 emptyPowerNodes
                (n2, batId) = addPowerNode battery PowerStorage 5000 n1
                positions   = HM.fromList [(srcId, (0, 0)), (batId, (1, 0))]
                nets        = computeSnapshots noon HM.empty HS.empty n2 positions
            nets `shouldBe` []
              -- adjacent tiles, but NO wire tile between them => not networked

        it "a node with no wire adjacent to it isn't networked, even if another node is" $ do
            let (n1, srcId) = addPowerNode panel PowerSource 400 emptyPowerNodes
                (n2, farId) = addPowerNode farPanel PowerSource 400 n1
                positions   = HM.fromList [(srcId, (0, 0)), (farId, (50, 50))]
                wire        = HS.singleton (1, 0)
                nets        = computeSnapshots noon HM.empty wire n2 positions
            case nets of
                [net] → pnwNodeIds net `shouldBe` [srcId]
                _     → expectationFailure ("expected exactly one network, got " <> show nets)

    describe "computeSnapshots — instantaneous status" $ do
        it "generation covering drain reads Powered even with no storage" $ do
            let (n1, srcId) = addPowerNode panel PowerSource 400 emptyPowerNodes
                positions   = HM.singleton srcId (0, 0)
                wire        = HS.singleton (1, 0)
                drain       = HM.singleton srcId 100
                nets        = computeSnapshots noon drain wire n1 positions
            map pnwStatus nets `shouldBe` [Powered]

        it "drain exceeding generation with no storage reads Brownout" $ do
            let (n1, srcId) = addPowerNode panel PowerSource 400 emptyPowerNodes
                positions   = HM.singleton srcId (0, 0)
                wire        = HS.singleton (1, 0)
                drain       = HM.singleton srcId 500
                nets        = computeSnapshots noon drain wire n1 positions
            map pnwStatus nets `shouldBe` [Brownout]

        it "a deficit is Powered as long as the battery still holds charge" $ do
            let (n1, srcId) = addPowerNode panel PowerSource 100 emptyPowerNodes
                (n2, batId) = addPowerNode battery PowerStorage 5000 n1
                seeded      = seedStored batId 10 n2
                positions   = HM.fromList [(srcId, (0, 0)), (batId, (2, 0))]
                wire        = HS.singleton (1, 0)
                drain       = HM.singleton srcId 1000
                nets        = computeSnapshots midnight drain wire seeded positions
            map pnwStatus nets `shouldBe` [Powered]

    describe "tickPowerNodes — charging" $ do
        it "a full hour at noon charges the battery by peakWatts Wh" $ do
            let (n1, srcId) = addPowerNode panel PowerSource 400 emptyPowerNodes
                (n2, batId) = addPowerNode battery PowerStorage 5000 n1
                positions   = HM.fromList [(srcId, (0, 0)), (batId, (2, 0))]
                wire        = HS.singleton (1, 0)
                n3          = tickPowerNodes noon HM.empty 3600 wire positions n2
            pnStoredWh ⊚ lookupPowerNode batId n3 `shouldBe` Just 400

        it "charging never exceeds capacity — surplus is curtailed" $ do
            let (n1, srcId) = addPowerNode panel PowerSource 400 emptyPowerNodes
                (n2, batId) = addPowerNode battery PowerStorage 500 n1
                seeded      = seedStored batId 400 n2
                positions   = HM.fromList [(srcId, (0, 0)), (batId, (2, 0))]
                wire        = HS.singleton (1, 0)
                n3          = tickPowerNodes noon HM.empty 3600 wire positions seeded
            pnStoredWh ⊚ lookupPowerNode batId n3 `shouldBe` Just 500

        it "no time passing (dtGameSeconds <= 0) is a no-op" $ do
            let (n1, srcId) = addPowerNode panel PowerSource 400 emptyPowerNodes
                (n2, batId) = addPowerNode battery PowerStorage 5000 n1
                positions   = HM.fromList [(srcId, (0, 0)), (batId, (2, 0))]
                wire        = HS.singleton (1, 0)
                n3          = tickPowerNodes noon HM.empty 0 wire positions n2
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
                n3          = tickPowerNodes midnight drain 3600 wire positions seeded
            pnStoredWh ⊚ lookupPowerNode batId n3 `shouldBe` Just 0

        it "depleting the battery reports Brownout on the next query" $ do
            let (n1, srcId) = addPowerNode panel PowerSource 400 emptyPowerNodes
                (n2, batId) = addPowerNode battery PowerStorage 5000 n1
                positions   = HM.fromList [(srcId, (0, 0)), (batId, (2, 0))]
                wire        = HS.singleton (1, 0)
                drain       = HM.singleton srcId 100
                nets        = computeSnapshots midnight drain wire n2 positions
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
                ticked = tickPowerNodes midnight drain 3600 wire positions seeded
            pnStoredWh ⊚ lookupPowerNode batId1 ticked `shouldBe` Just 0
            pnStoredWh ⊚ lookupPowerNode batId2 ticked `shouldBe` Just 0
