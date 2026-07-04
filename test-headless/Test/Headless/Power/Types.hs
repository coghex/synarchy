{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
-- | Power-node registry tests (#358): the pure add/remove/lookup/prune
--   transitions in Power.Types that the power.* verbs wrap, plus the
--   save-format roundtrip (nodes persist in WorldPageSave, v73). The
--   engine-integrated path (power.placeNode consuming a unit's item)
--   is exercised by hand over the debug console / a future probe.
module Test.Headless.Power.Types (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashSet as HS
import qualified Data.Serialize as S
import Power.Types
import Building.Types (BuildingId(..))

panel1, panel2, battery1 ∷ BuildingId
panel1   = BuildingId 7
panel2   = BuildingId 8
battery1 = BuildingId 9

spec ∷ Spec
spec = do
    describe "powerNodeSpecFor" $ do
        it "solar_panel is a source" $
            powerNodeSpecFor "solar_panel" `shouldBe` Just (PowerSource, 400)

        it "high_voltage_battery is storage" $
            powerNodeSpecFor "high_voltage_battery"
                `shouldBe` Just (PowerStorage, 5000)

        it "an unknown item name isn't placeable" $
            powerNodeSpecFor "wiring" `shouldBe` Nothing

    describe "registry" $ do
        it "addPowerNode stores the role + the relevant parameter only" $ do
            let (nodes, nid) = addPowerNode panel1 PowerSource 400 emptyPowerNodes
            case lookupPowerNode nid nodes of
                Nothing   → expectationFailure "node missing"
                Just node → do
                    pnBuilding node `shouldBe` panel1
                    pnRole node `shouldBe` PowerSource
                    pnPeakWatts node `shouldBe` 400
                    pnCapacityWh node `shouldBe` 0

        it "a storage node keeps capacity and zeroes peak watts" $ do
            let (nodes, nid) =
                    addPowerNode battery1 PowerStorage 5000 emptyPowerNodes
            case lookupPowerNode nid nodes of
                Nothing   → expectationFailure "node missing"
                Just node → do
                    pnRole node `shouldBe` PowerStorage
                    pnPeakWatts node `shouldBe` 0
                    pnCapacityWh node `shouldBe` 5000

        it "ids are unique and the counter survives removal" $ do
            let (n1, i1) = addPowerNode panel1 PowerSource 400 emptyPowerNodes
                (n2, _)  = removePowerNode i1 n1
                (n3, i2) = addPowerNode panel2 PowerSource 400 n2
            i2 `shouldNotBe` i1
            lookupPowerNode i1 n3 `shouldBe` Nothing

        it "removePowerNode is False for unknown ids" $ do
            let (nodes, _) = addPowerNode panel1 PowerSource 400 emptyPowerNodes
            snd (removePowerNode (PowerNodeId 999) nodes) `shouldBe` False

        it "nodeForBuilding finds the node riding a given building" $ do
            let (n1, i1) = addPowerNode panel1 PowerSource 400 emptyPowerNodes
                (n2, _)  = addPowerNode battery1 PowerStorage 5000 n1
            pnId ⊚ nodeForBuilding panel1 n2 `shouldBe` Just i1
            nodeForBuilding (BuildingId 999) n2 `shouldBe` Nothing

        it "allNodes lists oldest first" $ do
            let (n1, i1) = addPowerNode panel1 PowerSource 400 emptyPowerNodes
                (n2, i2) = addPowerNode battery1 PowerStorage 5000 n1
            map pnId (allNodes n2) `shouldBe` [i1, i2]

    describe "persistence" $ do
        it "roundtrips through the save encoding" $ do
            let (n1, _) = addPowerNode panel1 PowerSource 400 emptyPowerNodes
                (n2, _) = addPowerNode battery1 PowerStorage 5000 n1
            S.decode (S.encode n2) `shouldBe` Right n2

        it "pruneToBuildings drops nodes whose building is gone" $ do
            let (n1, i1) = addPowerNode panel1 PowerSource 400 emptyPowerNodes
                (n2, i2) = addPowerNode battery1 PowerStorage 5000 n1
                pruned = pruneToBuildings (HS.singleton battery1) n2
            lookupPowerNode i1 pruned `shouldBe` Nothing
            pnBuilding ⊚ lookupPowerNode i2 pruned `shouldBe` Just battery1
