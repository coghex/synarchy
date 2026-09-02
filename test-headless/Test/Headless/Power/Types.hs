-- | Power-node registry tests (#358): the pure add/remove/lookup
--   transitions in Power.Types that the power.* verbs wrap, plus the
--   save-format roundtrip (nodes persist in WorldPageSave, v73).
--
--   Since #1148 it also covers the DECLARATION side that replaced the
--   hardcoded two-name catalogue: the YAML schema validator
--   ('Power.Base.powerNodeSpecFromYaml'), its mapping onto the frozen
--   wire role ('powerNodeRole'), and the SHIPPED building defs, parsed
--   through the production decoder — a constructed def would prove the
--   code works without proving the content does. The engine-integrated
--   answers (@power.isPlaceable@ / @power.placeNode@ refusals) live in
--   'Test.Headless.Power.Placement'.
module Test.Headless.Power.Types (spec) where

import UPrelude
import Test.Hspec
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Data.Serialize as S
import qualified Data.Yaml as Yaml
import Power.Types
import Power.Base
import Engine.Asset.YamlBuildings (BuildingYamlDef(..), BuildingYamlFile(..))
import Building.Types (BuildingId(..))

panel1, panel2, battery1 ∷ BuildingId
panel1   = BuildingId 7
panel2   = BuildingId 8
battery1 = BuildingId 9

spec ∷ Spec
spec = do
    describe "power node declarations (#1148)" $ do
        it "maps each declared spec onto its wire role" $ do
            powerNodeRole (PowerNodeSource 400) `shouldBe` PowerSource
            powerNodeRole (PowerNodeStorage 5000) `shouldBe` PowerStorage

        it "a def with no power_role is not a node" $
            powerNodeSpecFromYaml Nothing Nothing Nothing
                `shouldBe` Right Nothing

        it "reads a source's peak watts and a storage bank's capacity" $ do
            powerNodeSpecFromYaml (Just "source") (Just 400) Nothing
                `shouldBe` Right (Just (PowerNodeSource 400))
            powerNodeSpecFromYaml (Just "storage") Nothing (Just 5000)
                `shouldBe` Right (Just (PowerNodeStorage 5000))

        it "rejects an unknown role" $
            powerNodeSpecFromYaml (Just "generator") (Just 400) Nothing
                `shouldSatisfy` rejectedFor "unknown power_role"

        it "rejects a role whose own rating is missing" $ do
            powerNodeSpecFromYaml (Just "source") Nothing Nothing
                `shouldSatisfy` rejectedFor "no power_peak"
            powerNodeSpecFromYaml (Just "storage") Nothing Nothing
                `shouldSatisfy` rejectedFor "no power_capacity"

        it "rejects the OTHER role's rating alongside a role" $ do
            powerNodeSpecFromYaml (Just "source") (Just 400) (Just 5000)
                `shouldSatisfy` rejectedFor "power_capacity"
            powerNodeSpecFromYaml (Just "storage") (Just 400) (Just 5000)
                `shouldSatisfy` rejectedFor "power_peak"

        it "rejects a rating orphaned by a missing role" $ do
            powerNodeSpecFromYaml Nothing (Just 400) Nothing
                `shouldSatisfy` rejectedFor "without a power_role"
            powerNodeSpecFromYaml Nothing Nothing (Just 5000)
                `shouldSatisfy` rejectedFor "without a power_role"

        it "rejects a negative rating" $
            powerNodeSpecFromYaml (Just "source") (Just (-1)) Nothing
                `shouldSatisfy` rejectedFor "negative"

        it "rejects a non-finite rating" $ do
            powerNodeSpecFromYaml (Just "storage") Nothing (Just (1 / 0))
                `shouldSatisfy` rejectedFor "finite"
            powerNodeSpecFromYaml (Just "source") (Just (0 / 0)) Nothing
                `shouldSatisfy` rejectedFor "finite"

    describe "shipped power building defs (#1148)" $ do
        it "solar_panel declares a 400 W source" $
            shippedPowerNode "data/buildings/solar_panel.yaml"
                `shouldReturn` Just (PowerNodeSource 400)

        it "high_voltage_battery declares a 5000 Wh bank" $
            shippedPowerNode "data/buildings/high_voltage_battery.yaml"
                `shouldReturn` Just (PowerNodeStorage 5000)

        it "an ordinary shipped def declares no node" $
            shippedPowerNode "data/buildings/workbench.yaml"
                `shouldReturn` Nothing

        it "refuses a malformed declaration instead of dropping it" $
            -- The whole file fails to decode, so a content mistake can
            -- never leave a def that ordinary building placement would
            -- happily spawn for free.
            -- Everything else about this def is valid (#2080 made
            -- `visual_class` mandatory, so it has to be here or the
            -- rejection would be about the wrong field), leaving the
            -- orphaned power_role as the only thing to reject.
            decodeBuildings (BS.unlines
                [ "buildings:"
                , "  - name: \"broken\""
                , "    sprite: \"x.png\""
                , "    visual_class: \"freestanding_installation\""
                , "    power_role: \"source\""
                ]) `shouldSatisfy` isLeft

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

-- | A rejection whose message names @needle@ — asserted on the reason,
--   not just on Left, so a rule can't pass by failing for the wrong
--   cause.
rejectedFor ∷ Text → Either Text (Maybe PowerNodeSpec) → Bool
rejectedFor needle (Left err) = needle `T.isInfixOf` err
rejectedFor _      (Right _)  = False

isLeft ∷ Either a b → Bool
isLeft (Left _) = True
isLeft _        = False

-- | The @power_role@ declaration of the single def in a shipped
--   building YAML, through the production decoder.
shippedPowerNode ∷ FilePath → IO (Maybe PowerNodeSpec)
shippedPowerNode path = do
    parsed ← Yaml.decodeFileEither path
    case parsed of
        Left err → do
            expectationFailure (path <> " failed to parse: " <> show err)
            pure Nothing
        Right file → case byfBuildings file of
            [def] → pure (bydPowerNode def)
            defs  → do
                expectationFailure $ path <> " holds " <> show (length defs)
                    <> " defs, expected exactly 1"
                pure Nothing

decodeBuildings ∷ BS.ByteString → Either Yaml.ParseException BuildingYamlFile
decodeBuildings = Yaml.decodeEither'
