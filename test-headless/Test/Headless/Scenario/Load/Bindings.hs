{-# LANGUAGE Strict #-}
-- | What the construction composes from the adapters (#2713, review round
--   1): the items a location can bind to, the paths its diagnostics carry,
--   and the simulation's consumption of what was installed.
module Test.Headless.Scenario.Load.Bindings (spec) where

import UPrelude
import Test.Hspec
import Data.IORef (readIORef)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Building.Types (BuildingManager(..))
import Engine.Core.Capability.SaveLoad (SaveLoadCapability(..), toSaveLoadCapability)
import Engine.Core.State (EngineEnv(..))
import Engine.Load.Status
import Item.Types (ItemInstance(..))
import Location.Instance
import Scenario.Load.Types
import Scenario.Types (DiagnosticEffect(..), ScenarioDiagnostic(..))
import Test.Headless.Scenario.Load.Harness
import Test.Headless.Scenario.Placement.Fixtures (Fx(..))
import Unit.Types
import World.Generate.Types (WorldGenParams(..))
import World.State.Types

spec ∷ SpecWith H
spec = do
    carriedSpec
    pathSpec
    nestedPathSpec
    simulationSpec

-- | The location instance the published page holds, and the page's item
--   instance ids by scenario reference.
locationOf ∷ H → IO LocationInstance
locationOf h = do
    Just ws ← activeArena h
    Just params ← readIORef (wsGenParamsRef ws)
    case instancesToList (wgpLocationInstances params) of
        [inst] → pure inst
        other → fail ("expected one location, found " ⧺ show (length other))

-- | Every physical item of every unit, by instance id.
carriedItems ∷ H → IO [ItemInstance]
carriedItems h = do
    um ← readIORef (unitManagerRef (fxEnv (hFx h)))
    let nodes i = i : concatMap nodes (iiContents i)
    pure [ n | u ← HM.elems (umInstances um)
             , root ← uiInventory u ⧺ HM.elems (uiEquipment u) ⧺ uiAccessories u
             , n ← nodes root ]

-- * Items an installed unit carries

carriedSpec ∷ SpecWith H
carriedSpec = describe "items an installed unit carries" $ do

    it "binds a significant slot to a carried item as taken, with no warning" $ \h → do
        path ← writeScenario "carried-significant.yaml" $ doc
            [ "map: {width: 40, height: 40}"
            , "locations:"
            , "  - {id: ruin, definition: ruin_small, x: -5, y: -5, significant_items: {1: gem}}"
            , "units:"
            , "  - {id: carrier, definition: acolyte, x: 3, y: 3, inventory: [{id: gem, definition: processing_unit}]}" ]
        s ← runLoad h (SourceFile path)
        slsOutcome s `shouldBe` Just ScenarioReady
        slsWarnings s `shouldBe` []
        inst ← locationOf h
        [slot] ← pure (liSignificant inst)
        items ← carriedItems h
        fmap iiInstanceId (filter ((≡ "processing_unit") ∘ iiDefName) items) `shouldBe` [fromMaybe 0 (lsiInstanceId slot)]
        -- a carried obligation is one the player already holds
        lsiTaken slot `shouldBe` True

    it "binds a container slot to a carried realized shell" $ \h → do
        path ← writeScenario "carried-container.yaml" $ doc
            [ "map: {width: 40, height: 40}"
            , "locations:"
            , "  - {id: crypt, definition: crypt_fx, x: -5, y: -5, significant_items: {}, incidental_content: false"
            , "     , containers: {1: {item: box, realized: true}}}"
            , "units:"
            , "  - {id: carrier, definition: acolyte, x: 3, y: 3, inventory: [{id: box, definition: field_toolbox}]}" ]
        s ← runLoad h (SourceFile path)
        slsOutcome s `shouldBe` Just ScenarioReady
        slsWarnings s `shouldBe` []
        inst ← locationOf h
        items ← carriedItems h
        [shell] ← pure (filter ((≡ "field_toolbox") ∘ iiDefName) items)
        fmap (\c → (lcsInstanceId c, lcsRealized c)) (filter ((≡ 1) ∘ lcsSlot) (liContainers inst))
            `shouldBe` [(Just (iiInstanceId shell), True)]

    it "still refuses a PENDING shell that is carried: its provenance is the ground" $ \h → do
        path ← writeScenario "carried-pending.yaml" $ doc
            [ "map: {width: 40, height: 40}"
            , "locations:"
            , "  - {id: crypt, definition: crypt_fx, x: -5, y: -5, significant_items: {}, incidental_content: false"
            , "     , containers: {1: {item: box}}}"
            , "units:"
            , "  - {id: carrier, definition: acolyte, x: 3, y: 3, inventory: [{id: box, definition: field_toolbox}]}" ]
        s ← runLoad h (SourceFile path)
        slsOutcome s `shouldBe` Just ScenarioReady
        -- the binding is dropped, loudly, and the slot stays unbound
        map sdEffect (slsWarnings s) `shouldSatisfy` any (≡ FieldRejected)
        inst ← locationOf h
        fmap lcsInstanceId (filter ((≡ 1) ∘ lcsSlot) (liContainers inst)) `shouldBe` [Nothing]

-- * Source paths

pathSpec ∷ SpecWith H
pathSpec = describe "diagnostic paths" $ do

    it "name an entry by the path it had in the document, not by its index among the survivors" $ \h → do
        path ← writeScenario "paths.yaml" $ doc
            [ "map: {width: 40, height: 40}"
            , "buildings:"
            , "  - {id: ghost, definition: no_such_building, x: 5, y: 5}"
            , "  - {id: first, definition: solar_panel, x: 10, y: 10}"
            , "  - {id: second, definition: solar_panel, x: 10, y: 10}"
            , "ground_items:"
            , "  - {id: nope, definition: no_such_item, x: 1, y: 1}"
            , "  - {id: kit, definition: field_toolbox, x: 2, y: 2, contents: [{id: bad, definition: no_such_item}]}" ]
        s ← runLoad h (SourceFile path)
        slsOutcome s `shouldBe` Just ScenarioReady
        let at p = [ sdReason d | d ← slsWarnings s, sdPath d ≡ p ]
        -- the decoder's rejection of buildings[0]…
        at "buildings[0].definition" `shouldSatisfy` (not ∘ null)
        -- …and the adapter's rejection of the OVERLAPPING building: it is the third
        -- entry in the file, so it is buildings[2], never buildings[1]
        slsWarnings s `shouldSatisfy` any (\d → sdPath d ≡ "buildings[2]" ∧ sdEffect d ≡ EntryRejected)
        slsWarnings s `shouldSatisfy` all (\d → sdPath d ≢ "buildings[1]")
        -- a nested item of a retained ground item: the second entry in the file
        slsWarnings s `shouldSatisfy` any (T.isPrefixOf "ground_items[1].contents" ∘ sdPath)
        bm ← readIORef (buildingManagerRef (fxEnv (hFx h)))
        Map.elems (bmScenarioRefs bm) `shouldBe` ["first"]

-- | Nested entries: the survivors of a list are numbered by the adapters, so
--   a rejection reported below a rejected sibling must be translated back.
nestedPathSpec ∷ SpecWith H
nestedPathSpec = describe "nested diagnostic paths" $ do

    it "name a nested item by its position in the document when an earlier sibling was rejected and its owner is rejected later" $ \h → do
        path ← writeScenario "nested-paths.yaml" $ doc
            [ "map: {width: 40, height: 40}"
            , "buildings:"
            , "  - {id: first, definition: cargo_hold_S, x: 10, y: 10}"
            , "  - id: second"
            , "    definition: cargo_hold_S"
            , "    x: 10"
            , "    y: 10"
            , "    storage: [{id: ghost, definition: no_such_item}, {id: bar, definition: steel_bar}]" ]
        s ← runLoad h (SourceFile path)
        slsOutcome s `shouldBe` Just ScenarioReady
        let found = [ (sdPath d, sdEffect d) | d ← slsWarnings s ]
        -- the decoder dropped storage[0]; the adapter then rejected the overlapping building
        -- and cascaded to the survivor, which is the item the author wrote SECOND
        found `shouldSatisfy` elem ("buildings[1].storage[0].definition", EntryRejected)
        found `shouldSatisfy` elem ("buildings[1]", EntryRejected)
        found `shouldSatisfy` elem ("buildings[1].storage[1]", CascadeRejected)
        found `shouldSatisfy` notElem ("buildings[1].storage[0]", CascadeRejected)

-- * The simulation has consumed what was installed

simulationSpec ∷ SpecWith H
simulationSpec = describe "the simulation" $ do

    it "has consumed the installed content before the request is ready" $ \h → do
        let env = fxEnv (hFx h)
            lsr = slLoadStatusRef (toSaveLoadCapability env)
        holdSimulation (hOwners h) True
        path ← writeScenario "sim-held.yaml" $ doc
            [ "map: {width: 40, height: 40}"
            , "terrain:"
            , "  - {id: pad, rect: {x0: -3, y0: -3, x1: 3, y1: 3}, material: granite, surface_z: 6}"
            , "units:"
            , "  - {id: a, definition: acolyte, x: 1, y: 1}" ]
        n ← request h (SourceFile path)
        pump ← startPump h
        -- wait for the content to be installed and the service to be waiting on the simulation
        waitSimulationAck h
        Just s ← statusOf h
        slsOutcome s `shouldBe` Nothing
        slsStage s `shouldBe` StageConstructing
        loadInProgress lsr `shouldReturn` True
        um ← readIORef (unitManagerRef env)
        HM.size (umInstances um) `shouldBe` 1
        holdSimulation (hOwners h) False
        done ← await h n 60
        stopPump pump
        slsOutcome done `shouldBe` Just ScenarioReady

    it "a simulation that never consumes ends the request failed, with recovery that cannot complete either, never ready" $ \h → do
        let lsr = slLoadStatusRef (toSaveLoadCapability (fxEnv (hFx h)))
        holdSimulation (hOwners h) True
        armSeamsSimWait h 0.3
        path ← writeScenario "sim-never.yaml" $ doc
            [ "map: {width: 40, height: 40}"
            , "units:"
            , "  - {id: a, definition: acolyte, x: 1, y: 1}" ]
        s ← runLoad h (SourceFile path)
        disarm h
        holdSimulation (hOwners h) False
        case slsOutcome s of
            Just (ScenarioUnsuccessful ctx) → do
                show (fcCause ctx) `shouldSatisfy` T.isInfixOf "did not consume" ∘ T.pack
                fcDetails ctx `shouldSatisfy` any (T.isInfixOf "recovery incomplete")
            other → expectationFailure ("expected an unsuccessful outcome, got " ⧺ show other)
        slsRecovery s `shouldSatisfy` (\r → case r of RecoveryFailed _ → True; _ → False)
        Just ls ← readLoadStatus lsr
        lsPhase ls `shouldBe` LoadReplacementFailed

-- | Wait (bounded) until the held simulation has received the round trip.
waitSimulationAck ∷ H → IO ()
waitSimulationAck h = waitCondition 30 ((> 0) <$> pendingSimulationAcks (hOwners h))
