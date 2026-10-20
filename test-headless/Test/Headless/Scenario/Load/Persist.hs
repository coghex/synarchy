{-# LANGUAGE Strict #-}
-- | Ordinary-save integration of a loaded scenario session (#2713, D-52):
--   an ordinary save of the session carries the arena bounds, tags and
--   scenario identities, restores without the source YAML, reapplies no
--   default and rerolls nothing — and a FRESH PROCESS restores the same
--   thing from the saved bytes alone.
--
--   The fresh-process example follows the pattern "Scenario.Units" set: the
--   child entry is a no-op outside a child run, and the parent example
--   spawns the test executable itself, so the process boundary is real in
--   every run of this group.
module Test.Headless.Scenario.Load.Persist (spec, integratedDoc) where

import UPrelude
import Test.Hspec
import Data.IORef (readIORef)
import Data.List (isInfixOf)
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import System.Directory (createDirectoryIfMissing, doesFileExist, getTemporaryDirectory, removeFile)
import System.Environment (getEnvironment, getExecutablePath, lookupEnv)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.Process (CreateProcess(..), proc, readCreateProcessWithExitCode)
import Building.Types
import Engine.Core.State (EngineEnv(..))
import Scenario.Load.Session (arenaPageId)
import Scenario.Load.Types
import Test.Headless.Scenario.Load.Harness
import Test.Headless.Scenario.Placement.Fixtures (Fx(..))
import Test.Headless.Scenario.Placement.Glue
import Unit.Types
import World.Arena.Bounds (ArenaBounds, mkArenaBounds)
import World.Generate.Types (WorldGenParams(..))
import World.State.Types

-- | A scenario with every family and nondefault values, the one the ready
--   example installs.
integratedDoc ∷ String
integratedDoc = doc
    [ "map: {width: 40, height: 40}"
    , "camera: {x: 3, y: 4}"
    , "terrain:"
    , "  - {id: pad, rect: {x0: -3, y0: -3, x1: 3, y1: 3}, material: granite, surface_z: 6, tags: [pad]}"
    , "  - {id: pit, tiles: [[8, 8], [9, 8]], material: sand, surface_z: -2}"
    , "fluids:"
    , "  - {id: pond, rect: {x0: 8, y0: 8, x1: 9, y1: 8}, fluid: lake, surface_z: 4.375}"
    , "flora:"
    , "  - {id: grove, species: oak, x: 12, y: 12, age: 120, tags: [grove]}"
    , "structures:"
    , "  - {id: f1, pack: dungeon_1, piece: floor, x: 5, y: 5}"
    , "buildings:"
    , "  - {id: bat, definition: high_voltage_battery, x: 14, y: 4, power_charge: 2500, tags: [power]}"
    , "  - {id: crate, definition: cargo_hold_S, x: 16, y: 8, storage: [{id: bars, definition: steel_bar}]}"
    , "locations:"
    , "  - {id: ruin, definition: ruin_small, x: 10, y: -10, significant_items: {1: gem}, tags: [site]}"
    , "units:"
    , "  - id: medic"
    , "    definition: acolyte"
    , "    x: 1"
    , "    y: 1"
    , "    tags: [squad]"
    , "    stats: {hydration: 4.0, calories: 600}"
    , "    blood: 3.2"
    , "    wounds: [{part: torso, kind: slash, severity: 0.2}]"
    , "    inventory: [{id: kit, definition: field_toolbox, contents: [{id: spare, definition: steel_bar}]}]"
    , "  - {id: n1, definition: nomad_primitive, x: 10.5, y: -9.5, encounter: ruin}"
    , "ground_items:"
    , "  - {id: gem, definition: processing_unit, x: 10.5, y: -9.5, tags: [loot]}" ]

-- | Everything a capture holds, as one comparable value.
data Snapshot = Snapshot
    { sStructures ∷ Text, sBuildings ∷ Text, sLocations ∷ Text, sUnits ∷ Text, sGround ∷ Text }
    deriving (Show, Eq)

snapshotOf ∷ Captured' → Snapshot
snapshotOf c = Snapshot
    (tshow (cpStructures c)) (tshow (cpBuildings c)) (tshow (cpLocations c))
    (tshow (cpUnits c)) (tshow (cpGround c))

arena40 ∷ ArenaBounds
arena40 = either (error ∘ show) id (mkArenaBounds 40 40)

childVar ∷ String
childVar = "SYNARCHY_SCENARIO_LOAD_FRESH"

spec ∷ SpecWith H
spec = describe "ordinary-save integration" $ do

    it "restores a saved scenario session without the source YAML: bounds, tags, identities and physical values" $ \h → do
        let fx = hFx h
            env = fxEnv fx
        path ← writeScenario "persisted.yaml" integratedDoc
        s ← runLoad h (SourceFile path)
        slsOutcome s `shouldBe` Just ScenarioReady
        Just ws ← activeArena h
        live ← capturePage fx arenaPageId ws arena40
        -- the source changes and then disappears: nothing may need it
        writeFile path "version: 3\nunits: []\n"
        removeFile path
        src ← saveAndStage fx arenaPageId ws
        restored ← capturePageFrom fx src arena40
        snapshotOf restored `shouldBe` snapshotOf live
        -- bounds
        Just liveParams ← readIORef (wsGenParamsRef ws)
        Just restoredParams ← readIORef (wsGenParamsRef (csWorld src))
        wgpArenaBounds restoredParams `shouldBe` wgpArenaBounds liveParams
        wgpArenaBounds restoredParams `shouldBe` Just arena40
        -- identities and tags survive on every owner
        Map.elems (umScenarioRefs (csUnits src)) `shouldMatchList` ["medic", "n1"]
        Map.elems (bmScenarioRefs (csBuildings src)) `shouldMatchList` ["bat", "crate"]
        -- physical values are the saved ones: nothing was rerolled, no default
        -- reapplied (the omitted stats are the ones the load rolled once)
        um ← readIORef (unitManagerRef env)
        forM_ (HM.toList (umInstances um)) $ \(uid, u) → do
            let r = HM.lookup uid (umInstances (csUnits src))
            fmap uiStats r `shouldBe` Just (uiStats u)
            fmap uiSkills r `shouldBe` Just (uiSkills u)
            fmap uiBlood r `shouldBe` Just (uiBlood u)
            fmap uiInventory r `shouldBe` Just (uiInventory u)

    it "a fresh process restores the same session from the saved bytes alone" $ \h → do
        mChild ← lookupEnv childVar
        when (isNothing mChild) $ do
            let fx = hFx h
            path ← writeScenario "fresh.yaml" integratedDoc
            s ← runLoad h (SourceFile path)
            slsOutcome s `shouldBe` Just ScenarioReady
            Just ws ← activeArena h
            live ← capturePage fx arenaPageId ws arena40
            bytes ← encodeSession fx arenaPageId ws
            tmp ← getTemporaryDirectory
            let dir = tmp </> "synarchy-scenario-load-fresh"
            createDirectoryIfMissing True dir
            BS.writeFile (dir </> "session.bin") bytes
            writeFile (dir </> "expected.txt") (show (snapshotOf live))
            -- the source is gone before the other process starts
            removeFile path
            exe ← getExecutablePath
            parentEnv ← getEnvironment
            let args = ["--match", "Scenario.Load/ordinary-save integration/fresh process child entry"]
            (code, so, se) ← readCreateProcessWithExitCode
                (proc exe args) { env = Just ((childVar, dir) : filter ((≢ childVar) ∘ fst) parentEnv) } ""
            putStrLn ("fresh-process command: " ⧺ exe ⧺ " " ⧺ unwords args ⧺ "  [" ⧺ childVar ⧺ "=" ⧺ dir ⧺ "]")
            putStrLn ("fresh-process exit: " ⧺ show code)
            putStrLn ("fresh-process output tail: " ⧺ lastLines 6 so)
            when (code ≢ ExitSuccess) $
                expectationFailure ("the fresh process failed:\n" ⧺ so ⧺ se)
            so `shouldSatisfy` ("1 example, 0 failures" `isInfixOf`)
            -- it did restore: the child writes what it read back
            restored ← readFile (dir </> "restored.txt")
            restored `shouldBe` show (snapshotOf live)
            doesFileExist path `shouldReturn` False

    it "fresh process child entry (a no-op outside a child run)" $ \h → do
        mChild ← lookupEnv childVar
        forM_ mChild $ \dir → do
            let fx = hFx h
            bytes ← BS.readFile (dir </> "session.bin")
            expected ← readFile (dir </> "expected.txt")
            src ← stageEnvelope fx arenaPageId bytes
            restored ← capturePageFrom fx src arena40
            writeFile (dir </> "restored.txt") (show (snapshotOf restored))
            show (snapshotOf restored) `shouldBe` expected
            -- this process never saw the source
            doesFileExist (dir </> "fresh.yaml") `shouldReturn` False
            Just restoredParams ← readIORef (wsGenParamsRef (csWorld src))
            wgpArenaBounds restoredParams `shouldBe` Just arena40
            Map.elems (umScenarioRefs (csUnits src)) `shouldMatchList` ["medic", "n1"]
  where
    lastLines n = unlines ∘ reverse ∘ take n ∘ reverse ∘ lines
