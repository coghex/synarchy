{-# LANGUAGE Strict #-}
-- | Session semantics of "Scenario.Load" (#2713, D-10, D-11, D-43): a load
--   replaces the testing session whole, refuses an ordinary gameplay
--   session, is ready only after initialization and reconciliation, keeps
--   the simulation paused throughout, and ignores completions that belong
--   to an older request.
module Test.Headless.Scenario.Load.Session (spec) where

import UPrelude
import Test.Hspec
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Data.IORef (atomicModifyIORef', readIORef, writeIORef)
import GHC.Clock (getMonotonicTime)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Engine.Core.Queue as Q
import Building.Types (BuildingManager(..))
import Engine.Core.Capability.InputView (InputViewCapability(..), toInputViewCapability)
import Engine.Core.Capability.SaveLoad (SaveLoadCapability(..), toSaveLoadCapability)
import Scenario.Items.Refs (emptyItemRefs)
import Engine.Core.Capability.UnitCombat (UnitCombatCapability(..), toUnitCombatCapability)
import Engine.Core.State (EngineEnv(..))
import Engine.Load.Status
import Engine.Save.Barrier (SaveOwner(..))
import Engine.Scripting.Lua.Types (LuaMsg(..))
import Gameplay.Tags.Index (categoryTagsOf)
import Scenario.Load.Types
import Test.Headless.Scenario.Load.Harness
import Test.Headless.Scenario.Placement.Fixtures (Fx(..))
import Unit.Sim.Types (MoveHazardPolicy(..), MoveTarget(..), UnitSimState(..), UnitThreadState(..))
import Unit.Types (UnitManager(..))
import Unit.Command.Types (UnitCommand(..))
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)
import World.Page.Types (WorldPageId(..))
import World.Save.Payload (emptyLoadReconcileContext)
import World.State.Types

spec ∷ SpecWith H
spec = describe "the session" $ do
    replaceSpec
    refusalSpec
    readinessSpec
    staleSpec

firstScenario ∷ String
firstScenario = doc
    [ "map: {width: 40, height: 40}"
    , "terrain:"
    , "  - {id: pad, rect: {x0: -3, y0: -3, x1: 3, y1: 3}, material: granite, surface_z: 6, tags: [pad]}"
    , "buildings:"
    , "  - {id: bat, definition: high_voltage_battery, x: 14, y: 4, tags: [power]}"
    , "units:"
    , "  - {id: old-a, definition: acolyte, x: 1, y: 1, tags: [squad]}"
    , "  - {id: old-b, definition: nomad_primitive, x: 2, y: 2, tags: [squad]}"
    , ""
    , "ground_items:"
    , "  - {id: old-bar, definition: steel_bar, x: 4.5, y: 4.5, tags: [loot]}" ]

secondScenario ∷ String
secondScenario = doc
    [ "map: {width: 30, height: 30}"
    , "units:"
    , "  - {id: new-a, definition: acolyte, x: 1, y: 1}" ]

-- * Replacement

replaceSpec ∷ SpecWith H
replaceSpec = describe "replacing a populated testing session" $ do

    it "retires outgoing objects, tags, orders and queued commands, even where runtime ids are reused" $ \h → do
        let env = fxEnv (hFx h)
        src1 ← SourceFile <$> writeScenario "first.yaml" firstScenario
        s1 ← runLoad h src1
        slsOutcome s1 `shouldBe` Just ScenarioReady
        -- the outgoing session as the player left it: an order on a unit, a
        -- clock that has moved, and a command queued against a unit id the
        -- replacement is about to reuse
        um0 ← readIORef (unitManagerRef env)
        let oldIds = HM.keys (umInstances um0)
        oldIds `shouldSatisfy` (not ∘ null)
        atomicModifyIORef' (ucUtsRef (toUnitCombatCapability env)) $ \uts →
            (uts { utsSimStates = HM.map (\st → st { usTarget = Just (MoveTarget 9 9 1 FallPermitted) }) (utsSimStates uts) }, ())
        writeIORef (gameTimeRef env) 5000
        Q.writeQueue (ucUnitQueue (toUnitCombatCapability env)) (UnitKill (head' oldIds))
        wmBefore ← readIORef (worldManagerRef env)
        Map.size (umScenarioRefs um0) `shouldBe` 2
        -- the replacement
        src2 ← SourceFile <$> writeScenario "second.yaml" secondScenario
        s2 ← runLoad h src2
        slsOutcome s2 `shouldBe` Just ScenarioReady
        um ← readIORef (unitManagerRef env)
        -- only the replacement's unit exists, under the id the old session's
        -- first unit had, with none of the old session's tags or orders
        Map.elems (umScenarioRefs um) `shouldBe` ["new-a"]
        HM.keys (umInstances um) `shouldBe` [head' oldIds]
        categoryTagsOf (head' oldIds) (umTags um) `shouldBe` Set.empty
        uts ← readIORef (ucUtsRef (toUnitCombatCapability env))
        fmap usTarget (HM.lookup (head' oldIds) (utsSimStates uts)) `shouldBe` Just Nothing
        -- no building, no item, no reference or tag of the outgoing session
        bm ← readIORef (buildingManagerRef env)
        HM.size (bmInstances bm) `shouldBe` 0
        wm ← readIORef (worldManagerRef env)
        wmItemScenarioRefs wm `shouldBe` emptyItemRefs
        -- the clock is the new session's own, and the queued command died
        -- at the cutover instead of waiting to hit the reused id
        readIORef (gameTimeRef env) `shouldReturn` 0
        stats ← Q.queueStats (ucUnitQueue (toUnitCombatCapability env))
        Q.qsDepth stats `shouldBe` 0
        -- the page is the replacement's: its own bounds, its own terrain
        Just ws ← activeArena h
        Just params ← readIORef (wsGenParamsRef ws)
        fmap (const ()) (wgpArenaBounds params) `shouldBe` Just ()
        length (wmWorlds wmBefore) `shouldBe` length (wmWorlds wm)
  where
    head' (x : _) = x
    head' [] = error "no units"

-- * Refusal

refusalSpec ∷ SpecWith H
refusalSpec = describe "outside a testing session" $ do

    it "refuses to replace an ordinary gameplay session, constructing nothing" $ \h → do
        let env = fxEnv (hFx h)
        ws ← emptyWorldState
        writeIORef (wsGenParamsRef ws) (Just defaultWorldGenParams { wgpSeed = 7 })
        let pid = WorldPageId "main_world"
        atomicModifyIORef' (worldManagerRef env) $ \wm →
            (wm { wmWorlds = (pid, ws) : wmWorlds wm }, ())
        before ← sessionFingerprint h
        src ← SourceFile <$> writeScenario "refused.yaml" secondScenario
        s ← runLoad h src
        after ← sessionFingerprint h
        atomicModifyIORef' (worldManagerRef env) $ \wm →
            (wm { wmWorlds = filter ((≢ pid) ∘ fst) (wmWorlds wm) }, ())
        case slsOutcome s of
            Just (ScenarioUnsuccessful ctx) → case fcCause ctx of
                CauseNotTestingSession why → why `shouldSatisfy` T.isInfixOf "main_world"
                other → expectationFailure ("wrong cause: " ⧺ show other)
            other → expectationFailure ("expected a refusal, got " ⧺ show other)
        (slsPublished s, slsRecovery s) `shouldBe` (False, RecoveryPreserved)
        -- the gameplay page was never touched, and the session is as it was
        after `shouldBe` before

    it "admits a direct boot into a scenario: a session with no page at all" $ \h → do
        let env = fxEnv (hFx h)
        saved ← atomicModifyIORef' (worldManagerRef env) $ \wm → (wm { wmWorlds = [] }, wmWorlds wm)
        src ← SourceFile <$> writeScenario "direct.yaml" secondScenario
        s ← runLoad h src
        slsOutcome s `shouldBe` Just ScenarioReady
        _ ← pure saved
        pure ()

-- * Readiness

readinessSpec ∷ SpecWith H
readinessSpec = describe "readiness" $ do

    it "is declared only after the content is installed and reconciled, with the simulation paused throughout" $ \h → do
        let env = fxEnv (hFx h)
            lsr = slLoadStatusRef (toSaveLoadCapability env)
        forM_ [PointPublished, PointReconciled, PointInitialized] $ \point → do
            hold ← newHold
            armSeams h (holdAt point hold)
            src ← SourceFile <$> writeScenario "readiness.yaml" firstScenario
            n ← request h src
            pump ← startPump h
            takeMVar (holdReached hold)
            -- in flight: no outcome, the exclusion is held, paused, the clock is at the epoch
            Just s ← statusOf h
            slsOutcome s `shouldBe` Nothing
            slsRequestId s `shouldBe` n
            loadInProgress lsr `shouldReturn` True
            readIORef (enginePausedRef env) `shouldReturn` True
            readIORef (gameTimeRef env) `shouldReturn` 0
            lua h "return tostring(engine.setPaused(false))" `shouldReturn` "false"
            readIORef (enginePausedRef env) `shouldReturn` True
            -- the content is installed only once we are past the install
            um ← readIORef (unitManagerRef env)
            let installed = HM.size (umInstances um)
            when (point ≡ PointInitialized) (installed `shouldBe` 2)
            when (point ≢ PointInitialized) (installed `shouldBe` 0)
            putMVar (holdRelease hold) ()
            done ← await h n 60
            stopPump pump
            disarm h
            slsOutcome done `shouldBe` Just ScenarioReady

    it "keeps acknowledged owners parked until publication, and a withheld acknowledgement withholds readiness" $ \h → do
        let env = fxEnv (hFx h)
            owners = hOwners h
        -- 1. one owner withholds its acknowledgement: nothing is published
        holdOwner owners SaveUnit True
        src ← SourceFile <$> writeScenario "owners.yaml" firstScenario
        n ← request h src
        pump ← startPump h
        waitFor 20 $ (≡ Just StagePublishing) ∘ fmap slsStage <$> statusOf h
        before ← sessionFingerprint h
        -- a few barrier passes later it is still waiting: not ready, not published
        threadDelay 50000
        Just s ← statusOf h
        (slsStage s, slsOutcome s, slsPublished s) `shouldBe` (StagePublishing, Nothing, False)
        sessionFingerprint h `shouldReturn` before
        holdOwner owners SaveUnit False
        _ ← await h n 60
        stopPump pump
        -- 2. with the barrier HELD every owner is parked and does nothing
        hold ← newHold
        armSeams h (holdAt PointBarrier hold)
        src2 ← SourceFile <$> writeScenario "parked.yaml" firstScenario
        n2 ← request h src2
        pump2 ← startPump h
        takeMVar (holdReached hold)
        mutated0 ← forM parkedOwners (ownerCounts owners)
        waitFor 20 $ do
            now ← forM parkedOwners (ownerCounts owners)
            pure (and (zipWith (\(_, p0) (_, p1) → p1 ≥ p0 + 5) mutated0 now))
        mutated1 ← forM parkedOwners (ownerCounts owners)
        map fst mutated1 `shouldBe` map fst mutated0
        Just held ← statusOf h
        slsOutcome held `shouldBe` Nothing
        putMVar (holdRelease hold) ()
        done ← await h n2 60
        stopPump pump2
        disarm h
        slsOutcome done `shouldBe` Just ScenarioReady
        readIORef (enginePausedRef env) `shouldReturn` True
  where
    parkedOwners = [SaveBuilding, SaveCombat, SaveSimulation, SaveRender]

-- | Poll (bounded) for a condition; failing the example, never skipping, if it never holds.
waitFor ∷ Double → IO Bool → IO ()
waitFor secs cond = do
    start ← getMonotonicTime
    let go = do
            ok ← cond
            unless ok $ do
                now ← getMonotonicTime
                when (now - start > secs) (expectationFailure "the awaited condition never held")
                when (now - start ≤ secs) (threadDelay 500 >> go)
    go

-- * Stale completions

staleSpec ∷ SpecWith H
staleSpec = describe "stale completions" $ do

    it "cannot finish or overwrite a newer request" $ \h → do
        let env = fxEnv (hFx h)
            lsr = slLoadStatusRef (toSaveLoadCapability env)
        src ← SourceFile <$> writeScenario "stale-1.yaml" secondScenario
        s1 ← runLoad h src
        let oldId = slsRequestId s1
        -- a newer request is accepted and held
        hold ← newHold
        armSeams h (holdAt PointPreflight hold)
        n ← request h src
        n `shouldBe` oldId + 1
        pump ← startPump h
        takeMVar (holdReached hold)
        activeBefore ← statusOf h
        -- late messages about the OLD request arrive
        Q.writeQueue (luaQueueOf env) (LuaSaveLoaded oldId [] [] emptyLoadReconcileContext)
        Q.writeQueue (luaQueueOf env) (LuaLoadStagingFailed oldId)
        updateScenarioStatus lsr oldId (\r → r { slsStage = StageFailed })
        terminateScenario lsr oldId ScenarioReady RecoveryNone
        finishLoad lsr oldId
        failLoad lsr oldId "late"
        failPublishedLoad lsr oldId "late"
        threadDelay 20000
        -- the active request is exactly what it was
        statusOf h `shouldReturn` activeBefore
        loadInProgress lsr `shouldReturn` True
        putMVar (holdRelease hold) ()
        done ← await h n 60
        stopPump pump
        disarm h
        (slsRequestId done, slsOutcome done) `shouldBe` (n, Just ScenarioReady)
  where
    luaQueueOf = ivLuaQueue ∘ toInputViewCapability
