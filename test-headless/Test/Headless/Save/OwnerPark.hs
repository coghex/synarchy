-- | Issue #2221: what the post-final-acknowledgement park actually buys
--   at the OWNER-LOOP level, driven through the real tick entry points
--   ("Unit.Thread.unitTickWith", "World.Thread.worldTickWith") against a
--   real barrier rather than a re-implementation of either.
--
--   "Test.Headless.Save.Barrier" pins the primitive: an owner that has
--   acknowledged the final quiescence pass reads its gate as locked
--   while the global capture lock is still open. What is pinned HERE is
--   the consequence the issue is actually about — that a mutation such
--   an owner would otherwise have made in that window does not happen,
--   and that the two transaction kinds dispose of it differently:
--
--     * a SAVE leaves the attempted work queued, so it is absent from
--       the snapshot that transaction captures and simply runs once the
--       barrier releases; while
--     * a LOAD publish DISCARDS it ('discardStaleQueues'), because it
--       was queued against the session being replaced and must never
--       reach the replacement.
--
--   Contract: @docs/engine_contracts.md@ §Save\/load transaction.
module Test.Headless.Save.OwnerPark (spec) where

import UPrelude
import Test.Hspec
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Core.Capability.UnitCombat
    (UnitCombatCapability(..), toUnitCombatCapability)
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), toWorldSimCapability)
import qualified Engine.Core.Queue as Q
import Engine.Core.Monad (EngineM', runEngineM)
import Engine.Core.State
    ( EngineEnv, loadStatusRef, loggerRef, luaToEngineQueue, saveBarrierRef
    , unitManagerRef )
import Engine.Load.Status (beginLoad, loadInProgress)
import Engine.Loop.Headless (headlessMode)
import Engine.Loop.Mode (runGatedByCaptureLock)
import Engine.Save.Barrier
    ( SaveOwner(..), acknowledgeSave, beginSave, captureLocked, failSave
    , ownersGated, ownerGated, reachSnapshot, releaseCaptureLock )
import Engine.Scripting.Lua.Types (LuaLogLevel(..), LuaToEngineMsg(..))
import Test.Headless.Harness (withHeadlessEngineNoWorld)
import Unit.Command.Types (UnitCommand(..))
import Unit.Direction (Direction(..))
import Unit.Faction (Faction(..))
import Unit.Thread
    (UnitTickSeams(..), productionUnitTickSeams, unitTickWith)
import Unit.Types
import World.Command.Types (WorldCommand(..))
import World.Load.Publish (discardStaleQueues)
import World.Page.Types (WorldPageId(..))
import World.State.Types
    (WorldManager(..), WorldState(..), emptyWorldManager, emptyWorldState)
import World.Thread (worldTickWith)
import World.Time.Types (WorldDate(..), WorldTime(..))

-- * Fixtures

parkPage ∷ WorldPageId
parkPage = WorldPageId "owner_park"

parkUnit ∷ UnitId
parkUnit = UnitId 1

-- | One live unit, so a 'UnitClearAll' that runs is unmistakable (the
--   manager empties) and one that does not is equally unmistakable.
parkUnitInstance ∷ UnitInstance
parkUnitInstance = UnitInstance
    { uiDefName = "owner_park_test", uiName = "", uiPage = parkPage
    , uiTexture = TextureHandle 0, uiDirSprites = Map.empty
    , uiBaseWidth = 0, uiGridX = 0, uiGridY = 0, uiGridZ = 0
    , uiRealZ = 0, uiFacing = DirS
    , uiCurrentAnim = "", uiAnimStart = 0, uiAnimReverse = False
    , uiActivity = "idle", uiPose = "standing", uiAnimStride = 1
    , uiStats = HM.empty, uiModifiers = HM.empty, uiSkills = HM.empty
    , uiKnowledge = HM.empty, uiInventory = [], uiEquipment = HM.empty
    , uiAccessories = [], uiFactionId = FactionPlayer, uiWounds = []
    , uiScars = [], uiImmuneResponse = 0, uiImmunities = HM.empty
    , uiBlood = 5.0, uiLastAttackerUid = Nothing, uiLastAttackerAt = 0
    , uiAnimOverride = "", uiFrozen = False, uiForceLoop = False
    , uiClimbDest = Nothing, uiTrailState = Nothing
    }

-- | The owner set a real save registers (see
--   'Engine.Scripting.Lua.API.Save.saveOwnerSet'), minus the input
--   thread this harness never starts. 'SaveLua' is the transaction
--   driver and is included exactly as production includes it.
parkOwners ∷ [SaveOwner]
parkOwners = [SaveLua, SaveWorld, SaveUnit, SaveBuilding, SaveCombat, SaveSimulation]

-- | Begin a transaction and drive it to the START of its final
--   quiescence pass, then acknowledge that final pass for @acked@ only.
--   Every owner NOT in @acked@ is still outstanding, so the boundary is
--   deliberately unreachable and the global capture lock stays open —
--   which is the entire window this issue is about.
armFinalPassOf ∷ EngineEnv → [SaveOwner] → [SaveOwner] → IO Int
armFinalPassOf env owners acked = do
    let barrier = saveBarrierRef env
    Right n ← beginSave barrier (Set.fromList owners)
    -- Two complete passes: acknowledgeSave resets the acknowledgement
    -- set at each pass boundary, so this leaves the transaction at
    -- @requiredQuiescencePasses - 1@ with nobody parked.
    mapM_ (acknowledgeSave barrier n) (owners <> owners)
    mapM_ (acknowledgeSave barrier n) acked
    pure n

armFinalPass ∷ EngineEnv → [SaveOwner] → IO Int
armFinalPass env = armFinalPassOf env parkOwners

-- | The owner set a real LOAD registers
--   ('Engine.Scripting.Lua.Thread.Dispatch.handleLoadStaged'): every
--   save owner plus 'SaveRender', which a plain save never lists.
loadOwners ∷ [SaveOwner]
loadOwners = SaveRender : parkOwners

-- | One render/headless-loop gated tick — the single shared save-barrier
--   handshake all three main loops run.
runRenderTick ∷ EngineEnv → IO ()
runRenderTick env = do
    let action ∷ EngineM' ()
        action = runGatedByCaptureLock headlessMode env
    _ ← runEngineM action env pure
    pure ()

queuedLuaToEngine ∷ EngineEnv → IO Int
queuedLuaToEngine env = do
    pending ← Q.flushQueue (luaToEngineQueue env)
    mapM_ (Q.writeQueue (luaToEngineQueue env)) pending
    pure (length pending)

-- | A unit tick with no real clock, movement or sleep: this module
--   asserts what the tick's GATE let through, never its timing.
inertSeams ∷ IORef [Double] → UnitTickSeams
inertSeams samples = productionUnitTickSeams
    { tickClock    = do
        current ← readIORef samples
        case current of
            (x:rest) → writeIORef samples rest >> pure x
            []       → pure 0
    , tickMovement = \_ _ _ → pure ()
    , tickSleep    = \_ → pure ()
    }

runUnitTick ∷ EngineEnv → IO ()
runUnitTick env = do
    samples ← newIORef [0, 0]
    lastRef ← newIORef 0
    _ ← unitTickWith (inertSeams samples) env lastRef
                     (ucUtsRef (toUnitCombatCapability env))
    pure ()

installParkFixture ∷ EngineEnv → IO ()
installParkFixture env = do
    writeIORef (unitManagerRef env) emptyUnitManager
        { umInstances = HM.fromList [(parkUnit, parkUnitInstance)]
        , umNextId = 2 }
    writeIORef (wsEnginePausedRef (toWorldSimCapability env)) True

liveUnits ∷ EngineEnv → IO Int
liveUnits env = HM.size ∘ umInstances <$> readIORef (unitManagerRef env)

queuedUnitCommands ∷ EngineEnv → IO Int
queuedUnitCommands env = do
    pending ← Q.flushQueue (ucUnitQueue (toUnitCombatCapability env))
    mapM_ (Q.writeQueue (ucUnitQueue (toUnitCombatCapability env))) pending
    pure (length pending)

-- * Spec

spec ∷ Spec
spec = describe "save snapshot barrier owner park (issue #2221)" $ do

    it "a unit owner past its final acknowledgement performs no gated \
       \work, even though the global capture lock is still open" $
        withHeadlessEngineNoWorld $ \env → do
        installParkFixture env
        _ ← armFinalPass env [SaveUnit, SaveBuilding]
        -- The whole premise: the boundary does NOT exist yet, because
        -- the world/combat/simulation owners have not acknowledged the
        -- final pass. Before #2221 this is exactly what the unit loop
        -- read, and it read False.
        captureLocked (saveBarrierRef env) `shouldReturn` False
        ownersGated (saveBarrierRef env) [SaveUnit, SaveBuilding]
            `shouldReturn` True

        Q.writeQueue (ucUnitQueue (toUnitCombatCapability env)) UnitClearAll
        runUnitTick env
        liveUnits env          `shouldReturn` 1
        queuedUnitCommands env `shouldReturn` 1

    it "a mutation attempted after the final acknowledgement is absent \
       \from the save that transaction captures, and runs once the \
       \barrier releases" $ withHeadlessEngineNoWorld $ \env → do
        installParkFixture env
        n ← armFinalPass env [SaveUnit, SaveBuilding]
        Q.writeQueue (ucUnitQueue (toUnitCombatCapability env)) UnitClearAll
        runUnitTick env
        -- Nothing mutated, so nothing this owner attempted can be in
        -- the snapshot the initiator is about to capture.
        liveUnits env `shouldReturn` 1

        -- Complete the transaction the way a real save does.
        let barrier = saveBarrierRef env
        mapM_ (acknowledgeSave barrier n)
              [SaveWorld, SaveCombat, SaveSimulation]
        reachSnapshot barrier n
        captureLocked barrier `shouldReturn` True
        runUnitTick env
        liveUnits env `shouldReturn` 1

        -- #758: owners resume at releaseCaptureLock, and the work the
        -- park deferred is still there to run. A save defers; it does
        -- not destroy.
        releaseCaptureLock barrier n
        ownersGated barrier [SaveUnit, SaveBuilding] `shouldReturn` False
        runUnitTick env
        liveUnits env          `shouldReturn` 0
        queuedUnitCommands env `shouldReturn` 0

    it "a mutation attempted after the final acknowledgement does not \
       \survive into a published load: the publish discards it and the \
       \replacement session is never touched" $
        withHeadlessEngineNoWorld $ \env → do
        installParkFixture env
        n ← armFinalPass env [SaveUnit, SaveBuilding]
        Q.writeQueue (ucUnitQueue (toUnitCombatCapability env)) UnitClearAll
        runUnitTick env
        liveUnits env `shouldReturn` 1

        let barrier = saveBarrierRef env
        mapM_ (acknowledgeSave barrier n)
              [SaveWorld, SaveCombat, SaveSimulation]
        reachSnapshot barrier n
        -- What 'World.Load.Publish.publishStagedSession' does inside the
        -- boundary: the old session's queued owner work is discarded
        -- outright, not deferred.
        logger ← readIORef (loggerRef env)
        discardStaleQueues env logger
        releaseCaptureLock barrier n
        runUnitTick env
        -- The stale mutation is gone for good; the (here, unreplaced)
        -- session it was queued against never sees it.
        liveUnits env          `shouldReturn` 1
        queuedUnitCommands env `shouldReturn` 0

    it "a failed transaction unparks the owner with the attempted work \
       \still queued, so nothing is lost when a save aborts" $
        withHeadlessEngineNoWorld $ \env → do
        installParkFixture env
        n ← armFinalPass env [SaveUnit, SaveBuilding]
        Q.writeQueue (ucUnitQueue (toUnitCombatCapability env)) UnitClearAll
        runUnitTick env
        liveUnits env `shouldReturn` 1
        failSave (saveBarrierRef env) n "an owner did not respond"
        ownersGated (saveBarrierRef env) [SaveUnit, SaveBuilding]
            `shouldReturn` False
        runUnitTick env
        liveUnits env `shouldReturn` 0

    it "a world owner past its final acknowledgement stops advancing \
       \the calendar and defers every non-authorized command, keeping \
       \only its authorized-command exception" $
        withHeadlessEngineNoWorld $ \env → do
        let sim = toWorldSimCapability env
        ws ← emptyWorldState
        writeIORef (wsTimeRef ws) (WorldTime 8 0)
        writeIORef (wsDateRef ws) (WorldDate 1 1 1)
        writeIORef (wsTimeScaleRef ws) 60
        writeIORef (wsWorldManagerRef sim) emptyWorldManager
            { wmWorlds = [(parkPage, ws)], wmVisible = [parkPage] }
        writeIORef (wsEnginePausedRef sim) False

        n ← armFinalPass env [SaveWorld]
        captureLocked (saveBarrierRef env) `shouldReturn` False
        ownerGated (saveBarrierRef env) SaveWorld `shouldReturn` True

        Q.writeQueue (wsWorldQueue sim) (WorldSetTime parkPage 23 59)
        lastRef ← newIORef 0
        clock ← newIORef [1.0 ∷ Double]
        let scripted = do
                current ← readIORef clock
                case current of
                    (x:rest) → writeIORef clock rest >> pure x
                    []       → pure 1.0
        _ ← worldTickWith scripted env lastRef
        -- Neither the queued command nor the calendar tick ran: this is
        -- the 'drainInitQueues'/'tickWorldTime'/'updateChunkLoading'
        -- work the issue names as able to enqueue fresh unit/building
        -- work after every owner already drained its three passes.
        readIORef (wsTimeRef ws) `shouldReturn` WorldTime 8 0

        -- Deferred, not discarded: a SAVE keeps the live session, so its
        -- non-authorized commands still run afterwards, in order.
        failSave (saveBarrierRef env) n "an owner did not respond"
        ownerGated (saveBarrierRef env) SaveWorld `shouldReturn` False
        _ ← worldTickWith scripted env lastRef
        readIORef (wsTimeRef ws) `shouldReturn` WorldTime 23 59

    -- Round-1 review: the render owner's park and its DISCARD are gated
    -- on different things on purpose. Parking is reversible — whatever
    -- stays in 'luaToEngineQueue' is still there however the
    -- transaction ends — but the flush is not, and a load that fails
    -- anywhere before the publish leaves the OLD session live and
    -- unchanged by contract. Flushing from the render owner's own final
    -- acknowledgement would have destroyed that session's queued
    -- scene/UI work on every aborted load.
    it "the render owner's park does NOT discard the old session's \
       \queued Lua-to-engine work: an aborted load leaves it intact" $
        withHeadlessEngineNoWorld $ \env → do
        Right _ ← beginLoad (loadStatusRef env) "owner_park_load"
        loadInProgress (loadStatusRef env) `shouldReturn` True
        Q.writeQueue (luaToEngineQueue env) (LuaLog LuaLogInfo "pre-load work")

        n ← armFinalPassOf env loadOwners [SaveRender]
        ownerGated (saveBarrierRef env) SaveRender `shouldReturn` True
        captureLocked (saveBarrierRef env) `shouldReturn` False
        runRenderTick env
        queuedLuaToEngine env `shouldReturn` 1

        -- The abort this protects: another owner never answers, so the
        -- transaction fails with the old session still live.
        failSave (saveBarrierRef env) n "an owner did not respond"
        queuedLuaToEngine env `shouldReturn` 1

    it "the render owner discards the old session's queued work once the \
       \publication boundary is actually reached" $
        withHeadlessEngineNoWorld $ \env → do
        Right _ ← beginLoad (loadStatusRef env) "owner_park_load"
        Q.writeQueue (luaToEngineQueue env) (LuaLog LuaLogInfo "pre-load work")

        n ← armFinalPassOf env loadOwners [SaveRender]
        runRenderTick env
        queuedLuaToEngine env `shouldReturn` 1

        -- Every remaining owner answers and the initiator declares the
        -- boundary: the publish is now committed to, so the stale work
        -- must not survive into the replacement session.
        mapM_ (acknowledgeSave (saveBarrierRef env) n) parkOwners
        reachSnapshot (saveBarrierRef env) n
        captureLocked (saveBarrierRef env) `shouldReturn` True
        runRenderTick env
        queuedLuaToEngine env `shouldReturn` 0

    it "a parked render owner discards nothing when no load is in \
       \flight, so a save never loses queued Lua-to-engine work" $
        withHeadlessEngineNoWorld $ \env → do
        loadInProgress (loadStatusRef env) `shouldReturn` False
        Q.writeQueue (luaToEngineQueue env) (LuaLog LuaLogInfo "save-time work")
        n ← armFinalPassOf env loadOwners [SaveRender]
        runRenderTick env
        mapM_ (acknowledgeSave (saveBarrierRef env) n) parkOwners
        reachSnapshot (saveBarrierRef env) n
        runRenderTick env
        queuedLuaToEngine env `shouldReturn` 1
