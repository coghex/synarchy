{-# LANGUAGE Strict #-}
{-# LANGUAGE OverloadedStrings #-}
-- | APPLY-TIME binding discharge for the "Build placement page binding"
--   (#1602) gate: enqueuing is not committing, so a bound placement's
--   binding is re-checked on the WORLD thread — the one that owns page
--   selection — and the insert happens there too, leaving no later
--   drain able to resurrect a rejected placement. Unbound spawns and
--   designations never reach that gate.
--
--   These are fixture-CONSUMING fragments: the engine, the Lua backend
--   and the isolated resource root are the façade's
--   ("Test.Headless.Building.PageBinding").
module Test.Headless.Building.PageBinding.ApplyTime
    ( applyTimeSpec
    ) where

import UPrelude
import Test.Hspec
import Data.IORef (readIORef)

import Building.Command.Types (BuildingCommand(..))
import Building.Types (BuildingId(..))
import Engine.Core.Capability.WorldSim (toWorldSimCapability)
import Engine.Core.State (EngineEnv(..))
import qualified Engine.Core.Queue as Q
import Engine.Scripting.Lua.Types (LuaBackendState)
import Test.Headless.Building.PageBinding.Support
    ( applyQueuedBuildings, clearStubs, designationKeys
    , drainBuildingQueue, occupiedA, occupiedB, pageA, pageB, placeTile
    , placedBuildings, portalName, resetScene, runWorldQueue
    , selectionGen, shedName, terrainZA )
import World.Command.Types (WorldCommand(..))
import World.Construct.Types (ConstructTarget(..))
import World.State.Types (WorldManager(..))
import World.Thread.Command.Cursor.Construct
    (handleWorldDesignateConstructCommand)
import World.Thread.Command.UI
    (handleWorldHideCommand, handleWorldShowCommand)

-- | Enqueuing is not committing. The Lua-side check answers the caller,
--   but page selection belongs to the WORLD thread, so that is where a
--   bound placement's binding is actually discharged — for the spawn via
--   'World.Command.Types.WorldSpawnBoundBuilding', for the designation
--   in its own handler. These examples move the selection AFTER the
--   command was enqueued, through the real handlers, and drive the real
--   dispatchers over it.
applyTimeSpec ∷ SpecWith (EngineEnv, LuaBackendState)
applyTimeSpec =
  describe "the binding is discharged on the thread that owns selection" $ do

    it "the world thread forwards NOTHING for a bound spawn whose \
       \binding went stale after it was enqueued" $ \(env, ls) → do
        _ ← resetScene env
        _ ← clearStubs ls
        gen ← selectionGen env
        logger ← readIORef (loggerRef env)
        let wsc = toWorldSimCapability env
        Q.writeQueue (worldQueue env) $
            WorldSpawnBoundBuilding (BuildingId 9) portalName
                (fst placeTile) (snd placeTile) terrainZA pageA gen
        -- Selection moves AFTER the command was enqueued — exactly the
        -- window a Lua-thread check cannot cover — and it moves through
        -- the REAL handlers, on the same thread that then drains the
        -- command, so the two cannot interleave.
        handleWorldHideCommand wsc logger pageA
        handleWorldShowCommand wsc logger pageB
        runWorldQueue env
        -- The insert happens on THIS thread, so there is nothing left
        -- for a later drain to apply — which is the point: a check that
        -- only authorised a queued write would leave that window open.
        forwarded ← drainBuildingQueue env
        map show forwarded `shouldBe` []
        applyQueuedBuildings env
        placed ← placedBuildings env
        placed `shouldMatchList`
            [ (pageA, fst occupiedA, snd occupiedA)
            , (pageB, fst occupiedB, snd occupiedB) ]

    it "the world thread INSERTS the building itself when the binding \
       \held, with nothing left to drain" $ \(env, ls) → do
        _ ← resetScene env
        _ ← clearStubs ls
        gen ← selectionGen env
        Q.writeQueue (worldQueue env) $
            WorldSpawnBoundBuilding (BuildingId 9) portalName
                (fst placeTile) (snd placeTile) terrainZA pageA gen
        runWorldQueue env
        -- Placed already, before any building-queue drain runs.
        placed ← placedBuildings env
        placed `shouldMatchList`
            [ (pageA, fst occupiedA, snd occupiedA)
            , (pageB, fst occupiedB, snd occupiedB)
            , (pageA, fst placeTile, snd placeTile) ]
        forwarded ← drainBuildingQueue env
        map show forwarded `shouldBe` []

    it "a hide landing between the check and a LATER drain cannot \
       \resurrect the placement, because there is no later drain" $
        \(env, ls) → do
            _ ← resetScene env
            _ ← clearStubs ls
            gen ← selectionGen env
            logger ← readIORef (loggerRef env)
            let wsc = toWorldSimCapability env
            Q.writeQueue (worldQueue env) $
                WorldSpawnBoundBuilding (BuildingId 9) portalName
                    (fst placeTile) (snd placeTile) terrainZA pageA gen
            runWorldQueue env
            -- pageA stays REGISTERED, so the drain's own world-gone
            -- guard would not have caught this: only doing the insert
            -- on the selection-owning thread does.
            handleWorldHideCommand wsc logger pageA
            handleWorldShowCommand wsc logger pageB
            applyQueuedBuildings env
            mgr ← readIORef (worldManagerRef env)
            map fst (wmWorlds mgr) `shouldMatchList` [pageA, pageB]
            placed ← placedBuildings env
            placed `shouldMatchList`
                [ (pageA, fst occupiedA, snd occupiedA)
                , (pageB, fst occupiedB, snd occupiedB)
                , (pageA, fst placeTile, snd placeTile) ]

    it "an UNBOUND spawn never reaches that gate at all" $ \(env, ls) → do
        _ ← resetScene env
        _ ← clearStubs ls
        logger ← readIORef (loggerRef env)
        let wsc = toWorldSimCapability env
        -- Location content-spawning, blueprint staking and power
        -- placement carry no click binding: they go straight to the
        -- building queue and keep landing on their explicit page
        -- however selection moves.
        Q.writeQueue (buildingQueue env) $
            BuildingSpawn (BuildingId 9) portalName
                (fst placeTile) (snd placeTile) terrainZA pageA
        handleWorldHideCommand wsc logger pageA
        handleWorldShowCommand wsc logger pageB
        applyQueuedBuildings env
        placed ← placedBuildings env
        placed `shouldMatchList`
            [ (pageA, fst occupiedA, snd occupiedA)
            , (pageB, fst occupiedB, snd occupiedB)
            , (pageA, fst placeTile, snd placeTile) ]

    it "the world thread writes NO designation for a binding that went \
       \stale after the command was enqueued" $ \(env, ls) → do
        (wsA, wsB) ← resetScene env
        _ ← clearStubs ls
        gen ← selectionGen env
        logger ← readIORef (loggerRef env)
        let wsc = toWorldSimCapability env
        handleWorldHideCommand wsc logger pageA
        handleWorldShowCommand wsc logger pageB
        handleWorldDesignateConstructCommand env logger pageA
            (fst placeTile) (snd placeTile) (fst placeTile) (snd placeTile)
            (CtBuilding shedName) (Just gen)
        designationKeys wsA wsB `shouldReturn` ([], [])

    it "the world thread writes the SAME designation when the binding \
       \held" $ \(env, ls) → do
        (wsA, wsB) ← resetScene env
        _ ← clearStubs ls
        gen ← selectionGen env
        logger ← readIORef (loggerRef env)
        handleWorldDesignateConstructCommand env logger pageA
            (fst placeTile) (snd placeTile) (fst placeTile) (snd placeTile)
            (CtBuilding shedName) (Just gen)
        designationKeys wsA wsB `shouldReturn` ([placeTile], [])

    it "an UNBOUND designation is unaffected by a selection change" $
        \(env, ls) → do
            (wsA, wsB) ← resetScene env
            _ ← clearStubs ls
            logger ← readIORef (loggerRef env)
            let wsc = toWorldSimCapability env
            handleWorldHideCommand wsc logger pageA
            handleWorldShowCommand wsc logger pageB
            handleWorldDesignateConstructCommand env logger pageA
                (fst placeTile) (snd placeTile)
                (fst placeTile) (snd placeTile) (CtBuilding shedName) Nothing
            designationKeys wsA wsB `shouldReturn` ([placeTile], [])
