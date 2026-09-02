{-# LANGUAGE Strict #-}
module Engine.Scripting.Lua.API.Buildings.Progress
    ( buildingSetSpawnRemainingFn
    , buildingGetSpawnRemainingFn
    , buildingConsumeSpawnFn
    , buildingGetBuildProgressFn
    , buildingGetBuildRequiredFn
    , buildingAddBuildProgressFn
    , buildingGetActivityFn
    ) where

import UPrelude
import Engine.Core.Capability.Building
    (BuildingCapability(..), toBuildingCapability)
import Engine.Core.Capability.ContentRegistriesView (toContentRegistriesViewCapability)
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), toWorldSimCapability)
import Building.Knowledge (SeedTrigger(..), seedTriggerFor)
import Building.Knowledge.Live (containerObserver, seedBuiltContainer)
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM
import qualified HsLua as Lua
import Data.IORef (readIORef, atomicModifyIORef')
import Engine.Core.State (EngineEnv)
import Building.Types

-- | building.setSpawnRemaining(bid, n) — initialize the spawn-roster
--   countdown on a building. Called once by Lua spawn sequencers when
--   they first see a built building. Engine-owned because the value
--   needs to survive save/load and chunk eviction without a Lua
--   serializer (Phase 5 work).
buildingSetSpawnRemainingFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
buildingSetSpawnRemainingFn env = do
    idArg ← Lua.tointeger 1
    nArg  ← Lua.tointeger 2
    case (idArg, nArg) of
        (Just n, Just count) → do
            let bid = BuildingId (fromIntegral n)
                rem = max 0 (fromIntegral count)
            Lua.liftIO $ atomicModifyIORef' (bcBuildingManagerRef (toBuildingCapability env)) $ \bm →
                case HM.lookup bid (bmInstances bm) of
                    Nothing → (bm, ())
                    Just inst →
                        let inst' = inst { biSpawnRemaining = rem }
                        in (bm { bmInstances = HM.insert bid inst' (bmInstances bm) }, ())
            Lua.pushboolean True
            return 1
        _ → do
            Lua.pushboolean False
            return 1

-- | building.getSpawnRemaining(bid) → Int. Number of units still to
--   spawn from this building's roster. 0 = done (or building has no
--   spawn sequencer attached). nil if the bid doesn't exist.
buildingGetSpawnRemainingFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
buildingGetSpawnRemainingFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushnil
            return 1
        Just n → do
            let bid = BuildingId (fromIntegral n)
            mRem ← Lua.liftIO $ do
                bm ← readIORef (bcBuildingManagerRef (toBuildingCapability env))
                pure (biSpawnRemaining <$> HM.lookup bid (bmInstances bm))
            case mRem of
                Just r → do
                    Lua.pushinteger (fromIntegral r)
                    return 1
                Nothing → do
                    Lua.pushnil
                    return 1

-- | building.consumeSpawn(bid) → Int (new remaining). Decrement by 1,
--   clamped at 0. Returns the value AFTER the decrement so the caller
--   can branch on "still more to spawn" cheaply.
buildingConsumeSpawnFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
buildingConsumeSpawnFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushnil
            return 1
        Just n → do
            let bid = BuildingId (fromIntegral n)
            mRem ← Lua.liftIO $ atomicModifyIORef' (bcBuildingManagerRef (toBuildingCapability env)) $ \bm →
                case HM.lookup bid (bmInstances bm) of
                    Nothing → (bm, Nothing)
                    Just inst →
                        let newRem = max 0 (biSpawnRemaining inst - 1)
                            inst'  = inst { biSpawnRemaining = newRem }
                        in (bm { bmInstances = HM.insert bid inst' (bmInstances bm) }
                           , Just newRem)
            case mRem of
                Just r → do
                    Lua.pushinteger (fromIntegral r)
                    return 1
                Nothing → do
                    Lua.pushnil
                    return 1

-- | building.getBuildProgress(bid) → Float (accumulated worker-seconds).
--   nil if the bid doesn't exist.
buildingGetBuildProgressFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
buildingGetBuildProgressFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushnil
            return 1
        Just n → do
            let bid = BuildingId (fromIntegral n)
            mProg ← Lua.liftIO $ do
                bm ← readIORef (bcBuildingManagerRef (toBuildingCapability env))
                pure (biBuildProgress <$> HM.lookup bid (bmInstances bm))
            case mProg of
                Just p → do
                    Lua.pushnumber (Lua.Number (realToFrac p))
                    return 1
                Nothing → do
                    Lua.pushnil
                    return 1

-- | building.getBuildRequired(bid) → Float (bdBuildWork from the def).
--   0 means instant-build (legacy time-based, no worker assignment).
--   nil if the bid doesn't exist or its def is missing.
buildingGetBuildRequiredFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
buildingGetBuildRequiredFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushnil
            return 1
        Just n → do
            let bid = BuildingId (fromIntegral n)
            mReq ← Lua.liftIO $ do
                bm ← readIORef (bcBuildingManagerRef (toBuildingCapability env))
                pure $ do
                    inst ← HM.lookup bid (bmInstances bm)
                    def  ← HM.lookup (biDefName inst) (bmDefs bm)
                    pure (bdBuildWork def)
            case mReq of
                Just w → do
                    Lua.pushnumber (Lua.Number (realToFrac w))
                    return 1
                Nothing → do
                    Lua.pushnil
                    return 1

-- | building.addBuildProgress(bid, delta) → Float (new progress) | nil.
--   Clamps the result at [0, ∞). Currently called by the Lua
--   construction tick once per frame with delta = R(workers) * dt.
--
--   #1087: this is also where a storage-capable building's knowledge
--   record is SEEDED as known-empty — the player watched it go up, so
--   reporting a freshly finished cargo hold as never-inspected would be
--   wrong. The trigger is the FIRST crossing of the completion
--   threshold ('Building.Types.currentActivity''s worker-driven arm:
--   'biBuildProgress' reaching 'bdBuildWork'), deliberately NOT
--   @BuildingSpawn@, which creates a worker-built building at zero
--   progress — and deliberately not anything a LOAD can re-trigger, so
--   restoring an already-built container never masquerades as a new
--   construction event. This arm covers exactly
--   'Building.Knowledge.SeedAtBuildCompletion' defs; the INSTANT-BUILT
--   class ('Building.Knowledge.SeedAtSpawn', which never calls this
--   verb at all) is seeded by "Building.Thread.Command" at placement.
--   'Building.Knowledge.Live.seedBuiltContainer'
--   additionally refuses to overwrite an existing record, so a later
--   re-crossing (progress driven back down and up again) cannot erase a
--   real observation.
buildingAddBuildProgressFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
buildingAddBuildProgressFn env = do
    idArg    ← Lua.tointeger 1
    deltaArg ← Lua.tonumber   2
    case (idArg, deltaArg) of
        (Just n, Just (Lua.Number d)) → do
            let bid   = BuildingId (fromIntegral n)
                delta = realToFrac d ∷ Float
            (mNew, justCompleted) ← Lua.liftIO $
                atomicModifyIORef' (bcBuildingManagerRef (toBuildingCapability env)) $ \bm →
                case HM.lookup bid (bmInstances bm) of
                    Nothing → (bm, (Nothing, False))
                    Just inst →
                        let newProg = max 0 (biBuildProgress inst + delta)
                            inst'   = inst { biBuildProgress = newProg }
                            -- The same two facts currentActivity reads:
                            -- a worker-driven def (bdBuildWork > 0) is
                            -- Built exactly once progress reaches it.
                            crossed = case HM.lookup (biDefName inst) (bmDefs bm) of
                                Nothing  → False
                                Just def →
                                    seedTriggerFor def ≡ SeedAtBuildCompletion
                                      ∧ biBuildProgress inst < bdBuildWork def
                                      ∧ newProg ≥ bdBuildWork def
                        in (bm { bmInstances = HM.insert bid inst' (bmInstances bm) }
                           , (Just newProg, crossed))
            when justCompleted $ Lua.liftIO $ void $
                seedBuiltContainer
                    (containerObserver (toBuildingCapability env)
                                       (toWorldSimCapability env)
                                       (toContentRegistriesViewCapability env))
                    bid
            case mNew of
                Just p → do
                    Lua.pushnumber (Lua.Number (realToFrac p))
                    return 1
                Nothing → do
                    Lua.pushnil
                    return 1
        _ → do
            Lua.pushnil
            return 1

-- | building.getActivity(id) — "constructing" while a worker-driven
--   build is short of its target, "appearing" while a zero-work
--   definition is still inside its timed appearance, "built"
--   afterwards (#2080 split the old overloaded "appearing"). nil if the
--   building doesn't exist. Derived, with no stored state to query.
--
--   Code that only asks whether a building is OPERABLE keeps comparing
--   against "built": both "constructing" and "appearing" are not-built.
buildingGetActivityFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
buildingGetActivityFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushnil
            return 1
        Just n → do
            let bid = BuildingId (fromIntegral n)
            mLabel ← Lua.liftIO $ do
                bm ← readIORef (bcBuildingManagerRef (toBuildingCapability env))
                case HM.lookup bid (bmInstances bm) of
                    Nothing → pure Nothing
                    Just inst → case HM.lookup (biDefName inst) (bmDefs bm) of
                        Nothing  → pure Nothing
                        Just def → do
                            -- Game-clock matches biSpawnedAt, so the
                            -- Appearing→Built transition of a zero-work
                            -- def freezes on pause and doesn't drift
                            -- against POSIX. (Constructing→Built is
                            -- driven by progress, not by any clock.)
                            now ← readIORef (wsGameTimeRef (toWorldSimCapability env))
                            pure $ Just
                                (buildingActivityLabel
                                     (currentActivity now inst def) ∷ Text)
            case mLabel of
                Just lbl → do
                    Lua.pushstring (TE.encodeUtf8 lbl)
                    return 1
                Nothing → do
                    Lua.pushnil
                    return 1
