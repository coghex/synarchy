{-# LANGUAGE Strict, UnicodeSyntax #-}
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
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM
import qualified HsLua as Lua
import Data.IORef (readIORef, atomicModifyIORef')
import Engine.Core.State (EngineEnv(..))
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
            Lua.liftIO $ atomicModifyIORef' (buildingManagerRef env) $ \bm →
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
                bm ← readIORef (buildingManagerRef env)
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
            mRem ← Lua.liftIO $ atomicModifyIORef' (buildingManagerRef env) $ \bm →
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
                bm ← readIORef (buildingManagerRef env)
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
                bm ← readIORef (buildingManagerRef env)
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
buildingAddBuildProgressFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
buildingAddBuildProgressFn env = do
    idArg    ← Lua.tointeger 1
    deltaArg ← Lua.tonumber   2
    case (idArg, deltaArg) of
        (Just n, Just (Lua.Number d)) → do
            let bid   = BuildingId (fromIntegral n)
                delta = realToFrac d ∷ Float
            mNew ← Lua.liftIO $ atomicModifyIORef' (buildingManagerRef env) $ \bm →
                case HM.lookup bid (bmInstances bm) of
                    Nothing → (bm, Nothing)
                    Just inst →
                        let newProg = max 0 (biBuildProgress inst + delta)
                            inst'   = inst { biBuildProgress = newProg }
                        in (bm { bmInstances = HM.insert bid inst' (bmInstances bm) }
                           , Just newProg)
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

-- | building.getActivity(id) — returns "appearing" while the appear
--   animation is still playing, "built" afterwards. nil if the
--   building doesn't exist. Computed from elapsed time vs the def's
--   appearing anim duration (no stored state to query).
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
                bm ← readIORef (buildingManagerRef env)
                case HM.lookup bid (bmInstances bm) of
                    Nothing → pure Nothing
                    Just inst → case HM.lookup (biDefName inst) (bmDefs bm) of
                        Nothing  → pure Nothing
                        Just def → do
                            -- Game-clock matches biSpawnedAt, so the
                            -- Appearing→Built transition freezes on
                            -- pause and doesn't drift against POSIX.
                            now ← readIORef (gameTimeRef env)
                            pure $ Just $ case currentActivity now inst def of
                                Appearing → "appearing" ∷ Text
                                Built     → "built"
            case mLabel of
                Just lbl → do
                    Lua.pushstring (TE.encodeUtf8 lbl)
                    return 1
                Nothing → do
                    Lua.pushnil
                    return 1
