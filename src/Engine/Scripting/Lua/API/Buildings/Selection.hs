{-# LANGUAGE Strict, UnicodeSyntax #-}
module Engine.Scripting.Lua.API.Buildings.Selection
    ( buildingHitTestAtFn
    , buildingSelectFn
    , buildingDeselectFn
    , buildingGetSelectedFn
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified HsLua as Lua
import Data.IORef (readIORef, atomicModifyIORef')
import Engine.Core.State (EngineEnv(..), activeWorldPage)
import Building.Types
import Building.HitTest (hitTestBuildingAt)

-- | building.hitTestAt(pixelX, pixelY) → id|nil
--   Topmost (highest gridZ) building whose sprite quad contains the
--   click. Pixel coords are framebuffer pixels.
buildingHitTestAtFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
buildingHitTestAtFn env = do
    xArg ← Lua.tonumber 1
    yArg ← Lua.tonumber 2
    case (xArg, yArg) of
        (Just (Lua.Number x), Just (Lua.Number y)) → do
            mBid ← Lua.liftIO $ hitTestBuildingAt env (realToFrac x) (realToFrac y)
            case mBid of
                Just (BuildingId n) → do
                    Lua.pushinteger (fromIntegral n)
                    return 1
                Nothing → do
                    Lua.pushnil
                    return 1
        _ → do
            Lua.pushnil
            return 1

-- | building.select(id) — single-select; replaces any prior selection.
buildingSelectFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
buildingSelectFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Just n → do
            let bid = BuildingId (fromIntegral n)
            mActive ← Lua.liftIO $ activeWorldPage env
            Lua.liftIO $ atomicModifyIORef' (buildingManagerRef env) $ \bm →
                -- Only select a building of the ACTIVE world (#76) that
                -- still exists; otherwise leave the previous selection.
                case (mActive, HM.lookup bid (bmInstances bm)) of
                    (Just (pid, _), Just bi) | biPage bi ≡ pid →
                        (bm { bmSelected = Just bid }, ())
                    _ → (bm, ())
        Nothing → pure ()
    return 0

-- | building.deselect() — clear any selection.
buildingDeselectFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
buildingDeselectFn env = do
    Lua.liftIO $ atomicModifyIORef' (buildingManagerRef env) $ \bm →
        (bm { bmSelected = Nothing }, ())
    return 0

-- | building.getSelected() → id|nil. Validates the selection still
--   exists; returns nil if the building was destroyed.
buildingGetSelectedFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
buildingGetSelectedFn env = do
    mBid ← Lua.liftIO $ do
        bm ← readIORef (buildingManagerRef env)
        mActive ← activeWorldPage env
        pure $ case bmSelected bm of
            Just bid
                | Just bi      ← HM.lookup bid (bmInstances bm)
                , Just (pid,_) ← mActive
                , biPage bi ≡ pid → Just bid
            _ → Nothing
    case mBid of
        Just (BuildingId n) → do
            Lua.pushinteger (fromIntegral n)
            return 1
        Nothing → do
            Lua.pushnil
            return 1
