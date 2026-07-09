{-# LANGUAGE Strict, UnicodeSyntax #-}
module Engine.Scripting.Lua.API.Buildings.Materials
    ( buildingGetMaterialNeedFn
    , buildingGetMaterialDeliveredFn
    , buildingAreMaterialsSatisfiedFn
    , buildingGetStorageFn
    , buildingGetStorageCapacityFn
    , buildingGetStorageWeightFn
    ) where

import UPrelude
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM
import qualified HsLua as Lua
import Data.IORef (readIORef)
import Engine.Core.State (EngineEnv(..))
import Building.Types
import Item.Types (itemTotalWeight)
import Engine.Scripting.Lua.API.Equipment (pushItemInstance)

-- | building.getMaterialNeed(bid) → table {[defName] = count}. The
--   def-declared requirement map. nil if the bid or its def is gone.
buildingGetMaterialNeedFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
buildingGetMaterialNeedFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushnil
            return 1
        Just n → do
            let bid = BuildingId (fromIntegral n)
            mMat ← Lua.liftIO $ do
                bm ← readIORef (buildingManagerRef env)
                pure $ do
                    inst ← HM.lookup bid (bmInstances bm)
                    def  ← HM.lookup (biDefName inst) (bmDefs bm)
                    pure (bdMaterials def)
            case mMat of
                Nothing → do
                    Lua.pushnil
                    return 1
                Just mats → do
                    Lua.newtable
                    forM_ (HM.toList mats) $ \(name, count) → do
                        Lua.pushinteger (fromIntegral count)
                        Lua.setfield (-2) (Lua.Name (TE.encodeUtf8 name))
                    return 1

-- | building.getMaterialDelivered(bid) → table {[defName] = count}.
--   Just the count per type — the actual ItemInstances stay engine-
--   side until deconstruction recovery surfaces them. nil if the bid
--   doesn't exist.
buildingGetMaterialDeliveredFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
buildingGetMaterialDeliveredFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushnil
            return 1
        Just n → do
            let bid = BuildingId (fromIntegral n)
            mMap ← Lua.liftIO $ do
                bm ← readIORef (buildingManagerRef env)
                pure (biMaterialsDelivered <$> HM.lookup bid (bmInstances bm))
            case mMap of
                Nothing → do
                    Lua.pushnil
                    return 1
                Just mats → do
                    Lua.newtable
                    forM_ (HM.toList mats) $ \(name, items) → do
                        Lua.pushinteger (fromIntegral (length items))
                        Lua.setfield (-2) (Lua.Name (TE.encodeUtf8 name))
                    return 1

-- | building.areMaterialsSatisfied(bid) → bool. True iff for every
--   material type in bdMaterials the delivered count meets the need.
--   Empty bdMaterials (default for legacy defs like the portal)
--   trivially satisfies. nil if the bid is gone.
buildingAreMaterialsSatisfiedFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
buildingAreMaterialsSatisfiedFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushnil
            return 1
        Just n → do
            let bid = BuildingId (fromIntegral n)
            mOk ← Lua.liftIO $ do
                bm ← readIORef (buildingManagerRef env)
                pure $ do
                    inst ← HM.lookup bid (bmInstances bm)
                    def  ← HM.lookup (biDefName inst) (bmDefs bm)
                    let need = bdMaterials def
                        delivered = biMaterialsDelivered inst
                    pure $ all (\(t, n') →
                                  length (HM.lookupDefault [] t delivered) >= n')
                               (HM.toList need)
            case mOk of
                Just ok → do
                    Lua.pushboolean ok
                    return 1
                Nothing → do
                    Lua.pushnil
                    return 1

-- | building.getStorage(bid) → array of item-instance tables (same
--   shape as unit.getInventory entries: defName, displayName, weight,
--   quality / condition when applicable, iconTex, category, …). nil
--   if the bid is unknown. Empty array for buildings with storage
--   capacity but nothing deposited yet.
buildingGetStorageFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
buildingGetStorageFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushnil
            return 1
        Just n → do
            let bid = BuildingId (fromIntegral n)
            mInst ← Lua.liftIO $ do
                bm ← readIORef (buildingManagerRef env)
                pure (HM.lookup bid (bmInstances bm))
            case mInst of
                Nothing → do
                    Lua.pushnil
                    return 1
                Just inst → do
                    itemMgr ← Lua.liftIO $ readIORef (itemManagerRef env)
                    Lua.newtable
                    forM_ (zip [1 ∷ Int ..] (biStorage inst)) $ \(i, item) → do
                        Lua.newtable
                        pushItemInstance item itemMgr
                        Lua.rawseti (-2) (fromIntegral i)
                    return 1

-- | building.getStorageCapacity(bid) → Float kg (def-declared cap).
--   0 means "no storage". nil if the bid is gone.
buildingGetStorageCapacityFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
buildingGetStorageCapacityFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushnil
            return 1
        Just n → do
            let bid = BuildingId (fromIntegral n)
            mCap ← Lua.liftIO $ do
                bm ← readIORef (buildingManagerRef env)
                pure $ do
                    inst ← HM.lookup bid (bmInstances bm)
                    def  ← HM.lookup (biDefName inst) (bmDefs bm)
                    pure (bdStorageCapacity def)
            case mCap of
                Just c → do
                    Lua.pushnumber (Lua.Number (realToFrac c))
                    return 1
                Nothing → do
                    Lua.pushnil
                    return 1

-- | building.getStorageWeight(bid) → Float kg currently stored. Sums
--   the full weight of each ItemInstance in biStorage via
--   'itemTotalWeight' (instance weight + container fill + nested
--   contents) — so a stored canteen counts at its filled weight and a
--   first-aid kit counts its contents, matching the unit-side
--   'unit.getCarryingWeight' convention and the depositToCargo capacity
--   gate. nil if the bid is gone.
buildingGetStorageWeightFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
buildingGetStorageWeightFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushnil
            return 1
        Just n → do
            let bid = BuildingId (fromIntegral n)
            mW ← Lua.liftIO $ do
                bm      ← readIORef (buildingManagerRef env)
                itemMgr ← readIORef (itemManagerRef env)
                pure $ do
                    inst ← HM.lookup bid (bmInstances bm)
                    pure $ sum (map (itemTotalWeight itemMgr) (biStorage inst))
            case mW of
                Just w → do
                    Lua.pushnumber (Lua.Number (realToFrac w))
                    return 1
                Nothing → do
                    Lua.pushnil
                    return 1
