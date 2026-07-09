{-# LANGUAGE Strict, UnicodeSyntax #-}
module Engine.Scripting.Lua.API.Buildings.Spawn
    ( buildingSpawnFn
    , buildingDestroyFn
    , buildingCanPlaceAtFn
    , buildingSetGhostFn
    , buildingClearGhostFn
    ) where

import UPrelude
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM
import qualified HsLua as Lua
import Data.IORef (readIORef, atomicModifyIORef', writeIORef)
import Engine.Core.State (EngineEnv(..), activeWorldPage)
import World.Page.Types (WorldPageId(..))
import qualified Engine.Core.Queue as Q
import Building.Types
import Building.Command.Types (BuildingCommand(..))
import Building.Placement (canPlaceAt, PlacementResult(..))
import Unit.Pathing.Cost (lookupTerrainZ)
import World.Types (WorldManager(..), WorldState(..))
import World.Tile.Types (WorldTileData)

-- * Spawn / destroy

-- | building.spawn(defName, gx, gy [, pageId]) — returns the new
--   building id on success, nil otherwise (unknown def, placement
--   invalid). Placement is validated server-side too so Lua scripts
--   can't accidentally place into water etc. An explicit pageId (slot
--   4) pins the spawn — AND the occupancy/terrain-Z check — to that
--   live page (even hidden) instead of the active world: location
--   content-spawning (#90) passes its own page so a building lands (and
--   validates) on the page its location is on, not whichever happens to
--   be visible. Omitted → the active world, as before (#76).
buildingSpawnFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
buildingSpawnFn env = do
    nameArg ← Lua.tostring 1
    xArg    ← Lua.tointeger 2
    yArg    ← Lua.tointeger 3
    pageArg ← Lua.tostring 4
    case (nameArg, xArg, yArg) of
        (Just nameBS, Just x, Just y) → do
            let defName = TE.decodeUtf8 nameBS
                gx      = fromIntegral x
                gy      = fromIntegral y
            mBid ← Lua.liftIO $ do
                bm ← readIORef (buildingManagerRef env)
                mTarget ← case pageArg of
                    Just pidBS → do
                        let pid = WorldPageId (TE.decodeUtf8 pidBS)
                        wm ← readIORef (worldManagerRef env)
                        pure $ (\ws → (pid, ws)) <$> lookup pid (wmWorlds wm)
                    Nothing → activeWorldPage env
                case (HM.lookup defName (bmDefs bm), mTarget) of
                    (Just def, Just (pid, ws)) → do
                        wtd ← readIORef (wsTilesRef ws)
                        case canPlaceAt
                                (bm { bmInstances =
                                        buildingsOnPage pid (bmInstances bm) })
                                wtd def gx gy of
                            NotPlaceable _ → pure Nothing
                            Placeable → do
                                let gz = floorZAt wtd gx gy
                                bid ← atomicModifyIORef'
                                        (buildingManagerRef env) $ \bm' →
                                            let (bid', bm'') = nextBuildingId bm'
                                            in (bm'', bid')
                                Q.writeQueue (buildingQueue env) $
                                    BuildingSpawn bid defName gx gy gz pid
                                pure (Just bid)
                    _ → pure Nothing
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

buildingDestroyFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
buildingDestroyFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushboolean False
            return 1
        Just n → do
            let bid = BuildingId (fromIntegral n)
            Lua.liftIO $ Q.writeQueue (buildingQueue env) $ BuildingDestroy bid
            Lua.pushboolean True
            return 1

-- * Placement check

-- | building.canPlaceAt(defName, gx, gy) — returns @(true, nil)@ on
--   success or @(false, reason)@ on rejection. Cheap to call every
--   frame from the build tool's ghost preview update.
buildingCanPlaceAtFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
buildingCanPlaceAtFn env = do
    nameArg ← Lua.tostring 1
    xArg    ← Lua.tointeger 2
    yArg    ← Lua.tointeger 3
    case (nameArg, xArg, yArg) of
        (Just nameBS, Just x, Just y) → do
            let defName = TE.decodeUtf8 nameBS
                gx      = fromIntegral x
                gy      = fromIntegral y
            result ← Lua.liftIO $ do
                bm ← readIORef (buildingManagerRef env)
                mActive ← activeWorldPage env
                -- Occupancy is checked only against the ACTIVE world's
                -- buildings — a building in another world must not block
                -- placement here (#76).
                case (HM.lookup defName (bmDefs bm), mActive) of
                    (Nothing, _) → pure (NotPlaceable "unknown building")
                    (_, Nothing) → pure (NotPlaceable "no active world")
                    (Just def, Just (pid, _)) → do
                        mWtd ← snapshotVisibleWorldTiles env
                        case mWtd of
                            Nothing  → pure (NotPlaceable "no world loaded")
                            Just wtd → pure (canPlaceAt
                                (bm { bmInstances =
                                        buildingsOnPage pid (bmInstances bm) })
                                wtd def gx gy)
            case result of
                Placeable → do
                    Lua.pushboolean True
                    Lua.pushnil
                    return 2
                NotPlaceable reason → do
                    Lua.pushboolean False
                    Lua.pushstring (TE.encodeUtf8 reason)
                    return 2
        _ → do
            Lua.pushboolean False
            Lua.pushstring "bad arguments"
            return 2

-- * Ghost preview

-- | building.setGhost(defName, gx, gy, valid) — install or update the
--   single ghost preview slot. Cleared via clearGhost or by passing
--   an empty string as defName.
buildingSetGhostFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
buildingSetGhostFn env = do
    nameArg  ← Lua.tostring 1
    xArg     ← Lua.tointeger 2
    yArg     ← Lua.tointeger 3
    validArg ← Lua.toboolean 4
    case (nameArg, xArg, yArg) of
        (Just nameBS, Just x, Just y) → do
            let name = TE.decodeUtf8 nameBS
                gx   = fromIntegral x
                gy   = fromIntegral y
            Lua.liftIO $ do
                -- Sample the terrain Z at the ghost tile so the
                -- render pass can elevate the preview to where the
                -- building will actually land. Matches the spawn
                -- path's `floorZAt`; falls back to 0 if the chunk
                -- isn't loaded.
                gz ← do
                    mWtd ← snapshotVisibleWorldTiles env
                    case mWtd of
                        Just wtd → pure (floorZAt wtd gx gy)
                        Nothing  → pure 0
                writeIORef (buildingGhostRef env) $ Just BuildingGhost
                    { bgDefName = name
                    , bgGridX   = gx
                    , bgGridY   = gy
                    , bgGridZ   = gz
                    , bgValid   = validArg
                    }
            Lua.pushboolean True
            return 1
        _ → do
            Lua.pushboolean False
            return 1

buildingClearGhostFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
buildingClearGhostFn env = do
    Lua.liftIO $ writeIORef (buildingGhostRef env) Nothing
    Lua.pushboolean True
    return 1

-- * Helpers

snapshotVisibleWorldTiles ∷ EngineEnv → IO (Maybe WorldTileData)
snapshotVisibleWorldTiles env = do
    wm ← readIORef (worldManagerRef env)
    case wmVisible wm of
        []          → pure Nothing
        (pageId:_)  → case lookup pageId (wmWorlds wm) of
            Nothing → pure Nothing
            Just ws → Just <$> readIORef (wsTilesRef ws)

-- | Terrain Z at the anchor tile. Falls back to 0 if the chunk isn't
--   loaded — shouldn't happen since canPlaceAt already verified, but
--   defensive.
floorZAt ∷ WorldTileData → Int → Int → Int
floorZAt wtd gx gy = case lookupTerrainZ wtd gx gy of
    Just z  → z
    Nothing → 0
