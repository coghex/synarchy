{-# LANGUAGE Strict #-}
module Engine.Scripting.Lua.API.Buildings.Spawn
    ( buildingSpawnFn
    , buildingDestroyFn
    , buildingCanPlaceAtFn
    , buildingRemoteCheckFn
    , buildingSetGhostFn
    , buildingClearGhostFn
    ) where

import UPrelude
import Engine.Core.Capability.Building
    (BuildingCapability(..), toBuildingCapability)
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), toWorldSimCapability)
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM
import qualified HsLua as Lua
import Data.IORef (readIORef, atomicModifyIORef', writeIORef)
import Engine.Core.State (EngineEnv, activeWorldPageFrom)
import World.Page.Types (WorldPageId(..))
import qualified Engine.Core.Queue as Q
import Building.Types
import Building.Command.Types (BuildingCommand(..))
import Building.Placement
    ( canPlaceAt, PlacementResult(..), RemoteCheck(..), remoteCheck, isRemote
    )
import Location.Bounds (remotePortalThresholdTiles)
import Unit.Pathing.Cost (lookupTerrainZ)
import World.Types (WorldManager(..), WorldState(..), WorldGenParams(..))
import World.Generate.Coordinates (canonicalTile)
import World.Tile.Types (WorldTileData)
import Location.Instance (emptyLocationInstances)

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
            let defName = TE.decodeUtf8Lenient nameBS
                gx      = fromIntegral x
                gy      = fromIntegral y
            mBid ← Lua.liftIO $ do
                bm ← readIORef (bcBuildingManagerRef (toBuildingCapability env))
                mTarget ← case pageArg of
                    Just pidBS → do
                        let pid = WorldPageId (TE.decodeUtf8Lenient pidBS)
                        wm ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
                        pure $ (\ws → (pid, ws)) <$> lookup pid (wmWorlds wm)
                    Nothing → activeWorldPageFrom (wsWorldManagerRef (toWorldSimCapability env))
                case (HM.lookup defName (bmDefs bm), mTarget) of
                    (Just def, Just (pid, ws)) → do
                        wtd ← readIORef (wsTilesRef ws)
                        mParams ← readIORef (wsGenParamsRef ws)
                        let locInstances = maybe emptyLocationInstances
                                                 wgpLocationInstances mParams
                            worldSizeChunks = maybe 0 wgpWorldSize mParams
                            -- #1175: resolve the anchor into the stored
                            -- frame BEFORE validating, reading terrain z, or
                            -- recording the spawn. A CtBuilding construct job
                            -- restored from a pre-#1175 save can hold an
                            -- alias, and validating that raw reports the
                            -- (loaded) canonical chunk as missing — the AI
                            -- then cancels a perfectly good designation.
                            -- Identity away from the seam.
                            (cgx, cgy) = canonicalTile worldSizeChunks gx gy
                        case canPlaceAt
                                (bm { bmInstances =
                                        buildingsOnPage pid (bmInstances bm) })
                                wtd locInstances worldSizeChunks def cgx cgy of
                            NotPlaceable _ → pure Nothing
                            Placeable → do
                                let gz = floorZAt worldSizeChunks wtd cgx cgy
                                bid ← atomicModifyIORef'
                                        (bcBuildingManagerRef (toBuildingCapability env)) $ \bm' →
                                            let (bid', bm'') = nextBuildingId bm'
                                            in (bm'', bid')
                                Q.writeQueue (bcBuildingQueue (toBuildingCapability env)) $
                                    BuildingSpawn bid defName cgx cgy gz pid
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
            Lua.liftIO $ Q.writeQueue (bcBuildingQueue (toBuildingCapability env)) $ BuildingDestroy bid
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
            let defName = TE.decodeUtf8Lenient nameBS
                gx      = fromIntegral x
                gy      = fromIntegral y
            result ← Lua.liftIO $ do
                bm ← readIORef (bcBuildingManagerRef (toBuildingCapability env))
                mActive ← activeWorldPageFrom (wsWorldManagerRef (toWorldSimCapability env))
                -- Occupancy is checked only against the ACTIVE world's
                -- buildings — a building in another world must not block
                -- placement here (#76).
                case (HM.lookup defName (bmDefs bm), mActive) of
                    (Nothing, _) → pure (NotPlaceable "unknown building")
                    (_, Nothing) → pure (NotPlaceable "no active world")
                    (Just def, Just (pid, ws)) → do
                        mWtd ← snapshotVisibleWorldTiles env
                        case mWtd of
                            Nothing  → pure (NotPlaceable "no world loaded")
                            Just wtd → do
                                mParams ← readIORef (wsGenParamsRef ws)
                                let locInstances = maybe emptyLocationInstances
                                                         wgpLocationInstances mParams
                                    worldSizeChunks = maybe 0 wgpWorldSize mParams
                                    -- #1175: the ghost preview must answer
                                    -- for the tile building.spawn will
                                    -- actually use, or the tool says
                                    -- "placeable" and the spawn refuses.
                                    (cgx, cgy) = canonicalTile worldSizeChunks gx gy
                                pure (canPlaceAt
                                    (bm { bmInstances =
                                            buildingsOnPage pid (bmInstances bm) })
                                    wtd locInstances worldSizeChunks def cgx cgy)
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

-- * Remote-settlement check (#779)

-- | building.remoteCheck(defName, gx, gy) — (remote, distance,
--   thresholdTiles). For a starting building, `distance` is the
--   seam-aware nearest footprint→placed-location distance among every
--   location placed on the ACTIVE world page, or nil when that page
--   has none at all (still `remote = true` in that case — see
--   'Building.Placement.RemoteCheck'). Always @(false, nil,
--   thresholdTiles)@ for a non-starting def, an unknown def, or when
--   there's no active world — mirrors 'canPlaceAt's own #778 gate on
--   `bdIsStarting`. Cheap enough to call once per click, unlike
--   'canPlaceAt' which runs every ghost-preview frame.
buildingRemoteCheckFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
buildingRemoteCheckFn env = do
    nameArg ← Lua.tostring 1
    xArg    ← Lua.tointeger 2
    yArg    ← Lua.tointeger 3
    check ← case (nameArg, xArg, yArg) of
        (Just nameBS, Just x, Just y) → Lua.liftIO $ do
            let defName = TE.decodeUtf8Lenient nameBS
                gx      = fromIntegral x
                gy      = fromIntegral y
            bm ← readIORef (bcBuildingManagerRef (toBuildingCapability env))
            mActive ← activeWorldPageFrom (wsWorldManagerRef (toWorldSimCapability env))
            case (HM.lookup defName (bmDefs bm), mActive) of
                (Just def, Just (_pid, ws)) → do
                    mParams ← readIORef (wsGenParamsRef ws)
                    let locInstances = maybe emptyLocationInstances
                                             wgpLocationInstances mParams
                        worldSizeChunks = maybe 0 wgpWorldSize mParams
                        -- #1175: measured against the same anchor
                        -- canPlaceAt/spawn resolve, so the remoteness
                        -- answer describes the tile that gets built on.
                        (cgx, cgy) = canonicalTile worldSizeChunks gx gy
                    pure (remoteCheck locInstances worldSizeChunks def cgx cgy)
                _ → pure NotStartingBuilding
        _ → pure NotStartingBuilding
    Lua.pushboolean (isRemote check)
    case check of
        RemoteDistance (Just d) → Lua.pushinteger (fromIntegral d)
        _                       → Lua.pushnil
    Lua.pushinteger (fromIntegral remotePortalThresholdTiles)
    return 3

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
            let name = TE.decodeUtf8Lenient nameBS
                gx   = fromIntegral x
                gy   = fromIntegral y
            Lua.liftIO $ do
                -- Sample the terrain Z at the ghost tile so the
                -- render pass can elevate the preview to where the
                -- building will actually land. Matches the spawn
                -- path's `floorZAt`; falls back to 0 if the chunk
                -- isn't loaded.
                --
                -- #1175: the ghost is drawn at the CANONICAL tile the
                -- spawn will use, so the preview and the building that
                -- follows it never sit a world apart at the seam.
                worldSize ← visibleWorldSize env
                let (cgx, cgy) = canonicalTile worldSize gx gy
                gz ← do
                    mWtd ← snapshotVisibleWorldTiles env
                    case mWtd of
                        Just wtd → pure (floorZAt worldSize wtd cgx cgy)
                        Nothing  → pure 0
                writeIORef (bcBuildingGhostRef (toBuildingCapability env)) $ Just BuildingGhost
                    { bgDefName = name
                    , bgGridX   = cgx
                    , bgGridY   = cgy
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
    Lua.liftIO $ writeIORef (bcBuildingGhostRef (toBuildingCapability env)) Nothing
    Lua.pushboolean True
    return 1

-- * Helpers

snapshotVisibleWorldTiles ∷ EngineEnv → IO (Maybe WorldTileData)
snapshotVisibleWorldTiles env = do
    wm ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
    case wmVisible wm of
        []          → pure Nothing
        (pageId:_)  → case lookup pageId (wmWorlds wm) of
            Nothing → pure Nothing
            Just ws → Just <$> readIORef (wsTilesRef ws)

-- | The VISIBLE page's u-wrap world size (#1175), for the ghost preview:
--   it has no page argument of its own, and must resolve its tile the
--   same way the spawn it previews will. 0 (no wrapping) when no page is
--   visible or it has no gen params, matching 'buildingSpawnFn'.
visibleWorldSize ∷ EngineEnv → IO Int
visibleWorldSize env = do
    wm ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
    case wmVisible wm of
        []         → pure 0
        (pageId:_) → case lookup pageId (wmWorlds wm) of
            Nothing → pure 0
            Just ws → maybe 0 wgpWorldSize <$> readIORef (wsGenParamsRef ws)

-- | Terrain Z at the anchor tile. Falls back to 0 if the chunk isn't
--   loaded — shouldn't happen since canPlaceAt already verified, but
--   defensive.
floorZAt ∷ Int → WorldTileData → Int → Int → Int
floorZAt worldSize wtd gx gy =
    let (cgx, cgy) = canonicalTile worldSize gx gy
    in case lookupTerrainZ wtd cgx cgy of
    Just z  → z
    Nothing → 0
