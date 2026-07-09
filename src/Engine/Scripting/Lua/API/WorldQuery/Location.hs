{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Placed-location queries: world.listPlacedLocations,
--   world.hasSpawnedLocationContents, world.hasStampedLocation.
module Engine.Scripting.Lua.API.WorldQuery.Location
    ( worldListPlacedLocationsFn
    , worldHasSpawnedLocationContentsFn
    , worldHasStampedLocationFn
    ) where

import UPrelude
import qualified HsLua as Lua
import qualified Data.HashSet as HS
import qualified Data.Text.Encoding as TE
import Data.IORef (readIORef)
import Engine.Core.State (EngineEnv(..), activeWorldState)
import World.Types
import Location.Overlay.Types (overlayToList)
import World.Generate.Coordinates (globalToChunk)
import Engine.Scripting.Lua.API.WorldQuery.Lookup (worldStateByPage)

-- | world.listPlacedLocations([pageId]) → array of placed-location
--   tables, each:
--     { cx, cy,    -- chunk coordinate hosting the location
--       gx, gy,    -- the chunk's centre tile (anchor for stamping)
--       id }       -- the LocationDef id (#88) placed there
--   With a page-id string argument the named page's overlay is read
--   (the location stamper needs a specific world's placements even
--   before it becomes the active page); with no argument the active
--   world is used. Reads the deterministic overlay computed at world
--   init and carried in the world's gen params (#89). The Lua
--   `locations` module wraps this as locations.listPlaced(); join `id`
--   against locations.getDef for label/type/builder. Returns an empty
--   table when no such world exists or none were placed.
worldListPlacedLocationsFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldListPlacedLocationsFn env = do
    mPage ← Lua.tostring 1
    mParams ← Lua.liftIO $ do
        mWs ← case mPage of
            Just pidBS → worldStateByPage env (TE.decodeUtf8 pidBS)
            Nothing    → activeWorldState env
        case mWs of
            Just ws → readIORef (wsGenParamsRef ws)
            Nothing → pure Nothing
    Lua.newtable
    case mParams of
        Nothing → return 1
        Just params → do
            let placed = overlayToList (wgpLocationOverlay params)
                half   = chunkSize `div` 2
            forM_ (zip [1 ..] placed) $ \(i, (ChunkCoord cx cy, lid)) → do
                Lua.newtable
                Lua.pushinteger (fromIntegral cx)
                Lua.setfield (-2) "cx"
                Lua.pushinteger (fromIntegral cy)
                Lua.setfield (-2) "cy"
                Lua.pushinteger (fromIntegral (cx * chunkSize + half))
                Lua.setfield (-2) "gx"
                Lua.pushinteger (fromIntegral (cy * chunkSize + half))
                Lua.setfield (-2) "gy"
                Lua.pushstring (TE.encodeUtf8 lid)
                Lua.setfield (-2) "id"
                Lua.rawseti (-2) i
            return 1

-- | world.hasSpawnedLocationContents(gx, gy [, pageId]) → bool.
--   One-time content-spawn flag (#90): true once the chunk containing
--   (gx, gy) has had its placed location's `contents` spawned.
--   Independent of structure-geometry state (structure.hasAt) — see
--   'World.Command.Types.WorldMarkLocationContentsSpawned'. With no
--   page argument the active world is read; false when no such world
--   or its gen params aren't live.
worldHasSpawnedLocationContentsFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldHasSpawnedLocationContentsFn env = do
    gxA ← Lua.tointeger 1
    gyA ← Lua.tointeger 2
    pageA ← Lua.tostring 3
    case (gxA, gyA) of
        (Just gx, Just gy) → do
            spawned ← Lua.liftIO $ do
                mWs ← case pageA of
                    Just pidBS → worldStateByPage env (TE.decodeUtf8 pidBS)
                    Nothing    → activeWorldState env
                case mWs of
                    Nothing → pure False
                    Just ws → do
                        mParams ← readIORef (wsGenParamsRef ws)
                        case mParams of
                            Nothing → pure False
                            Just params →
                                let (coord, _) =
                                        globalToChunk (fromIntegral gx)
                                                       (fromIntegral gy)
                                in pure (HS.member coord
                                    (wgpLocationContentsSpawned params))
            Lua.pushboolean spawned
            return 1
        _ → Lua.pushboolean False >> return 1

-- | world.hasStampedLocation(gx, gy [, pageId]) → bool. One-time
--   geometry-stamp flag (#424): true once the chunk containing (gx, gy)
--   has had its placed location's builder run. This is the idempotency
--   check 'scripts/location_stamper.lua' consults instead of
--   @structure.hasAt gx gy "floor"@ — a check that a player clearing the
--   anchor floor tile would otherwise defeat. With no page argument the
--   active world is read; false when no such world or its gen params
--   aren't live.
worldHasStampedLocationFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldHasStampedLocationFn env = do
    gxA ← Lua.tointeger 1
    gyA ← Lua.tointeger 2
    pageA ← Lua.tostring 3
    case (gxA, gyA) of
        (Just gx, Just gy) → do
            stamped ← Lua.liftIO $ do
                mWs ← case pageA of
                    Just pidBS → worldStateByPage env (TE.decodeUtf8 pidBS)
                    Nothing    → activeWorldState env
                case mWs of
                    Nothing → pure False
                    Just ws → do
                        mParams ← readIORef (wsGenParamsRef ws)
                        case mParams of
                            Nothing → pure False
                            Just params →
                                let (coord, _) =
                                        globalToChunk (fromIntegral gx)
                                                       (fromIntegral gy)
                                in pure (HS.member coord
                                    (wgpLocationStamped params))
            Lua.pushboolean stamped
            return 1
        _ → Lua.pushboolean False >> return 1
