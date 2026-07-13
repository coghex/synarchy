{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Per-tile terrain/slope/vegetation surface queries: world.getTerrainAt,
--   world.getSlopeAt, world.getVegAt, world.isPlantable.
module Engine.Scripting.Lua.API.WorldQuery.Terrain
    ( worldGetTerrainAtFn
    , worldGetSlopeAtFn
    , worldGetVegAtFn
    , worldIsPlantableFn
    ) where

import UPrelude
import qualified HsLua as Lua
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Text.Encoding as TE
import Data.IORef (readIORef)
import Engine.Core.State (EngineEnv(..))
import World.Types
import World.Vegetation (isTilledSoil)
import World.Generate.Coordinates (globalToChunk)
import Engine.Scripting.Lua.API.WorldQuery.Lookup
    (getWorldTileData, worldStateByPage)

-- | world.getTerrainAt(gx, gy [, pageId]) → surfaceZ, terrainSurfaceZ or nil
--   Returns the surface elevation and terrain-only surface elevation. With
--   a page-id string argument it reads that page's tiles instead of the
--   active world's — so the location stamper can author geometry against a
--   specific (possibly hidden, non-active) page and still read its real
--   terrain height (#89 multiworld).
worldGetTerrainAtFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldGetTerrainAtFn env = do
    mGx ← Lua.tointeger 1
    mGy ← Lua.tointeger 2
    mPage ← Lua.tostring 3
    case (mGx, mGy) of
        (Just gx', Just gy') → do
            let gx = fromIntegral gx'
                gy = fromIntegral gy'
                (coord, (lx, ly)) = globalToChunk gx gy
                idx = ly * chunkSize + lx
            mTd ← Lua.liftIO $ case mPage of
                Just pidBS → do
                    mWs ← worldStateByPage env (TE.decodeUtf8Lenient pidBS)
                    case mWs of
                        Just ws → Just <$> readIORef (wsTilesRef ws)
                        Nothing → pure Nothing
                Nothing → getWorldTileData env
            case mTd >>= lookupChunk coord of
                Nothing → do
                    Lua.pushnil
                    return 1
                Just lc → do
                    let surfZ = (lcSurfaceMap lc) VU.! idx
                        terrZ = (lcTerrainSurfaceMap lc) VU.! idx
                    Lua.pushinteger (fromIntegral surfZ)
                    Lua.pushinteger (fromIntegral terrZ)
                    return 2
        _ → do
            Lua.pushnil
            return 1

-- | world.getSlopeAt(gx, gy) → slope bitmask | nil (chunk unloaded).
--   The SURFACE tile's slope id (bit0=N, 1=E, 2=S, 3=W; 0 = flat) on
--   the active world — the value the dig display and the construction
--   corner-progress display (#96) write, so headless tests can assert
--   a tile is visibly mid-work. Read-only sibling of world.setSlope.
worldGetSlopeAtFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldGetSlopeAtFn env = do
    mGx ← Lua.tointeger 1
    mGy ← Lua.tointeger 2
    case (mGx, mGy) of
        (Just gx', Just gy') → do
            let gx = fromIntegral gx'
                gy = fromIntegral gy'
                (coord, (lx, ly)) = globalToChunk gx gy
                idx = ly * chunkSize + lx
            mTd ← Lua.liftIO $ getWorldTileData env
            case mTd >>= lookupChunk coord of
                Nothing → do
                    Lua.pushnil
                    return 1
                Just lc → do
                    let col = lcTiles lc V.! idx
                        z   = lcSurfaceMap lc VU.! idx
                        i   = z - ctStartZ col
                        s   = if i ≥ 0 ∧ i < VU.length (ctSlopes col)
                              then ctSlopes col VU.! i
                              else 0
                    Lua.pushinteger (fromIntegral s)
                    return 1
        _ → do
            Lua.pushnil
            return 1

-- | world.getVegAt(gx, gy) → vegetation id | nil (chunk unloaded).
--   The SURFACE tile's vegetation id on the active world — the value
--   the till AI (#333) writes via world.setVegAt. Read-only sibling of
--   world.setVegAt, mirroring world.getSlopeAt.
worldGetVegAtFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldGetVegAtFn env = do
    mGx ← Lua.tointeger 1
    mGy ← Lua.tointeger 2
    case (mGx, mGy) of
        (Just gx', Just gy') → do
            let gx = fromIntegral gx'
                gy = fromIntegral gy'
                (coord, (lx, ly)) = globalToChunk gx gy
                idx = ly * chunkSize + lx
            mTd ← Lua.liftIO $ getWorldTileData env
            case mTd >>= lookupChunk coord of
                Nothing → do
                    Lua.pushnil
                    return 1
                Just lc → do
                    let col = lcTiles lc V.! idx
                        z   = lcSurfaceMap lc VU.! idx
                        i   = z - ctStartZ col
                        vg  = if i ≥ 0 ∧ i < VU.length (ctVeg col)
                              then ctVeg col VU.! i
                              else 0
                    Lua.pushinteger (fromIntegral vg)
                    return 1
        _ → do
            Lua.pushnil
            return 1

-- | world.isPlantable(gx, gy) → bool | nil (chunk unloaded). The
--   formal "can a crop go here" contract (#333): true iff the
--   SURFACE tile's vegetation id is tilled soil ('isTilledSoil').
--   Farming's planting tool (#335) and any other future consumer
--   should call this rather than compare world.getVegAt's raw id to
--   77 — if a soil-type-variant tilled texture ever adds ids
--   alongside vegTilledSoil, only isTilledSoil needs to grow to match.
worldIsPlantableFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldIsPlantableFn env = do
    mGx ← Lua.tointeger 1
    mGy ← Lua.tointeger 2
    case (mGx, mGy) of
        (Just gx', Just gy') → do
            let gx = fromIntegral gx'
                gy = fromIntegral gy'
                (coord, (lx, ly)) = globalToChunk gx gy
                idx = ly * chunkSize + lx
            mTd ← Lua.liftIO $ getWorldTileData env
            case mTd >>= lookupChunk coord of
                Nothing → do
                    Lua.pushnil
                    return 1
                Just lc → do
                    let col = lcTiles lc V.! idx
                        z   = lcSurfaceMap lc VU.! idx
                        i   = z - ctStartZ col
                        vg  = if i ≥ 0 ∧ i < VU.length (ctVeg col)
                              then ctVeg col VU.! i
                              else 0
                    Lua.pushboolean (isTilledSoil vg)
                    return 1
        _ → do
            Lua.pushnil
            return 1
