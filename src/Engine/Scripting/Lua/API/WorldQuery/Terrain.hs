{-# LANGUAGE Strict #-}
-- | Per-tile terrain/slope/vegetation/material surface queries:
--   world.getTerrainAt, world.getSlopeAt, world.getVegAt,
--   world.getMaterialAt, world.getIceAt, world.isPlantable.
module Engine.Scripting.Lua.API.WorldQuery.Terrain
    ( worldGetTerrainAtFn
    , worldGetSlopeAtFn
    , worldGetVegAtFn
    , worldGetMaterialAtFn
    , worldGetIceAtFn
    , worldIsPlantableFn
    ) where

import UPrelude
import qualified HsLua as Lua
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Text.Encoding as TE
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..))
import Data.IORef (readIORef)
import World.Material (MaterialId(..), MaterialProps(..), getMaterialProps)
import World.Types
import World.Vegetation (isTilledSoil)
import World.Generate.Coordinates (canonicalTileFrame, globalToChunk)
import Engine.Scripting.Lua.API.WorldQuery.Lookup
    (getWorldTileData, targetWorldState, worldStateByPage)

-- | world.getTerrainAt(gx, gy [, pageId]) → surfaceZ, terrainSurfaceZ or nil
--   Returns the surface elevation and terrain-only surface elevation. With
--   a page-id string argument it reads that page's tiles instead of the
--   active world's — so the location stamper can author geometry against a
--   specific (possibly hidden, non-active) page and still read its real
--   terrain height (#89 multiworld).
worldGetTerrainAtFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
worldGetTerrainAtFn wsc = do
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
                    mWs ← worldStateByPage wsc (TE.decodeUtf8Lenient pidBS)
                    case mWs of
                        Just ws → Just <$> readIORef (wsTilesRef ws)
                        Nothing → pure Nothing
                Nothing → getWorldTileData wsc
            case mTd ⌦ lookupChunk coord of
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
worldGetSlopeAtFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
worldGetSlopeAtFn wsc = do
    mGx ← Lua.tointeger 1
    mGy ← Lua.tointeger 2
    case (mGx, mGy) of
        (Just gx', Just gy') → do
            let gx = fromIntegral gx'
                gy = fromIntegral gy'
                (coord, (lx, ly)) = globalToChunk gx gy
                idx = ly * chunkSize + lx
            mTd ← Lua.liftIO $ getWorldTileData wsc
            case mTd ⌦ lookupChunk coord of
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
worldGetVegAtFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
worldGetVegAtFn wsc = do
    mGx ← Lua.tointeger 1
    mGy ← Lua.tointeger 2
    case (mGx, mGy) of
        (Just gx', Just gy') → do
            let gx = fromIntegral gx'
                gy = fromIntegral gy'
                (coord, (lx, ly)) = globalToChunk gx gy
                idx = ly * chunkSize + lx
            mTd ← Lua.liftIO $ getWorldTileData wsc
            case mTd ⌦ lookupChunk coord of
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
worldIsPlantableFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
worldIsPlantableFn wsc = do
    mGx ← Lua.tointeger 1
    mGy ← Lua.tointeger 2
    case (mGx, mGy) of
        (Just gx', Just gy') → do
            let gx = fromIntegral gx'
                gy = fromIntegral gy'
                (coord, (lx, ly)) = globalToChunk gx gy
                idx = ly * chunkSize + lx
            mTd ← Lua.liftIO $ getWorldTileData wsc
            case mTd ⌦ lookupChunk coord of
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

-- | world.getMaterialAt(gx, gy [, pageId]) → material id, material name | nil
--
--   The material at the top of the column's TERRAIN on the active world,
--   or on the named page — deliberately the terrain top and not
--   @lcSurfaceMap@'s rendered surface, which folds in fluid and would
--   answer about a cell that has no material at all.
--
--   Read-only, and the only way a script or probe can ask what a tile is
--   MADE of: 'worldGetTerrainAtFn' answers how high the column is,
--   @world.getDigInfoAt@ answers only for a designated mine tile, and
--   @world.listMaterials@ answers only what the registry holds. Added for
--   #2485, whose durability probe has to record the product material a
--   solidification chose and compare it across a fresh-process load.
--
--   The optional page argument mirrors 'worldGetTerrainAtFn' exactly, and
--   for the same reason (#89 multiworld): a caller reading a specific
--   page's terrain height has to be able to read that same page's
--   material, or the two answers can come from different worlds.
worldGetMaterialAtFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
worldGetMaterialAtFn wsc = do
    mGx ← Lua.tointeger 1
    mGy ← Lua.tointeger 2
    mPage ← Lua.tostring 3
    case (mGx, mGy) of
        (Just gx', Just gy') → do
            let rawGX = fromIntegral gx'
                rawGY = fromIntegral gy'
            mWs ← Lua.liftIO $ targetWorldState wsc
                (TE.decodeUtf8Lenient <$> mPage)
            case mWs of
                Nothing → Lua.pushnil ≫ return 1
                Just ws → do
                    -- Canonicalize FIRST: a point query accepts a seam
                    -- alias and answers about the tile the page actually
                    -- stores (CLAUDE.md §Tile coordinates). A bare
                    -- 'globalToChunk' would resolve an alias to a chunk
                    -- key nothing is stored under and answer nil about a
                    -- tile that is right there.
                    worldSize ← Lua.liftIO $ pageWrapWorldSize ws
                    td ← Lua.liftIO $ readIORef (wsTilesRef ws)
                    registry ← Lua.liftIO $ readIORef (wsMaterialRegistryRef wsc)
                    let (coord, (lx, ly), _) =
                            canonicalTileFrame worldSize rawGX rawGY
                        idx = ly * chunkSize + lx
                    case lookupChunk coord td of
                        Nothing → Lua.pushnil ≫ return 1
                        Just lc → do
                            let col = lcTiles lc V.! idx
                                z   = lcTerrainSurfaceMap lc VU.! idx
                                i   = z - ctStartZ col
                            if i < 0 ∨ i ≥ VU.length (ctMats col)
                              then Lua.pushnil ≫ return 1
                              else do
                                let matId = ctMats col VU.! i
                                    props = getMaterialProps registry
                                                (MaterialId matId)
                                Lua.pushinteger (fromIntegral matId)
                                Lua.pushstring (TE.encodeUtf8 (mpName props))
                                return 2
        _ → Lua.pushnil ≫ return 1

-- | world.getIceAt(gx, gy [, pageId]) → surfaceZ, mode | nil
--
--   The ice cell covering a column, or nil where there is none. @mode@
--   is @"basin"@ or @"drape"@, matching the dump's own spelling.
--
--   Read-only, and the only way a script or probe can ask whether a tile
--   is FROZEN: ice is not terrain, not fluid and not vegetation, so
--   'worldGetTerrainAtFn', @world.getFluidAt@ and 'worldGetVegAtFn' all
--   answer about a tile without mentioning it. Added for #2485, whose
--   visual probe has to pick an ice-FREE tile to grade the zoom map on:
--   an iced tile is coloured through snow vegetation
--   ('World.ZoomMap.Cache.ChunkPass.snowVegFor') whatever material lies
--   beneath it, so a solidification under ice correctly changes nothing
--   there and the probe would otherwise be measuring an invisible edit.
--
--   The optional page argument mirrors 'worldGetTerrainAtFn' exactly and
--   for the same reason (#89 multiworld).
worldGetIceAtFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
worldGetIceAtFn wsc = do
    mGx ← Lua.tointeger 1
    mGy ← Lua.tointeger 2
    mPage ← Lua.tostring 3
    case (mGx, mGy) of
        (Just gx', Just gy') → do
            mWs ← Lua.liftIO $ targetWorldState wsc
                (TE.decodeUtf8Lenient <$> mPage)
            case mWs of
                Nothing → Lua.pushnil ≫ return 1
                Just ws → do
                    -- Canonicalized before the lookup, exactly as
                    -- 'worldGetMaterialAtFn' is and for the same reason
                    -- (CLAUDE.md §Tile coordinates).
                    worldSize ← Lua.liftIO $ pageWrapWorldSize ws
                    td ← Lua.liftIO $ readIORef (wsTilesRef ws)
                    let (coord, (lx, ly), _) =
                            canonicalTileFrame worldSize
                                (fromIntegral gx') (fromIntegral gy')
                        idx = ly * chunkSize + lx
                    case lookupChunk coord td of
                        Nothing → Lua.pushnil ≫ return 1
                        Just lc → case lcIceMap lc V.! idx of
                            Nothing → Lua.pushnil ≫ return 1
                            Just ic → do
                                Lua.pushinteger
                                    (fromIntegral (icSurface ic))
                                Lua.pushstring $ case icMode ic of
                                    BasinIce → "basin"
                                    DrapeIce → "drape"
                                return 2
        _ → Lua.pushnil ≫ return 1
