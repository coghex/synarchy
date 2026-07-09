{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Top-level world-query API module (#557). Re-exports every
--   Lua-facing function from the sub-modules so existing call-sites
--   keep working: WorldQuery.Terrain is per-tile terrain/slope/veg
--   surface reads, WorldQuery.Fluid is fluid-surface + area-fluid
--   scans, WorldQuery.Chunk is chunk-info + region loading,
--   WorldQuery.River is the river-network dump, WorldQuery.Climate is
--   climate/ambient-temperature/sun-angle sampling, WorldQuery.Pick is
--   screen-pixel/cursor hit-testing, and WorldQuery.Location is the
--   placed-location queries. WorldQuery.Lookup holds the read-only
--   world/tile/gen-params resolution helpers shared across those
--   submodules and stays internal (not re-exported here), matching
--   the original file's export surface.
module Engine.Scripting.Lua.API.WorldQuery
    ( worldGetTerrainAtFn
    , worldGetSlopeAtFn
    , worldGetVegAtFn
    , worldIsPlantableFn
    , worldGetFluidAtFn
    , worldGetSurfaceAtFn
    , worldGetChunkInfoFn
    , worldGetAreaFluidFn
    , worldGetRiversFn
    , worldLoadChunksInRegionFn
    , worldWaitForChunksFn
    , worldGetHoverTileFn
    , worldGetHoverPosFn
    , worldPickTileFn
    , worldPickPosFn
    , worldGetClimateAtFn
    , worldGetAmbientAtFn
    , worldGetSunAngleAtFn
    , worldListPlacedLocationsFn
    , worldHasSpawnedLocationContentsFn
    , worldHasStampedLocationFn
    ) where

import Engine.Scripting.Lua.API.WorldQuery.Terrain
import Engine.Scripting.Lua.API.WorldQuery.Fluid
import Engine.Scripting.Lua.API.WorldQuery.Chunk
import Engine.Scripting.Lua.API.WorldQuery.River
import Engine.Scripting.Lua.API.WorldQuery.Climate
import Engine.Scripting.Lua.API.WorldQuery.Pick
import Engine.Scripting.Lua.API.WorldQuery.Location
