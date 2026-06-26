{-# LANGUAGE Strict, UnicodeSyntax #-}
module Engine.Scripting.Lua.API.WorldQuery
    ( worldGetTerrainAtFn
    , worldGetFluidAtFn
    , worldGetSurfaceAtFn
    , worldGetChunkInfoFn
    , worldGetAreaFluidFn
    , worldGetRiversFn
    , worldLoadChunksInRegionFn
    , worldWaitForChunksFn
    , worldGetHoverTileFn
    , worldGetHoverPosFn
    , worldGetClimateAtFn
    ) where

import UPrelude
import qualified HsLua as Lua
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.IORef (readIORef, atomicModifyIORef')
import Control.Concurrent (threadDelay)
import Engine.Core.State (EngineEnv(..), activeWorldState)
import World.Types
import World.Geology.Timeline.Types
import World.Hydrology.Types
import World.Cursor.Types (CursorState(..))
import World.Generate.Types (WorldGenParams(..))
import World.Weather.Lookup (lookupLocalClimate, LocalClimate(..))

import World.Generate (globalToChunk)

-- | Helper: get the first active world's tile data
getWorldTileData ∷ EngineEnv → IO (Maybe WorldTileData)
getWorldTileData env = do
    mWs ← activeWorldState env
    case mWs of
        Just ws → Just <$> readIORef (wsTilesRef ws)
        Nothing → pure Nothing

-- | world.getTerrainAt(gx, gy) → surfaceZ, terrainSurfaceZ or nil
--   Returns the surface elevation and terrain-only surface elevation.
worldGetTerrainAtFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldGetTerrainAtFn env = do
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
                    let surfZ = (lcSurfaceMap lc) VU.! idx
                        terrZ = (lcTerrainSurfaceMap lc) VU.! idx
                    Lua.pushinteger (fromIntegral surfZ)
                    Lua.pushinteger (fromIntegral terrZ)
                    return 2
        _ → do
            Lua.pushnil
            return 1

-- | world.getFluidAt(gx, gy) → type, surface or nil
--   Returns fluid type string ("ocean","lake","river","lava") and surface Z.
worldGetFluidAtFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldGetFluidAtFn env = do
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
                Just lc →
                    case (lcFluidMap lc) V.! idx of
                        Nothing → do
                            Lua.pushnil
                            return 1
                        Just fc → do
                            let typeStr = case fcType fc of
                                    Ocean → "ocean"
                                    Lake  → "lake"
                                    River → "river"
                                    Lava  → "lava"
                            Lua.pushstring typeStr
                            Lua.pushinteger (fromIntegral (fcSurface fc))
                            return 2
        _ → do
            Lua.pushnil
            return 1

-- | world.getSurfaceAt(gx, gy) → surfaceZ, terrainSurfaceZ, fluidType, fluidSurface
--   Combined query: returns all surface info at a global coordinate.
worldGetSurfaceAtFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldGetSurfaceAtFn env = do
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
                    let surfZ = (lcSurfaceMap lc) VU.! idx
                        terrZ = (lcTerrainSurfaceMap lc) VU.! idx
                    Lua.pushinteger (fromIntegral surfZ)
                    Lua.pushinteger (fromIntegral terrZ)
                    case (lcFluidMap lc) V.! idx of
                        Nothing → do
                            Lua.pushnil
                            Lua.pushnil
                        Just fc → do
                            let typeStr = case fcType fc of
                                    Ocean → "ocean"
                                    Lake  → "lake"
                                    River → "river"
                                    Lava  → "lava"
                            Lua.pushstring typeStr
                            Lua.pushinteger (fromIntegral (fcSurface fc))
                    return 4
        _ → do
            Lua.pushnil
            return 1

-- | world.getChunkInfo(cx, cy) → table with summary
--   Returns: { loaded=true, fluidCounts={ocean=N,lake=N,river=N,lava=N},
--              minSurf=N, maxSurf=N, minTerrSurf=N, maxTerrSurf=N }
worldGetChunkInfoFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldGetChunkInfoFn env = do
    mCx ← Lua.tointeger 1
    mCy ← Lua.tointeger 2
    case (mCx, mCy) of
        (Just cx', Just cy') → do
            let coord = ChunkCoord (fromIntegral cx') (fromIntegral cy')
            mTd ← Lua.liftIO $ getWorldTileData env
            case mTd >>= lookupChunk coord of
                Nothing → do
                    Lua.newtable
                    Lua.pushboolean False
                    Lua.setfield (Lua.nth 2) "loaded"
                    return 1
                Just lc → do
                    let sz = chunkSize * chunkSize
                        surfMap = lcSurfaceMap lc
                        terrMap = lcTerrainSurfaceMap lc
                        fluidMap = lcFluidMap lc
                        -- Count fluids
                        countFluids = V.foldl' (\(!o,!l,!r,!v) mfc →
                            case mfc of
                                Nothing → (o,l,r,v)
                                Just fc → case fcType fc of
                                    Ocean → (o+1,l,r,v)
                                    Lake  → (o,l+1,r,v)
                                    River → (o,l,r+1,v)
                                    Lava  → (o,l,r,v+1)
                            ) (0∷Int,0∷Int,0∷Int,0∷Int) fluidMap
                        (nOcean, nLake, nRiver, nLava) = countFluids
                        minSurf = VU.minimum surfMap
                        maxSurf = VU.maximum surfMap
                        minTerr = VU.minimum terrMap
                        maxTerr = VU.maximum terrMap
                    Lua.newtable
                    Lua.pushboolean True
                    Lua.setfield (Lua.nth 2) "loaded"
                    -- Fluid counts sub-table
                    Lua.newtable
                    Lua.pushinteger (fromIntegral nOcean)
                    Lua.setfield (Lua.nth 2) "ocean"
                    Lua.pushinteger (fromIntegral nLake)
                    Lua.setfield (Lua.nth 2) "lake"
                    Lua.pushinteger (fromIntegral nRiver)
                    Lua.setfield (Lua.nth 2) "river"
                    Lua.pushinteger (fromIntegral nLava)
                    Lua.setfield (Lua.nth 2) "lava"
                    Lua.setfield (Lua.nth 2) "fluidCounts"
                    -- Surface stats
                    Lua.pushinteger (fromIntegral minSurf)
                    Lua.setfield (Lua.nth 2) "minSurf"
                    Lua.pushinteger (fromIntegral maxSurf)
                    Lua.setfield (Lua.nth 2) "maxSurf"
                    Lua.pushinteger (fromIntegral minTerr)
                    Lua.setfield (Lua.nth 2) "minTerrSurf"
                    Lua.pushinteger (fromIntegral maxTerr)
                    Lua.setfield (Lua.nth 2) "maxTerrSurf"
                    return 1
        _ → do
            Lua.pushnil
            return 1

-- | world.getAreaFluid(gx, gy, radius) → table of {x,y,type,surface,terrainZ}
--   Scans a square area and returns all fluid cells found.
--   Useful for debugging river coverage around a point.
worldGetAreaFluidFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldGetAreaFluidFn env = do
    mGx ← Lua.tointeger 1
    mGy ← Lua.tointeger 2
    mR  ← Lua.tointeger 3
    case (mGx, mGy, mR) of
        (Just gx', Just gy', Just r') → do
            let gx = fromIntegral gx' ∷ Int
                gy = fromIntegral gy' ∷ Int
                radius = min 64 (fromIntegral r' ∷ Int)  -- cap at 64
            mTd ← Lua.liftIO $ getWorldTileData env
            case mTd of
                Nothing → do
                    Lua.pushnil
                    return 1
                Just td → do
                    Lua.newtable
                    let go !n !x !y
                            | y > gy + radius = return n
                            | x > gx + radius = go n (gx - radius) (y + 1)
                            | otherwise = do
                                let (coord, (lx, ly)) = globalToChunk x y
                                    idx = ly * chunkSize + lx
                                case lookupChunk coord td of
                                    Nothing → go n (x + 1) y
                                    Just lc →
                                        case (lcFluidMap lc) V.! idx of
                                            Nothing → go n (x + 1) y
                                            Just fc → do
                                                let terrZ = (lcTerrainSurfaceMap lc) VU.! idx
                                                    typeStr = case fcType fc of
                                                        Ocean → "ocean"
                                                        Lake  → "lake"
                                                        River → "river"
                                                        Lava  → "lava"
                                                Lua.newtable
                                                Lua.pushinteger (fromIntegral x)
                                                Lua.setfield (Lua.nth 2) "x"
                                                Lua.pushinteger (fromIntegral y)
                                                Lua.setfield (Lua.nth 2) "y"
                                                Lua.pushstring typeStr
                                                Lua.setfield (Lua.nth 2) "type"
                                                Lua.pushinteger (fromIntegral (fcSurface fc))
                                                Lua.setfield (Lua.nth 2) "surface"
                                                Lua.pushinteger (fromIntegral terrZ)
                                                Lua.setfield (Lua.nth 2) "terrainZ"
                                                Lua.rawseti (Lua.nth 2) n
                                                go (n + 1) (x + 1) y
                    _ ← go 1 (gx - radius) (gy - radius)
                    return 1
        _ → do
            Lua.pushnil
            return 1

-- | Helper: get the first active world's gen params
getWorldGenParams ∷ EngineEnv → IO (Maybe WorldGenParams)
getWorldGenParams env = do
    mWs ← activeWorldState env
    case mWs of
        Just ws → readIORef (wsGenParamsRef ws)
        Nothing → pure Nothing

-- | world.getClimateAt(gx, gy) → { temp, summerTemp, winterTemp, precip,
--   humidity, snow } | nil. Region climate sampled (bilinear) at a global
--   tile. temp in °C; precip/humidity/snow 0..1. nil if no world is active.
worldGetClimateAtFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldGetClimateAtFn env = do
    gxArg ← Lua.tointeger 1
    gyArg ← Lua.tointeger 2
    case (gxArg, gyArg) of
        (Just gx, Just gy) → do
            mParams ← Lua.liftIO (getWorldGenParams env)
            case mParams of
                Nothing → Lua.pushnil >> return 1
                Just p → do
                    let c = lookupLocalClimate (wgpClimateState p)
                                (wgpWorldSize p) (fromIntegral gx)
                                (fromIntegral gy)
                        putN k v = Lua.pushnumber (Lua.Number (realToFrac v))
                                   >> Lua.setfield (-2) k
                    Lua.newtable
                    putN "temp"       (lcTemp c)
                    putN "summerTemp" (lcSummerTemp c)
                    putN "winterTemp" (lcWinterTemp c)
                    putN "precip"     (lcPrecip c)
                    putN "humidity"   (lcHumidity c)
                    putN "snow"       (lcSnow c)
                    return 1
        _ → Lua.pushnil >> return 1

-- | world.getRivers() → array of river tables
--   Each river: { source={x,y}, mouth={x,y}, flowRate=N, segments={...} }
--   Each segment: { sx,sy, ex,ey, width, valleyWidth, depth, flowRate,
--                   startElev, endElev }
--
--   Water surface elevation is no longer carried on the segment — it
--   is derived per-tile from the water-table compute at chunk gen.
--   Scripts that need surface heights should call world.getSurfaceAt.
worldGetRiversFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldGetRiversFn env = do
    mParams ← Lua.liftIO $ getWorldGenParams env
    case mParams of
        Nothing → do
            Lua.pushnil
            return 1
        Just params → do
            let timeline = wgpGeoTimeline params
                -- Extract rivers from all periods' events
                rivers = concatMap extractRivers (gtPeriods timeline)
            Lua.newtable
            mapM_ (\(rIdx, river) → do
                Lua.newtable
                -- Source
                let GeoCoord srcX srcY = rpSourceRegion river
                Lua.newtable
                Lua.pushinteger (fromIntegral srcX)
                Lua.setfield (Lua.nth 2) "x"
                Lua.pushinteger (fromIntegral srcY)
                Lua.setfield (Lua.nth 2) "y"
                Lua.setfield (Lua.nth 2) "source"
                -- Mouth
                let GeoCoord mthX mthY = rpMouthRegion river
                Lua.newtable
                Lua.pushinteger (fromIntegral mthX)
                Lua.setfield (Lua.nth 2) "x"
                Lua.pushinteger (fromIntegral mthY)
                Lua.setfield (Lua.nth 2) "y"
                Lua.setfield (Lua.nth 2) "mouth"
                -- Flow rate
                Lua.pushnumber (Lua.Number (realToFrac (rpFlowRate river)))
                Lua.setfield (Lua.nth 2) "flowRate"
                -- Segment count
                Lua.pushinteger (fromIntegral (V.length (rpSegments river)))
                Lua.setfield (Lua.nth 2) "segmentCount"
                -- Segments
                Lua.newtable
                V.iforM_ (rpSegments river) $ \sIdx seg → do
                    let GeoCoord sx sy = rsStart seg
                        GeoCoord ex ey = rsEnd seg
                    Lua.newtable
                    Lua.pushinteger (fromIntegral sx)
                    Lua.setfield (Lua.nth 2) "sx"
                    Lua.pushinteger (fromIntegral sy)
                    Lua.setfield (Lua.nth 2) "sy"
                    Lua.pushinteger (fromIntegral ex)
                    Lua.setfield (Lua.nth 2) "ex"
                    Lua.pushinteger (fromIntegral ey)
                    Lua.setfield (Lua.nth 2) "ey"
                    Lua.pushinteger (fromIntegral (rsWidth seg))
                    Lua.setfield (Lua.nth 2) "width"
                    Lua.pushinteger (fromIntegral (rsValleyWidth seg))
                    Lua.setfield (Lua.nth 2) "valleyWidth"
                    Lua.pushinteger (fromIntegral (rsDepth seg))
                    Lua.setfield (Lua.nth 2) "depth"
                    Lua.pushnumber (Lua.Number (realToFrac (rsFlowRate seg)))
                    Lua.setfield (Lua.nth 2) "flowRate"
                    Lua.pushinteger (fromIntegral (rsStartElev seg))
                    Lua.setfield (Lua.nth 2) "startElev"
                    Lua.pushinteger (fromIntegral (rsEndElev seg))
                    Lua.setfield (Lua.nth 2) "endElev"
                    Lua.rawseti (Lua.nth 2) (fromIntegral sIdx + 1)
                Lua.setfield (Lua.nth 2) "segments"
                Lua.rawseti (Lua.nth 2) rIdx
                ) (zip [1..] rivers)
            return 1

-- | Extract RiverParams from all HydroEvents in a period
extractRivers ∷ GeoPeriod → [RiverParams]
extractRivers period = concatMap go (gpEvents period)
  where
    go (HydroEvent (RiverFeature rp)) = [rp]
    go _ = []

-- | world.loadChunksInRegion(cx1, cy1, cx2, cy2) → count
--   Queues chunk coordinates in the rectangle [cx1..cx2] × [cy1..cy2]
--   for generation. Chunks are generated by the world thread at
--   ~4 per tick. Use world.waitForChunks() to block until done.
--   Returns the number of chunks queued.
worldLoadChunksInRegionFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldLoadChunksInRegionFn env = do
    mCx1 ← Lua.tointeger 1
    mCy1 ← Lua.tointeger 2
    mCx2 ← Lua.tointeger 3
    mCy2 ← Lua.tointeger 4
    case (mCx1, mCy1, mCx2, mCy2) of
        (Just cx1', Just cy1', Just cx2', Just cy2') → do
            let cx1 = fromIntegral cx1' ∷ Int
                cy1 = fromIntegral cy1' ∷ Int
                cx2 = fromIntegral cx2' ∷ Int
                cy2 = fromIntegral cy2' ∷ Int
                coords = [ ChunkCoord x y
                         | x ← [min cx1 cx2 .. max cx1 cx2]
                         , y ← [min cy1 cy2 .. max cy1 cy2]
                         ]
            count ← Lua.liftIO $ do
                mWs ← activeWorldState env
                case mWs of
                    Just ws → do
                        -- Filter the requested coords against both the loaded
                        -- tiles and the pending/in-flight queue, race-free
                        -- against the world thread's load handoff.
                        -- drainInitQueues (Thread/ChunkLoading) keeps a chunk
                        -- queued for the whole of its generation, then inserts
                        -- it into wsTilesRef and only AFTER that removes it
                        -- from this queue — both writes via atomicModifyIORef'.
                        -- So we must read in the matching order: snapshot the
                        -- queue FIRST (an atomicModifyIORef' read, which
                        -- acquires that release), THEN read the tiles. A coord
                        -- already gone from the queue snapshot is then
                        -- guaranteed visible in the tile snapshot, so a chunk
                        -- that is in flight or just-loaded is caught by one
                        -- snapshot or the other, never missed by both (no
                        -- duplicate queueing, no inflated count). Reading the
                        -- tiles first would reopen that window.
                        pending ← atomicModifyIORef' (wsInitQueueRef ws) $ \q → (q, q)
                        let queued = HS.fromList pending
                        td ← readIORef (wsTilesRef ws)
                        let needed = filter (\c → isNothing (lookupChunk c td)
                                                ∧ not (HS.member c queued)) coords
                        -- `needed` is disjoint from the queue snapshot, and the
                        -- Lua thread is the sole appender, so this append can't
                        -- introduce a duplicate even if the world thread
                        -- removed (loaded) some coords in between.
                        atomicModifyIORef' (wsInitQueueRef ws) $ \q →
                            (q ++ needed, length needed)
                    Nothing → pure 0
            Lua.pushinteger (fromIntegral count)
            return 1
        _ → do
            Lua.pushnil
            return 1

-- | world.waitForChunks(timeout_seconds) → remaining
--   Blocks until the chunk init queue is empty or timeout is reached.
--   Default timeout: 120 seconds. Returns the number of chunks still
--   remaining (0 if all loaded).
worldWaitForChunksFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldWaitForChunksFn env = do
    timeoutArg ← Lua.tointeger 1
    let timeoutSec = case timeoutArg of
            Just t | t > 0 → fromIntegral t ∷ Int
            _              → 120
        maxIter = timeoutSec * 4  -- poll at 250ms intervals
    remaining ← Lua.liftIO $ waitLoop maxIter
    Lua.pushinteger (fromIntegral remaining)
    return 1
  where
    waitLoop 0 = checkRemaining
    waitLoop n = do
        r ← checkRemaining
        if r ≡ 0
            then return 0
            else do
                threadDelay 250000
                waitLoop (n - 1)
    checkRemaining = do
        mWs ← activeWorldState env
        case mWs of
            Just ws → length <$> readIORef (wsInitQueueRef ws)
            Nothing → pure 0

-- | world.getHoverTile() → gx, gy or nil
--   Returns the tile coordinates currently under the mouse cursor in
--   world-view mode. Reads the resolved tile that the render-thread
--   hit-test wrote to worldHoverTile each frame — accounts for the
--   isometric tilt, camera facing, elevation, and u-wrap boundary.
worldGetHoverTileFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldGetHoverTileFn env = do
    mWs ← Lua.liftIO $ activeWorldState env
    case mWs of
        Just ws → do
            cs ← Lua.liftIO $ readIORef (wsCursorRef ws)
            case worldHoverTile cs of
                Just (gx, gy) → do
                    Lua.pushinteger (fromIntegral gx)
                    Lua.pushinteger (fromIntegral gy)
                    return 2
                Nothing → do
                    Lua.pushnil
                    return 1
        Nothing → do
            Lua.pushnil
            return 1

-- | world.getHoverPos() → x, y or nil
--   Fractional grid position of the point under the mouse cursor
--   (item/unit convention: tile k spans [k, k+1)). Same hit-test as
--   getHoverTile; use this for sub-tile placements — ground-item
--   spawn lands exactly where the player clicked instead of snapping
--   to the tile center.
worldGetHoverPosFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldGetHoverPosFn env = do
    mWs ← Lua.liftIO $ activeWorldState env
    case mWs of
        Just ws → do
            cs ← Lua.liftIO $ readIORef (wsCursorRef ws)
            case worldHoverPos cs of
                Just (hx, hy) → do
                    Lua.pushnumber (Lua.Number (realToFrac hx))
                    Lua.pushnumber (Lua.Number (realToFrac hy))
                    return 2
                Nothing → do
                    Lua.pushnil
                    return 1
        Nothing → do
            Lua.pushnil
            return 1
