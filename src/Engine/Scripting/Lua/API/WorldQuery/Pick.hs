{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Screen-pixel / cursor hit-testing queries: world.getHoverTile,
--   world.getHoverPos, world.pickTile, world.pickPos.
module Engine.Scripting.Lua.API.WorldQuery.Pick
    ( worldGetHoverTileFn
    , worldGetHoverPosFn
    , worldPickTileFn
    , worldPickPosFn
    ) where

import UPrelude
import qualified HsLua as Lua
import Data.IORef (readIORef)
import Engine.Core.State (EngineEnv(..), activeWorldState)
import World.Types
import Engine.Graphics.Camera (Camera2D(..))
import World.Render.HitTest (pickWorldTile)
import World.Render.ViewBounds (computeViewBounds)
import World.Generate (viewDepth)
import Engine.Scripting.Lua.API.WorldQuery.Lookup (mVisibleWorldState)

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

-- | world.pickTile(pixX, pixY) → gx, gy, z or nil
--   Synchronous screen-pixel → tile hit-test from the given click
--   coordinates. Unlike getHoverTile (which reads the cached
--   worldHoverTile the render thread resolves from the periodically
--   pushed cursor position), this runs the hit-test NOW against the
--   live camera + window + tile state, so it reflects exactly where the
--   passed pixel points this instant. Use it on click paths (e.g. build
--   placement) where the async hover cache can lag a fast cursor move
--   off-world and place on a stale tile. Returns nil when the pixel is
--   off-world / over no solid tile.
--
--   The third result @z@ is the elevation of the resolved tile at the
--   current z-slice — the actual tile under the cursor, which below the
--   surface is NOT the column top. Pass it to @world.selectTile@ so a
--   click selects the clicked tile, not the surface (issue #367).
--   Existing callers that bind only @gx, gy@ are unaffected.
worldPickTileFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldPickTileFn env = do
    -- Click coords arrive as Lua numbers (Doubles from GLFW.getCursorPos),
    -- not integers — parse with tonumber and round, like the other
    -- screen-coordinate APIs (e.g. setWorldCursorHover).
    mPx ← Lua.tonumber 1
    mPy ← Lua.tonumber 2
    case (mPx, mPy) of
        (Just px', Just py') → do
            let px = round px'
                py = round py'
            manager ← Lua.liftIO $ readIORef (worldManagerRef env)
            -- Resolve the VISIBLE world (head of wmVisible), not the raw
            -- wmWorlds head: rendering and building validation/spawn both
            -- operate on wmVisible, and a hidden page (e.g. test_arena) can
            -- sit at the wmWorlds head while main_world is shown. Picking
            -- against the wrong world would silently desync ghost/placement.
            case mVisibleWorldState manager of
                Just ws → do
                    camera   ← Lua.liftIO $ readIORef (cameraRef env)
                    tileData ← Lua.liftIO $ readIORef (wsTilesRef ws)
                    paramsM  ← Lua.liftIO $ readIORef (wsGenParamsRef ws)
                    (winW, winH) ← Lua.liftIO $ readIORef (windowSizeRef env)
                    (fbW, fbH)   ← Lua.liftIO $ readIORef (framebufferSizeRef env)
                    let facing   = camFacing camera
                        zoom     = camZoom camera
                        zSlice   = camZSlice camera
                        (camX, camY) = camPosition camera
                        worldSize = case paramsM of
                                      Nothing     → 128
                                      Just params → wgpWorldSize params
                        effectiveDepth = min viewDepth
                                           (max 8 (round (zoom * 80.0 + 8.0 ∷ Float)))
                        vb = computeViewBounds camera fbW fbH effectiveDepth
                    case pickWorldTile facing zoom zSlice camX camY fbW fbH winW winH
                                       worldSize effectiveDepth vb tileData px py of
                        Just (gx, gy, z, _, _) → do
                            Lua.pushinteger (fromIntegral gx)
                            Lua.pushinteger (fromIntegral gy)
                            Lua.pushinteger (fromIntegral z)
                            return 3
                        Nothing → do
                            Lua.pushnil
                            return 1
                Nothing → do
                    Lua.pushnil
                    return 1
        _ → do
            Lua.pushnil
            return 1

-- | world.pickPos(pixX, pixY) → hx, hy or nil
--   Synchronous fractional-position analog of pickTile: the live
--   screen-pixel → sub-tile hit-test from the given click coordinates
--   (item/unit convention, tile k spans [k, k+1)). Like pickTile it
--   runs the hit-test NOW against the live camera + window + tile state
--   rather than reading the periodically-pushed worldHoverPos cache, so
--   it reflects exactly where the passed pixel points this instant. Use
--   it on click paths that place sub-tile content (ground-item / quarter-
--   corner structure placement) where the async hover cache can lag a
--   fast cursor move and place at a stale fractional position. Returns
--   nil when the pixel is off-world / over no solid tile.
worldPickPosFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldPickPosFn env = do
    mPx ← Lua.tonumber 1
    mPy ← Lua.tonumber 2
    case (mPx, mPy) of
        (Just px', Just py') → do
            let px = round px'
                py = round py'
            manager ← Lua.liftIO $ readIORef (worldManagerRef env)
            case mVisibleWorldState manager of
                Just ws → do
                    camera   ← Lua.liftIO $ readIORef (cameraRef env)
                    tileData ← Lua.liftIO $ readIORef (wsTilesRef ws)
                    paramsM  ← Lua.liftIO $ readIORef (wsGenParamsRef ws)
                    (winW, winH) ← Lua.liftIO $ readIORef (windowSizeRef env)
                    (fbW, fbH)   ← Lua.liftIO $ readIORef (framebufferSizeRef env)
                    let facing   = camFacing camera
                        zoom     = camZoom camera
                        zSlice   = camZSlice camera
                        (camX, camY) = camPosition camera
                        worldSize = case paramsM of
                                      Nothing     → 128
                                      Just params → wgpWorldSize params
                        effectiveDepth = min viewDepth
                                           (max 8 (round (zoom * 80.0 + 8.0 ∷ Float)))
                        vb = computeViewBounds camera fbW fbH effectiveDepth
                    case pickWorldTile facing zoom zSlice camX camY fbW fbH winW winH
                                       worldSize effectiveDepth vb tileData px py of
                        Just (_, _, _, _, (hx, hy)) → do
                            Lua.pushnumber (Lua.Number (realToFrac hx))
                            Lua.pushnumber (Lua.Number (realToFrac hy))
                            return 2
                        Nothing → do
                            Lua.pushnil
                            return 1
                Nothing → do
                    Lua.pushnil
                    return 1
        _ → do
            Lua.pushnil
            return 1
