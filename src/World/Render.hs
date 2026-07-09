{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Render
    ( updateWorldTiles
    , surfaceHeadroom
    ) where

import UPrelude
import qualified Data.Map as Map
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V
import Data.IORef (readIORef, writeIORef, atomicModifyIORef')
import Engine.Core.State (EngineEnv(..), resolveActiveWorld)
import Engine.Scene.Types (LayeredQuads(..), mergeSortedQuads, sortQuadsByLayer)
import Engine.Graphics.Camera (Camera2D(..))
import World.Types
import World.Generate (viewDepth, globalToChunk)
import World.Grid (zoomFadeStart, zoomFadeEnd, worldToGrid)
import World.ZoomMap (generateZoomMapQuads)

import World.Render.Camera (cameraChanged)
import World.Render.Quads (renderWorldQuads)
import World.Render.CursorQuads (renderWorldCursorQuads)
import World.Render.GroundItemQuads (renderGroundItemQuads)
import World.Render.SpoilQuads (renderSpoilQuads)
import World.Render.BloodQuads (renderBloodDecalQuads)
import Unit.Render (renderUnitQuads)
import Building.Render (renderBuildingQuads, renderGhostQuad)
import Structure.Render (renderStructureQuads)

-- * Surface Headroom

surfaceHeadroom ∷ Int
surfaceHeadroom = 25

-- * Top-Level Entry Point

updateWorldTiles ∷ EngineEnv → IO LayeredQuads
updateWorldTiles env = do
    camera ← readIORef (cameraRef env)
    (fbW, fbH) ← readIORef (framebufferSizeRef env)

    let zoom = camZoom camera
        tileAlpha = clamp01 (1.0 - (zoom - zoomFadeStart) / (zoomFadeEnd - zoomFadeStart))
        _zoomAlpha = clamp01 ((zoom - zoomFadeStart) / (zoomFadeEnd - zoomFadeStart))
        -- Terrain's visible Z-band depth below the slice (same formula as
        -- Quads.hs). Units/buildings cull to this so a sprite is hidden
        -- exactly when its ground tile is — only ABOVE the slice (camera
        -- below it) or past the view depth, never just for being below
        -- the camera's own terrain level (the old fixed-25 bug: base-of-
        -- cliff sprites vanished when viewed from the top).
        effDepth = min viewDepth (max 8 (round (zoom * 80.0 + 8.0 ∷ Float)))

    worldManager ← readIORef (worldManagerRef env)

    tileQuads ← if tileAlpha ≤ 0.001
        then return Map.empty
        else do
            let currentSnap = WorldCameraSnapshot
                    { wcsPosition = camPosition camera
                    , wcsZoom     = zoom
                    , wcsZSlice   = camZSlice camera
                    , wcsFbSize   = (fbW, fbH)
                    , wcsFacing   = camFacing camera
                    }
            quads ← forM (wmVisible worldManager) $ \pageId →
                case lookup pageId (wmWorlds worldManager) of
                    Just worldState → do
                        -- Snapshot the invalidation generation BEFORE building.
                        -- A cache is only reusable when its generation still
                        -- matches; if an invalidation lands while we rebuild,
                        -- the generation we stamp is already stale and the cache
                        -- rebuilds next frame (the invalidation is never lost,
                        -- even though the render thread is the sole writer of
                        -- wsQuadCacheRef).
                        curGen ← readIORef (wsQuadCacheGenRef worldState)
                        cached ← readIORef (wsQuadCacheRef worldState)
                        case cached of
                            Just wqc | wqcGen wqc ≡ curGen
                                     , not (cameraChanged (wqcCamera wqc) currentSnap) →
                                return (wqcQuads wqc)
                            _ → do
                                result ← renderWorldQuads env worldState tileAlpha currentSnap
                                -- Group + depth-sort ONCE per rebuild, here on
                                -- the world thread — the frame loop then only
                                -- linear-merges dynamic quads into these runs
                                -- (#446).
                                let sorted = sortQuadsByLayer result
                                writeIORef (wsQuadCacheRef worldState) $
                                    Just (WorldQuadCache curGen currentSnap sorted)
                                return sorted
                    Nothing → return Map.empty
            return $ Map.unionsWith mergeSortedQuads quads

    -- Cursor quads are generated every frame (cheap: just 1-2 quads)
    -- so they respond instantly to mouse movement
    worldCursorQuads ← if tileAlpha ≤ 0.001
        then return V.empty
        else do
            cursorResults ← forM (wmVisible worldManager) $ \pageId →
                case lookup pageId (wmWorlds worldManager) of
                    Just worldState →
                        renderWorldCursorQuads env worldState tileAlpha
                    Nothing → return V.empty
            return $ V.concat cursorResults

    -- Ground-item quads, also per-frame: resting height derives from
    -- the CURRENT terrain each frame, so items drop with dug tiles
    -- and sit on slopes without any re-grounding machinery.
    groundItemQuads ← if tileAlpha ≤ 0.001
        then return V.empty
        else do
            giResults ← forM (wmVisible worldManager) $ \pageId →
                case lookup pageId (wmWorlds worldManager) of
                    Just worldState →
                        renderGroundItemQuads env worldState tileAlpha
                    Nothing → return V.empty
            return $ V.concat giResults

    -- Spoil-pile overlays (dig yields): per-frame for the same
    -- reason — piles change every dig tick, and the partial fringe
    -- is small (full cells promote to real terrain and render
    -- through the cached tile pass).
    spoilQuads ← if tileAlpha ≤ 0.001
        then return V.empty
        else do
            spResults ← forM (wmVisible worldManager) $ \pageId →
                case lookup pageId (wmWorlds worldManager) of
                    Just worldState →
                        renderSpoilQuads env worldState tileAlpha
                    Nothing → return V.empty
            return $ V.concat spResults

    -- Blood decal quads (#606): per-frame, same reason as ground items —
    -- aging tint is derived from the current game time, and a texture
    -- only has GPU-resident data once 'uploadBloodTextures' catches up
    -- (Engine.Scripting.Lua.Message), so a decal simply doesn't
    -- contribute a quad until then.
    bloodQuads ← if tileAlpha ≤ 0.001
        then return V.empty
        else do
            blResults ← forM (wmVisible worldManager) $ \pageId →
                case lookup pageId (wmWorlds worldManager) of
                    Just worldState →
                        renderBloodDecalQuads env pageId worldState tileAlpha
                    Nothing → return V.empty
            return $ V.concat blResults

    -- Unit quads are generated every frame (cheap: handful of sprites)
    -- so they respond instantly to movement
    unitQuads ← if tileAlpha ≤ 0.001
        then return V.empty
        else do
            let facing = camFacing camera
                zSlice = camZSlice camera
            renderUnitQuads env facing zSlice effDepth tileAlpha

    -- Buildings: same shape as units, simpler internals. Plus the
    -- optional ghost preview while in placement mode.
    buildingQuads ← if tileAlpha ≤ 0.001
        then return V.empty
        else do
            let facing = camFacing camera
                zSlice = camZSlice camera
            renderBuildingQuads env facing zSlice effDepth tileAlpha

    -- Structures (walls / floors / ceilings) — same iso-sorted quad path
    -- as buildings, with each piece's own facemap slot.
    structureQuads ← if tileAlpha ≤ 0.001
        then return V.empty
        else do
            let facing = camFacing camera
                zSlice = camZSlice camera
            stResults ← forM (wmVisible worldManager) $ \pageId →
                case lookup pageId (wmWorlds worldManager) of
                    Just worldState →
                        renderStructureQuads env worldState facing zSlice
                                             effDepth tileAlpha
                    Nothing → return V.empty
            return $ V.concat stResults

    ghostQuads ← if tileAlpha ≤ 0.001
        then return V.empty
        else do
            let facing = camFacing camera
                zSlice = camZSlice camera
            renderGhostQuad env facing zSlice

    zoomQuads ← generateZoomMapQuads env camera fbW fbH

    let shouldTrack = camZTracking camera
                    ∨ (tileAlpha > 0.001 ∧ tileAlpha < 0.999)
    when shouldTrack $ do
        when (not (camZTracking camera)) $
            atomicModifyIORef' (cameraRef env) $ \cam →
                (cam { camZTracking = True }, ())
        -- Track the ACTIVE world's terrain under the camera. Previously this
        -- looped every visible world and let the LAST one win, disagreeing
        -- with camera.gotoTile (also last-wins) and findVisualCenterTile
        -- (first-visible). All three now resolve the one active world (#81).
        worldManager' ← readIORef (worldManagerRef env)
        case resolveActiveWorld worldManager' of
            Just (_, worldState) → do
                tileData ← readIORef (wsTilesRef worldState)
                let (camX, camY) = camPosition camera
                    facing = camFacing camera
                    (gx, gy) = worldToGrid facing camX camY
                    (chunkCoord, (lx, ly)) = globalToChunk gx gy
                case lookupChunk chunkCoord tileData of
                    Just lc → do
                        let surfElev = (lcSurfaceMap lc) VU.! columnIndex lx ly
                            targetZ = surfElev + surfaceHeadroom
                        atomicModifyIORef' (cameraRef env) $ \cam →
                            (cam { camZSlice = targetZ }, ())
                    Nothing → return ()
            Nothing → return ()

    -- Static terrain rides pre-sorted per layer; everything per-tick
    -- stays a flat run the frame loop sorts (it's small) and merges in.
    let dynQuads = worldCursorQuads <> spoilQuads
                <> bloodQuads <> groundItemQuads
                <> buildingQuads <> structureQuads
                <> unitQuads <> ghostQuads <> zoomQuads
    return (LayeredQuads tileQuads dynQuads)
