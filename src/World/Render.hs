{-# LANGUAGE Strict #-}
module World.Render
    ( updateWorldTiles
    , surfaceHeadroom
    ) where

import UPrelude
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), toWorldSimCapability)
import qualified Data.Map as Map
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V
import Data.IORef (readIORef, writeIORef, atomicModifyIORef')
import Engine.Core.State (EngineEnv, resolveActiveWorld)
import Engine.Core.Capability.RenderView
  (RenderViewCapability(..), toRenderViewCapability)
import Engine.Scene.Types (LayeredQuads(..), SortableQuad, mergeSortedQuads
                          , sortQuadsByLayer, stampSolarPage)
import Engine.Scene.Stats
    ( SceneCategory(..), forcedLayeredQuadCount, forcedQuadCount
    , measureCategory, publishSceneStats )
import Engine.Core.Capability.RenderHandoff
    (RenderHandoffCapability(..), toRenderHandoffCapability)
import Engine.Graphics.Solar (SolarBase(..), SolarPageTable, solarPageNone)
import World.Render.Solar (solarSlotAssignment, buildSolarPageTable)
import qualified Data.HashMap.Strict as HM
import Engine.Graphics.Camera (Camera2D(..))
import World.Types
import World.Generate (viewDepth)
import World.Grid (zoomFadeStart, zoomFadeEnd, worldToGrid)
import World.Generate.Coordinates (canonicalTileFrame)

import World.Render.Zoom.Quads (generateZoomMapQuadsScanned)
import World.Render.Camera (cameraChanged)
import World.Render.Quads (renderWorldQuadsScanned)
import World.Render.CursorQuads (renderWorldCursorQuadsScanned)
import World.Render.GroundItemQuads (renderGroundItemQuadsScanned)
import World.Render.SpoilQuads (renderSpoilQuadsScanned)
import World.Render.BloodQuads (renderBloodDecalQuadsScanned)
import Unit.Render (renderUnitQuadsScanned)
import Building.Render (renderBuildingQuadsScanned, renderGhostQuadScanned)
import Structure.Render (renderStructureQuadsScanned)

-- * Surface Headroom

surfaceHeadroom ∷ Int
surfaceHeadroom = 25

-- * Top-Level Entry Point

updateWorldTiles ∷ EngineEnv → IO LayeredQuads
updateWorldTiles env = do
    camera ← readIORef (rvCameraRef (toRenderViewCapability env))
    (fbW, fbH) ← readIORef (rvFramebufferSizeRef (toRenderViewCapability env))

    let zoom = camZoom camera
        tileAlpha = clamp01 (1.0 - (zoom - zoomFadeStart) / (zoomFadeEnd - zoomFadeStart))
        -- Terrain's visible Z-band depth below the slice (same formula as
        -- Quads.hs). Units/buildings cull to this so a sprite is hidden
        -- exactly when its ground tile is — only ABOVE the slice (camera
        -- below it) or past the view depth, never just for being below
        -- the camera's own terrain level (the old fixed-25 bug: base-of-
        -- cliff sprites vanished when viewed from the top).
        effDepth = min viewDepth (max 8 (round (zoom * 80.0 + 8.0 ∷ Float)))

    worldManager ← readIORef (wsWorldManagerRef (toWorldSimCapability env))

    -- Per-page solar attribution (#1869). Several pages can be visible
    -- at once, each on its own clock and its own circumference, and the
    -- merge below deliberately discards page identity — so every quad
    -- is stamped with its page's slot HERE, while the page is still
    -- known, and the table those slots index travels out with the quads.
    (solarSlotOf, solarTable) ← buildFrameSolar env worldManager

    -- Scene-assembly telemetry (#1921). Every category below is
    -- wrapped in 'measureCategory', which times it from BEFORE its
    -- activation guard to AFTER both its scanned count and its emitted
    -- quads have been forced — so a guard's early return is charged to
    -- the category that took it, and no deferred assembly work is
    -- charged to a later one. The measurement adds no per-object work
    -- and no allocation proportional to the sources counted; only the
    -- fixed-size snapshot published at the end of the pass.
    (tilesStat, tileQuads) ← measureCategory ScTiles forcedLayeredQuadCount $
      if tileAlpha ≤ 0.001
        then return (0, Map.empty)
        else do
            let currentSnap = WorldCameraSnapshot
                    { wcsPosition = camPosition camera
                    , wcsZoom     = zoom
                    , wcsZSlice   = camZSlice camera
                    , wcsFbSize   = (fbW, fbH)
                    , wcsFacing   = camFacing camera
                    }
            pages ← forM (wmVisible worldManager) $ \pageId →
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
                        let solarSlot = solarSlotOf pageId
                        case cached of
                            -- The stamped solar slot joins the reuse test
                            -- (#1869): attribution is baked into these
                            -- vertices, so a cache built when this page held
                            -- a different slot must rebuild rather than draw
                            -- with another page's sun. A visible-list REORDER
                            -- leaves the assignment identical and so reuses.
                            Just wqc | wqcGen wqc ≡ curGen
                                     , wqcSolarSlot wqc ≡ solarSlot
                                     , not (cameraChanged (wqcCamera wqc) currentSnap) →
                                -- A reuse visits no terrain cell (#1921),
                                -- but still contributes its cached quads.
                                return (0, wqcQuads wqc)
                            _ → do
                                (cells, result) ← renderWorldQuadsScanned
                                                      env worldState tileAlpha
                                                      currentSnap
                                -- Group + depth-sort ONCE per rebuild, here on
                                -- the world thread — the frame loop then only
                                -- linear-merges dynamic quads into these runs
                                -- (#446).
                                let sorted = Map.map (stampSolarPage solarSlot)
                                                     (sortQuadsByLayer result)
                                writeIORef (wsQuadCacheRef worldState) $
                                    Just (WorldQuadCache curGen currentSnap solarSlot sorted)
                                return (cells, sorted)
                    Nothing → return (0, Map.empty)
            return ( sum (map fst pages)
                   , Map.unionsWith mergeSortedQuads (map snd pages) )

    -- Cursor quads are generated every frame (cheap: just 1-2 quads)
    -- so they respond instantly to mouse movement
    (cursorStat, worldCursorQuads) ← measureCategory ScCursor forcedQuadCount $
      if tileAlpha ≤ 0.001
        then return (0, V.empty)
        else perVisiblePage worldManager $ \pageId worldState →
                stampPageQuads (solarSlotOf pageId) ⊚
                    renderWorldCursorQuadsScanned env pageId worldState tileAlpha

    -- Ground-item quads, also per-frame: resting height derives from
    -- the CURRENT terrain each frame, so items drop with dug tiles
    -- and sit on slopes without any re-grounding machinery.
    (groundItemStat, groundItemQuads) ←
      measureCategory ScGroundItems forcedQuadCount $
        if tileAlpha ≤ 0.001
          then return (0, V.empty)
          else perVisiblePage worldManager $ \pageId worldState →
                  stampPageQuads (solarSlotOf pageId) ⊚
                      renderGroundItemQuadsScanned env worldState tileAlpha

    -- Spoil-pile overlays (dig yields): per-frame for the same
    -- reason — piles change every dig tick, and the partial fringe
    -- is small (full cells promote to real terrain and render
    -- through the cached tile pass).
    (spoilStat, spoilQuads) ← measureCategory ScSpoil forcedQuadCount $
      if tileAlpha ≤ 0.001
        then return (0, V.empty)
        else perVisiblePage worldManager $ \pageId worldState →
                stampPageQuads (solarSlotOf pageId) ⊚
                    renderSpoilQuadsScanned env worldState tileAlpha

    -- Blood decal quads (#606): per-frame, same reason as ground items —
    -- aging tint is derived from the current game time, and a texture
    -- only has GPU-resident data once 'uploadBloodTextures' catches up
    -- (Engine.Scripting.Lua.Message), so a decal simply doesn't
    -- contribute a quad until then.
    (bloodStat, bloodQuads) ← measureCategory ScBlood forcedQuadCount $
      if tileAlpha ≤ 0.001
        then return (0, V.empty)
        else perVisiblePage worldManager $ \pageId worldState →
                stampPageQuads (solarSlotOf pageId) ⊚
                    renderBloodDecalQuadsScanned env pageId worldState tileAlpha

    -- Unit quads are generated every frame (cheap: handful of sprites)
    -- so they respond instantly to movement
    (unitStat, unitQuads) ← measureCategory ScUnits forcedQuadCount $
      if tileAlpha ≤ 0.001
        then return (0, V.empty)
        else do
            let facing = camFacing camera
                zSlice = camZSlice camera
            renderUnitQuadsScanned env solarSlotOf facing zSlice effDepth tileAlpha

    -- Buildings: same shape as units, simpler internals. Plus the
    -- optional ghost preview while in placement mode.
    (buildingStat, buildingQuads) ← measureCategory ScBuildings forcedQuadCount $
      if tileAlpha ≤ 0.001
        then return (0, V.empty)
        else do
            let facing = camFacing camera
                zSlice = camZSlice camera
            renderBuildingQuadsScanned env solarSlotOf facing zSlice effDepth tileAlpha

    -- Structures (walls / floors / ceilings) — same iso-sorted quad path
    -- as buildings, with each piece's own facemap slot.
    (structureStat, structureQuads) ← measureCategory ScStructures forcedQuadCount $
      if tileAlpha ≤ 0.001
        then return (0, V.empty)
        else do
            let facing = camFacing camera
                zSlice = camZSlice camera
            perVisiblePage worldManager $ \pageId worldState →
                stampPageQuads (solarSlotOf pageId) ⊚
                    renderStructureQuadsScanned env worldState facing zSlice
                                                effDepth tileAlpha

    (ghostStat, ghostQuads) ← measureCategory ScGhost forcedQuadCount $
      if tileAlpha ≤ 0.001
        then return (0, V.empty)
        else do
            let facing = camFacing camera
                zSlice = camZSlice camera
                -- The ghost previews a placement on the ACTIVE page, so
                -- it is lit by that page (#1869).
                activeSolarSlot = maybe solarPageNone (solarSlotOf . fst)
                                        (resolveActiveWorld worldManager)
            renderGhostQuadScanned env activeSolarSlot facing zSlice
                                   effDepth tileAlpha

    (zoomStat, zoomQuads) ← measureCategory ScZoomMap forcedQuadCount $
        generateZoomMapQuadsScanned env solarSlotOf camera fbW fbH

    let shouldTrack = camZTracking camera
                    ∨ (tileAlpha > 0.001 ∧ tileAlpha < 0.999)
    when shouldTrack $ do
        when (not (camZTracking camera)) $
            atomicModifyIORef' (rvCameraRef (toRenderViewCapability env)) $ \cam →
                (cam { camZTracking = True }, ())
        -- Track the ACTIVE world's terrain under the camera. Previously this
        -- looped every visible world and let the LAST one win, disagreeing
        -- with camera.gotoTile (also last-wins) and findVisualCenterTile
        -- (first-visible). All three now resolve the one active world (#81).
        worldManager' ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
        case resolveActiveWorld worldManager' of
            Just (_, worldState) → do
                tileData ← readIORef (wsTilesRef worldState)
                trackParams ← readIORef (wsGenParamsRef worldState)
                let (camX, camY) = camPosition camera
                    facing = camFacing camera
                    worldSize' = maybe 128 wgpWorldSize trackParams
                    (gx, gy) = worldToGrid facing camX camY
                    -- Camera-derived, so canonical only as far as the
                    -- pan path's own wrap plus rounding: right on the
                    -- seam boundary the rounded tile can land one past
                    -- the canonical range and silently stall z-tracking
                    -- for that frame. Resolve the STORED chunk instead
                    -- (#1135). Read-only at a local index — no tile
                    -- coord travels on from here, so the key is all
                    -- there is to canonicalise.
                    (chunkCoord, (lx, ly), _) =
                        canonicalTileFrame worldSize' gx gy
                case lookupChunk chunkCoord tileData of
                    Just lc → do
                        let surfElev = (lcSurfaceMap lc) VU.! columnIndex lx ly
                            targetZ = surfElev + surfaceHeadroom
                        atomicModifyIORef' (rvCameraRef (toRenderViewCapability env)) $ \cam →
                            (cam { camZSlice = targetZ }, ())
                    Nothing → return ()
            Nothing → return ()

    -- One atomic publication per completed pass (#1921), so a reader
    -- never sees rows from two passes together. The sequence advances
    -- exactly once here; a world teardown clears the whole snapshot.
    publishSceneStats (rhSceneStatsRef (toRenderHandoffCapability env))
        [ tilesStat, cursorStat, groundItemStat, spoilStat, bloodStat
        , unitStat, buildingStat, structureStat, ghostStat, zoomStat ]

    -- Static terrain rides pre-sorted per layer; everything per-tick
    -- stays a flat run the frame loop sorts (it's small) and merges in.
    let dynQuads = worldCursorQuads <> spoilQuads
                <> bloodQuads <> groundItemQuads
                <> buildingQuads <> structureQuads
                <> unitQuads <> ghostQuads <> zoomQuads
    return (LayeredQuads tileQuads dynQuads solarTable)

-- | Run one per-page producer over every visible page, summing the
--   scanned counts and concatenating the quads.
--
--   The five per-page categories all had this shape already; naming it
--   keeps the telemetry (#1921) from duplicating the traversal five
--   times. A visible id with no page state contributes nothing, exactly
--   as before.
perVisiblePage
    ∷ WorldManager
    → (WorldPageId → WorldState → IO (Int, V.Vector SortableQuad))
    → IO (Int, V.Vector SortableQuad)
perVisiblePage worldManager produce = do
    results ← forM (wmVisible worldManager) $ \pageId →
        case lookup pageId (wmWorlds worldManager) of
            Just worldState → produce pageId worldState
            Nothing         → return (0, V.empty)
    return (sum (map fst results), V.concat (map snd results))

-- | Stamp a per-page producer's quads with that page's solar slot
--   (#1869), leaving its scanned count (#1921) untouched.
stampPageQuads ∷ Word32 → (Int, V.Vector SortableQuad)
               → (Int, V.Vector SortableQuad)
stampPageQuads slot (scanned, quads) = (scanned, stampSolarPage slot quads)


-- | This frame's page→slot lookup and the table those slots index.
--
--   Both are derived from the SAME visible list in one read, so the
--   stamps a frame's quads carry and the table published beside them
--   can never describe different page sets. The base angle comes from
--   'wsSunAngleRef' and stands in only for a visible id with no page
--   state; each real page's own clock and world size come from its own
--   'WorldState'. @world.setSunAngle@ is NOT applied here — it is
--   overlaid onto whichever table the frame draws, at upload
--   ('Engine.Graphics.Solar.solarUniformEntries').
buildFrameSolar ∷ EngineEnv → WorldManager
                → IO (WorldPageId → Word32, SolarPageTable)
buildFrameSolar env worldManager = do
    solarBase ← readIORef (wsSunAngleRef (toWorldSimCapability env))
    let visible = wmVisible worldManager
    pageInputs ← fmap (HM.fromList . catMaybes) $ forM visible $ \pageId →
        case lookup pageId (wmWorlds worldManager) of
            Nothing → return Nothing
            Just worldState → do
                wt ← readIORef (wsTimeRef worldState)
                mParams ← readIORef (wsGenParamsRef worldState)
                return $ Just
                    ( pageId
                    , (worldTimeToSunAngle wt, wgpWorldSize ⊚ mParams) )
    let slots = solarSlotAssignment visible
        table = buildSolarPageTable (sbAngle solarBase)
                                    (`HM.lookup` pageInputs) visible
    return (\pageId → HM.lookupDefault solarPageNone pageId slots, table)
