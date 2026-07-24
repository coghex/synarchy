{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Render.CursorQuads
    ( renderWorldCursorQuads
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V
import Data.IORef (readIORef, atomicModifyIORef')
import Engine.Core.State (EngineEnv, buildingManagerRef, loggerRef)
import Engine.Core.Capability.RenderView
  (RenderViewCapability(..), toRenderViewCapability)
import Engine.Core.Log (logWarn, LogCategory(..))
import Engine.Asset.Handle (toInt)
import Engine.Scene.Types (SortableQuad(..))
import Engine.Graphics.Camera (Camera2D(..))
import Building.Types (BuildingManager(..))
import World.Types
import World.Generate (viewDepth)
import World.Generate.Coordinates (globalToChunk)
import World.Mine.Types (MineDesignation(..))
import World.Construct.Types (ConstructDesignation(..), ConstructTarget(..)
                            , constructDesignationFootprint)
import World.Chop.Types (ChopDesignation(..))
import World.Till.Types (TillDesignation(..))
import World.Plant.Types (PlantDesignation(..))
import World.Render.ViewBounds (computeViewBounds)
import World.Render.ChunkCulling (isChunkVisibleWrapped)
import World.Render.HitTest (pickWorldTile)
import World.Render.TileQuads (worldCursorToQuad, worldCursorBgToQuad)

-- * World Cursor Quads (generated every frame, not cached)

-- | Cap on the mine-tool preview rectangle's side length (the commit
--   handler has its own, larger cap). Keeps a cross-map drag from
--   generating tens of thousands of per-frame quads.
maxMinePreviewSide ∷ Int
maxMinePreviewSide = 64

renderWorldCursorQuads ∷ EngineEnv → WorldState → Float → IO (V.Vector SortableQuad)
renderWorldCursorQuads env worldState tileAlpha = do
    let rv = toRenderViewCapability env
    camera   ← readIORef (rvCameraRef rv)
    tileData ← readIORef (wsTilesRef worldState)
    textures ← readIORef (wsTexturesRef worldState)
    paramsM  ← readIORef (wsGenParamsRef worldState)
    cs       ← readIORef (wsCursorRef worldState)
    toolMode ← readIORef (wsToolModeRef worldState)

    (winW, winH) ← readIORef (rvWindowSizeRef rv)
    (fbW, fbH)   ← readIORef (rvFramebufferSizeRef rv)

    -- Stable handle ids; resolved to live slots in the shader (#286).
    let lookupSlot texHandle = fromIntegral (toInt texHandle)
        lookupFmSlot texHandle = fromIntegral (toInt texHandle)
        facing    = camFacing camera
        zoom      = camZoom camera
        zSlice    = camZSlice camera
        (camX, camY) = camPosition camera
        worldSize = case paramsM of
                      Nothing     → 128
                      Just params → wgpWorldSize params
        effectiveDepth = min viewDepth (max 8 (round (zoom * 80.0 + 8.0 ∷ Float)))
        vb = computeViewBounds camera fbW fbH effectiveDepth

    -- Hit-test: unproject screen pixel considering elevation. Shared with
    -- the synchronous Lua pick (@world.pickTile@) so the two can't drift —
    -- see 'World.Render.HitTest'. Always runs (independent of toolMode) so
    -- unit-move, info, and any other consumer of `worldHoverTile` can see
    -- the current tile.
    let hitTest pixX pixY =
            pickWorldTile facing zoom zSlice camX camY fbW fbH winW winH
                          worldSize effectiveDepth vb tileData pixX pixY

    -- Compute hover tile
    let hoverResult = case worldCursorPos cs of
            Nothing           → Nothing
            Just (pixX, pixY) → hitTest pixX pixY

    -- Persist the resolved hover tile so Lua callers can read tile
    -- coords (right-click → move uses this), and snapshot the selected
    -- tile when worldSelectNow is set.
    --
    -- This uses atomicModifyIORef' AND returns the merged state for
    -- the rest of this frame to use. Two reasons it must be atomic:
    --
    --   1. The world command thread can update wsCursorRef between our
    --      earlier readIORef and this writeback (e.g. a freshly
    --      processed WorldSelectTileByCoord). A plain writeIORef of a
    --      stale-cs-derived value would silently clobber that update.
    --
    --   2. Using the merged result downstream (instead of a cs' built
    --      from the stale initial cs) lets the highlight render on the
    --      SAME frame the selection landed, not the next one — which
    --      matters for one-shot selections triggered from Lua: there's
    --      no continuous hover to pick the tile up on the next tick.
    let newHoverTile = case hoverResult of
            Just (gx, gy, _, _, _) → Just (gx, gy)
            Nothing                → Nothing
        newHoverPos = case hoverResult of
            Just (_, _, _, _, hp) → Just hp
            Nothing               → Nothing
    cs' ← atomicModifyIORef' (wsCursorRef worldState) $ \current →
        let committedTile = worldSelectNow current ∧ case hoverResult of
                Just _  → True
                Nothing → False
            mergedSelected = if worldSelectNow current
                then case hoverResult of
                    Just (gx, gy, z, _, _) → Just (gx, gy, z)
                    Nothing                → worldSelectedTile current
                else worldSelectedTile current
            -- The newest selection owns the cursor: committing a tile
            -- selection drops any zoom-map chunk selection, so returning
            -- to the zoomed-out view shows no stale chunk highlight and
            -- the two can't coexist (issue #135). Cleared together with
            -- the tile set in this one atomic write — no blank window.
            mergedZoom = if committedTile then Nothing else zoomSelectedPos current
            merged = current { worldHoverTile    = newHoverTile
                             , worldHoverPos     = newHoverPos
                             , worldSelectNow    = False
                             , worldSelectedTile = mergedSelected
                             , zoomSelectedPos   = mergedZoom
                             }
        in (merged, merged)

    -- Mine-designation markers: world annotations, visible in every
    -- tool mode. Rendered from the surface z stored at designation
    -- time — no per-frame column reads.
    designations ← readIORef (wsMineDesignationsRef worldState)
    let designQuads = case mineDesignTexture cs' of
            Nothing → V.empty
            Just tex
                | HM.null designations → V.empty
                | otherwise → V.fromList
                    [ worldCursorToQuad lookupSlot lookupFmSlot textures
                          facing dgx dgy (mdZ md) zSlice effectiveDepth
                          tileAlpha xOff tex
                    | ((dgx, dgy), md) ← HM.toList designations
                    , let (chunkCoord, _) = globalToChunk dgx dgy
                    , Just xOff ← [isChunkVisibleWrapped facing worldSize
                                       vb camX chunkCoord]
                    ]

    -- Chop-designation markers (#97): world annotations like the mine
    -- markers, visible in every tool mode. Rendered from the surface z
    -- stored at designation time.
    chopDesigns ← readIORef (wsChopDesignationsRef worldState)
    let chopDesignQuads = case chopDesignTexture cs' of
            Nothing → V.empty
            Just tex
                | HM.null chopDesigns → V.empty
                | otherwise → V.fromList
                    [ worldCursorToQuad lookupSlot lookupFmSlot textures
                          facing dgx dgy (chZ cd) zSlice effectiveDepth
                          tileAlpha xOff tex
                    | ((dgx, dgy), cd) ← HM.toList chopDesigns
                    , let (chunkCoord, _) = globalToChunk dgx dgy
                    , Just xOff ← [isChunkVisibleWrapped facing worldSize
                                       vb camX chunkCoord]
                    ]

    -- Till-designation markers (#333): world annotations like the chop
    -- markers, visible in every tool mode. Rendered from the surface z
    -- stored at designation time.
    tillDesigns ← readIORef (wsTillDesignationsRef worldState)
    let tillDesignQuads = case tillDesignTexture cs' of
            Nothing → V.empty
            Just tex
                | HM.null tillDesigns → V.empty
                | otherwise → V.fromList
                    [ worldCursorToQuad lookupSlot lookupFmSlot textures
                          facing dgx dgy (tlZ td) zSlice effectiveDepth
                          tileAlpha xOff tex
                    | ((dgx, dgy), td) ← HM.toList tillDesigns
                    , let (chunkCoord, _) = globalToChunk dgx dgy
                    , Just xOff ← [isChunkVisibleWrapped facing worldSize
                                       vb camX chunkCoord]
                    ]

    -- Plant-designation markers (#335): world annotations like the
    -- till markers, visible in every tool mode. Rendered from the
    -- surface z stored at designation time.
    plantDesigns ← readIORef (wsPlantDesignationsRef worldState)
    let plantDesignQuads = case plantDesignTexture cs' of
            Nothing → V.empty
            Just tex
                | HM.null plantDesigns → V.empty
                | otherwise → V.fromList
                    [ worldCursorToQuad lookupSlot lookupFmSlot textures
                          facing dgx dgy (ptZ pd) zSlice effectiveDepth
                          tileAlpha xOff tex
                    | ((dgx, dgy), pd) ← HM.toList plantDesigns
                    , let (chunkCoord, _) = globalToChunk dgx dgy
                    , Just xOff ← [isChunkVisibleWrapped facing worldSize
                                       vb camX chunkCoord]
                    ]

    -- Construction-designation ghosts (#95): world annotations like the
    -- mine markers, visible in every tool mode. Each renders with the
    -- ghost texture for its target category (structure vs building).
    constructDesigns ← readIORef (wsConstructDesignationsRef worldState)
    bm ← readIORef (buildingManagerRef env)

    -- #807: a CtBuilding designation is stored as ONE anchor-only map
    -- entry regardless of footprint size (one durable job), so it must
    -- be expanded here into the def's full footprint — the ghost used
    -- to render only the anchor tile. A designation naming a def
    -- missing from bmDefs (a broken save/mod) falls back to that
    -- anchor-only tile instead of guessing geometry
    -- (constructDesignationFootprint); since this pass runs every
    -- frame, warn about it only ONCE per distinct missing name per
    -- session rather than flooding the log.
    let missingBuildingDefs = HS.fromList
            [ defName
            | (_, cd) ← HM.toList constructDesigns
            , CtBuilding defName ← [cdTarget cd]
            , not (HM.member defName (bmDefs bm))
            ]
        newlyMissingDefs = HS.difference missingBuildingDefs
                                          (constructMissingDefsWarned cs')
    unless (HS.null newlyMissingDefs) $ do
        logger ← readIORef (loggerRef env)
        forM_ (HS.toList newlyMissingDefs) $ \defName →
            logWarn logger CatRender $
                "construction blueprint: unknown building def '"
                <> defName <> "' — rendering anchor tile only"
        atomicModifyIORef' (wsCursorRef worldState) $ \cs →
            ( cs { constructMissingDefsWarned =
                     HS.union newlyMissingDefs (constructMissingDefsWarned cs) }
            , () )

    let constructTexFor cd = case cdTarget cd of
            CtStructure _ → constructStructTexture cs'
            CtBuilding  _ → constructBuildingTexture cs'
        -- Build-progress display (#96): the blueprint ghost solidifies
        -- as the build AI pours progress in — a fresh designation sits
        -- at 45 % alpha and ramps to opaque at progress 1.0 (the piece
        -- itself then replaces the ghost). The mining analogue carves
        -- terrain slopes per corner; construction ADDS material, so the
        -- ramp is the marker-level equivalent. Applied uniformly across
        -- every footprint tile of a building designation, same as the
        -- texture — one job, one consistent tint/alpha (#807 req 3).
        constructAlphaFor cd =
            tileAlpha * (0.45 + 0.55 * max 0.0 (min 1.0 (cdProgress cd)))
        constructDesignQuads
            | HM.null constructDesigns = V.empty
            | otherwise = V.fromList
                [ worldCursorToQuad lookupSlot lookupFmSlot textures
                      facing fx fy (cdZ cd) zSlice effectiveDepth
                      (constructAlphaFor cd) xOff tex
                | (anchor, cd) ← HM.toList constructDesigns
                , Just tex ← [constructTexFor cd]
                , (fx, fy) ← constructDesignationFootprint (bmDefs bm) anchor cd
                , let (chunkCoord, _) = globalToChunk fx fy
                , Just xOff ← [isChunkVisibleWrapped facing worldSize
                                   vb camX chunkCoord]
                ]

    -- Hover quads (bg + fg) — used by both info and mine tools.
    let hoverQuads = case hoverResult of
            Just (gx, gy, hz, xOff, _) →
                let fgQuad = case worldHoverTexture cs' of
                        Just tex → V.singleton $
                            worldCursorToQuad lookupSlot lookupFmSlot
                              textures facing gx gy hz zSlice effectiveDepth
                              tileAlpha xOff tex
                        Nothing → V.empty
                    bgQuad = case worldHoverBgTexture cs' of
                        Just tex → V.singleton $
                            worldCursorBgToQuad lookupSlot lookupFmSlot textures facing
                                gx gy hz zSlice effectiveDepth tileAlpha xOff tex
                        Nothing → V.empty
                in bgQuad <> fgQuad
            _ → V.empty

    -- Select quads (bg + fg) — info tool only.
    let selectQuads = case (worldSelectedTile cs', worldCursorTexture cs', worldCursorBgTexture cs') of
            (Just (sgx, sgy, sz), _, _) →
                let (chunkCoord, _) = globalToChunk sgx sgy
                in case isChunkVisibleWrapped facing worldSize vb camX chunkCoord of
                    Just xOff →
                        let fgQuad = case worldCursorTexture cs' of
                                Just tex → V.singleton $
                                    worldCursorToQuad lookupSlot lookupFmSlot
                                                      textures facing sgx sgy sz
                                                      zSlice effectiveDepth
                                                      tileAlpha xOff tex
                                Nothing → V.empty
                            bgQuad = case worldCursorBgTexture cs' of
                                Just tex → V.singleton $
                                    worldCursorBgToQuad lookupSlot lookupFmSlot textures facing
                                        sgx sgy sz zSlice effectiveDepth tileAlpha xOff tex
                                Nothing → V.empty
                        in bgQuad <> fgQuad
                    Nothing → V.empty
            _ → V.empty

    -- Mine tool: anchored rectangle preview, anchor→hover (DF-style).
    -- Drawn with the select-cursor texture so it reads as "about to be
    -- designated". DESIGNATIONS ARE PER-Z-LEVEL: only tiles whose
    -- surface z equals the ANCHOR tile's surface z are previewed
    -- (matching WorldDesignateMine's filter), so sweeping across a
    -- slope marks just the anchor's level. Tiles in unloaded chunks
    -- are skipped, same as the commit.
    let clampSide a b
            | b ≥ a     = min b (a + maxMinePreviewSide - 1)
            | otherwise = max b (a - maxMinePreviewSide + 1)
        surfaceZAt gx gy = do
            let (chunkCoord, (lx, ly)) = globalToChunk gx gy
            lc ← HM.lookup chunkCoord (wtdChunks tileData)
            pure (lcSurfaceMap lc VU.! columnIndex lx ly)
        minePreviewQuads = case (mineAnchor cs', hoverResult, worldCursorTexture cs') of
            (Just (ax, ay), Just (hx, hy, _, _, _), Just tex)
                | Just anchorZ ← surfaceZAt ax ay →
                let hx' = clampSide ax hx
                    hy' = clampSide ay hy
                    xLo = min ax hx'
                    xHi = max ax hx'
                    yLo = min ay hy'
                    yHi = max ay hy'
                in V.fromList
                    [ worldCursorToQuad lookupSlot lookupFmSlot textures
                          facing gx gy z zSlice effectiveDepth
                          tileAlpha xOff tex
                    | gx ← [xLo .. xHi]
                    , gy ← [yLo .. yHi]
                    , Just z ← [surfaceZAt gx gy]
                    , z ≡ anchorZ
                    , let (chunkCoord, _) = globalToChunk gx gy
                    , Just xOff ← [isChunkVisibleWrapped facing worldSize
                                       vb camX chunkCoord]
                    ]
            _ → V.empty

    -- Structure-piece designation preview (#403 — folded into the build
    -- tool): anchor→hover rectangle, mirroring the mine preview
    -- (per-z-level, unloaded chunks skipped). Drawn with the
    -- select-cursor texture so it reads as "about to be designated".
    --
    -- Wire path tool (#359): 'constructLineMode' constrains this to a
    -- straight 1-wide LINE along whichever axis has the larger extent
    -- from the anchor, instead of the filled rectangle — the build
    -- tool's commit (scripts/build_tool.lua) snaps the SAME way before
    -- calling construction.designate, so what previews is what commits.
    let constructPreviewQuads = case (constructAnchor cs', hoverResult, worldCursorTexture cs') of
            (Just (ax, ay), Just (hx, hy, _, _, _), Just tex)
                | Just anchorZ ← surfaceZAt ax ay →
                let hx' = clampSide ax hx
                    hy' = clampSide ay hy
                    dx = hx' - ax
                    dy = hy' - ay
                    tiles
                        | constructLineMode cs' =
                            if abs dx ≥ abs dy
                            then [ (gx, ay) | gx ← [min ax hx' .. max ax hx'] ]
                            else [ (ax, gy) | gy ← [min ay hy' .. max ay hy'] ]
                        | otherwise =
                            [ (gx, gy)
                            | gx ← [min ax hx' .. max ax hx']
                            , gy ← [min ay hy' .. max ay hy'] ]
                in V.fromList
                    [ worldCursorToQuad lookupSlot lookupFmSlot textures
                          facing gx gy z zSlice effectiveDepth
                          tileAlpha xOff tex
                    | (gx, gy) ← tiles
                    , Just z ← [surfaceZAt gx gy]
                    , z ≡ anchorZ
                    , let (chunkCoord, _) = globalToChunk gx gy
                    , Just xOff ← [isChunkVisibleWrapped facing worldSize
                                       vb camX chunkCoord]
                    ]
            _ → V.empty

    -- Chop tool: anchor→hover rectangle preview. Unlike the mine /
    -- construct previews there is NO per-z-level filter — the commit
    -- takes wood-tagged flora at any surface z (forests span slopes) —
    -- so every loaded tile in the rectangle previews at its own z.
    let chopPreviewQuads = case (chopAnchor cs', hoverResult, worldCursorTexture cs') of
            (Just (ax, ay), Just (hx, hy, _, _, _), Just tex) →
                let hx' = clampSide ax hx
                    hy' = clampSide ay hy
                    xLo = min ax hx'
                    xHi = max ax hx'
                    yLo = min ay hy'
                    yHi = max ay hy'
                in V.fromList
                    [ worldCursorToQuad lookupSlot lookupFmSlot textures
                          facing gx gy z zSlice effectiveDepth
                          tileAlpha xOff tex
                    | gx ← [xLo .. xHi]
                    , gy ← [yLo .. yHi]
                    , Just z ← [surfaceZAt gx gy]
                    , let (chunkCoord, _) = globalToChunk gx gy
                    , Just xOff ← [isChunkVisibleWrapped facing worldSize
                                       vb camX chunkCoord]
                    ]
            _ → V.empty

    -- Till tool: anchor→hover rectangle preview. Per-z-level like mine/
    -- construct — a farmed field is flat ground, unlike chop's
    -- slope-spanning forest sweep.
    let tillPreviewQuads = case (tillAnchor cs', hoverResult, worldCursorTexture cs') of
            (Just (ax, ay), Just (hx, hy, _, _, _), Just tex)
                | Just anchorZ ← surfaceZAt ax ay →
                let hx' = clampSide ax hx
                    hy' = clampSide ay hy
                    xLo = min ax hx'
                    xHi = max ax hx'
                    yLo = min ay hy'
                    yHi = max ay hy'
                in V.fromList
                    [ worldCursorToQuad lookupSlot lookupFmSlot textures
                          facing gx gy z zSlice effectiveDepth
                          tileAlpha xOff tex
                    | gx ← [xLo .. xHi]
                    , gy ← [yLo .. yHi]
                    , Just z ← [surfaceZAt gx gy]
                    , z ≡ anchorZ
                    , let (chunkCoord, _) = globalToChunk gx gy
                    , Just xOff ← [isChunkVisibleWrapped facing worldSize
                                       vb camX chunkCoord]
                    ]
            _ → V.empty

    -- Mine + construction + chop + till markers are world annotations:
    -- shown in every tool mode. The mode only adds its own hover/preview
    -- on top.
    let markerQuads = designQuads <> constructDesignQuads <> chopDesignQuads
                    <> tillDesignQuads <> plantDesignQuads
    return $ case toolMode of
        InfoTool  → markerQuads <> hoverQuads <> selectQuads
        MineTool  → markerQuads <> hoverQuads <> minePreviewQuads
        -- #403: the build tool now drives construction designation too
        -- (structure-piece rectangles), so it gets the same preview the
        -- standalone construct tool used to.
        BuildTool → markerQuads <> hoverQuads <> constructPreviewQuads
        ChopTool  → markerQuads <> hoverQuads <> chopPreviewQuads
        TillTool  → markerQuads <> hoverQuads <> tillPreviewQuads
        PlantTool → markerQuads <> hoverQuads
        _         → markerQuads
