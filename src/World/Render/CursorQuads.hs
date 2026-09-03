{-# LANGUAGE Strict #-}
module World.Render.CursorQuads
    ( renderWorldCursorQuads
    , renderWorldCursorQuadsScanned
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V
import Data.IORef (readIORef, atomicModifyIORef')
import Engine.Core.State (EngineEnv, loggerRef)
import Engine.Core.Capability.RenderHandoff
  (RenderHandoffCapability(..), toRenderHandoffCapability)
import Engine.Core.Capability.RenderView
  (RenderViewCapability(..), toRenderViewCapability)
import Engine.Core.Capability.WorldSim
  (WorldSimCapability(..), toWorldSimCapability)
import Engine.Core.Log (logWarn, LogCategory(..))
import Engine.Asset.Handle (toInt)
import Engine.Scene.Types (SortableQuad(..))
import Engine.Graphics.Camera (Camera2D(..))
import Building.Types (BuildingManager(..))
import Building.Render (buildingGhostQuad)
import Building.Placement (buildingAnchorZ)
import Building.Visual (buildingStakedAt, designatedGhostAlpha)
import Engine.Graphics.Vulkan.Types.Vertex (noFaceMapVertexId)
import World.Types
import World.Generate (viewDepth)
import World.Generate.Coordinates
    (globalToChunk, canonicalTileFrame, localizeTileToAnchor)
import World.Mine.Types (MineDesignation(..))
import World.Construct.Types (ConstructDesignation(..), ConstructTarget(..))
import World.Till.Types (TillDesignation(..))
import World.Plant.Types (PlantDesignationOf(..))
import World.Construct.Extent (structureDragExtent)
import World.Construct.Plan (PlanWorld(..))
import Structure.Render (translateQuad)
import World.Render.StructureGhost
    ( GhostEnv(..), structureDesignationGhosts, structurePreviewGhosts )
import World.Render.Camera (placementCamera, quadCacheMargins)
import World.Render.ViewBounds
    (computeViewBounds, expandViewBounds, viewBoundsAt)
import World.Render.ChunkCulling (isChunkVisibleWrapped)
import World.Render.HitTest (pickWorldTile)
import World.Render.TileQuads
    (worldCursorToQuad, worldFlatCursorToQuad, worldCursorBgToQuad)
import World.Render.FloraDraws (FloraDraw(..), chunkFloraDraws)
import World.Render.FloraMarker (floraMarkerQuad)
import World.Render.FloraProjection
    (FloraGeom(..), floraGeom, floraTexSize, floraVisibleInSlice)
import World.Render.SpriteDepth (frameFrontWallLift, liftSpriteSortKey)

-- * World Cursor Quads (generated every frame, not cached)

-- | Cap on the mine-tool preview rectangle's side length (the commit
--   handler has its own, larger cap). Keeps a cross-map drag from
--   generating tens of thousands of per-frame quads.
maxMinePreviewSide ∷ Int
maxMinePreviewSide = 64

renderWorldCursorQuads ∷ EngineEnv → BuildingManager → WorldPageId
                       → WorldState → Float → IO (V.Vector SortableQuad)
renderWorldCursorQuads env bm pageId worldState tileAlpha =
    snd ⊚ renderWorldCursorQuadsScanned env bm pageId worldState tileAlpha

-- | 'renderWorldCursorQuads' with the scene-assembly telemetry (#1921)
--   this pass contributes: the marker-tile CANDIDATES it evaluated,
--   paired with the quads it produced.
--
--   \"Candidates\" is the set the ACTIVE tool mode's returned vector is
--   built from, and nothing else: the always-on marker builders (mine,
--   chop, till, plant, and the two construction ghosts — one candidate
--   per committed BUILDING designation since #1845 made it one sprite
--   rather than one marker per footprint tile, and one per unpaid
--   STRUCTURE site, counted by that builder itself) plus that mode's
--   own hover, selection and preview builders. This module is @Strict@, so every builder's bindings are
--   forced whichever mode is active — but a builder whose quads the
--   mode does not return contributed no candidate to the answer, and
--   counting it would pair a scanned total with an unrelated emitted
--   one. Pinning the count to the returned set also keeps it correct if
--   the unused builders ever become lazy.
--
--   A candidate is counted BEFORE per-chunk visibility culling, and one
--   candidate may still emit two quads (a background and a foreground),
--   so no @emitted <= scanned@ relation holds here.
renderWorldCursorQuadsScanned
    ∷ EngineEnv → BuildingManager → WorldPageId → WorldState → Float
    → IO (Int, V.Vector SortableQuad)
renderWorldCursorQuadsScanned env bm pageId worldState tileAlpha = do
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
                          tileAlpha wrapOff tex
                    | ((dgx, dgy), md) ← HM.toList designations
                    , let (chunkCoord, _) = globalToChunk dgx dgy
                    , Just wrapOff ← [isChunkVisibleWrapped facing worldSize
                                          vb camX camY chunkCoord]
                    ]

    -- Chop-designation markers (#97, re-anchored by #1856): world
    -- annotations like the mine markers, visible in every tool mode.
    --
    -- ONE alpha icon per designated TREE, anchored to that tree's own
    -- rendered ground contact — not a full-tile ground overlay. The
    -- anchor and the painter depth come from the shared projection
    -- boundary the Chop selection oracle reads, so the icon lands on
    -- exactly the sprite the player selected even where two wood-tagged
    -- co-tenants share a tile.
    --
    -- The marker is driven by the LIVE instance, found in the resident
    -- chunk: a felled tree has no instance to draw against, so its
    -- marker disappears with it rather than lingering as an orphaned
    -- annotation (requirement 8 — the durable entry is separately swept
    -- by 'World.Flora.Designation.forgetFloraInstances'). An EVICTED
    -- chunk draws nothing either, exactly as its trees draw nothing.
    chopDesigns ← readIORef (wsChopDesignationsRef worldState)
    cachedQuads ← readIORef (wsQuadCacheRef worldState)
    floraCat    ← readIORef (wsFloraCatalogRef (toWorldSimCapability env))
    harvests    ← readIORef (wsFloraHarvestsRef worldState)
    worldDate   ← readIORef (wsDateRef worldState)
    texSizes    ← readIORef (rvTextureSizeRef rv)
    let calendar = maybe defaultCalendarConfig wgpCalender paramsM
        daysPerYear = calendarDaysPerYear calendar
        absDay = worldAbsoluteDay calendar worldDate
        -- This pass is per-frame while the flora it annotates is
        -- CACHED, so the marker must place itself with the camera those
        -- cached quads were built with (#1856). Reading the live camera
        -- instead sends the icon a whole world away from its tree for
        -- as long as a reused cache straddles the wrap-alias midpoint.
        --
        -- EVERY input the cached run was built with comes from that
        -- snapshot — the z-band cull and the front-wall lift included.
        -- 'cameraChanged' tolerates a zoom delta of camEpsilon (0.075)
        -- while the band steps every 0.0125, so a marker culled at the
        -- live depth could outlive the tree it annotates, or vanish off
        -- one still on screen.
        placed = placementCamera cachedQuads WorldCameraSnapshot
            { wcsPosition = camPosition camera
            , wcsZoom     = zoom
            , wcsZSlice   = camZSlice camera
            , wcsFbSize   = (fbW, fbH)
            , wcsFacing   = camFacing camera
            }
        (placeX, placeY) = wcsPosition placed
        markerDepth = min viewDepth
            (max 8 (round (wcsZoom placed * 80.0 + 8.0 ∷ Float)))
        -- The same front-wall sprite lift the render pass and the
        -- selection oracle build (#418/#1856): a tree lifted to clear a
        -- wall carries its marker up with it, instead of leaving the
        -- annotation sunk behind the trunk it belongs to.
        spriteLift = frameFrontWallLift (wcsFacing placed) worldSize
                         (wcsZSlice placed) markerDepth (wtdChunks tileData)
        markerBounds = expandViewBounds (quadCacheMargins placed)
            (viewBoundsAt (wcsPosition placed) (wcsZoom placed)
                 fbW fbH markerDepth)
        chopDesignQuads = case chopDesignTexture cs' of
            Nothing → V.empty
            Just tex
                | HM.null chopDesigns → V.empty
                | otherwise → V.fromList
                    [ floraMarkerQuad lookupSlot geom
                          (floraTexSize texSizes tex) tileAlpha
                          (fdGX fd) (fdGY fd) tex
                    | (coord, lc) ← HM.toList (wtdChunks tileData)
                    , Just wrapOff ← [isChunkVisibleWrapped
                                          (wcsFacing placed) worldSize
                                          markerBounds placeX placeY coord]
                    , fd ← chunkFloraDraws floraCat daysPerYear absDay
                               harvests (lcCoord lc) lc
                    , let inst = fdInstance fd
                    , HM.member (fiInstanceId inst) chopDesigns
                    , floraVisibleInSlice (wcsZSlice placed) markerDepth inst
                    , let base = floraGeom (wcsFacing placed) (fdGX fd)
                                     (fdGY fd) inst (fdTexture fd) texSizes
                                     (wcsZSlice placed) wrapOff
                          geom = base { fgSortKey =
                              liftSpriteSortKey spriteLift (lcCoord lc)
                                  (fdGX fd) (fdGY fd) (fgSortKey base) }
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
                    [ worldFlatCursorToQuad lookupSlot lookupFmSlot textures
                          facing dgx dgy (tlZ td) zSlice effectiveDepth
                          tileAlpha wrapOff tex
                    | ((dgx, dgy), td) ← HM.toList tillDesigns
                    , let (chunkCoord, _) = globalToChunk dgx dgy
                    , Just wrapOff ← [isChunkVisibleWrapped facing worldSize
                                          vb camX camY chunkCoord]
                    ]

    -- Plant-designation markers (#335): world annotations like the
    -- till markers, visible in every tool mode. Rendered from the
    -- surface z stored at designation time, through #1857's FLAT
    -- top-surface helper — crop planting is work on level ground, so
    -- the authored alpha owns the whole shape and no three-face mask
    -- applies (#1858 requirement 1; the same one call Till makes, not
    -- a copy of it).
    --
    -- The residency guard is a DRAWING suppression, never a validity
    -- filter: a designation whose chunk is not resident is UNKNOWN, so
    -- it is kept and simply not drawn until 'World.Plant.Validate' can
    -- resolve it (requirement 6). Validity itself is world-owned and
    -- lives entirely in that module — a resident record reaching here
    -- is drawn, never re-judged. #1175: the storing chunk comes from
    -- the canonical frame, so a legacy u-alias key resolves too.
    plantDesigns ← readIORef (wsPlantDesignationsRef worldState)
    let plantDesignQuads = case plantDesignTexture cs' of
            Nothing → V.empty
            Just tex
                | HM.null plantDesigns → V.empty
                | otherwise → V.fromList
                    [ worldFlatCursorToQuad lookupSlot lookupFmSlot textures
                          facing dgx dgy (ptZ pd) zSlice effectiveDepth
                          tileAlpha wrapOff tex
                    | ((dgx, dgy), pd) ← HM.toList plantDesigns
                    , let (chunkCoord, _, _) =
                              canonicalTileFrame worldSize dgx dgy
                    , Just _ ← [lookupChunk chunkCoord tileData]
                    , Just wrapOff ← [isChunkVisibleWrapped facing worldSize
                                          vb camX camY chunkCoord]
                    ]

    -- Construction-designation ghosts (#95): world annotations like the
    -- mine markers, visible in every tool mode. Since #1846 a STRUCTURE
    -- renders its own piece art through 'World.Render.StructureGhost';
    -- since #1845 a BUILDING renders its OWN art too, so no category
    -- placeholder is left in this pass at all.
    constructDesigns ← readIORef (wsConstructDesignationsRef worldState)

    -- A designation naming a def missing from bmDefs (a broken save or
    -- mod) draws NOTHING (#1845 requirement 7). #807's anchor-tile
    -- fallback is gone: a fabricated marker for a definition this
    -- session cannot resolve claims a footprint, an anchor and an
    -- appearance nobody can substantiate, and the honest report of that
    -- state is the diagnostic alone. Since this pass runs every frame,
    -- warn only ONCE per distinct missing name per session rather than
    -- flooding the log.
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
                <> defName <> "' — drawing no ghost"
        atomicModifyIORef' (wsCursorRef worldState) $ \cs →
            ( cs { constructMissingDefsWarned =
                     HS.union newlyMissingDefs (constructMissingDefsWarned cs) }
            , () )

    -- Structure-ghost inputs (#1846), read ONCE so every candidate in
    -- this frame is judged against the same world — the discipline
    -- 'PlanWorld' exists for. The wall catalogue and the texture sizes
    -- are the very refs the PLACED structure pass reads, so a ghost and
    -- the piece it previews cannot rotate or measure differently.
    let handoff = toRenderHandoffCapability env
    stage      ← readIORef (wsStructureStageRef worldState)
    artCatalog ← readIORef (rhStructureArtCatalogRef handoff)
    wallCat    ← readIORef (rhStructureWallCatalogRef handoff)
    texSizes   ← readIORef (rvTextureSizeRef rv)
    let ghostEnv = GhostEnv
            { geCatalog    = wallCat
              -- The structure passes bake the stable handle id as a
              -- Word32 (#286); the cursor helpers take an Int. Same id,
              -- two arities of the same projection.
            , geLookupSlot = \h → fromIntegral (toInt h)
            , geTexSizes   = texSizes
            , geFacing     = facing
            , geZSlice     = zSlice
            , geEffDepth   = effectiveDepth
            , geTileAlpha  = tileAlpha
            , geViewBounds = vb
            , geCamX       = camX
            , geCamY       = camY
            , gePlan       = PlanWorld
                { pwWorldSize    = worldSize
                , pwTiles        = tileData
                , pwStage        = stage
                , pwDesignations = constructDesigns
                , pwCatalog      = artCatalog
                , pwProposedWire = HS.empty
                }
            }
        -- The DESIGNATED state (D-19): a world annotation like every
        -- other marker, so it shows in every tool mode.
        structureGhosts = structureDesignationGhosts ghostEnv
        structureGhostQuads = snd structureGhosts

    -- The committed BUILDING designation (#1845, D-19): ONE ghost of the
    -- building's own art, at D-19's 60 %, never tinted — the red is
    -- placement feedback and the player already committed to this job
    -- (requirement 5). #807's per-footprint-tile repetition of a
    -- category marker is deliberately reversed: one building, one
    -- sprite, sized and anchored by 'buildingGhostQuad', the very body
    -- the placement preview draws through.
    --
    -- Two designations are NOT drawn. A def this session cannot resolve
    -- draws nothing (warned about above), and a designation whose
    -- building has already been STAKED yields to the instance: the
    -- spawn lands on the building queue while the completion removes
    -- the designation on the world queue, so a frame can see both, and
    -- both are the same 60 % ghost of the same def at the same anchor
    -- (requirement 3). Drawing them together would double the opacity
    -- for the width of that hand-off.
    -- The z the ghost sits at is the z the STAKE will land on, read
    -- live from the anchor's own terrain by the very function
    -- 'building.spawn' stamps 'biGridZ' from. 'cdZ' — the surface level
    -- captured at designation time — is the fallback for a
    -- non-resident chunk, the one state nobody can answer for. Reading
    -- the stored value instead would leave a designation whose ground
    -- has since been edited drawing at a z the building will not land
    -- on, and the hand-off this slice makes invisible would move it.
    let designationZ ax ay cd =
            fromMaybe (cdZ cd) (buildingAnchorZ worldSize tileData ax ay)
        constructDesignQuads
            | HM.null constructDesigns = V.empty
            | otherwise = V.fromList
                [ quad
                | ((ax, ay), cd) ← HM.toList constructDesigns
                , CtBuilding defName ← [cdTarget cd]
                , Just def ← [HM.lookup defName (bmDefs bm)]
                , not (buildingStakedAt pageId defName (ax, ay)
                                        (bmInstances bm))
                , let (chunkCoord, _) = globalToChunk ax ay
                , Just wrapOff ← [isChunkVisibleWrapped facing worldSize
                                      vb camX camY chunkCoord]
                , Just built ← [buildingGhostQuad lookupSlot noFaceMapVertexId
                                    facing zSlice effectiveDepth texSizes
                                    tileAlpha designatedGhostAlpha True def
                                    ax ay (designationZ ax ay cd)]
                  -- Chunks are stored u-wrapped, so the visibility test
                  -- answers through the NEAREST alias and hands back that
                  -- alias's screen offset. The quad is built at the
                  -- tile's own coordinates and then translated by it —
                  -- the same two-step every other world annotation takes
                  -- (#1175/#1176), and the same one
                  -- 'World.Render.StructureGhost' takes for a structure
                  -- ghost. 'translateQuad' moves positions only: sort
                  -- key and world UV are untouched, so a seam-side ghost
                  -- keeps sorting and lighting as the tile it plans on.
                  -- Away from the seam the offset is (0, 0) and this is
                  -- the identity.
                , let quad = translateQuad wrapOff built
                ]

    -- Hover quads (bg + fg) — used by both info and mine tools.
    let hoverQuads = case hoverResult of
            Just (gx, gy, hz, wrapOff, _) →
                let fgQuad = case worldHoverTexture cs' of
                        Just tex → V.singleton $
                            worldCursorToQuad lookupSlot lookupFmSlot
                              textures facing gx gy hz zSlice effectiveDepth
                              tileAlpha wrapOff tex
                        Nothing → V.empty
                    bgQuad = case worldHoverBgTexture cs' of
                        Just tex → V.singleton $
                            worldCursorBgToQuad lookupSlot lookupFmSlot textures facing
                                gx gy hz zSlice effectiveDepth tileAlpha wrapOff tex
                        Nothing → V.empty
                in bgQuad <> fgQuad
            _ → V.empty

    -- Select quads (bg + fg) — info tool only.
    let selectQuads = case (worldSelectedTile cs', worldCursorTexture cs', worldCursorBgTexture cs') of
            (Just (sgx, sgy, sz), _, _) →
                let (chunkCoord, _) = globalToChunk sgx sgy
                in case isChunkVisibleWrapped facing worldSize vb
                            camX camY chunkCoord of
                    Just wrapOff →
                        let fgQuad = case worldCursorTexture cs' of
                                Just tex → V.singleton $
                                    worldCursorToQuad lookupSlot lookupFmSlot
                                                      textures facing sgx sgy sz
                                                      zSlice effectiveDepth
                                                      tileAlpha wrapOff tex
                                Nothing → V.empty
                            bgQuad = case worldCursorBgTexture cs' of
                                Just tex → V.singleton $
                                    worldCursorBgToQuad lookupSlot lookupFmSlot textures facing
                                        sgx sgy sz zSlice effectiveDepth tileAlpha wrapOff tex
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
        -- #1175: preview and commit share ONE frame discipline, or they
        -- disagree about what a drag marked. Each previewed tile arrives
        -- in the ANCHOR's local alias frame (see 'localizeHover' below)
        -- and is canonicalised HERE, for the column read only — the
        -- coord itself stays local, because that is the frame the quad's
        -- screen position and wrap offset are computed in. Identity
        -- inland; exactly what World.Thread.Command.Cursor.* does at
        -- commit time.
        surfaceZAt gx gy = do
            let (chunkCoord, (lx, ly), _) = canonicalTileFrame worldSize gx gy
            lc ← HM.lookup chunkCoord (wtdChunks tileData)
            pure (lcSurfaceMap lc VU.! columnIndex lx ly)
        -- The hover pick comes back canonical, so a seam-crossing drag
        -- would otherwise span the whole world here exactly as it would
        -- at commit. Re-express it against the anchor first.
        localizeHover ax ay hx hy = localizeTileToAnchor worldSize (ax, ay) (hx, hy)
        -- (#1921) The candidate count is the previewed RECTANGLE's
        -- area, derived arithmetically rather than by re-walking it:
        -- every tile in the rectangle is a candidate the comprehension
        -- below evaluates, before the per-tile z and visibility filters.
        minePreview = case (mineAnchor cs', hoverResult, worldCursorTexture cs') of
            (Just (ax, ay), Just (hxRaw, hyRaw, _, _, _), Just tex)
                | Just anchorZ ← surfaceZAt ax ay →
                let (hx, hy) = localizeHover ax ay hxRaw hyRaw
                    hx' = clampSide ax hx
                    hy' = clampSide ay hy
                    xLo = min ax hx'
                    xHi = max ax hx'
                    yLo = min ay hy'
                    yHi = max ay hy'
                in ( (xHi - xLo + 1) * (yHi - yLo + 1)
                   , V.fromList
                    [ worldCursorToQuad lookupSlot lookupFmSlot textures
                          facing gx gy z zSlice effectiveDepth
                          tileAlpha wrapOff tex
                    | gx ← [xLo .. xHi]
                    , gy ← [yLo .. yHi]
                    , Just z ← [surfaceZAt gx gy]
                    , z ≡ anchorZ
                    , let (chunkCoord, _) = globalToChunk gx gy
                    , Just wrapOff ← [isChunkVisibleWrapped facing worldSize
                                          vb camX camY chunkCoord]
                    ] )
            _ → (0, V.empty)
        minePreviewQuads = snd minePreview

    -- Structure-piece PREVIEW (#403 → #1846): the armed piece's own art
    -- over every candidate of the current gesture, at D-19's 25 % and
    -- red where #1844's resolver would refuse it. The candidate set and
    -- the per-tile verdict both come from that resolver; this block only
    -- decides WHICH tiles are being gestured at.
    --
    -- Before the first click there is one candidate — the hovered tile,
    -- at its OWN surface z — which is requirement 6's single-piece hover
    -- state. After the anchor lands the candidates are
    -- 'structureDragExtent' at the ANCHOR's surface z, the same z the
    -- commit requires.
    --
    -- #1844: ONE bounded-drag helper, shared with the commit. The
    -- endpoint arrives canonical, and the helper localizes it into the
    -- anchor's frame itself — which is also the frame each quad's screen
    -- position is computed in, so nothing here re-localizes it first. It
    -- clamps OUTWARD to 64 cells per axis INCLUDING the anchor, and for
    -- a wire path ('constructLineMode', #359) it picks the dominant axis
    -- from the RAW localized delta before clamping. Preview and commit
    -- therefore cannot disagree about which tiles a drag names.
    let constructPreview = case (constructStructureTarget cs', hoverResult) of
            (Just piece, Just (hxRaw, hyRaw, _, _, _)) →
                case constructAnchor cs' of
                    Just (ax, ay)
                        | Just anchorZ ← surfaceZAt ax ay →
                            structurePreviewGhosts ghostEnv piece anchorZ $
                                structureDragExtent worldSize
                                    (constructLineMode cs') (ax, ay)
                                    (hxRaw, hyRaw)
                    Nothing
                        | Just hoverZ ← surfaceZAt hxRaw hyRaw →
                            structurePreviewGhosts ghostEnv piece hoverZ
                                [(hxRaw, hyRaw)]
                    _ → (0, V.empty)
            _ → (0, V.empty)
        constructPreviewQuads = snd constructPreview

    -- Till tool: anchor→hover rectangle preview. Per-z-level like mine/
    -- construct — a farmed field is flat ground, unlike chop's
    -- slope-spanning forest sweep.
    let tillPreview = case (tillAnchor cs', hoverResult, tillDesignTexture cs') of
            (Just (ax, ay), Just (hxRaw, hyRaw, _, _, _), Just tex)
                | Just anchorZ ← surfaceZAt ax ay →
                let (hx, hy) = localizeHover ax ay hxRaw hyRaw
                    hx' = clampSide ax hx
                    hy' = clampSide ay hy
                    xLo = min ax hx'
                    xHi = max ax hx'
                    yLo = min ay hy'
                    yHi = max ay hy'
                in ( (xHi - xLo + 1) * (yHi - yLo + 1)
                   , V.fromList
                    [ worldFlatCursorToQuad lookupSlot lookupFmSlot textures
                          facing gx gy z zSlice effectiveDepth
                          tileAlpha wrapOff tex
                    | gx ← [xLo .. xHi]
                    , gy ← [yLo .. yHi]
                    , Just z ← [surfaceZAt gx gy]
                    , z ≡ anchorZ
                    , let (chunkCoord, _) = globalToChunk gx gy
                    , Just wrapOff ← [isChunkVisibleWrapped facing worldSize
                                          vb camX camY chunkCoord]
                    ] )
            _ → (0, V.empty)
        tillPreviewQuads = snd tillPreview

    -- Mine + construction + chop + till markers are world annotations:
    -- shown in every tool mode. The mode only adds its own hover/preview
    -- on top.
    let markerQuads = designQuads <> constructDesignQuads
                    <> structureGhostQuads <> chopDesignQuads
                    <> tillDesignQuads <> plantDesignQuads

    -- Scene-assembly telemetry (#1921). Each count is the number of
    -- marker-tile CANDIDATES the matching builder enumerated, taken
    -- before that builder's own per-chunk visibility cull — a builder
    -- short-circuited by a missing texture or an empty designation map
    -- enumerated nothing and contributes zero. 'markerScanned' is
    -- always in the answer because the marker builders are; the rest
    -- join it only in the modes whose quads they are part of, so the
    -- reported scanned total always pairs with the returned vector.
    let designationScanned tex m
            | isNothing tex   = 0
            | HM.null m       = 0
            | otherwise       = HM.size m
        markerScanned =
            designationScanned (mineDesignTexture cs') designations
          + designationScanned (chopDesignTexture cs') chopDesigns
          + designationScanned (tillDesignTexture cs') tillDesigns
          + designationScanned (plantDesignTexture cs') plantDesigns
          -- One candidate per committed BUILDING designation (#1845):
          -- the map entry IS the candidate now that the ghost is one
          -- sprite rather than one marker per footprint tile. Counted
          -- before the def lookup, the staking yield and the visibility
          -- cull reject any of them — those rejections are exactly what
          -- an emitted count below the scanned one records. Folded
          -- strictly over the map itself rather than over a second
          -- 'HM.toList': #1921 requirement 9 forbids the counter
          -- allocating in proportion to the candidates it counts, and
          -- an empty map folds to 0 without a guard.
          + HM.foldl' (\acc cd → case cdTarget cd of
                CtBuilding _  → acc + 1
                CtStructure _ → acc) 0 constructDesigns
          -- Structure designations left that fold when they stopped
          -- using a category marker (#1846); their candidates are the
          -- unpaid structure sites the ghost builder enumerates, counted
          -- by that builder itself so the two cannot drift.
          + fst structureGhosts
        hoverScanned  = if isJust hoverResult then 1 else 0
        selectScanned = if isJust (worldSelectedTile cs') then 1 else 0
    return $ case toolMode of
        InfoTool  → ( markerScanned + hoverScanned + selectScanned
                    , markerQuads <> hoverQuads <> selectQuads )
        MineTool  → ( markerScanned + hoverScanned + fst minePreview
                    , markerQuads <> hoverQuads <> minePreviewQuads )
        -- #403: the build tool now drives construction designation too
        -- (structure-piece rectangles), so it gets the same preview the
        -- standalone construct tool used to.
        BuildTool → ( markerScanned + hoverScanned + fst constructPreview
                    , markerQuads <> hoverQuads <> constructPreviewQuads )
        -- #1856: Chop's gesture is a screen-space press-drag whose box
        -- is a UI overlay ('scripts/unit_drag_select.lua'), not a
        -- world-space tile rectangle, so this pass has no chop preview
        -- to build — only the hover cursor and the committed markers.
        ChopTool  → (markerScanned + hoverScanned, markerQuads <> hoverQuads)
        TillTool  → ( markerScanned + hoverScanned + fst tillPreview
                    , markerQuads <> hoverQuads <> tillPreviewQuads )
        PlantTool → ( markerScanned + hoverScanned
                    , markerQuads <> hoverQuads )
        _         → (markerScanned, markerQuads)
