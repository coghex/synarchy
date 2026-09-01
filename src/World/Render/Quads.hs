{-# LANGUAGE Strict #-}
module World.Render.Quads
    ( renderWorldQuads
    , renderWorldQuadsScanned
    , structureFrontWallClear
    ) where

import UPrelude
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), toWorldSimCapability)
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V
import Data.IORef (readIORef)
import Control.Parallel.Strategies (parListChunk, rdeepseq, using)
import Engine.Core.State (EngineEnv)
import Engine.Core.Capability.RenderView
  (RenderViewCapability(..), toRenderViewCapability)
import Engine.Asset.Handle (toInt)
import Engine.Scene.Types (SortableQuad(..))
import Engine.Graphics.Camera (CameraFacing)
import World.Types
import World.Flora.Render (resolveFloraTexture)
import World.Flora.CropPlot (cropPlotElapsedDays, cropPlotInstance)
import World.Generate (chunkToGlobal, viewDepth)
import World.Generate.Coordinates (canonicalTileFrame)
import World.Grid (gridToScreen, tileSideHeight, applyFacing)
import Structure.Types (StructureSlot(..), ChunkStructures, spdGridZ)
import Structure.Render
  (isScreenFrontWall, wallTieBreak, frontWallDepthSteps, pieceWithinSliceBand)
import World.Render.ViewBounds (viewBoundsAt, expandViewBounds, isTileVisible)
import World.Render.Camera (quadCacheMargins)
import World.Render.ChunkCulling (isChunkRelevantForSlice, isChunkVisibleWrapped)
import World.Render.FloraQuads (floraToQuad)
import World.Render.FloraDraws
    (FloraDraw(..), chunkFloraDraws)
import World.Render.ChunkLookup (canonicalChunkLookup)
import World.Render.QuadContext (QuadContext(..), WorldX(..), WorldY(..)
                                , WorldZ(..), ZSlice(..), EffectiveDepth(..))
import World.Render.SideDecoQuads (waterSideFaceQuads)
import World.Render.WaterSlope (waterSlopeAt)
import World.Render.TileQuads
    ( tileToQuad, blankTileToQuad, oceanTileToQuad, iceTileToQuad
    , lavaTileToQuad, freshwaterTileToQuad, vegToQuad, vegQuadWithTexture
    )

-- * Render World Quads

-- | Build one page's cached tile quads for the camera state described by
--   @snap@.
--
--   __Every camera-derived value comes from that snapshot (#1720)__ —
--   facing, z-slice, zoom, effective depth, position and the view bounds
--   the margins expand. This function does NOT read 'rvCameraRef': its
--   caller stamps the cache entry with the same snapshot, and the camera
--   has concurrent writers (the main thread's pan integration rewrites it
--   at frame rate while this runs on the world thread), so a second live
--   read here would let the geometry describe one camera while the stamp
--   describes another. That mismatch does not repair itself for position:
--   'World.Render.Camera.cameraChanged' licenses reuse within the STAMP's
--   margin, so quads built around a different centre leave an uncovered
--   strip that no rebuild is triggered for. Only the snapshot decides.
--
--   The dynamic per-frame passes (cursor, ground items, spoil, blood) are
--   the deliberate opposite and still read the live camera: they are
--   rebuilt every frame, so responding to input immediately is the point.
renderWorldQuads ∷ EngineEnv → WorldState → Float → WorldCameraSnapshot
  → IO (V.Vector SortableQuad)
renderWorldQuads env worldState zoomAlpha snap =
    snd ⊚ renderWorldQuadsScanned env worldState zoomAlpha snap

-- | 'renderWorldQuads' with the scene-assembly telemetry (#1921) this
--   pass contributes: the number of terrain CELLS the rebuild visits,
--   paired with the quads it produced.
--
--   That count is the whole reason this variant exists — every visible
--   chunk's column grid is walked in full by the two loops below
--   ('realQuads' folds the chunk's tile map, and the blank-tile fill
--   enumerates the same @chunkSize × chunkSize@ grid), so the cell
--   count is exactly the visible-chunk count times a chunk's area and
--   costs nothing to derive. It is a REBUILD count by construction:
--   'World.Render.updateWorldTiles' only reaches this function when the
--   page's quad cache missed, and a cache HIT reports zero scanned
--   cells while still reporting its reused quads as emitted.
renderWorldQuadsScanned ∷ EngineEnv → WorldState → Float → WorldCameraSnapshot
  → IO (Int, V.Vector SortableQuad)
renderWorldQuadsScanned env worldState zoomAlpha snap = do
    tileData ← readIORef (wsTilesRef worldState)
    textures ← readIORef (wsTexturesRef worldState)
    paramsM ← readIORef (wsGenParamsRef worldState)
    floraCat ← readIORef (wsFloraCatalogRef (toWorldSimCapability env))
    worldDate ← readIORef (wsDateRef worldState)
    texSizes ← readIORef (rvTextureSizeRef (toRenderViewCapability env))
    harvests ← readIORef (wsFloraHarvestsRef worldState)
    cropPlots ← readIORef (wsCropPlotsRef worldState)

    let (fbW, fbH) = wcsFbSize snap
        facing = wcsFacing snap
        -- Flora growth is derived from the absolute world day (#332):
        -- the annual cycle takes its year-relative ordinal day from it,
        -- and the life phase its derived age. Convert through the world
        -- calendar (falling back to the default calendar when gen params
        -- aren't loaded yet).
        calendar = maybe defaultCalendarConfig wgpCalender paramsM
        daysPerYear = calendarDaysPerYear calendar
        absDay = worldAbsoluteDay calendar worldDate

    -- Vertices carry a STABLE texture-handle id (#286); the bindless
    -- fragment shader resolves it to a live slot at draw time, so the
    -- cache never encodes a recyclable/stale slot. The default-face-map
    -- fallback (handle → slot 0 → default) now lives in the shader too.
    let lookupSlot texHandle = fromIntegral (toInt texHandle)
        lookupFmSlot texHandle = fromIntegral (toInt texHandle)
        worldSize = case paramsM of
                      Nothing → 128
                      Just params → wgpWorldSize params
    let zSlice = wcsZSlice snap
        zoom   = wcsZoom snap
        -- Caught by the #1135 audit's search, but not a coordinate
        -- lookup: this ENUMERATES the stored LoadedChunk values and
        -- accepts no caller coord, so there is no key to canonicalise.
        -- Every element is already in the stored frame — insertion keys
        -- on the canonical lcCoord (World.Thread.ChunkLoading →
        -- World.Tile.Types.insertChunk), and consumers below read each
        -- chunk's own lcCoord rather than reconstructing one.
        chunks = HM.elems (wtdChunks tileData)
        (camX, camY) = wcsPosition snap

        effectiveDepth = min viewDepth (max 8 (round (zoom * 80.0 + 8.0 ∷ Float)))

        -- Every cross-chunk probe in this pass goes through ONE
        -- canonicalising boundary (#1135). Chunks are stored under
        -- u-wrapped coords, but the callers below step one chunk outward
        -- in their home chunk's RAW frame (waterSlopeAt, and
        -- waterSideFaceQuads' neighborCell), which lands outside the
        -- canonical range exactly at the cylindrical U seam — the
        -- neighbour is loaded, the key is just an alias. Away from the
        -- seam the wrap is the identity.
        chunkLookup = canonicalChunkLookup worldSize (wtdChunks tileData)

        -- Lookup neighbor chunk fluid/terrain maps for cross-chunk water
        -- slopes. Both are read at a LOCAL index, which the whole-chunk
        -- wrap leaves untouched, so canonicalising the key is the whole
        -- fix here.
        fluidMapLookup cc = lcFluidMap ⊚ chunkLookup cc
        terrMapLookup cc = lcTerrainSurfaceMap ⊚ chunkLookup cc

        -- Chunks that actually carry structures. A sprite is only considered
        -- for the lift when its chunk is within ONE chunk of one of these, so
        -- the check stays a no-op in structure-free areas — but, unlike a
        -- same-chunk-only gate, it still fires for a sprite sitting across a
        -- chunk seam from a wall on the next chunk (structureFrontWallClear
        -- already resolves that wall cross-chunk).
        structureChunkCoords =
            [ lcCoord lc | lc ← chunks, not (HM.null (lcStructures lc)) ]
        -- structureFrontWallClear already canonicalises the coord it
        -- probes with (it has to shift the structure TILE key by the same
        -- wrap delta), so this is idempotent — routed through the shared
        -- boundary anyway so no lookup in this pass is the raw one.
        structLookup cc = lcStructures ⊚ chunkLookup cc

        -- Cached pass: widen the bounds by the pan margin so the camera
        -- can travel that far before cameraChanged forces a rebuild
        -- (#447). Both halves come from the SAME snapshot the cache is
        -- stamped with — the margins AND the bounds they expand (#1720)
        -- — pairing coverage with invalidation.
        vb = expandViewBounds (quadCacheMargins snap) $
                 viewBoundsAt (wcsPosition snap) zoom fbW fbH effectiveDepth

        visibleChunksWithOffset =
            [ (lc, offset)
            | lc ← chunks
            , isChunkRelevantForSlice zSlice lc
            , Just offset ← [isChunkVisibleWrapped facing worldSize vb
                                 camX camY (lcCoord lc)]
            ]

    let chunkVectors = map (\(lc, wrapOff) →
            -- One offset per chunk, from the SAME call that judged it
            -- visible (#1176 req 2): every subpass below shifts BOTH
            -- screen axes by it, because at east/west facings the
            -- u-wrap displaces screen Y and not screen X at all.
            let (wrapX, wrapY) = wrapOff
                -- One named context for the two producers that take it
                -- (#1138), built here rather than once per frame because
                -- 'qcWrapOffset' is the one per-chunk member.
                ctx = QuadContext
                    { qcLookupSlot     = lookupSlot
                    , qcLookupFmSlot   = lookupFmSlot
                    , qcTextures       = textures
                    , qcFacing         = facing
                    , qcZSlice         = ZSlice zSlice
                    , qcEffectiveDepth = EffectiveDepth effectiveDepth
                    , qcTileAlpha      = zoomAlpha
                    , qcWrapOffset     = wrapOff
                    }
                coord  = lcCoord lc
                tileMap = lcTiles lc
                fluidMap = lcFluidMap lc
                iceMap   = lcIceMap lc
                chunkHasFluid = V.any isJust fluidMap
                terrainSurfMap = lcTerrainSurfaceMap lc

                -- #418: only pay the front-wall clearance lookup in chunks at
                -- or adjacent to one carrying structures (rooms are localised —
                -- most chunks are nowhere near one, so this is a no-op there).
                -- Adjacency (not same-chunk-only) is what lets a sprite across
                -- a chunk seam from a wall still get lifted — measured with
                -- 'chunkSeamChebyshev' so a wall just across the cylindrical
                -- U seam still qualifies (#423). A qualifying sprite is raised
                -- to sit fully in front of any front wall it overlaps;
                -- everything else is untouched.
                chunkNearStructures =
                    any (\sc → chunkSeamChebyshev worldSize sc coord ≤ 1)
                        structureChunkCoords
                bump gx gy q
                    | not chunkNearStructures = q
                    | otherwise = case structureFrontWallClear facing worldSize
                                           zSlice effectiveDepth structLookup
                                           gx gy of
                        Just c  → q { sqSortKey = max (sqSortKey q) (c + 0.0001) }
                        Nothing → q

                !realQuads = V.ifoldl' (\acc idx col →
                        let lx = idx `mod` chunkSize
                            ly = idx `div` chunkSize
                            mFluid = fluidMap V.! idx
                            (gx, gy) = chunkToGlobal coord lx ly
                            (rawX, rawY) = gridToScreen facing gx gy
                            isUnderLava = case mFluid of
                                Just fc → fcType fc ≡ Lava ∧ fcSurface fc > zSlice - effectiveDepth
                                Nothing → False

                            zLo = max (ctStartZ col) (zSlice - effectiveDepth)
                            zHi = min (ctStartZ col + VU.length (ctMats col) - 1) zSlice

                            surfZ = terrainSurfMap VU.! idx
                        in if isUnderLava ∨ zHi < zLo
                           then acc
                           else foldl' (\acc2 z →
                                let mat = ctMats col VU.! (z - ctStartZ col)
                                    drawY' = rawY + wrapY
                                           - fromIntegral (z - zSlice) * tileSideHeight
                                in if mat ≡ 0 ∨ not (isTileVisible vb (rawX + wrapX) drawY')
                                   then acc2
                                   else let slopeId = ctSlopes col VU.! (z - ctStartZ col)
                                            tile = Tile mat slopeId
                                            tq = tileToQuad ctx
                                                    (WorldX gx) (WorldY gy) (WorldZ z)
                                                    tile mFluid chunkHasFluid

                                            -- Vegetation: only on surface tile, only when
                                            -- surface is above the fluid level
                                            vegQ = if z ≡ surfZ ∧ maybe True (\fc → surfZ > fcSurface fc) mFluid
                                                   then let i = z - ctStartZ col
                                                            slopeId = ctSlopes col VU.! i
                                                        in case HM.lookup (gx, gy) cropPlots of
                                                            -- Planted crop tile (#334): the tile-fill
                                                            -- texture is DERIVED from the #332 growth
                                                            -- runtime instead of the static ctVeg id.
                                                            Just cp →
                                                                let elapsed = cropPlotElapsedDays absDay cp
                                                                    tex = resolveFloraTexture floraCat
                                                                              daysPerYear elapsed
                                                                              (cropPlotInstance cp)
                                                                in vegQuadWithTexture lookupSlot lookupFmSlot
                                                                       textures facing gx gy z tex slopeId
                                                                       zSlice effectiveDepth zoomAlpha wrapOff
                                                            Nothing →
                                                                let vegId = ctVeg col VU.! i
                                                                in vegToQuad lookupSlot lookupFmSlot textures facing
                                                                       gx gy z vegId slopeId zSlice effectiveDepth
                                                                       zoomAlpha wrapOff
                                                   else Nothing

                                        in case vegQ of
                                            Just vq → bump gx gy vq : tq : acc2
                                            Nothing → tq : acc2
                                ) acc [zLo .. zHi]
                    ) [] tileMap
                -- Flora sprites
                --
                -- #1856: which instances there are, their live
                -- terrain-derived z and the texture each is drawing all
                -- come from the shared resolver the Chop selection
                -- oracle reads, so a picker can never consider a plant
                -- this pass skipped.
                floraData = lcFlora lc
                !floraQuads =
                    [ bump (fdGX fd) (fdGY fd) fq
                    | fd ← chunkFloraDraws floraCat daysPerYear absDay
                               harvests coord lc { lcFlora = floraData }
                    , Just fq ← [floraToQuad lookupSlot textures facing
                                     (fdGX fd) (fdGY fd) (fdInstance fd)
                                     (fdTexture fd) zSlice effectiveDepth
                                     zoomAlpha wrapOff texSizes]
                    ]
                -- Water side-face quads: fill elevation gaps where water
                -- drops over cliff edges
                !waterSideQuads = if chunkHasFluid
                    then waterSideFaceQuads ctx coord fluidMap terrainSurfMap
                             fluidMapLookup terrMapLookup vb
                    else []

                !blankQuads =
                    [ blankTileToQuad lookupSlot lookupFmSlot textures facing
                        gx gy zSlice zSlice zoomAlpha wrapOff
                    | lx ← [0 .. chunkSize - 1]
                    , ly ← [0 .. chunkSize - 1]
                    , let idx = columnIndex lx ly
                          terrainZ = terrainSurfMap VU.! idx
                    , terrainZ > zSlice
                    , let col = tileMap V.! idx
                          i = zSlice - ctStartZ col
                          hasTile = i ≥ 0
                                  ∧ i < VU.length (ctMats col)
                                  ∧ ctMats col VU.! i ≢ 0
                    , not hasTile
                    , let (gx, gy) = chunkToGlobal coord lx ly
                          (rawX, rawY) = gridToScreen facing gx gy
                          drawX = rawX + wrapX
                          drawY = rawY + wrapY
                    , isTileVisible vb drawX drawY
                    ]

                _mkFreshwaterQuad gx gy ft fc slopeId =
                        freshwaterTileToQuad lookupSlot lookupFmSlot textures facing
                            gx gy (fcSurface fc) ft zSlice effectiveDepth
                            zoomAlpha wrapOff slopeId

                -- Ice surface quads: rendered above ocean/freshwater
                !iceQuads =
                    [ iceTileToQuad lookupSlot lookupFmSlot textures facing
                        gx gy (icSurface ic) zSlice effectiveDepth zoomAlpha wrapOff
                    | idx ← [0 .. chunkSize * chunkSize - 1]
                    , Just ic ← [iceMap V.! idx]
                    , icSurface ic ≤ zSlice
                    , icSurface ic ≥ (zSlice - effectiveDepth)
                    , let lx = idx `mod` chunkSize
                          ly = idx `div` chunkSize
                          (gx, gy) = chunkToGlobal coord lx ly
                          (rawX, rawY) = gridToScreen facing gx gy
                          relativeZ = icSurface ic - zSlice
                          heightOffset = fromIntegral relativeZ * tileSideHeight
                          drawX = rawX + wrapX
                          drawY = rawY + wrapY - heightOffset
                    , isTileVisible vb drawX drawY
                    ]

                (!oceanQuads, !lavaQuads, !freshwaterQuads) =
                    V.ifoldl' (\(!oAcc, !lAcc, !fAcc) idx mFluid ->
                        case mFluid of
                            Nothing → (oAcc, lAcc, fAcc)
                            Just fc ->
                                if fcSurface fc > zSlice ∨ fcSurface fc < (zSlice - effectiveDepth)
                                then (oAcc, lAcc, fAcc)
                                else
                                    let lx = idx `mod` chunkSize
                                        ly = idx `div` chunkSize
                                        (gx, gy) = chunkToGlobal coord lx ly
                                        (rawX, rawY) = gridToScreen facing gx gy
                                        relativeZ = fcSurface fc - zSlice
                                        heightOffset = fromIntegral relativeZ * tileSideHeight
                                        drawX = rawX + wrapX
                                        drawY = rawY + wrapY - heightOffset
                                        -- Skip ocean/lake rendering where ice covers the surface
                                        hasIce = isJust (iceMap V.! idx)
                                    in if not (isTileVisible vb drawX drawY)
                                       then (oAcc, lAcc, fAcc)
                                       else case fcType fc of
                                            Ocean
                                              | hasIce → (oAcc, lAcc, fAcc)
                                              | otherwise ->
                                                ( oceanTileToQuad lookupSlot lookupFmSlot textures facing
                                                    gx gy (fcSurface fc) zSlice effectiveDepth zoomAlpha wrapOff
                                                  : oAcc
                                                , lAcc
                                                , fAcc
                                                )
                                            Lava  ->
                                                ( oAcc
                                                , lavaTileToQuad lookupSlot lookupFmSlot textures facing
                                                    gx gy (fcSurface fc) zSlice effectiveDepth zoomAlpha wrapOff
                                                  : lAcc
                                                , fAcc
                                                )
                                            Lake
                                              | hasIce → (oAcc, lAcc, fAcc)
                                              | otherwise ->
                                                let wSlope = waterSlopeAt fluidMap terrainSurfMap coord fluidMapLookup terrMapLookup lx ly (fcSurface fc)
                                                in ( oAcc
                                                , lAcc
                                                , freshwaterTileToQuad lookupSlot lookupFmSlot textures facing
                                                    gx gy (fcSurface fc) Lake zSlice effectiveDepth
                                                    zoomAlpha wrapOff wSlope
                                                  : fAcc
                                                )
                                            River ->
                                                let wSlope = waterSlopeAt fluidMap terrainSurfMap coord fluidMapLookup terrMapLookup lx ly (fcSurface fc)
                                                in ( oAcc
                                                , lAcc
                                                , freshwaterTileToQuad lookupSlot lookupFmSlot textures facing
                                                    gx gy (fcSurface fc) River zSlice effectiveDepth
                                                    zoomAlpha wrapOff wSlope
                                                  : fAcc
                                                )
                    ) ([], [], []) fluidMap

            in V.fromList (realQuads <> floraQuads <> waterSideQuads
                                     <> blankQuads <> iceQuads <> oceanQuads
                                     <> lavaQuads <> freshwaterQuads)
            ) visibleChunksWithOffset
            -- Chunks build independently, so rebuilds (already off the
            -- render thread — this runs on the world thread) spread
            -- across cores, same pattern as ChunkLoading /
            -- ZoomMap.Cache (#447). Chunk-of-4 keeps spark overhead
            -- low at typical visible-chunk counts (~20–100).
            `using` parListChunk 4 rdeepseq
        -- One cell per column of every chunk this rebuild walked
        -- (#1921). Both per-chunk loops cover the whole grid, so this
        -- is the exact visited-cell count and not an estimate.
        scannedCells = length visibleChunksWithOffset * chunkSize * chunkSize
    return $! (scannedCells, V.concat chunkVectors)

-- | #418: a flora/veg billboard sitting in front of a structure's FRONT
--   wall must draw over the WHOLE wall, not slice through the wall's
--   depth-sorted strips (#417). A single-depth sprite otherwise beats
--   the wall's back strips but loses its clamped south strips → the
--   "leaf over the wall / frond cut off" straddle. This finds the
--   highest front-wall strip key the sprite at (gx,gy) is spatially IN
--   FRONT of, so the caller lifts the sprite's key just above it and it
--   clears the entire strip range as one unit. Returns Nothing when no
--   such wall is near (the sprite keeps its normal key).
--
--   The strip-key formula is not restated here: the candidate slots, the
--   tie-break each gets and how far in front of its own tile a wall's
--   clamped strips reach all come from "Structure.Render" itself
--   ('isScreenFrontWall', 'wallTieBreak', 'frontWallDepthSteps'), so the
--   lift and the strips cannot drift apart. The first two are
--   facing-DEPENDENT since #1712 — which walls are screen-front, and
--   which tie-break their rotated sprite takes — while the depth reach is
--   deliberately facing-INVARIANT, measured in painter-depth steps from
--   the tile's own grid origin rather than from a rotating vertex. All
--   three are the historical SE 0.0006 / SW 0.0005 / (wgx+1,wgy+1) at
--   'FaceSouth'. The applyFacing depth test keeps the comparison
--   rotation-correct. Wall lookups cross chunks; the per-chunk gate at
--   the call site keeps it free where there are none.
--
--   Slice-bounded (#1715): eligibility is restricted to walls the
--   structure renderer would EMIT for this same frame, by taking the
--   frame's effective depth and gating on 'pieceWithinSliceBand' — the
--   very predicate 'frontWallStrips' gates on. A wall the camera slice
--   cut away, or one deeper than the zoom-derived depth window, is on
--   screen nowhere and clears nothing, so it must not move a sprite.
--   (Renderer parity is the whole claim here, not just the visible
--   above-slice promotion: that band is zoom-dependent, and every other
--   structure decision in the frame already follows it.) Suppressions
--   the renderer makes on information this helper does not have — an
--   unresolved palette handle, an unavailable texture system — are out
--   of its reach and deliberately out of scope.
--
--   Seam-aware (#423): loaded chunks are keyed by canonical (u-wrapped)
--   coords ('World.Thread.ChunkLoading'), and a chunk's structures are
--   keyed by tile coords in that canonical frame. A neighbour probed
--   just across the cylindrical U seam therefore needs BOTH its chunk
--   coord canonicalised and its tile key shifted by the same wrap
--   delta, or the wall on the far side is silently missed.
structureFrontWallClear
    ∷ CameraFacing
    → Int                                   -- ^ world size in chunks
    → Int                                   -- ^ camera z-slice
    → Int                                   -- ^ frame's effective depth
    → (ChunkCoord → Maybe ChunkStructures)  -- ^ loaded-chunk structure lookup
    → Int → Int                             -- ^ sprite tile (gx, gy)
    → Maybe Float
structureFrontWallClear facing worldSize zSlice effDepth structLookup gx gy =
    let (fa, fb) = applyFacing facing gx gy
        spriteDepth = fa + fb
        -- The walls whose edge is drawn at the SCREEN front right now,
        -- each with the tie-break its rotated sprite gets — the same two
        -- answers Structure.Render gives, from the same functions.
        frontCands =
            [ (fromIntegral (fromEnum slot) ∷ Word8, wallTieBreak facing slot)
            | slot ← [SWallNE, SWallNW, SWallSE, SWallSW]
            , isScreenFrontWall facing slot ]
        wallKeyAt wgx wgy tag tieB = do
            let (cc, _, (dgx, dgy)) = canonicalTileFrame worldSize wgx wgy
                -- Tile key in the stored (canonical) chunk's frame. The
                -- chunk wrap shifts u by whole worlds and preserves
                -- v = cx + cy, so this is the identity away from the seam.
                sgx = wgx + dgx
                sgy = wgy + dgy
            structs ← structLookup cc
            spd ← HM.lookup (sgx, sgy, tag) structs
            -- The wall's strips sort at keys computed from its STORED
            -- coords, while the sprite's own key is in its local frame.
            -- The u-wrap preserves v = gx + gy, so at north/south facings
            -- (depth = ±v) the two frames agree and the cross-seam lift
            -- is exact. At east/west facings depth follows u, which the
            -- wrap shifts by a whole world width, so the two keys are a
            -- whole world apart: skip when the frames disagree.
            --
            -- RE-EXAMINED under #1176 and DELIBERATELY KEPT. That issue
            -- made the wrap offset two-dimensional, so the sprite and
            -- the wall across an east/west seam now do land next to each
            -- other on SCREEN — the original wording here ("the wall
            -- renders nowhere near the sprite") no longer describes the
            -- placement. It does not describe the sort key either, and
            -- the sort key is what this function returns. 'sqSortKey' is
            -- painter DEPTH derived from grid coords ('applyFacing'),
            -- which a screen-space translation cannot and does not
            -- touch, so lifting the sprite to 'scDepth' would still
            -- shove its key a whole world past every quad it is drawn
            -- among and corrupt its local ordering. Removing the guard
            -- needs the two ends to share a translated sort key, which
            -- is a change to the depth frame, not to placement.
            --
            -- (Structure quads DO carry the wrap offset since #1706 —
            -- 'Structure.Render' resolves each chunk's nearest alias
            -- through the same 'isChunkVisibleWrapped' — so a wall now
            -- lands beside the sprite on screen. That still does not
            -- change this guard's verdict, which rests on the key
            -- alone: the offset is a screen translation and leaves
            -- 'sqSortKey' grid-derived, exactly as reasoned above.)
            -- A front wall's clamped strips all land 'frontWallDepthSteps'
            -- in front of its own tile's depth (the tile's front corner,
            -- (wgx+1,wgy+1) at FaceSouth), at every facing.
            let (sa, sb) = applyFacing facing sgx sgy
                scDepth  = sa + sb + frontWallDepthSteps
                (la, lb) = applyFacing facing wgx wgy
                localDepth = la + lb + frontWallDepthSteps
            -- #1715: only a wall the structure renderer would actually
            -- EMIT this frame can be cleared. 'pieceWithinSliceBand' is
            -- the renderer's own gate, shared rather than restated, so
            -- 'frontWallStrips' and this cannot disagree for any
            -- (gridZ, zSlice, effDepth). Both ends bite. ABOVE the
            -- slice: dropping the slice below a room's walls is the
            -- ordinary way to look inside it, and those walls stop
            -- being emitted — yet 'spdGridZ spd - zSlice' is then
            -- POSITIVE, so a wall nobody can see used to lift an
            -- adjacent sprite HARDER than a drawn one would. BELOW the
            -- window: that wall is equally not on screen, and while its
            -- key happens to lose to the sprite's today, the parity is
            -- the contract, not the arithmetic that currently masks the
            -- gap. Inside the band this is a no-op, which is why the
            -- in-slice keys below are untouched.
            if not (pieceWithinSliceBand zSlice effDepth (spdGridZ spd))
               ∨ spriteDepth < localDepth   -- sprite is NOT fully in front
               ∨ scDepth ≢ localDepth     -- frames disagree (E/W seam)
               then Nothing
               else Just (fromIntegral scDepth
                          + fromIntegral (spdGridZ spd - zSlice) * 0.001
                          + tieB)
        cands = [ wallKeyAt (gx + dx) (gy + dy) tag tieB
                | dx ← [-2 .. 2], dy ← [-2 .. 2], (dx, dy) ≢ (0, 0)
                , (tag, tieB) ← frontCands ]
    in case [ k | Just k ← cands ] of
         [] → Nothing
         ks → Just (maximum ks)

