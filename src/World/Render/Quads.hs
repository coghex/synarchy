{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Render.Quads
    ( renderWorldQuads
    , renderWorldCursorQuads
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V
import Data.IORef (readIORef, atomicModifyIORef')
import Control.Parallel.Strategies (parListChunk, rdeepseq, using)
import Engine.Core.State (EngineEnv(..))
import Engine.Asset.Handle (TextureHandle(..), toInt)
import Engine.Scene.Types (SortableQuad(..))
import Engine.Graphics.Camera (Camera2D(..))
import World.Types
import World.Flora.Render (resolveFloraTexture)
import World.Generate (chunkToGlobal, viewDepth)
import World.Generate.Coordinates (globalToChunk)
import World.Grid (gridToScreen, tileSideHeight, applyFacing)
import Structure.Types (StructureSlot(..), spdGridZ)
import World.Mine.Types (MineDesignation(..))
import World.Construct.Types (ConstructDesignation(..), ConstructTarget(..))
import World.Render.ViewBounds (computeViewBounds, expandViewBounds, isTileVisible)
import World.Render.Camera (quadCacheMargins)
import World.Render.ChunkCulling (isChunkRelevantForSlice, isChunkVisibleWrapped)
import World.Render.HitTest (pickWorldTile)
import World.Render.FloraQuads (floraToQuad)
import World.Render.SideDecoQuads (waterSideFaceQuads)
import World.Render.TileQuads
    ( tileToQuad, blankTileToQuad, oceanTileToQuad, iceTileToQuad
    , lavaTileToQuad, freshwaterTileToQuad, worldCursorToQuad
    , worldCursorBgToQuad, vegToQuad
    )

-- * Water Slope Helpers

-- | Compute a slope ID (Word8) for a water tile by checking if
--   adjacent water tiles have lower surfaces.
--
--   Grid-space directions map to isometric pixel-space as:
--     Grid N (ly-1) → pixel right  (East,  bit 2)
--     Grid E (lx+1) → pixel bottom (South, bit 4)
--     Grid S (ly+1) → pixel left   (West,  bit 8)
--     Grid W (lx-1) → pixel top    (North, bit 1)
--
--   Supports cross-chunk lookups via the chunkLookup function.
waterSlopeAt ∷ V.Vector (Maybe FluidCell) → VU.Vector Int → ChunkCoord
             → (ChunkCoord → Maybe (V.Vector (Maybe FluidCell)))
             → (ChunkCoord → Maybe (VU.Vector Int))
             → Int → Int → Int → Word8
waterSlopeAt fluidMap terrSurfMap coord chunkLookup terrLookup lx ly mySurf =
    let checkNeighbor nx ny
            | nx ≥ 0 ∧ nx < chunkSize ∧ ny ≥ 0 ∧ ny < chunkSize =
                case fluidMap V.! (ny * chunkSize + nx) of
                    Just fc → fcSurface fc ≡ mySurf - 1
                    -- Dry neighbor: slope toward it if terrain is
                    -- exactly 1 below water surface (river bank).
                    Nothing → terrSurfMap VU.! (ny * chunkSize + nx) ≡ mySurf - 1
            | otherwise =
                -- Cross-chunk lookup
                let ChunkCoord cx cy = coord
                    (cx', lx') = if nx < 0 then (cx - 1, nx + chunkSize)
                                 else if nx ≥ chunkSize then (cx + 1, nx - chunkSize)
                                 else (cx, nx)
                    (cy', ly') = if ny < 0 then (cy - 1, ny + chunkSize)
                                 else if ny ≥ chunkSize then (cy + 1, ny - chunkSize)
                                 else (cy, ny)
                in case chunkLookup (ChunkCoord cx' cy') of
                    Nothing → False
                    Just neighborFM →
                        case neighborFM V.! (ly' * chunkSize + lx') of
                            Just fc → fcSurface fc ≡ mySurf - 1
                            Nothing → case terrLookup (ChunkCoord cx' cy') of
                                Nothing → False
                                Just nTerrMap →
                                    nTerrMap VU.! (ly' * chunkSize + lx') ≡ mySurf - 1
        -- Grid XY → UV/screen mapping. Each grid step is diagonal
        -- in UV space (u=x-y, v=x+y):
        --   Grid N (y-1) → u+, v- → pixel NE → bits 1+2 = 3
        --   Grid E (x+1) → u+, v+ → pixel SE → bits 2+4 = 6
        --   Grid S (y+1) → u-, v+ → pixel SW → bits 4+8 = 12
        --   Grid W (x-1) → u-, v- → pixel NW → bits 1+8 = 9
        gridN = checkNeighbor lx (ly - 1)
        gridE = checkNeighbor (lx + 1) ly
        gridS = checkNeighbor lx (ly + 1)
        gridW = checkNeighbor (lx - 1) ly
        raw = (if gridN then 3  else 0)   -- grid N → pixel NE (bits 1+2)
          .|. (if gridE then 6  else 0)   -- grid E → pixel SE (bits 2+4)
          .|. (if gridS then 12 else 0)   -- grid S → pixel SW (bits 4+8)
          .|. (if gridW then 9  else 0)   -- grid W → pixel NW (bits 1+8)
          ∷ Word8
    in if raw ≡ 15 then 0 else raw

-- * Render World Quads

renderWorldQuads ∷ EngineEnv → WorldState → Float → WorldCameraSnapshot
  → IO (V.Vector SortableQuad)
renderWorldQuads env worldState zoomAlpha snap = do
    tileData ← readIORef (wsTilesRef worldState)
    textures ← readIORef (wsTexturesRef worldState)
    paramsM ← readIORef (wsGenParamsRef worldState)
    camera ← readIORef (cameraRef env)
    floraCat ← readIORef (floraCatalogRef env)
    worldDate ← readIORef (wsDateRef worldState)
    texSizes ← readIORef (textureSizeRef env)
    harvests ← readIORef (wsFloraHarvestsRef worldState)

    let (fbW, fbH) = wcsFbSize snap
        facing = camFacing camera
        -- Annual-cycle flora wants a year-relative ordinal day, not the
        -- day-of-month field. Convert through the world calendar (falling
        -- back to the default calendar when gen params aren't loaded yet).
        calendar = maybe defaultCalendarConfig wgpCalender paramsM
        dayOfYear = worldDateToDayOfYear calendar worldDate

    -- Vertices carry a STABLE texture-handle id (#286); the bindless
    -- fragment shader resolves it to a live slot at draw time, so the
    -- cache never encodes a recyclable/stale slot. The default-face-map
    -- fallback (handle → slot 0 → default) now lives in the shader too.
    let lookupSlot texHandle = fromIntegral (toInt texHandle)
        lookupFmSlot texHandle = fromIntegral (toInt texHandle)
        worldSize = case paramsM of
                      Nothing → 128
                      Just params → wgpWorldSize params
    let zSlice = camZSlice camera
        zoom   = camZoom camera
        chunks = HM.elems (wtdChunks tileData)
        (camX, _camY) = camPosition camera

        effectiveDepth = min viewDepth (max 8 (round (zoom * 80.0 + 8.0 ∷ Float)))

        -- Lookup neighbor chunk fluid/terrain maps for cross-chunk water slopes
        fluidMapLookup cc = case HM.lookup cc (wtdChunks tileData) of
            Just lc' → Just (lcFluidMap lc')
            Nothing  → Nothing
        terrMapLookup cc = case HM.lookup cc (wtdChunks tileData) of
            Just lc' → Just (lcTerrainSurfaceMap lc')
            Nothing  → Nothing

        -- #418: a flora/veg billboard sitting in front of a structure's FRONT
        -- wall must draw over the WHOLE wall, not slice through the wall's
        -- depth-sorted strips (#417). A single-depth sprite otherwise beats
        -- the wall's back strips but loses its clamped south strips → the
        -- "leaf over the wall / frond cut off" straddle. Here we find the
        -- highest front-wall strip key the sprite at (gx,gy) is spatially IN
        -- FRONT of, so the caller lifts the sprite's key just above it and it
        -- clears the entire strip range as one unit. Returns Nothing when no
        -- such wall is near (the sprite keeps its normal key).
        --
        -- The strip-key formula mirrors 'frontWallStrips'/'tieBreak' in
        -- Structure.Render (SE 0.0006, SW 0.0005; the clamped south strip
        -- anchors at the S vertex (wgx+1,wgy+1)) — keep them in sync. The
        -- applyFacing depth test keeps it rotation-correct. Wall lookups cross
        -- chunks; the per-chunk gate below keeps it free where there are none.
        seTag = fromIntegral (fromEnum SWallSE) ∷ Word8
        swTag = fromIntegral (fromEnum SWallSW) ∷ Word8
        -- Chunks that actually carry structures. A sprite is only considered
        -- for the lift when its chunk is within ONE chunk of one of these, so
        -- the check stays a no-op in structure-free areas — but, unlike a
        -- same-chunk-only gate, it still fires for a sprite sitting across a
        -- chunk seam from a wall on the next chunk (structureFrontWallClear
        -- already resolves that wall cross-chunk).
        structureChunkCoords =
            [ lcCoord lc | lc ← chunks, not (HM.null (lcStructures lc)) ]
        structureFrontWallClear gx gy =
            let (fa, fb) = applyFacing facing gx gy
                spriteDepth = fa + fb
                wallKeyAt wgx wgy tag tieB = do
                    let (cc, _) = globalToChunk wgx wgy
                    lc' ← HM.lookup cc (wtdChunks tileData)
                    spd ← HM.lookup (wgx, wgy, tag) (lcStructures lc')
                    let (sa, sb) = applyFacing facing (wgx + 1) (wgy + 1)
                        scDepth  = sa + sb
                    if spriteDepth < scDepth   -- sprite is NOT fully in front
                       then Nothing
                       else Just (fromIntegral scDepth
                                  + fromIntegral (spdGridZ spd - zSlice) * 0.001
                                  + tieB)
                cands = [ wallKeyAt (gx + dx) (gy + dy) tag tieB
                        | dx ← [-2 .. 2], dy ← [-2 .. 2], (dx, dy) ≠ (0, 0)
                        , (tag, tieB) ← [(seTag, 0.0006 ∷ Float), (swTag, 0.0005)] ]
            in case [ k | Just k ← cands ] of
                 [] → Nothing
                 ks → Just (maximum ks)

        -- Cached pass: widen the bounds by the pan margin so the camera
        -- can travel that far before cameraChanged forces a rebuild
        -- (#447). The margins come from the SAME snapshot the cache is
        -- stamped with, pairing coverage with invalidation.
        vb = expandViewBounds (quadCacheMargins snap) $
                 computeViewBounds camera fbW fbH effectiveDepth

        visibleChunksWithOffset =
            [ (lc, offset)
            | lc ← chunks
            , isChunkRelevantForSlice zSlice lc
            , Just offset ← [isChunkVisibleWrapped facing worldSize vb camX (lcCoord lc)]
            ]

    let chunkVectors = map (\(lc, xOffset) →
            let coord  = lcCoord lc
                tileMap = lcTiles lc
                surfMap = lcSurfaceMap lc
                fluidMap = lcFluidMap lc
                iceMap   = lcIceMap lc
                chunkHasFluid = V.any isJust fluidMap
                terrainSurfMap = lcTerrainSurfaceMap lc

                -- #418: only pay the front-wall clearance lookup in chunks at
                -- or adjacent to one carrying structures (rooms are localised —
                -- most chunks are nowhere near one, so this is a no-op there).
                -- Adjacency (not same-chunk-only) is what lets a sprite across
                -- a chunk seam from a wall still get lifted. A qualifying sprite
                -- is raised to sit fully in front of any front wall it overlaps;
                -- everything else is untouched.
                ChunkCoord ccx ccy = coord
                chunkNearStructures =
                    any (\(ChunkCoord sx sy) → abs (sx - ccx) ≤ 1 ∧ abs (sy - ccy) ≤ 1)
                        structureChunkCoords
                bump gx gy q
                    | not chunkNearStructures = q
                    | otherwise = case structureFrontWallClear gx gy of
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
                                    drawY' = rawY - fromIntegral (z - zSlice) * tileSideHeight
                                in if mat ≡ 0 ∨ not (isTileVisible vb (rawX + xOffset) drawY')
                                   then acc2
                                   else let slopeId = ctSlopes col VU.! (z - ctStartZ col)
                                            tile = Tile mat slopeId
                                            tq = tileToQuad lookupSlot lookupFmSlot textures facing
                                                    gx gy z tile zSlice effectiveDepth zoomAlpha xOffset
                                                    mFluid chunkHasFluid

                                            -- Vegetation: only on surface tile, only when
                                            -- surface is above the fluid level
                                            vegQ = if z ≡ surfZ ∧ maybe True (\fc → surfZ > fcSurface fc) mFluid
                                                   then let i = z - ctStartZ col
                                                            vegId = ctVeg col VU.! i
                                                            slopeId = ctSlopes col VU.! i
                                                        in vegToQuad lookupSlot lookupFmSlot textures facing
                                                               gx gy z vegId slopeId zSlice effectiveDepth
                                                               zoomAlpha xOffset
                                                   else Nothing

                                        in case vegQ of
                                            Just vq → bump gx gy vq : tq : acc2
                                            Nothing → tq : acc2
                                ) acc [zLo .. zHi]
                    ) [] tileMap
                -- Flora sprites
                floraData = lcFlora lc
                !floraQuads =
                    [ bump gx gy fq
                    | inst ← fcdInstances floraData
                    , let tileX = fromIntegral (fiTileX inst)
                          tileY = fromIntegral (fiTileY inst)
                          idx   = columnIndex tileX tileY
                          col   = tileMap V.! idx
                          -- Find the actual topmost solid tile in the column
                          actualZ = findTopSolid col
                          inst' = inst { fiZ = actualZ }
                          (gx, gy) = chunkToGlobal coord tileX tileY
                          -- Harvested tile (#94): a HARVESTABLE species on
                          -- a tile with a live regrowth timer draws its
                          -- depleted texture (fruit stripped) — or nothing
                          -- at all when the species has no depleted art
                          -- (handle 0 falls out through the existing
                          -- filter below). Decorative co-tenants on the
                          -- same tile are unaffected.
                          mHarvest = lookupSpecies (fiSpecies inst) floraCat
                                       >>= fsHarvest
                          harvested = isJust mHarvest
                                    ∧ HM.member (gx, gy) harvests
                          texHandle = case (harvested, mHarvest) of
                              (True, Just fh) → fhHarvestedTexture fh
                              _ → resolveFloraTexture floraCat dayOfYear inst'
                    , actualZ > minBound  -- skip empty columns
                    , texHandle /= TextureHandle 0
                    , Just fq ← [floraToQuad lookupSlot lookupFmSlot textures facing
                                     gx gy inst' texHandle zSlice effectiveDepth
                                     zoomAlpha xOffset texSizes]
                    ]
                -- Water side-face quads: fill elevation gaps where water
                -- drops over cliff edges
                !waterSideQuads = if chunkHasFluid
                    then waterSideFaceQuads lookupSlot lookupFmSlot textures facing
                             coord fluidMap terrainSurfMap
                             fluidMapLookup terrMapLookup zSlice effectiveDepth
                             zoomAlpha xOffset vb
                    else []

                !blankQuads =
                    [ blankTileToQuad lookupSlot lookupFmSlot textures facing
                        gx gy zSlice zSlice zoomAlpha xOffset
                    | lx ← [0 .. chunkSize - 1]
                    , ly ← [0 .. chunkSize - 1]
                    , let idx = columnIndex lx ly
                          _surfZ = surfMap VU.! idx
                          terrainZ = terrainSurfMap VU.! idx
                    , terrainZ > zSlice
                    , let col = tileMap V.! idx
                          i = zSlice - ctStartZ col
                          hasTile = i ≥ 0
                                  ∧ i < VU.length (ctMats col)
                                  ∧ ctMats col VU.! i ≠ 0
                    , not hasTile
                    , let (gx, gy) = chunkToGlobal coord lx ly
                          (rawX, rawY) = gridToScreen facing gx gy
                          drawX = rawX + xOffset
                          drawY = rawY
                    , isTileVisible vb drawX drawY
                    ]

                _mkFreshwaterQuad gx gy ft fc slopeId =
                        freshwaterTileToQuad lookupSlot lookupFmSlot textures facing
                            gx gy (fcSurface fc) ft zSlice effectiveDepth
                            zoomAlpha xOffset slopeId

                -- Ice surface quads: rendered above ocean/freshwater
                !iceQuads =
                    [ iceTileToQuad lookupSlot lookupFmSlot textures facing
                        gx gy (icSurface ic) zSlice effectiveDepth zoomAlpha xOffset
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
                          drawX = rawX + xOffset
                          drawY = rawY - heightOffset
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
                                        drawX = rawX + xOffset
                                        drawY = rawY - heightOffset
                                        -- Skip ocean/lake rendering where ice covers the surface
                                        hasIce = isJust (iceMap V.! idx)
                                    in if not (isTileVisible vb drawX drawY)
                                       then (oAcc, lAcc, fAcc)
                                       else case fcType fc of
                                            Ocean
                                              | hasIce → (oAcc, lAcc, fAcc)
                                              | otherwise ->
                                                ( oceanTileToQuad lookupSlot lookupFmSlot textures facing
                                                    gx gy (fcSurface fc) zSlice effectiveDepth zoomAlpha xOffset
                                                  : oAcc
                                                , lAcc
                                                , fAcc
                                                )
                                            Lava  ->
                                                ( oAcc
                                                , lavaTileToQuad lookupSlot lookupFmSlot textures facing
                                                    gx gy (fcSurface fc) zSlice effectiveDepth zoomAlpha xOffset
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
                                                    zoomAlpha xOffset wSlope
                                                  : fAcc
                                                )
                                            River ->
                                                let wSlope = waterSlopeAt fluidMap terrainSurfMap coord fluidMapLookup terrMapLookup lx ly (fcSurface fc)
                                                in ( oAcc
                                                , lAcc
                                                , freshwaterTileToQuad lookupSlot lookupFmSlot textures facing
                                                    gx gy (fcSurface fc) River zSlice effectiveDepth
                                                    zoomAlpha xOffset wSlope
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
    return $! V.concat chunkVectors

-- * World Cursor Quads (generated every frame, not cached)

-- | Cap on the mine-tool preview rectangle's side length (the commit
--   handler has its own, larger cap). Keeps a cross-map drag from
--   generating tens of thousands of per-frame quads.
maxMinePreviewSide ∷ Int
maxMinePreviewSide = 64

renderWorldCursorQuads ∷ EngineEnv → WorldState → Float → IO (V.Vector SortableQuad)
renderWorldCursorQuads env worldState tileAlpha = do
    camera   ← readIORef (cameraRef env)
    tileData ← readIORef (wsTilesRef worldState)
    textures ← readIORef (wsTexturesRef worldState)
    paramsM  ← readIORef (wsGenParamsRef worldState)
    cs       ← readIORef (wsCursorRef worldState)
    toolMode ← readIORef (wsToolModeRef worldState)

    (winW, winH) ← readIORef (windowSizeRef env)
    (fbW, fbH)   ← readIORef (framebufferSizeRef env)

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

    -- Construction-designation ghosts (#95): world annotations like the
    -- mine markers, visible in every tool mode. Each renders with the
    -- ghost texture for its target category (structure vs building).
    constructDesigns ← readIORef (wsConstructDesignationsRef worldState)
    let constructTexFor cd = case cdTarget cd of
            CtStructure _ → constructStructTexture cs'
            CtBuilding  _ → constructBuildingTexture cs'
        -- Build-progress display (#96): the blueprint ghost solidifies
        -- as the build AI pours progress in — a fresh designation sits
        -- at 45 % alpha and ramps to opaque at progress 1.0 (the piece
        -- itself then replaces the ghost). The mining analogue carves
        -- terrain slopes per corner; construction ADDS material, so the
        -- ramp is the marker-level equivalent.
        constructAlphaFor cd =
            tileAlpha * (0.45 + 0.55 * max 0.0 (min 1.0 (cdProgress cd)))
        constructDesignQuads
            | HM.null constructDesigns = V.empty
            | otherwise = V.fromList
                [ worldCursorToQuad lookupSlot lookupFmSlot textures
                      facing dgx dgy (cdZ cd) zSlice effectiveDepth
                      (constructAlphaFor cd) xOff tex
                | ((dgx, dgy), cd) ← HM.toList constructDesigns
                , Just tex ← [constructTexFor cd]
                , let (chunkCoord, _) = globalToChunk dgx dgy
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

    -- Construction tool: anchor→hover rectangle preview, mirroring the
    -- mine preview (per-z-level, unloaded chunks skipped). Drawn with the
    -- select-cursor texture so it reads as "about to be designated".
    let constructPreviewQuads = case (constructAnchor cs', hoverResult, worldCursorTexture cs') of
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

    -- Mine + construction markers are world annotations: shown in every
    -- tool mode. The mode only adds its own hover/preview on top.
    let markerQuads = designQuads <> constructDesignQuads
    return $ case toolMode of
        InfoTool      → markerQuads <> hoverQuads <> selectQuads
        MineTool      → markerQuads <> hoverQuads <> minePreviewQuads
        ConstructTool → markerQuads <> hoverQuads <> constructPreviewQuads
        _             → markerQuads

-- | Find the topmost Z that has a non-zero material in a column.
--   This is the actual rendered surface — no trusting surface maps.
findTopSolid ∷ ColumnTiles → Int
findTopSolid col =
    let mats = ctMats col
        _start = ctStartZ col
        len = VU.length mats
    in go (len - 1)
  where
    go i
        | i < 0 = minBound
        | ctMats col VU.! i /= 0 = ctStartZ col + i
        | otherwise = go (i - 1)
