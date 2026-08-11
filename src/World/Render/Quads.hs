{-# LANGUAGE Strict #-}
module World.Render.Quads
    ( renderWorldQuads
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
import Engine.Asset.Handle (TextureHandle(..), toInt)
import Engine.Scene.Types (SortableQuad(..))
import Engine.Graphics.Camera (Camera2D(..), CameraFacing)
import World.Types
import World.Flora.Render (resolveFloraTexture)
import World.Flora.CropPlot (cropPlotElapsedDays, cropPlotInstance)
import World.Generate (chunkToGlobal, viewDepth)
import World.Generate.Coordinates (canonicalTileFrame)
import World.Grid (gridToScreen, tileSideHeight, applyFacing)
import Structure.Types (StructureSlot(..), ChunkStructures, spdGridZ)
import World.Render.ViewBounds (computeViewBounds, expandViewBounds, isTileVisible)
import World.Render.Camera (quadCacheMargins)
import World.Render.ChunkCulling (isChunkRelevantForSlice, isChunkVisibleWrapped)
import World.Render.FloraQuads (floraToQuad)
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

renderWorldQuads ∷ EngineEnv → WorldState → Float → WorldCameraSnapshot
  → IO (V.Vector SortableQuad)
renderWorldQuads env worldState zoomAlpha snap = do
    tileData ← readIORef (wsTilesRef worldState)
    textures ← readIORef (wsTexturesRef worldState)
    paramsM ← readIORef (wsGenParamsRef worldState)
    camera ← readIORef (rvCameraRef (toRenderViewCapability env))
    floraCat ← readIORef (wsFloraCatalogRef (toWorldSimCapability env))
    worldDate ← readIORef (wsDateRef worldState)
    texSizes ← readIORef (rvTextureSizeRef (toRenderViewCapability env))
    harvests ← readIORef (wsFloraHarvestsRef worldState)
    cropPlots ← readIORef (wsCropPlotsRef worldState)

    let (fbW, fbH) = wcsFbSize snap
        facing = camFacing camera
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
    let zSlice = camZSlice camera
        zoom   = camZoom camera
        -- Caught by the #1135 audit's search, but not a coordinate
        -- lookup: this ENUMERATES the stored LoadedChunk values and
        -- accepts no caller coord, so there is no key to canonicalise.
        -- Every element is already in the stored frame — insertion keys
        -- on the canonical lcCoord (World.Thread.ChunkLoading →
        -- World.Tile.Types.insertChunk), and consumers below read each
        -- chunk's own lcCoord rather than reconstructing one.
        chunks = HM.elems (wtdChunks tileData)
        (camX, camY) = camPosition camera

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
        -- (#447). The margins come from the SAME snapshot the cache is
        -- stamped with, pairing coverage with invalidation.
        vb = expandViewBounds (quadCacheMargins snap) $
                 computeViewBounds camera fbW fbH effectiveDepth

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
                                           zSlice structLookup gx gy of
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
                                       ⌦ fsHarvest
                          harvested = isJust mHarvest
                                    ∧ HM.member (gx, gy) harvests
                          texHandle = case (harvested, mHarvest) of
                              (True, Just fh) → fhHarvestedTexture fh
                              _ → resolveFloraTexture floraCat daysPerYear
                                      absDay inst'
                    , actualZ > minBound  -- skip empty columns
                    , texHandle ≢ TextureHandle 0
                    , Just fq ← [floraToQuad lookupSlot lookupFmSlot textures facing
                                     gx gy inst' texHandle zSlice effectiveDepth
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
                                  ∧ ctMats col VU.! i ≠ 0
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
    return $! V.concat chunkVectors

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
--   The strip-key formula mirrors 'frontWallStrips'/'tieBreak' in
--   Structure.Render (SE 0.0006, SW 0.0005; the clamped south strip
--   anchors at the S vertex (wgx+1,wgy+1)) — keep them in sync. The
--   applyFacing depth test keeps it rotation-correct. Wall lookups cross
--   chunks; the per-chunk gate at the call site keeps it free where
--   there are none.
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
    → (ChunkCoord → Maybe ChunkStructures)  -- ^ loaded-chunk structure lookup
    → Int → Int                             -- ^ sprite tile (gx, gy)
    → Maybe Float
structureFrontWallClear facing worldSize zSlice structLookup gx gy =
    let (fa, fb) = applyFacing facing gx gy
        spriteDepth = fa + fb
        seTag = fromIntegral (fromEnum SWallSE) ∷ Word8
        swTag = fromIntegral (fromEnum SWallSW) ∷ Word8
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
            -- (Structure quads carry no wrap offset at ALL — see
            -- 'Structure.Render', which is not one of the six
            -- 'isChunkVisibleWrapped' consumers — so a wall's own
            -- screen position across the seam is separately unfixed.
            -- That is out of #1176's scope and does not change this
            -- guard's verdict, which rests on the key alone.)
            let (sa, sb) = applyFacing facing (sgx + 1) (sgy + 1)
                scDepth  = sa + sb
                (la, lb) = applyFacing facing (wgx + 1) (wgy + 1)
                localDepth = la + lb
            if spriteDepth < localDepth   -- sprite is NOT fully in front
               ∨ scDepth ≠ localDepth     -- frames disagree (E/W seam)
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

-- | Find the topmost Z that has a non-zero material in a column.
--   This is the actual rendered surface — no trusting surface maps.
findTopSolid ∷ ColumnTiles → Int
findTopSolid col =
    let mats = ctMats col
        len = VU.length mats
    in go (len - 1)
  where
    go i
        | i < 0 = minBound
        | ctMats col VU.! i ≢ 0 = ctStartZ col + i
        | otherwise = go (i - 1)
