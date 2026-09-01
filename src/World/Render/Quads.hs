{-# LANGUAGE Strict #-}
module World.Render.Quads
    ( renderWorldQuads
    , renderWorldQuadsScanned
      -- | Re-exported from "World.Render.SpriteDepth", which #1856 moved
      --   it to so the Chop selection oracle and the designation marker
      --   could apply the SAME lift this pass does.
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
import World.Types
import World.Flora.Render (resolveFloraTexture)
import World.Flora.CropPlot (cropPlotElapsedDays, cropPlotInstance)
import World.Generate (chunkToGlobal, viewDepth)
import World.Grid (gridToScreen, tileSideHeight)
import World.Render.ViewBounds (viewBoundsAt, expandViewBounds, isTileVisible)
import World.Render.Camera (quadCacheMargins)
import World.Render.ChunkCulling (isChunkRelevantForSlice, isChunkVisibleWrapped)
import World.Render.FloraQuads (floraToQuad)
import World.Render.FloraDraws
    (FloraDraw(..), chunkFloraDraws)
import World.Render.SpriteDepth
    (frameFrontWallLift, liftSpriteSortKey, structureFrontWallClear)
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

        -- The frame's front-wall sprite lift, built ONCE from the loaded
        -- chunks (#1856) — the same construction the Chop picker and the
        -- designation marker use, so three passes cannot derive three
        -- subtly different gates. Per-chunk adjacency keeps it free
        -- where there are no structures, which is most of the map.
        spriteLift = frameFrontWallLift facing worldSize zSlice
                         effectiveDepth (wtdChunks tileData)

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

                -- #418: a qualifying sprite is raised to sit fully in
                -- front of any structure front wall it overlaps.
                --
                -- #1856 moved the rule to "World.Render.SpriteDepth" so
                -- the Chop selection oracle and the designation marker
                -- apply the IDENTICAL lift: a sprite's lifted key is its
                -- final painter depth, and a picker reading the unlifted
                -- one would disagree with what this pass drew.
                bump gx gy q = q
                    { sqSortKey =
                        liftSpriteSortKey spriteLift coord gx gy (sqSortKey q) }

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


