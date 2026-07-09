{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Generate.Chunk
    ( generateChunk
    , generateLoadedChunk
    , generateExposedColumn
    , generateZoomTerrain
    -- Post-classification soil gates (exposed for unit testing).
    , surfaceDemotion
    , demoteWetland
    , wetlandKeep
    , saltFlatKeep
    , nearFlat
    ) where

import UPrelude
import Control.Monad.ST (runST)
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector as V
import World.Types
import World.Material (MaterialId(..), matGlacier, MaterialRegistry)
import World.Plate (elevationAtGlobal, isBeyondGlacier, wrapGlobalU)
import World.Scale (computeWorldScale)
import World.Slope (computeChunkSlopes)
import Structure.Types (emptyChunkStructures)
import World.Fluid.Ice (computeChunkIce)
import World.Magma.Types (MagmaOverlay(..))
import World.Magma.Init (discoverChunkLava)
import World.Hydrology.WaterTable (computeWaterTable)
import World.Fluid.Lake.Types (WorldLakes(..))
import World.Fluid.River.Types (WorldRivers(..))
import World.Vegetation (computeChunkVegetation, vegSnow, vegHash)
import World.Flora.Placement (computeChunkFlora)
import World.Generate.Constants (chunkBorder)
import World.Generate.Coordinates (chunkToGlobal)
import World.Generate.Timeline (applyTimelineChunk, removeElevationSpikes)
import World.Geology.Coastal (applyCoastalTable)
import World.Fluid.Seabed (applySeabedTable)
import World.Generate.Chunk.Fluid
    ( composeFluidMap, chunkWaterSurfMap, applyBasaltCaps, lavaShellMask
    , applyLavaShell, smoothIslandColumns, mkSurfaceMap, chunkOrNeighborOceanic
    , mergeRimCaps, poolRimCaps )
import World.Generate.Chunk.SoilGates
    ( surfaceDemotion, demoteWetland, wetlandKeep, saltFlatKeep, nearFlat
    , applyFluidWt )
import World.Generate.Chunk.Columns (buildChunkColumns, generateExposedColumn)
import World.Generate.Chunk.Zoom (generateZoomTerrain)

-- | 'generateChunk' wrapped into a 'LoadedChunk'. The canonical
--   tuple→record assembly — chunk loading and the zoom-map ore survey
--   both go through here so the field mapping can't drift between
--   call sites. Side decorations start empty (the loading pipeline
--   computes them later; irrelevant for transient consumers).
generateLoadedChunk ∷ MaterialRegistry → FloraCatalog → WorldGenParams
                    → ChunkCoord → LoadedChunk
generateLoadedChunk registry catalog params coord =
    let (chunkTiles, surfMap, tMap, fluidMap, iceMap, flora, wtMap, magma) =
            generateChunk registry catalog params coord
    in LoadedChunk
        { lcCoord      = coord
        , lcTiles      = chunkTiles
        , lcSurfaceMap = surfMap
        , lcTerrainSurfaceMap = tMap
        , lcFluidMap   = fluidMap
        , lcIceMap     = iceMap
        , lcFlora      = flora
        , lcSideDeco   = VU.replicate (chunkSize * chunkSize) 0
        , lcWaterTableMap = wtMap
        , lcMagma      = magma
        , lcStructures = emptyChunkStructures
        }

-- | Generate a single chunk. Pure and deterministic.
--   Returns (tiles, surfaceMap) where surfaceMap maps (lx,ly) -> surfaceZ.
--
--   Erosion is computed per-period across all columns in the chunk,
--   using a shared elevation map so each tile can read its neighbors'
--   post-event elevations. This gives physically-based smoothing
--   that respects material hardness and geological time.
--
--   The border is expanded to chunkBorder tiles so erosion at
--   chunk edges has valid neighbor data.
generateChunk ∷ MaterialRegistry → FloraCatalog → WorldGenParams
  → ChunkCoord → (Chunk, VU.Vector Int, VU.Vector Int
                 , V.Vector (Maybe FluidCell), IceMap, FloraChunkData
                 , VU.Vector Int, Maybe MagmaOverlay)
generateChunk registry catalog params coord =
    let seed = wgpSeed params
        worldSize = wgpWorldSize params
        timeline = wgpGeoTimeline params
        plates = wgpPlates params
        wsc = computeWorldScale worldSize

        borderSize = chunkSize + 2 * chunkBorder
        borderArea = borderSize * borderSize

        toIndex lx ly =
            let bx = lx + chunkBorder
                by = ly + chunkBorder
            in by * borderSize + bx

        fromIndex idx =
            let (by, bx) = idx `divMod` borderSize
            in (bx - chunkBorder, by - chunkBorder)

        inBorder lx ly =
            lx ≥ negate chunkBorder ∧ lx < chunkSize + chunkBorder ∧
            ly ≥ negate chunkBorder ∧ ly < chunkSize + chunkBorder

        -- Base elevation/material grids (with border) built in one pass
        -- Beyond-glacier tiles use matGlacier + high elevation so the
        -- timeline preserves them and coastal erosion doesn't treat the
        -- world boundary as a coastline.
        (baseElevVec, baseMatVec) = runST $ do
            elevM ← VUM.new borderArea
            matM  ← VUM.new borderArea
            forM_ [0 .. borderArea - 1] $ \idx → do
                let (lx, ly) = fromIndex idx
                    (gx, gy) = chunkToGlobal coord lx ly
                    (gx', gy') = wrapGlobalU worldSize gx gy
                if isBeyondGlacier worldSize gx' gy'
                    then do
                        VUM.write elevM idx (seaLevel + 100)
                        VUM.write matM  idx matGlacier
                    else do
                        let (elev, mat) =
                                elevationAtGlobal seed plates worldSize gx' gy'
                        VUM.write elevM idx elev
                        VUM.write matM  idx mat
            elevF ← VU.unsafeFreeze elevM
            matF  ← VU.unsafeFreeze matM
            pure (elevF, matF)

        lookupBase lx ly =
            if inBorder lx ly
            then ( baseElevVec VU.! toIndex lx ly
                 , baseMatVec  VU.! toIndex lx ly
                 )
            else (0, MaterialId 1)

        -- Apply timeline using split vectors
        (timelineElevVec, timelineMatVec) =
            applyTimelineChunk timeline worldSize registry wsc coord
                (baseElevVec, baseMatVec)

        -- Post-timeline coastal erosion: apply the GLOBAL coastal
        -- table (computed once at world init on the stitched terrain,
        -- 'identifyCoastalErosion') — lowered coastal terrain +
        -- sand/gravel/wetland material rewrites. Border tiles read
        -- their owning chunk's entry, so adjacent chunks always agree
        -- on the coastline (the old per-window pass diverged up to
        -- ~18z at seams).
        (postCoastElev, finalMatVec) =
            applyCoastalTable (gtCoastal timeline) coord
                (timelineElevVec, timelineMatVec)

        -- Despike: remove single-tile elevation outliers that survived
        -- timeline events and coastal erosion. These are typically
        -- 1-tile-wide diagonal mountain ridges (features aligned with
        -- the u-v isometric axes that end up 1 tile wide in xy) where
        -- coastal erosion lowered the cardinal neighbors but the spike
        -- itself was outside the coastal range.
        (despikedElev, _) = removeElevationSpikes 12 4 (chunkSize + 2 * chunkBorder)
                                                  (postCoastElev, finalMatVec)

        -- Seabed pass: ocean-floor relief + materials + bedrock
        -- outcrops (global 'gtSeabed' table, computed once at init).
        -- Applied here — after the first despike, before the river
        -- carve — so the rest of the pipeline (carve, strata, slope,
        -- columns) sees the seabed terrain and materials. Supersedes
        -- the old flat seaLevel−1 lake carve (whose deltas are now
        -- empty). 'seabedMatVec' is the coastal material with seabed
        -- surface materials layered on; used for the column build.
        (seabedElev, seabedMatVec) =
            applySeabedTable (gtSeabed timeline) coord
                (despikedElev, finalMatVec)

        -- River and lake carves applied to the ENTIRE bordered region
        -- (not just the interior). The carve delta for each tile lives
        -- in the chunk that owns that tile — for border tiles this
        -- might be a neighbouring chunk. Per-tile lookup combines the
        -- river and lake deltas via @max@: rivers carve channels;
        -- coastal lakes whose surface has been clamped to sea level
        -- carve their basin floor sub-sea so water actually fills.
        -- Reading post-carve elevations everywhere keeps strata,
        -- slope, and column construction consistent with what the
        -- owning chunk actually renders.
        --
        -- WRAP-SEAM INVARIANT (why the chunk key below is derived from
        -- the UNWRAPPED gx/gy — "no u-wrap on the keys", same as
        -- 'World.Geology.Coastal.Types'): the carve tables are built
        -- from the stitched square grid, which DOUBLE-COVERS the seam
        -- region — every near-seam physical tile appears at both its
        -- canonical position and its u-alias, and the tables hold
        -- entries under BOTH chunk keys with identical content. A
        -- bordered lookup from a canonical chunk near the seam
        -- therefore hits the alias entry directly; wrapping the key
        -- would be redundant. The only unkeyed reach is a border that
        -- exits the square grid entirely, which by the diamond
        -- geometry is glacier-corner territory where no carve content
        -- exists (lookup misses resolve to delta 0, correctly).
        carveAt gx gy =
            let cx = gx `div` chunkSize
                cy = gy `div` chunkSize
                ilx = ((gx `mod` chunkSize) + chunkSize) `mod` chunkSize
                ily = ((gy `mod` chunkSize) + chunkSize) `mod` chunkSize
                li  = ily * chunkSize + ilx
                dr  = case HM.lookup (ChunkCoord cx cy)
                                     (wrCarveDelta (gtWorldRivers timeline)) of
                        Just dv → dv VU.! li
                        Nothing → 0
                dl  = case HM.lookup (ChunkCoord cx cy)
                                     (wlCarveDelta (gtWorldLakes timeline)) of
                        Just dv → dv VU.! li
                        Nothing → 0
            in max dr dl
        carvedElevVec = VU.generate borderArea $ \idx →
            let z = seabedElev VU.! idx
            in if z ≡ minBound
               then z
               else
                 let (lx, ly) = fromIndex idx
                     (gx, gy) = chunkToGlobal coord lx ly
                 in z - carveAt gx gy

        -- Second despike, post-carve. The pass above ran BEFORE the
        -- global river/lake carve deltas were subtracted — a natural
        -- cliff-edge tile beside a deeply carved channel is not a
        -- spike pre-carve (its high neighbour hides it) but becomes
        -- a 100z+ pillar once the neighbour is carved away (seed 7
        -- w128 @(-96,159): 6→132→177 natural, 177 carved to 29 ⇒
        -- 132 left standing — TERRAIN_SPIKE). Re-running the same
        -- bordered despike on the carved elevations collapses these;
        -- it only fires on 1-tile pillars >12 above ALL cardinal
        -- neighbours, so untouched natural terrain is unaffected.
        -- Mirrored in 'generateZoomTerrain' (chunk/fast parity).
        (finalElevVec, _) =
            removeElevationSpikes 12 4 (chunkSize + 2 * chunkBorder)
                                  (carvedElevVec, seabedMatVec)

        lookupFinal lx ly =
            if inBorder lx ly
            then ( finalElevVec VU.! toIndex lx ly
                 , seabedMatVec VU.! toIndex lx ly
                 )
            else (0, MaterialId 1)

        lookupElev lx ly = fst (lookupFinal lx ly)

        lookupElevOr lx ly fallback =
            if inBorder lx ly
            then finalElevVec VU.! toIndex lx ly
            else fallback

        -- (timeline elevation lookup removed — strata now use clamped
        -- post-coastal neighbors to prevent over-erosion without the
        -- mismatch that creates air-tile gaps near the surface)

        -- Pre-compute wrapped coordinates for the 16×16 chunk interior.
        -- Used by terrainSurfaceMap and strataCache to avoid redundant
        -- chunkToGlobal + wrapGlobalU + isBeyondGlacier calls.
        chunkArea = chunkSize * chunkSize

        (coordGX, coordGY, coordBeyond) = runST $ do
            gxM     ← VUM.new chunkArea
            gyM     ← VUM.new chunkArea
            beyondM ← VUM.new chunkArea
            forM_ [0 .. chunkArea - 1] $ \idx → do
                let lx = idx `mod` chunkSize
                    ly = idx `div` chunkSize
                    (gx, gy) = chunkToGlobal coord lx ly
                    (gx', gy') = wrapGlobalU worldSize gx gy
                VUM.write gxM     idx gx'
                VUM.write gyM     idx gy'
                VUM.write beyondM idx (isBeyondGlacier worldSize gx' gy')
            gxF     ← VU.unsafeFreeze gxM
            gyF     ← VU.unsafeFreeze gyM
            beyondF ← VU.unsafeFreeze beyondM
            pure (gxF, gyF, beyondF)

        -- Terrain surface map (vector). This is the RAW chunk-gen
        -- terrain before island-column smoothing; the smoothed
        -- version is bound below as 'terrainSurfaceMap' and is what
        -- the rest of the pipeline consumes. River carve is already
        -- baked into 'finalElevVec' above, so 'lookupElev' returns
        -- the post-carve elevation directly.
        rawTerrainSurfaceMap = VU.generate chunkArea $ \idx →
            if coordBeyond VU.! idx
            then minBound
            else lookupElev (idx `mod` chunkSize) (idx `div` chunkSize)

        -- Same per-chunk ocean test that 'composeFluidMap' uses
        -- internally — lifted here so we know whether the ocean
        -- column will sit above any cap the magma overlay leaves
        -- (used below for the lava-shell safety net only).
        chunkIsOceanicHere = chunkOrNeighborOceanic params coord

        -- Pure-function lava overlay: which surface tiles in this
        -- chunk have a chamber or chute breaking through? Per-tile
        -- decision: above water → lava cell in @moSurface@; sub-sea
        -- → basalt cap entry in @moBasaltCap@ (terrain raised +
        -- matBasalt at top so the ocean fills above instead of a
        -- black gap, or the cap stays exposed as a basalt outcrop
        -- in inland sub-sea pockets).
        magmaOverlayBase = discoverChunkLava (wgpVolcanoCtx params) coord
                                          rawTerrainSurfaceMap
                                          (chunkWaterSurfMap params coord)

        -- Containment rim: outermost pool tiles raised to the pool
        -- surface as basalt caps (see 'poolRimCaps').
        rimCaps = poolRimCaps params coord
                      (\lx ly → if inBorder lx ly
                                then Just (finalElevVec VU.! toIndex lx ly)
                                else Nothing)
        magmaOverlay = mergeRimCaps magmaOverlayBase rimCaps

        -- Patch terrain to raise capped tiles BEFORE composeFluidMap
        -- so the ocean classification + smoother see the new (sealed)
        -- terrain. Above-cap tiles stay at their original z.
        cappedTerrainMap = applyBasaltCaps coord magmaOverlay
                                            rawTerrainSurfaceMap

        -- All fluid placement — global lake table + ocean + lava.
        -- Shared with generateZoomTerrain so the two views agree about
        -- which tiles are water (and lava).
        rawFluidMap = composeFluidMap params coord cappedTerrainMap

        -- Lava-water boundary shell: any lava tile 8-adjacent to a
        -- non-lava tile (water OR dry land — see 'lavaShellMask')
        -- gets cleared and the column-build below stamps matBasalt
        -- on top, so lava never sits edge-to-edge with water or
        -- bare ground. Interior lava is preserved — only the
        -- contact rim becomes solid rock.
        lavaShell = lavaShellMask params coord
                        (\lx ly → if inBorder lx ly
                                  then Just (finalElevVec VU.! toIndex lx ly)
                                  else Nothing)
                        rawFluidMap
        shellFluidMap = applyLavaShell lavaShell cappedTerrainMap
                                        chunkIsOceanicHere rawFluidMap

        -- Smooth "island column" artifacts: dry tiles surrounded by
        -- ≥3 lake-of-same-surface neighbors whose terrain peeks 1-K z
        -- above that surface. These are usually noise-driven 1-tile
        -- spikes that fall under the global despike threshold. We
        -- override their fluid cell to Lake at the neighbors' surface
        -- and lower their rendered terrain to surface-1 so the
        -- renderer paints them as ordinary lake tiles. Downstream
        -- (slope, surface materials, surface vegetation, fluid map
        -- output) all read the smoothed binding.
        (terrainSurfaceMap, fluidMap) =
            smoothIslandColumns cappedTerrainMap shellFluidMap

        finalTerrain = terrainSurfaceMap

        -- Surface map: rivers render flat (water surface hides any
        -- minor terrain protrusions in the carved channel); other
        -- fluids use max(terrain, water).
        surfaceMap = mkSurfaceMap finalTerrain fluidMap

        -- Build per-column tile data ('buildChunkColumns', fusing
        -- stratigraphy computation with ColumnTiles construction to
        -- avoid an intermediate V.Vector ColumnStrata allocation).
        rawChunk = buildChunkColumns timeline worldSize wsc registry
                       coordBeyond coordGX coordGY lookupFinal lookupBase
                       magmaOverlay lavaShell terrainSurfaceMap lookupElevOr
                       rawFluidMap chunkArea

        noNeighborLookup ∷ ChunkCoord → Maybe (VU.Vector Int)
        noNeighborLookup _ = Nothing

        -- Initial single-chunk gen sees no neighbours; the cross-chunk
        -- slope recompute (recomputeNeighborSlopes) re-slides the border
        -- strip once neighbours load, supplying the real fluid lookup.
        noFluidNeighborLookup ∷ ChunkCoord → Maybe (V.Vector (Maybe FluidCell))
        noFluidNeighborLookup _ = Nothing

        slopedTiles = computeChunkSlopes seed coord terrainSurfaceMap registry
                                         fluidMap rawChunk noNeighborLookup
                                         noFluidNeighborLookup

        -- Extract surface material and slope for vegetation computation
        surfaceMatsRaw = VU.generate chunkArea $ \idx →
            let col = slopedTiles V.! idx
                surfZ = terrainSurfaceMap VU.! idx
                i = surfZ - ctStartZ col
            in if i ≥ 0 ∧ i < VU.length (ctMats col)
               then ctMats col VU.! i
               else 0

        -- Corrected surface materials (see 'demoteWetland'): vegetation
        -- and flora read these, so swamp species follow the demoted
        -- soils automatically.
        --
        -- Cross-border elevations for the wetland flat test come from
        -- the bordered post-carve vector, so the gate sees the same
        -- neighbours the renderer does.
        wetOutElev olx oly =
            if inBorder olx oly
            then Just (finalElevVec VU.! toIndex olx oly)
            else Nothing
        surfaceMats = VU.imap (\idx m →
            case surfaceDemotion wetOutElev terrainSurfaceMap waterTableMap idx m of
                Just demoted → demoted
                Nothing      → m
            ) surfaceMatsRaw

        surfaceSlopes = VU.generate chunkArea $ \idx →
            let col = slopedTiles V.! idx
                surfZ = terrainSurfaceMap VU.! idx
                i = surfZ - ctStartZ col
            in if i ≥ 0 ∧ i < VU.length (ctSlopes col)
               then ctSlopes col VU.! i
               else 0

        baseVegIds = computeChunkVegetation seed worldSize coord
                        terrainSurfaceMap surfaceMats surfaceSlopes
                        fluidMap (wgpClimateState params)
        -- Snow on ice-covered tiles: ice is an overlay not in the
        -- terrain material system, so inject snow vegetation here.
        vegIds = VU.imap (\idx v →
            case iceMap V.! idx of
                Just _  →
                    let ChunkCoord cx cy = coord
                        lx = idx `mod` chunkSize
                        ly = idx `div` chunkSize
                        gx = cx * chunkSize + lx
                        gy = cy * chunkSize + ly
                        h = vegHash seed gx gy
                        variant = fromIntegral ((h `shiftR` 8) .&. 0x03) ∷ Word8
                    in vegSnow + variant
                Nothing → v
            ) baseVegIds
        -- Flora sprites (trees, shrubs, wildflowers)
        floraData = computeChunkFlora seed worldSize coord
                        terrainSurfaceMap surfaceMats surfaceSlopes
                        fluidMap (wgpClimateState params) catalog

        -- Inject veg IDs into column tiles AND truncate strata above
        -- the smoothed surface. The raw column built earlier ran from
        -- 'exposeFrom' up to the RAW pre-smoothing terrain top; for
        -- island-column tiles 'smoothIslandColumns' lowered the
        -- terrain surface to @lake.surface − 1@ but the rock blocks
        -- at z ∈ (smoothed, raw] are still in 'ctMats'. The main
        -- rendering loop in 'World.Render.Quads' iterates up to
        -- @ctStartZ + length(ctMats) − 1@; without truncation those
        -- rock blocks reappear as a tiny grass cap above the water
        -- whenever the camera's zSlice rises above the lake surface.
        -- Setting them to 'matAir' (= MaterialId 0) makes the renderer
        -- skip them (it short-circuits on @mat ≡ 0@).
        finalTiles = V.imap (\idx col →
            let vegId    = vegIds VU.! idx
                matsLen  = VU.length (ctMats col)
                surfZ    = terrainSurfaceMap VU.! idx
                i        = surfZ - ctStartZ col
                truncatedMats =
                    if surfZ ≡ minBound ∨ i < 0 ∨ i ≥ matsLen - 1
                    then ctMats col
                    else
                      let topIdxs = [(j, 0) | j ← [i + 1 .. matsLen - 1]]
                      in ctMats col VU.// topIdxs
                -- Wetland demotion (see surfaceMats above): rewrite the
                -- contiguous wetland veneer at the surface so the
                -- rendered column matches the corrected surface material
                -- that vegetation/flora already saw.
                demotedMats =
                    if i < 0 ∨ i ≥ VU.length truncatedMats
                    then truncatedMats
                    else
                      let orig = truncatedMats VU.! i
                      in case surfaceDemotion wetOutElev terrainSurfaceMap waterTableMap idx orig of
                          Just demoted →
                              let go j | j ≥ 0 ∧ truncatedMats VU.! j ≡ orig = go (j - 1)
                                       | otherwise = j + 1
                                  runStart = go i
                              in truncatedMats VU.// [ (j, demoted) | j ← [runStart .. i] ]
                          Nothing → truncatedMats
                vegVec   = VU.replicate matsLen 0
                vegVec'  = if i ≥ 0 ∧ i < matsLen ∧ vegId > 0
                           then vegVec VU.// [(i, vegId)]
                           else vegVec
            in col { ctMats = demotedMats, ctVeg = vegVec' }
            ) slopedTiles

        -- Ice overlay: computed from climate, altitude, and the global
        -- ice level grid (pre-computed at world init in gtIceLevel).
        -- Sits on top of terrain or fluid for frozen ocean/lake.
        iceMap = computeChunkIce seed plates (wgpClimateState params)
                                 worldSize coord (gtIceLevel timeline)
                                 terrainSurfaceMap fluidMap

        -- Water table: subsurface saturation baseline from climate
        -- (Phase 2 simplified), made fluid-aware by 'applyFluidWt' —
        -- under-bed bump (lake/river/ocean) + fresh-water shore halo.
        wtBase = computeWaterTable (wgpClimateState params)
                                   worldSize coord terrainSurfaceMap
        waterTableMap = applyFluidWt fluidMap wtBase

    in (finalTiles, surfaceMap, finalTerrain, fluidMap, iceMap, floraData
       , waterTableMap, magmaOverlay)
