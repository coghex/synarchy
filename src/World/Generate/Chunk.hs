{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Generate.Chunk
    ( generateChunk
    , generateExposedColumn
    ) where

import UPrelude
import Data.Bits (shiftR, (.&.))
import Control.Monad.ST (runST, ST)
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector as V
import World.Types
import World.Material (MaterialId(..), matGlacier, getMaterialProps
                      , MaterialProps(..), matAir, MaterialRegistry(..))
import World.Plate (TectonicPlate(..), generatePlates
                   , elevationAtGlobal, isBeyondGlacier, wrapGlobalU)
import World.Geology (applyGeoEvent, GeoModification(..))
import World.Geology.Timeline.Types (GeoEvent(..))
import World.Hydrology.Types (HydroFeature(..), RiverParams(..))
import World.Scale (computeWorldScale, WorldScale(..))
import World.Slope (computeChunkSlopes)
import World.Fluids (isOceanChunk, hasAnyOceanFluid, computeChunkFluid
                    , computeChunkLava, computeChunkLakes, computeChunkRivers
                    , unionFluidMap, equilibrateFluidMap, fillCoastalGaps)
import World.Fluid.Internal (stripLakeRiverCliffs)
import World.Fluid.Ice (computeChunkIce)
import World.Vegetation (computeChunkVegetation, vegSnow, vegHash)
import World.Flora.Placement (computeChunkFlora)
import World.Generate.Constants (chunkBorder)
import World.Generate.Coordinates (chunkToGlobal)
import World.Generate.Timeline (applyTimelineChunk)
import World.Geology.Coastal (applyCoastalErosion)
import World.Generate.Strata
    ( buildStrataCache
    , buildColumnStrata
    )

-- * Chunk Generation

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
                 , V.Vector (Maybe FluidCell), IceMap, FloraChunkData)
generateChunk registry catalog params coord =
    let seed = wgpSeed params
        worldSize = wgpWorldSize params
        timeline = wgpGeoTimeline params
        plates = wgpPlates params
        wsc = computeWorldScale worldSize
        oceanMap = wgpOceanMap params

        -- True if this chunk or any of its 8 neighbors is oceanic.
        -- Used to extend strata to sea level for all coastal columns,
        -- preventing cliff-face voids at chunk boundaries.
        isCoastalChunk = hasAnyOceanFluid oceanMap coord

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
        (baseElevVec, baseMatVec) = runST $ do
            elevM ← VUM.new borderArea
            matM  ← VUM.new borderArea
            forM_ [0 .. borderArea - 1] $ \idx → do
                let (lx, ly) = fromIndex idx
                    (gx, gy) = chunkToGlobal coord lx ly
                    (gx', gy') = wrapGlobalU worldSize gx gy
                if isBeyondGlacier worldSize gx' gy'
                    then do
                        VUM.write elevM idx 0
                        VUM.write matM  idx (MaterialId 1)
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

        -- Post-timeline coastal erosion: lower coastal terrain,
        -- deposit sand/gravel/wetland materials based on plate tectonics
        -- and river mouth proximity
        (finalElevVec, finalMatVec) =
            applyCoastalErosion seed worldSize plates registry timeline oceanMap coord
                (timelineElevVec, timelineMatVec)

        lookupFinal lx ly =
            if inBorder lx ly
            then ( finalElevVec VU.! toIndex lx ly
                 , finalMatVec  VU.! toIndex lx ly
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

        -- Terrain surface map (vector)
        terrainSurfaceMap = VU.generate chunkArea $ \idx →
            if coordBeyond VU.! idx
            then minBound
            else lookupElev (idx `mod` chunkSize) (idx `div` chunkSize)

        -- Fluids
        oceanFluidMap = computeChunkFluid worldSize oceanMap coord terrainSurfaceMap
        features = gtFeatures timeline
        lavaFluidMap = computeChunkLava features seed plates worldSize
                                        coord terrainSurfaceMap
        lakeFluidMap = computeChunkLakes features seed plates worldSize
                                         coord terrainSurfaceMap
        -- Extract river params from events (not features) so fluid
        -- fill matches the carved terrain, which uses event segments.
        eventRivers = concatMap extractEventRivers (gtPeriods timeline)
        riverFluidMap = computeChunkRivers eventRivers worldSize
                                           coord terrainSurfaceMap

        -- Lava > River > Lake > Ocean: rivers override lakes because
        -- river channels are carved to specific elevations, while lakes
        -- fill to a uniform spillway height. Where a river runs through
        -- a lake basin, the river's lower surface is correct.
        rawFluidMap = unionFluidMap lavaFluidMap
                    $ unionFluidMap riverFluidMap
                    $ unionFluidMap lakeFluidMap oceanFluidMap

        -- Equilibrate: propagate water to adjacent empty tiles whose
        -- terrain is at or below the neighboring water surface.
        equilFluidMap = equilibrateFluidMap terrainSurfaceMap rawFluidMap

        -- Fill coastal gaps: below-sea-level terrain adjacent to any
        -- fluid that was missed by the ocean BFS (which uses pre-carving
        -- elevation) gets ocean water.
        coastalFluidMap = fillCoastalGaps terrainSurfaceMap equilFluidMap

        -- Strip lake tiles adjacent to river tiles with much lower
        -- surfaces. Must run AFTER equilibration so stripped tiles
        -- aren't refilled by lake propagation.
        fluidMap = stripLakeRiverCliffs terrainSurfaceMap coastalFluidMap


        -- Surface map with fluids (ice is a visual overlay, does not
        -- affect terrain column generation or surface-based logic)
        surfaceMap = VU.imap (\idx surfZ →
            case fluidMap V.! idx of
                Just fc → max surfZ (fcSurface fc)
                Nothing → surfZ
          ) terrainSurfaceMap

        -- Build per-column tile data directly, fusing stratigraphy
        -- computation with ColumnTiles construction to avoid an
        -- intermediate V.Vector ColumnStrata allocation.
        rawChunk = V.generate chunkArea $ \idx →
            if coordBeyond VU.! idx
            then ColumnTiles
                { ctStartZ = 0
                , ctMats   = VU.empty
                , ctSlopes = VU.empty
                , ctVeg    = VU.empty
                }
            else
                let lx = idx `mod` chunkSize
                    ly = idx `div` chunkSize
                    gx' = coordGX VU.! idx
                    gy' = coordGY VU.! idx
                    (surfZ, surfMat) = lookupFinal lx ly
                    base = lookupBase lx ly
                    -- Post-coastal neighbor elevations for determining how
                    -- far down to expose strata (cliff face visibility).
                    finalN = lookupElevOr lx (ly - 1) surfZ
                    finalS = lookupElevOr lx (ly + 1) surfZ
                    finalE = lookupElevOr (lx + 1) ly surfZ
                    finalW = lookupElevOr (lx - 1) ly surfZ
                    neighborMinZ = min finalN (min finalS (min finalE finalW))
                    exposeFromRaw = min surfZ neighborMinZ
                    exposeFrom = exposeFromRaw
                    -- Clamp neighbor elevations for the strata cache.
                    -- Coastal erosion can lower neighbors 30+ tiles below
                    -- a cliff column. The cache uses neighbors to compute
                    -- erosion at each geological period — extreme drops
                    -- cause over-erosion that produces air tiles in the
                    -- strata, which render as black voids. Clamping to
                    -- surfZ-20 limits the erosion to realistic levels.
                    -- This doesn't change ctStartZ or strata range.
                    clampN n = max (surfZ - 20) n
                    cache = buildStrataCache timeline worldSize wsc
                                             gx' gy' registry base
                                             (clampN finalN, clampN finalS
                                             , clampN finalE, clampN finalW)
                    mats = buildColumnStrata cache base exposeFrom surfZ
                    -- Correct the surface material to match the authoritative
                    -- timeline path. buildStrataCache uses final neighbor
                    -- elevations as an approximation, which can cause its
                    -- accumulated elevation to diverge from surfZ by ±1-2
                    -- tiles. This means the strata material at surfZ may be
                    -- from the wrong layer, creating a checkerboard pattern
                    -- in surface materials and vegetation.
                    surfIdx = surfZ - exposeFrom
                    correctedMats =
                        if surfIdx ≥ 0 ∧ surfIdx < VU.length mats
                        then mats VU.// [(surfIdx, surfMat)]
                        else mats
                    matIds = VU.map unMaterialId correctedMats
                in ColumnTiles
                    { ctStartZ = exposeFrom
                    , ctMats   = matIds
                    , ctSlopes = VU.replicate (VU.length matIds) 0
                    , ctVeg    = VU.replicate (VU.length matIds) 0
                    }

        noNeighborLookup ∷ ChunkCoord → Maybe (VU.Vector Int)
        noNeighborLookup _ = Nothing

        slopedTiles = computeChunkSlopes seed coord terrainSurfaceMap registry
                                         fluidMap rawChunk noNeighborLookup

        -- Extract surface material and slope for vegetation computation
        surfaceMats = VU.generate chunkArea $ \idx →
            let col = slopedTiles V.! idx
                surfZ = terrainSurfaceMap VU.! idx
                i = surfZ - ctStartZ col
            in if i ≥ 0 ∧ i < VU.length (ctMats col)
               then ctMats col VU.! i
               else 0

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

        -- Inject veg IDs into column tiles
        finalTiles = V.imap (\idx col →
            let vegId = vegIds VU.! idx
                vegVec = VU.replicate (VU.length (ctMats col)) 0
                surfZ = terrainSurfaceMap VU.! idx
                i = surfZ - ctStartZ col
                vegVec' = if i ≥ 0 ∧ i < VU.length vegVec ∧ vegId > 0
                          then vegVec VU.// [(i, vegId)]
                          else vegVec
            in col { ctVeg = vegVec' }
            ) slopedTiles

        -- Ice overlay: climate-based ice on frozen ocean/lake/alpine terrain.
        -- Computed after fluids so ice sits on top of water surfaces.
        iceMap = computeChunkIce seed (wgpClimateState params) worldSize
                                 coord terrainSurfaceMap fluidMap

    in (finalTiles, surfaceMap, terrainSurfaceMap, fluidMap, iceMap, floraData)

-- | Generate only the exposed tiles for a column.
--   Skips air tiles (MaterialId 0) to create caves and overhangs.
generateExposedColumn ∷ Int → Int → Int → Int → (Int → MaterialId)
                      → [((Int, Int, Int), Tile)]
generateExposedColumn lx ly surfaceZ exposeFrom lookupMat =
    [ ((lx, ly, z), Tile (unMaterialId mat) 0)
    | z ← [exposeFrom .. surfaceZ]
    , let mat = lookupMat z
    , mat ≠ matAir
    ]

-- | Extract RiverParams from all HydroEvents in a period's events.
--   These are the ORIGINAL traced segments that match the carved terrain.
extractEventRivers ∷ GeoPeriod → [RiverParams]
extractEventRivers period = concatMap go (gpEvents period)
  where
    go (HydroEvent (RiverFeature rp)) = [rp]
    go _ = []
