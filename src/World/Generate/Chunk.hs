{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Generate.Chunk
    ( generateChunk
    , generateExposedColumn
    , generateZoomTerrain
    ) where

import UPrelude
import Data.Bits (shiftR, (.&.))
import Control.Monad (when, forM_)
import Control.Monad.ST (runST)
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import World.Types
import World.Material (MaterialId(..), matGlacier, getMaterialProps
                      , MaterialProps(..), matAir, MaterialRegistry(..))
import World.Plate (TectonicPlate(..), generatePlates
                   , elevationAtGlobal, isBeyondGlacier, wrapGlobalU)
import World.Geology (applyGeoEvent, GeoModification(..))
import World.Geology.Timeline.Types (GeoEvent(..), GeoTimeline(..))
import World.Constants (seaLevel)
import World.Scale (computeWorldScale, WorldScale(..))
import World.Slope (computeChunkSlopes)
import World.Fluids (hasAnyOceanFluid)
import World.Ocean.Types (oceanDistAt)
import World.Fluid.Internal (emptyFluidMap)
import World.Fluid.Types (FluidCell(..), FluidType(..))
import World.Weather.Lookup (lookupWaterTable)
import World.Fluid.Types (emptyIceMap)
import World.River.Types (RiverMask, emptyRiverMask)
import World.Vegetation (computeChunkVegetation, vegSnow, vegHash)
import World.Flora.Placement (computeChunkFlora)
import World.Generate.Constants (chunkBorder)
import World.Generate.Coordinates (chunkToGlobal)
import World.Generate.Timeline (applyTimelineChunk, removeElevationSpikes)
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
                 , V.Vector (Maybe FluidCell), IceMap, FloraChunkData
                 , RiverMask)  -- ^ rich river mask (surface + type per tile)
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
        isCoastalChunk = hasAnyOceanFluid worldSize oceanMap coord

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

        -- Post-timeline coastal erosion: lower coastal terrain,
        -- deposit sand/gravel/wetland materials based on plate tectonics
        -- and river mouth proximity
        (postCoastElev, finalMatVec) =
            applyCoastalErosion seed worldSize plates registry timeline oceanMap coord
                (timelineElevVec, timelineMatVec)

        -- Despike: remove single-tile elevation outliers that survived
        -- timeline events and coastal erosion. These are typically
        -- 1-tile-wide diagonal mountain ridges (features aligned with
        -- the u-v isometric axes that end up 1 tile wide in xy) where
        -- coastal erosion lowered the cardinal neighbors but the spike
        -- itself was outside the coastal range.
        (finalElevVec, _) = removeElevationSpikes 12 4 (chunkSize + 2 * chunkBorder)
                                                  (postCoastElev, finalMatVec)

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

        -- Fluids: water level approach. Water appears where terrain
        -- is below the local water level.
        --
        -- Water level per tile: bilinearly interpolated from
        -- ocean distances of this chunk and its 4 neighbors.
        -- This eliminates the zig-zag at chunk boundaries by
        -- smoothly blending the water level across edges.
        oceanDist = wgpOceanDist params
        ChunkCoord cx cy = coord
        capDist d = if d > 100 then -1.0 else fromIntegral d ∷ Float
        -- Distances for this chunk and its cardinal neighbors.
        -- Use the chunk center as the "sample point" for interpolation.
        d00 = capDist (oceanDistAt oceanDist (ChunkCoord cx cy))
        d10 = capDist (oceanDistAt oceanDist (ChunkCoord (cx+1) cy))
        d01 = capDist (oceanDistAt oceanDist (ChunkCoord cx (cy+1)))
        d11 = capDist (oceanDistAt oceanDist (ChunkCoord (cx+1) (cy+1)))
        -- Also need the other two corners for full bilinear
        dN0 = capDist (oceanDistAt oceanDist (ChunkCoord (cx-1) cy))
        d0N = capDist (oceanDistAt oceanDist (ChunkCoord cx (cy-1)))

        gradient = 0.5 ∷ Float

        finalTerrain = terrainSurfaceMap
        fluidMap = V.generate chunkArea $ \idx →
            let terrZ = terrainSurfaceMap VU.! idx
                lx = idx `mod` chunkSize
                ly = idx `div` chunkSize
                -- Fractional position within chunk (0.0 = left/top edge,
                -- 1.0 = right/bottom edge). Offset by 0.5 so chunk
                -- center is at 0.5.
                tx = (fromIntegral lx + 0.5) / fromIntegral chunkSize ∷ Float
                ty = (fromIntegral ly + 0.5) / fromIntegral chunkSize ∷ Float
                -- Bilinear interpolation of ocean distance.
                -- Use (d00, d10, d01, d11) for the quadrant
                -- where tx,ty > 0.5, and the left/top neighbors
                -- for tx,ty < 0.5.
                (da, db, dc, dd, sx, sy) =
                    if tx ≥ 0.5 ∧ ty ≥ 0.5
                    then (d00, d10, d01, d11,
                          tx - 0.5, ty - 0.5)
                    else if tx < 0.5 ∧ ty ≥ 0.5
                    then (dN0, d00, capDist (oceanDistAt oceanDist (ChunkCoord (cx-1) (cy+1))), d01,
                          tx + 0.5, ty - 0.5)
                    else if tx ≥ 0.5 ∧ ty < 0.5
                    then (d0N, capDist (oceanDistAt oceanDist (ChunkCoord (cx+1) (cy-1))), d00, d10,
                          tx - 0.5, ty + 0.5)
                    else (capDist (oceanDistAt oceanDist (ChunkCoord (cx-1) (cy-1))), d0N, dN0, d00,
                          tx + 0.5, ty + 0.5)
                -- If any corner is unreachable (-1), use nearest valid
                validCorners = filter (≥ 0) [da, db, dc, dd]
                fallback = if null validCorners then -1.0
                           else sum validCorners / fromIntegral (length validCorners)
                sa = if da < 0 then fallback else da
                sb = if db < 0 then fallback else db
                sc = if dc < 0 then fallback else dc
                sd = if dd < 0 then fallback else dd
                -- Bilinear lerp
                topRow  = sa + sx * (sb - sa)
                botRow  = sc + sx * (sd - sc)
                interpDist = topRow + sy * (botRow - topRow)
                waterLevel = if interpDist < 0
                             then minBound
                             else round (fromIntegral seaLevel
                                       + interpDist * gradient)
            in if terrZ > minBound ∧ waterLevel > terrZ
               then Just (FluidCell Ocean waterLevel)
               else Nothing
        riverMask = emptyRiverMask


        -- Surface map with fluids (ice is a visual overlay, does not
        -- affect terrain column generation or surface-based logic)
        surfaceMap = VU.imap (\idx surfZ →
            case fluidMap V.! idx of
                Just fc → max surfZ (fcSurface fc)
                Nothing → surfZ
          ) finalTerrain

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
                        fluidMap riverMask (wgpClimateState params) catalog

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

        -- Ice overlay: disabled (fluid placement removed).
        iceMap = emptyIceMap

    in (finalTiles, surfaceMap, finalTerrain, fluidMap, iceMap, floraData, riverMask)

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


-- * Zoom-Optimized Terrain Generation

-- | Generate terrain + fluid for the zoom cache using the same pipeline
--   as the detail world (bordered region + full timeline + coastal erosion).
--   Skips strata, slopes, vegetation, and flora — the zoom cache computes
--   those itself.
--
--   Returns (elevation, materialId, fluidMap) for the 16×16 interior.
generateZoomTerrain ∷ MaterialRegistry → WorldGenParams → ChunkCoord
  → (VU.Vector Int, VU.Vector Word8, V.Vector (Maybe FluidCell))
generateZoomTerrain registry params coord =
    let seed      = wgpSeed params
        worldSize = wgpWorldSize params
        timeline  = wgpGeoTimeline params
        plates    = wgpPlates params
        oceanMap  = wgpOceanMap params
        wsc       = computeWorldScale worldSize

        borderSize = chunkSize + 2 * chunkBorder
        borderArea = borderSize * borderSize

        fromIndex idx =
            let (by, bx) = idx `divMod` borderSize
            in (bx - chunkBorder, by - chunkBorder)

        inBorder lx ly =
            lx ≥ negate chunkBorder ∧ lx < chunkSize + chunkBorder ∧
            ly ≥ negate chunkBorder ∧ ly < chunkSize + chunkBorder

        toIndex lx ly =
            let bx = lx + chunkBorder
                by = ly + chunkBorder
            in by * borderSize + bx

        -- Base elevation + material (bordered region, same as generateChunk)
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

        -- Full timeline with neighborhood-aware erosion (same as detail)
        (timelineElevVec, timelineMatVec) =
            applyTimelineChunk timeline worldSize registry wsc coord
                (baseElevVec, baseMatVec)

        -- Coastal erosion with contour smoothing (same as detail)
        (finalElevVec, finalMatVec) =
            applyCoastalErosion seed worldSize plates registry timeline
                oceanMap coord (timelineElevVec, timelineMatVec)

        -- Extract interior 16×16 from bordered region
        chunkArea = chunkSize * chunkSize
        interiorElev = VU.generate chunkArea $ \idx →
            let lx = idx `mod` chunkSize
                ly = idx `div` chunkSize
                (gx', gy') = wrapGlobalU worldSize
                    (fst (chunkToGlobal coord lx ly))
                    (snd (chunkToGlobal coord lx ly))
            in if isBeyondGlacier worldSize gx' gy'
               then minBound
               else if inBorder lx ly
                    then finalElevVec VU.! toIndex lx ly
                    else 0
        interiorMat = VU.generate chunkArea $ \idx →
            let lx = idx `mod` chunkSize
                ly = idx `div` chunkSize
            in if inBorder lx ly
               then unMaterialId (finalMatVec VU.! toIndex lx ly)
               else 1

        -- Fluid: same water table as detail world, with bilinear
        -- interpolation for smooth transitions.
        oceanDist' = wgpOceanDist params
        ChunkCoord zcx zcy = coord
        zcapDist d = if d > 100 then -1.0 else fromIntegral d ∷ Float
        zd00 = zcapDist (oceanDistAt oceanDist' (ChunkCoord zcx zcy))
        zd10 = zcapDist (oceanDistAt oceanDist' (ChunkCoord (zcx+1) zcy))
        zd01 = zcapDist (oceanDistAt oceanDist' (ChunkCoord zcx (zcy+1)))
        zd11 = zcapDist (oceanDistAt oceanDist' (ChunkCoord (zcx+1) (zcy+1)))
        zdN0 = zcapDist (oceanDistAt oceanDist' (ChunkCoord (zcx-1) zcy))
        zd0N = zcapDist (oceanDistAt oceanDist' (ChunkCoord zcx (zcy-1)))
        zGrad = 0.5 ∷ Float
        zoomFluid = V.generate chunkArea $ \idx →
            let terrZ = interiorElev VU.! idx
                lx = idx `mod` chunkSize
                ly = idx `div` chunkSize
                tx = (fromIntegral lx + 0.5) / fromIntegral chunkSize ∷ Float
                ty = (fromIntegral ly + 0.5) / fromIntegral chunkSize ∷ Float
                (za, zb, zc, zd, zsx, zsy) =
                    if tx ≥ 0.5 ∧ ty ≥ 0.5
                    then (zd00, zd10, zd01, zd11, tx - 0.5, ty - 0.5)
                    else if tx < 0.5 ∧ ty ≥ 0.5
                    then (zdN0, zd00, zcapDist (oceanDistAt oceanDist' (ChunkCoord (zcx-1) (zcy+1))), zd01, tx + 0.5, ty - 0.5)
                    else if tx ≥ 0.5 ∧ ty < 0.5
                    then (zd0N, zcapDist (oceanDistAt oceanDist' (ChunkCoord (zcx+1) (zcy-1))), zd00, zd10, tx - 0.5, ty + 0.5)
                    else (zcapDist (oceanDistAt oceanDist' (ChunkCoord (zcx-1) (zcy-1))), zd0N, zdN0, zd00, tx + 0.5, ty + 0.5)
                zvalidCorners = filter (≥ 0) [za, zb, zc, zd]
                zfallback = if null zvalidCorners then -1.0
                            else sum zvalidCorners / fromIntegral (length zvalidCorners)
                zsa = if za < 0 then zfallback else za
                zsb = if zb < 0 then zfallback else zb
                zsc = if zc < 0 then zfallback else zc
                zsd = if zd < 0 then zfallback else zd
                ztopRow = zsa + zsx * (zsb - zsa)
                zbotRow = zsc + zsx * (zsd - zsc)
                zinterp = ztopRow + zsy * (zbotRow - ztopRow)
                zwl = if zinterp < 0 then minBound
                      else round (fromIntegral seaLevel + zinterp * zGrad)
            in if terrZ > minBound ∧ zwl > terrZ
               then Just (FluidCell Ocean zwl)
               else Nothing

    in (interiorElev, interiorMat, zoomFluid)
