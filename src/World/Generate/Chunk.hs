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
import World.Fluid.Internal (emptyFluidMap, preferFluidMap
                            , wrapChunkCoordU, wrappedDeltaUVFluid)
import World.Fluid.Types (FluidCell(..), FluidType(..))
import World.Fluid.River (riverNearChunk)
import World.Fluid.Lava (computeChunkLava)
import World.Fluid.Ice (computeChunkIce)
import World.Hydrology.Types (HydroFeature(..), RiverParams(..)
                             , RiverSegment(..))
import World.Geology.Timeline.Types (GeoEvent(..), GeoPeriod(..))
import World.Hydrology.WaterTable (computeWaterTable)
import World.Fluid.Types (emptyIceMap)
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

-- * Fluid composition — water-table-driven (Phase B of river rework)
--
-- For each tile we read the precomputed water-table elevation and place
-- surface fluid where wt ≥ terrain. The type is decided by geometry, not
-- by which subsystem "owns" the tile:
--   * Ocean  if the chunk is ocean-BFS-reachable and the tile sits at
--            or below seaLevel (terrain ≤ seaLevel ∧ wt ≤ seaLevel + 0)
--   * River  if the tile is inside any river-segment channel mask
--   * Lake   otherwise — depression filled by water-table propagation
--
-- Lava is computed by the volcanic feature pipeline (unchanged) and
-- layered on top with leftmost-wins, preserving the
-- "lava always overrides other fluids" invariant the user called out.
--
-- See src/World/Hydrology/DESIGN.md for the architecture.
composeFluidMap ∷ WorldGenParams → ChunkCoord → VU.Vector Int
                → VU.Vector Int → VU.Vector Int
                → V.Vector (Maybe FluidCell)
composeFluidMap params coord terrainMap waterTableMap channelMask =
    let seed       = wgpSeed params
        worldSize  = wgpWorldSize params
        plates     = wgpPlates params
        timeline   = wgpGeoTimeline params
        oceanDist  = wgpOceanDist params
        chunkArea  = chunkSize * chunkSize

        -- Ocean classification per chunk: a tile classifies as Ocean
        -- only when its chunk is BFS-reachable from the world-edge
        -- ocean. Inland depressions below seaLevel (Caspian-style) are
        -- Lake even though their wt happens to equal seaLevel.
        chunkIsOceanic =
            oceanDistAt oceanDist
                (wrapChunkCoordU worldSize coord) < maxBound

        -- Water-table-driven fluid placement.
        waterFluid = V.generate chunkArea $ \idx →
            let terrZ = terrainMap VU.! idx
                wtZ   = waterTableMap VU.! idx
            in if wtZ < terrZ ∨ terrZ ≡ minBound
               then Nothing
               else
                 let ftype
                       | terrZ ≤ seaLevel ∧ chunkIsOceanic = Ocean
                       | channelMask VU.! idx ≠ noChannel  = River
                       | otherwise                         = Lake
                 in Just (FluidCell ftype wtZ)

        features = gtFeatures timeline
        lavaFluid = computeChunkLava features seed plates
                                     worldSize coord terrainMap

    in lavaFluid `preferFluidMap` waterFluid

-- | Sentinel for "this tile is not in any channel" in the channel-floor
--   map. Picked so it's an obviously-impossible elevation and easy to
--   detect with @≡ noChannel@.
noChannel ∷ Int
noChannel = maxBound

-- | Slice the chunk interior out of a bordered-region channel-floor map.
sliceInteriorMask ∷ VU.Vector Int → VU.Vector Int
sliceInteriorMask bordered =
    let bSize = chunkSize + 2 * chunkBorder
    in VU.generate (chunkSize * chunkSize) $ \i →
        let lx = i `mod` chunkSize
            ly = i `div` chunkSize
            bidx = (ly + chunkBorder) * bSize + (lx + chunkBorder)
        in bordered VU.! bidx

-- | Bordered channel-floor map: for each tile in the bordered region,
--   the segment-interpolated channel-floor elevation (= the elevation
--   the water surface should sit at), or @noChannel@ if no segment
--   covers this tile.
--
--   This is what gets pinned in the water-table compute and what
--   classifies tiles as river in the fluid composer. By storing the
--   channel-floor (not just a bool), the water table at every tile in
--   a channel cross-section is the SAME value — the river surface is
--   flat across its width even though terrain slopes up the banks.
--
--   Terrain-aware: only includes tiles whose actual terrain is at or
--   below the segment's reference elevation (with rsDepth+4 lower
--   tolerance, more for coastal segments). This excludes valley walls
--   and ridges that sit perpendicularly close to a segment centerline.
--
--   When multiple segments overlap a tile (confluence / braided rivers),
--   the LOWEST channel-floor wins — that's the deeper water, and
--   propagating that downstream produces continuous surfaces at junctions.
computeBorderedChannelMask ∷ Int → ChunkCoord → [RiverParams]
                           → VU.Vector Int → VU.Vector Int
computeBorderedChannelMask worldSize coord rivers borderedTerrain =
    let ChunkCoord cx cy = coord
        bSize = chunkSize + 2 * chunkBorder
        bArea = bSize * bSize
        chunkMinGX = cx * chunkSize
        chunkMinGY = cy * chunkSize
        nearby = filter (riverNearChunk worldSize chunkMinGX chunkMinGY) rivers
        nearbySegs = concatMap (V.toList . rpSegments) nearby

        floorFromSeg gx gy terrainHere seg =
            case tileInChannelMask worldSize gx gy terrainHere seg of
                Nothing → noChannel
                Just f  → f

        minFloor a b = if b < a then b else a

    in VU.generate bArea $ \idx →
        let bx = idx `mod` bSize
            by = idx `div` bSize
            gx = chunkMinGX + bx - chunkBorder
            gy = chunkMinGY + by - chunkBorder
            terrainHere = borderedTerrain VU.! idx
        in foldl' (\acc seg → minFloor acc
                              (floorFromSeg gx gy terrainHere seg))
                  noChannel nearbySegs

-- | Membership test for a tile against one river segment. Returns the
--   interpolated channel-floor elevation if the tile is in the mask,
--   or @Nothing@ otherwise.
--
--   Cuts:
--     1. Longitudinal: tile must project within the segment, with a
--        small overshoot at endpoints to prevent gaps. Coastal segments
--        (ending near sea level) get a larger downstream overshoot to
--        reach into ocean across coastal-erosion-lowered tiles.
--     2. Horizontal: perpendicular distance ≤ rsWidth (or 2× for coastal).
--     3. Vertical: tile terrain ≤ refElev and ≥ refElev − maxFillDepth.
tileInChannelMask ∷ Int → Int → Int → Int → RiverSegment → Maybe Int
tileInChannelMask worldSize gx gy terrainHere seg =
    let GeoCoord sx sy = rsStart seg
        GeoCoord ex ey = rsEnd seg
        (dxi, dyi) = wrappedDeltaUVFluid worldSize sx sy ex ey
        dx' = fromIntegral dxi ∷ Float
        dy' = fromIntegral dyi ∷ Float
        segLen2 = dx' * dx' + dy' * dy'
    in if segLen2 < 1.0
       then Nothing
       else
         let segLen = sqrt segLen2
             isCoastalSeg = rsEndElev seg ≤ seaLevel + 5
             upstreamOver   = 0.05
             downstreamOver = if isCoastalSeg
                              then min 2.0 (12.0 / segLen)
                              else 0.05
             (pxi, pyi) = wrappedDeltaUVFluid worldSize sx sy gx gy
             px = fromIntegral pxi ∷ Float
             py = fromIntegral pyi ∷ Float
             tRaw = (px * dx' + py * dy') / segLen2
         in if tRaw < negate upstreamOver ∨ tRaw > 1.0 + downstreamOver
            then Nothing
            else
              let perpDist = abs ((px * dy' - py * dx') / segLen)
                  baseHalf = fromIntegral (rsWidth seg) ∷ Float
                  -- Clamp mask reach to the carved valley extent. If
                  -- the mask reaches further than carving (rsValleyWidth/2),
                  -- tiles get classified as river but their terrain
                  -- wasn't lowered — producing dry "columns" sticking
                  -- up through the river surface. Coastal segments get
                  -- 2× width but still capped by the carved valley.
                  carveHalf = fromIntegral (rsValleyWidth seg) / 2.0 ∷ Float
                  rawMaskHalf = if isCoastalSeg then baseHalf * 2.0 else baseHalf
                  maskHalf = min rawMaskHalf carveHalf
                  tClamped = max 0.0 (min 1.0 tRaw)
                  startE = fromIntegral (rsStartElev seg) ∷ Float
                  endE   = fromIntegral (rsEndElev seg)   ∷ Float
                  refElev = floor (startE + tClamped * (endE - startE)) ∷ Int
                  maxFillDepth = rsDepth seg + (if isCoastalSeg then 12 else 4)
                  -- Channel-floor at this point along the segment. This
                  -- is the value water sits at, regardless of how far
                  -- across the cross-section this particular tile is.
                  channelFloor = max (seaLevel - 1) (refElev - rsDepth seg)
              in if perpDist ≤ maskHalf
                  ∧ terrainHere ≤ refElev
                  ∧ terrainHere ≥ refElev - maxFillDepth
                 then Just channelFloor
                 else Nothing

-- | Merge a river-flat surface rule into surface-map computation.
-- For River fluid, surface = fluid surface (hides terrain protrusions).
-- For other fluid types, surface = max(terrain, fluid).
-- Same rule lives in Sim/Thread.hs and the chunk-load seeding paths.
mkSurfaceMap ∷ VU.Vector Int → V.Vector (Maybe FluidCell) → VU.Vector Int
mkSurfaceMap terrain fluid =
    VU.imap (\idx surfZ →
        case fluid V.! idx of
            Just fc | fcType fc ≡ River → fcSurface fc
            Just fc                     → max surfZ (fcSurface fc)
            Nothing                     → surfZ
      ) terrain

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
                 , VU.Vector Int)
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

        finalTerrain = terrainSurfaceMap

        -- Channel mask: where any river segment passes through this
        -- chunk (bordered region for the wt compute, interior region
        -- for fluid classification). Computed once, sliced for both.
        eventRivers ∷ [RiverParams]
        eventRivers = concatMap goP (gtPeriods timeline)
          where
            goP p = concatMap goE (gpEvents p)
            goE (HydroEvent (RiverFeature rp)) = [rp]
            goE _                              = []
        borderedChannelMask = computeBorderedChannelMask worldSize coord
                                                         eventRivers
                                                         finalElevVec
        interiorChannelMask = sliceInteriorMask borderedChannelMask

        -- All fluid placement — water-table-driven plus lava.
        -- Shared with generateZoomTerrain so the two views agree about
        -- which tiles are water.
        fluidMap = composeFluidMap params coord terrainSurfaceMap
                                   waterTableMap interiorChannelMask

        -- Surface map: rivers render flat (water surface hides any
        -- minor terrain protrusions in the carved channel); other
        -- fluids use max(terrain, water).
        surfaceMap = mkSurfaceMap finalTerrain fluidMap

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

        -- Ice overlay: computed from climate, altitude, and the global
        -- ice level grid (pre-computed at world init in gtIceLevel).
        -- Sits on top of terrain or fluid for frozen ocean/lake.
        iceMap = computeChunkIce seed plates (wgpClimateState params)
                                 worldSize coord (gtIceLevel timeline)
                                 terrainSurfaceMap fluidMap

        -- Water table: climate gives the base depth, oceans + channels
        -- + sinks pin specific tiles, then flow-direction propagation
        -- lifts the rest. Computed over the bordered region; only the
        -- chunk interior is returned. See src/World/Hydrology/DESIGN.md.
        waterTableMap = computeWaterTable (wgpClimateState params)
                                          worldSize coord finalElevVec
                                          borderedChannelMask

    in (finalTiles, surfaceMap, finalTerrain, fluidMap, iceMap, floraData
       , waterTableMap)

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
        (postCoastElev, finalMatVec) =
            applyCoastalErosion seed worldSize plates registry timeline
                oceanMap coord (timelineElevVec, timelineMatVec)

        -- Despike: same as detail world, so single-tile elevation
        -- outliers don't differ between the two views.
        (finalElevVec, _) =
            removeElevationSpikes 12 4 (chunkSize + 2 * chunkBorder)
                                  (postCoastElev, finalMatVec)

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

        -- All fluid placement — same composeFluidMap as the detail
        -- world, so the zoom map shows rivers, lakes, and lava in the
        -- same tiles where the world view shows them. Water table and
        -- channel mask are recomputed from the zoom path's bordered
        -- terrain so the two views see identical wt (deterministic).
        zoomEventRivers ∷ [RiverParams]
        zoomEventRivers = concatMap goP (gtPeriods timeline)
          where
            goP p = concatMap goE (gpEvents p)
            goE (HydroEvent (RiverFeature rp)) = [rp]
            goE _                              = []
        zoomBorderedMask = computeBorderedChannelMask worldSize coord
                                                      zoomEventRivers
                                                      finalElevVec
        zoomInteriorMask = sliceInteriorMask zoomBorderedMask
        zoomWaterTable = computeWaterTable (wgpClimateState params)
                                           worldSize coord finalElevVec
                                           zoomBorderedMask
        zoomFluid = composeFluidMap params coord interiorElev
                                    zoomWaterTable zoomInteriorMask

    in (interiorElev, interiorMat, zoomFluid)
