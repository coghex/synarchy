{-# LANGUAGE Strict #-}
-- | The zoom map's per-chunk generation, in the two passes the pixels
--   have always been produced in (split out of
--   "World.ZoomMap.Cache.BuildPixels" for issue #2298).
--
--   Pass one is chunk-local: terrain, fluid, ice and the summary
--   'ZoomChunkEntry' come from 'generateZoomTerrain' and nothing else.
--   Pass two is NOT: 'World.ZoomMap.Cache.OceanFill.extendOceanBoundary'
--   closes one-tile shoreline gaps by reading the NEIGHBOURING chunk's
--   pass-one fluid map, wrapped across the cylindrical seam. That is
--   why a chunk's final pixels are reproducible only together with a
--   one-chunk source halo, and why this module exposes the two passes
--   separately: the whole-world builder runs pass one over every chunk
--   and then pass two over every chunk, while the map pyramid
--   ("World.ZoomMap.Pyramid.Cells") runs pass one over a page footprint
--   PLUS its halo and pass two over the footprint alone. Both get the
--   same bytes because both run this code.
--
--   Producer side only: no renderer import, no 'LoadedChunk', no chunk
--   demand.
module World.ZoomMap.Cache.ChunkPass
    ( ZoomChunkPass(..)
    , snowVegFor
    , zoomChunkPass
    , zoomChunkPixels
    , zoomChunkHaloNeighbours
    , zoomChunkInWorld
    ) where

import UPrelude
import Control.DeepSeq (NFData(..))
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import World.Chunk.Types (ChunkCoord(..), chunkSize, wrapChunkCoordU)
import World.Constants (seaLevel)
import World.Fluid.IceLevel (lookupIceLevel)
import World.Fluid.Internal (FluidMap)
import World.Fluid.Lake.Types (wlByChunk)
import World.Fluid.Lava (chunkHasLavaQuick)
import World.Fluid.Ocean (isOceanChunk, hasAnyOceanFluid)
import World.Fluid.Types
    (FluidCell(..), FluidType(..), IceCell(..), IceMap, IceMode(..))
import World.Generate.Chunk (generateZoomTerrain)
import World.Generate.InitTerrain (BorderedTerrainCache)
import World.Generate.Types (WorldGenParams(..))
import World.Geology.Timeline.Types (GeoTimeline(..))
import World.Material (MaterialRegistry)
import World.Plate (elevationAtGlobal, isBeyondGlacier, isGlacierZone, wrapGlobalU)
import World.Vegetation (vegHash, vegSnow)
import World.Weather.Lookup (lookupLocalClimate, LocalClimate(..))
import World.ZoomMap.Cache.Classify (majorityMaterial, vegCategoryFromClimate)
import World.ZoomMap.Cache.Noise (zoomIceNoise)
import World.ZoomMap.Cache.OceanFill (extendOceanBoundary)
import World.ZoomMap.Cache.Pixels (generateChunkPixels)
import World.ZoomMap.ColorPalette (ZoomColorPalette)
import World.ZoomMap.Types (ZoomChunkEntry(..))

-- | Everything pass one produces for one chunk. 'zcpRawFluid' is the
--   COMPOSED map, before any boundary extension, which is exactly what
--   a neighbour's extension pass is allowed to read.
data ZoomChunkPass = ZoomChunkPass
    { zcpEntry    ∷ !ZoomChunkEntry
    , zcpRawFluid ∷ !FluidMap
    , zcpIceMap   ∷ !IceMap
    , zcpTiles    ∷ !(V.Vector (Int, Word8, Word8, Int, Int))
      -- ^ Per-tile @(elev, material, vegetation, gx, gy)@ with snow
      --   vegetation already injected on ice-covered tiles.
    , zcpElevs    ∷ !(VU.Vector Int)
    }

instance NFData ZoomChunkPass where
    rnf (ZoomChunkPass entry fluid ice tiles elevs) =
        rnf entry `seq` rnf fluid `seq` rnf ice `seq` rnf tiles
        `seq` rnf elevs

-- | Whether a chunk exists in this world at all.
--
--   The whole-world builder enumerates @u@ and @v@ over
--   @[-h .. h - 1]@, so a chunk is present exactly when its latitude
--   @v = ccx + ccy@ is in that range: longitude wraps and is therefore
--   always in range, latitude does not. This is the predicate the
--   halo's @Map.lookup@ used to answer implicitly by missing.
zoomChunkInWorld ∷ Int → ChunkCoord → Bool
zoomChunkInWorld worldSize (ChunkCoord ccx ccy)
    | h ≤ 0     = True
    | otherwise = let v = ccx + ccy in v ≥ (-h) ∧ v < h
  where h = ((worldSize `div` 2) * 2) `div` 2

-- | The chunks a chunk's pass-two extension can read: its four
--   cardinal neighbours in CHUNK space, u-wrapped and filtered to the
--   ones this world actually has.
zoomChunkHaloNeighbours ∷ Int → ChunkCoord → [ChunkCoord]
zoomChunkHaloNeighbours worldSize (ChunkCoord ccx ccy) =
    [ wrapped
    | (dx, dy) ← [(-1, 0), (1, 0), (0, -1), (0, 1)]
    , let wrapped = wrapChunkCoordU worldSize (ChunkCoord (ccx + dx) (ccy + dy))
    , zoomChunkInWorld worldSize wrapped ]

-- | Pass one for a single chunk.
zoomChunkPass ∷ WorldGenParams → MaterialRegistry
              → Maybe BorderedTerrainCache → ChunkCoord → ZoomChunkPass
zoomChunkPass params registry mBorderedCache coord@(ChunkCoord ccx ccy) =
    ZoomChunkPass
        { zcpEntry    = entry
        , zcpRawFluid = chunkFluidMap
        , zcpIceMap   = chunkIceMap
        , zcpTiles    = V.fromList tileDataWithIce
        , zcpElevs    = zoomElev
        }
  where
    seed = wgpSeed params
    worldSize = wgpWorldSize params
    plates = wgpPlates params
    timeline = wgpGeoTimeline params
    oceanMap = wgpOceanMap params
    climate = wgpClimateState params

    baseGX = ccx * chunkSize
    baseGY = ccy * chunkSize
    chunkOcean = isOceanChunk oceanMap coord
               ∨ hasAnyOceanFluid worldSize oceanMap coord

    -- Use the full detail-world pipeline (bordered region + timeline +
    -- coastal erosion + fluid + vegetation) for accurate terrain.
    -- Vegetation comes from the SAME per-tile function the detail world
    -- runs — the old chunk-level ocean gate stripped veg from whole dry
    -- below-sea-level chunks (solid brown diamonds on the zoom map).
    (zoomElev, zoomMat, zoomVeg, chunkFluidMap) =
        generateZoomTerrain registry params mBorderedCache coord

    tileData = [ let gx = baseGX + lx
                     gy = baseGY + ly
                     idx = ly * chunkSize + lx
                 in ( zoomElev VU.! idx
                    , zoomMat  VU.! idx
                    , zoomVeg  VU.! idx
                    , gx, gy )
               | ly ← [0 .. chunkSize - 1]
               , lx ← [0 .. chunkSize - 1]
               ]

    -- Summary stats from all tiles (for ZoomChunkEntry). Beyond-glacier
    -- tiles (minBound elevation) are filtered out to prevent overflow
    -- in avgElev and material contamination.
    allMats = [ (e, m) | (e, m, _, _, _) ← tileData, e > minBound ]
    winnerMat = majorityMaterial allMats
    avgElev = if null allMats then 0
              else let s = sum (map fst allMats)
                   in s `div` length allMats
    chunkLava = chunkHasLavaQuick (wgpVolcanoCtx params) coord avgElev
              -- Pool lava can spread into chunks with no breach of
              -- their own — the global pool table is authoritative.
              ∨ HM.member coord (wlByChunk (gtWorldLavaPools timeline))
    vegCat = if chunkOcean ∨ winnerMat ≡ 250
             then 0
             else vegCategoryFromClimate climate worldSize
                      baseGX baseGY winnerMat

    -- Ice check at chunk center
    centerGX' = baseGX + chunkSize `div` 2
    centerGY' = baseGY + chunkSize `div` 2
    (cgx', cgy') = wrapGlobalU worldSize centerGX' centerGY'
    LocalClimate{lcTemp=cmt', lcSummerTemp=cst', lcWinterTemp=cwt'} =
        lookupLocalClimate climate worldSize cgx' cgy'
    -- Plate elevation at chunk center (globally deterministic) instead
    -- of avgElev, which includes erosion and varies per chunk.
    (centerElev', _) = elevationAtGlobal seed plates worldSize cgx' cgy'
    altAboveSea' = max 0 (centerElev' - seaLevel)
    altCool' = fromIntegral altAboveSea' * (0.065 ∷ Float)
    ocnPen' = if centerElev' < seaLevel then 5.0 else 0.0 ∷ Float
    iceNoise' = zoomIceNoise seed cgx' cgy'
    effT' = cmt' + ocnPen' - altCool' + iceNoise'
    chunkIce' = not (isBeyondGlacier worldSize cgx' cgy')
              ∧ (isGlacierZone worldSize cgx' cgy'
                 ∨ effT' < -2.0
                 ∨ (cwt' - altCool' < -10.0 ∧ cst' - altCool' < 5.0))

    entry = ZoomChunkEntry
        { zceChunkX = ccx
        , zceChunkY = ccy
        , zceBaseGX = baseGX
        , zceBaseGY = baseGY
        , zceTexIndex = winnerMat
        , zceElev     = avgElev
        , zceIsOcean  = chunkOcean
        , zceHasLava  = chunkLava
        , zceVegCategory = vegCat
        , zceHasIce  = chunkIce'
        }

    -- Ice overlay: per-tile decision using continuous noise that
    -- doesn't break at chunk boundaries. Uses the global ice level grid
    -- for the basin/drape decision.
    ilGrid = gtIceLevel timeline
    chunkIceMap = V.fromList
        [ let (e, _, _, gx, gy) = td
              (gx', gy') = wrapGlobalU worldSize gx gy
              LocalClimate{lcTemp=mt, lcSummerTemp=st, lcWinterTemp=wt} =
                  lookupLocalClimate climate worldSize gx' gy'
              (globalElev, _) = elevationAtGlobal seed plates worldSize gx' gy'
              altAboveSea = max 0 (globalElev - seaLevel)
              altCool = fromIntegral altAboveSea * (0.065 ∷ Float)
              ocnPen = if globalElev < seaLevel then 5.0 else 0.0 ∷ Float
              n = zoomIceNoise seed gx' gy'
              effT = mt + ocnPen - altCool + n
              ice = not (isBeyondGlacier worldSize gx' gy')
                  ∧ (isGlacierZone worldSize gx' gy'
                     ∨ effT < -2.0
                     ∨ (wt - altCool < -10.0 ∧ st - altCool < 5.0))
              mIceLevel = lookupIceLevel ilGrid worldSize gx' gy'
          in if ice ∧ e > minBound
             then case mIceLevel of
                Just iceLevel | e < iceLevel →
                    Just (IceCell (min iceLevel (e + 20)) BasinIce)
                _ → Just (IceCell (e + 1) DrapeIce)
             else Nothing
        | (td, _idx') ← zip tileData [0 ∷ Int ..]
        ]

    -- Inject snow veg on ice-covered tiles
    tileDataWithIce = zipWith (\idx' td →
        case chunkIceMap V.! idx' of
            Just _ → let (e, m, _, gx, gy) = td
                     in (e, m, snowVegFor seed gx gy, gx, gy)
            Nothing → td
        ) [0 ∷ Int ..] tileData

-- | The snow vegetation id an ICE-COVERED tile is coloured through.
--
--   'World.ZoomMap.Cache.Pixels.generateChunkPixels' has no ice branch of
--   its own — its @hasIce@ flag only suppresses the fluid tint — so ice
--   reaches the pixels ONLY as this vegetation id. Anything that
--   rebuilds a tile's tuple therefore has to reproduce it, or an iced
--   cell comes out as bare material: 'World.ZoomMap.Live' does exactly
--   that for every cell a live edit overrode, which is why this is
--   shared rather than inlined.
snowVegFor ∷ Word64 → Int → Int → Word8
snowVegFor seed gx gy =
    let h   = vegHash seed gx gy
        var = fromIntegral ((h `shiftR` 8) ⌃ 0x03) ∷ Word8
    in vegSnow + var

-- | Pass two for a single chunk: extend ocean across chunk boundaries
--   using the neighbours' pass-one fluid maps, then render.
--
--   The halo lookup is given as a function so the caller decides what
--   "neighbour" means for its own footprint. A neighbour the lookup
--   does not know answers dry, which is exactly how the whole-world
--   builder's missing map key has always behaved at the latitude edge.
zoomChunkPixels ∷ ZoomColorPalette → Int
                → (ChunkCoord → Maybe FluidMap)
                → ChunkCoord → ZoomChunkPass → BS.ByteString
zoomChunkPixels palette worldSize haloFluid (ChunkCoord ccx ccy) pass =
    generateChunkPixels palette (zceHasLava (zcpEntry pass)) worldSize
        extendedFluid (zcpIceMap pass) (zcpTiles pass)
  where
    wrapC = wrapChunkCoordU worldSize

    -- Check whether the neighbouring chunk has ocean at a tile just
    -- outside this chunk. One axis is out of range; the other is not.
    neighborHasOcean nx ny =
        let ncx = if ny < 0 then ccx else if ny ≥ chunkSize then ccx else ccx + (if nx < 0 then -1 else if nx ≥ chunkSize then 1 else 0)
            ncy = if nx < 0 ∨ nx ≥ chunkSize then ccy else if ny < 0 then ccy - 1 else if ny ≥ chunkSize then ccy + 1 else ccy
            nlx = if nx < 0 then chunkSize - 1 else if nx ≥ chunkSize then 0 else nx
            nly = if ny < 0 then chunkSize - 1 else if ny ≥ chunkSize then 0 else ny
            ncoord = wrapC (ChunkCoord ncx ncy)
        in case haloFluid ncoord of
            Just nfm → case nfm V.! (nly * chunkSize + nlx) of
                Just (FluidCell Ocean _) → True
                _ → False
            Nothing → False

    -- ONE cardinal dilation of the composed ocean mask, read from the
    -- immutable composed map — no other fluid kind seeds it and no
    -- synthesized cell seeds another, so the result does not depend on
    -- scan order (#2316).
    extendedFluid =
        extendOceanBoundary neighborHasOcean (zcpElevs pass) (zcpRawFluid pass)
