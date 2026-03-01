{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Flora.Placement
    ( computeChunkFlora
    ) where

import UPrelude
import Data.Bits (xor, shiftR, (.&.))
import Data.Word (Word8, Word16, Word64)
import Data.List (sortOn)
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import World.Types
import World.Chunk.Types (ChunkCoord(..), chunkSize)
import World.Fluid.Types (FluidCell(..))
import World.Vegetation (isBarrenMaterial)
import World.Weather.Types (ClimateState(..), ClimateGrid(..)
                           , RegionClimate(..), SeasonalClimate(..)
                           , ClimateCoord(..), climateRegionSize)
import World.Flora.Types

-----------------------------------------------------------
-- Chunk Flora Computation
-----------------------------------------------------------

-- | Place flora instances for an entire chunk.
--   Pure, deterministic, runs after vegetation groundcover
--   is computed.
--
--   For each tile, hashes to decide whether to place flora,
--   then checks each worldgen-registered species against the
--   local biome conditions. If a species fits, places one or
--   more instances depending on category (trees get 1 centered,
--   small plants get 2-4 scattered).
computeChunkFlora
    ∷ Word64                     -- ^ world seed
    → Int                        -- ^ worldSize
    → ChunkCoord                 -- ^ chunk coordinate
    → VU.Vector Int              -- ^ terrainSurfaceMap
    → VU.Vector Word8            -- ^ surface material IDs
    → VU.Vector Word8            -- ^ slope IDs
    → V.Vector (Maybe FluidCell) -- ^ fluid map
    → ClimateState               -- ^ climate for regional lookup
    → FloraCatalog               -- ^ registered flora species
    → FloraChunkData
computeChunkFlora seed worldSize coord surfMap surfMats surfSlopes
                  fluidMap climate catalog =
    let ChunkCoord cx cy = coord
        chunkArea = chunkSize * chunkSize
        wgSpecies = worldGenSpecies catalog

        allInstances = concatMap (\idx →
            let lx = idx `mod` chunkSize
                ly = idx `div` chunkSize
                gx = cx * chunkSize + lx
                gy = cy * chunkSize + ly
                matId    = surfMats   VU.! idx
                slopeId  = surfSlopes VU.! idx
                surfZ    = surfMap    VU.! idx
                hasFluid = case fluidMap V.! idx of
                    Just _  → True
                    Nothing → False
                (temp, precip, _, _) =
                    lookupLocalClimate climate worldSize gx gy
            in if hasFluid ∨ isBarrenMaterial matId ∨ surfZ ≡ minBound
               then []
               else placeTileFlora seed gx gy lx ly surfZ
                        matId slopeId temp precip wgSpecies
                        surfMap chunkSize
            ) [0 .. chunkArea - 1]

    in FloraChunkData allInstances

-- | Check that all tiles within a footprint radius have similar elevation.
--   "Similar" means no neighbor drops more than maxDrop below the center tile.
--   Only checks tiles within the chunk (border tiles conservatively pass).
footprintSafe ∷ Int → Int → Int → Int → VU.Vector Int → Int → Bool
footprintSafe lx ly surfZ radius surfMap chunkSz
    | radius ≤ 0 = True
    | otherwise  =
        let maxDrop = 3  -- max Z difference before we call it a cliff
        in all (\(nx, ny) →
            if nx < 0 ∨ nx >= chunkSz ∨ ny < 0 ∨ ny >= chunkSz
            then True  -- out of chunk, conservatively allow
            else let nIdx = ny * chunkSz + nx
                     nZ = surfMap VU.! nIdx
                 in surfZ - nZ ≤ maxDrop
            ) [ (lx + dx, ly + dy)
              | dx ← [negate radius .. radius]
              , dy ← [negate radius .. radius]
              , dx /= 0 ∨ dy /= 0
              ]

-----------------------------------------------------------
-- Per-Tile Placement
-----------------------------------------------------------

-- | Try to place flora on a single tile.
--   Iterates worldgen species sorted by density (highest first)
--   so dominant species claim tiles before rare ones.
--   Only one species wins per tile — first match takes it.
placeTileFlora
    ∷ Word64 → Int → Int → Int → Int → Int
    → Word8 → Word8 → Float → Float
    → [(FloraId, FloraWorldGen)]
    → VU.Vector Int → Int
    → [FloraInstance]
placeTileFlora seed gx gy lx ly surfZ matId slopeId temp precip wgSpecies
               surfMap chunkSz =
    let h0 = floraHash seed gx gy 0
        tileRoll = fromIntegral (h0 .&. 0xFFFF) / 65535.0 ∷ Float

        sorted = sortOn (negate . fwDensity . snd) wgSpecies
        match = firstMatch sorted
    in case match of
        Nothing → []
        Just (fid, wg) →
            let cat  = fwCategory wg
                baseW = fwFootprint wg
                count = instanceCount cat h0
            in [ mkInstance fid lx ly surfZ seed gx gy i count baseW
               | i ← [0 .. count - 1]
               ]
  where
    firstMatch [] = Nothing
    firstMatch ((fid, wg):rest)
        | matchesBiome wg matId slopeId temp precip
        , tileRoll < fwDensity wg
        , footprintSafe lx ly surfZ (round (fwFootprint wg)) surfMap chunkSz
            = Just (fid, wg)
        | otherwise = firstMatch rest

    tileRoll ∷ Float
    tileRoll =
        let h0 = floraHash seed gx gy 0
        in fromIntegral (h0 .&. 0xFFFF) / 65535.0

-----------------------------------------------------------
-- Instance Construction
-----------------------------------------------------------

-- | How many instances to place per tile for a given category.
instanceCount ∷ Text → Word64 → Int
instanceCount cat h
    | cat ≡ "tree"      = 1
    | cat ≡ "shrub"     = 1
    | cat ≡ "bush"      = 1 + fromIntegral ((h `shiftR` 16) .&. 0x01)  -- 1-2
    | cat ≡ "cactus"    = 1
    | otherwise          = 2 + fromIntegral ((h `shiftR` 16) .&. 0x01)  -- 2-3 (wildflowers etc)

-- | Build one FloraInstance with deterministic sub-tile offset.
--   clamped so the sprite base stays within the tile.
mkInstance ∷ FloraId → Int → Int → Int → Word64 → Int → Int
           → Int → Int → Float → FloraInstance
mkInstance fid lx ly surfZ seed gx gy i count baseWidth =
    let h = floraHash seed gx gy (fromIntegral i + 1)
        -- Sub-tile offset: centered for single, scattered for multiple
        (rawU, rawV) = if count ≡ 1
            then (0.0, 0.0)
            else let u = fromIntegral ((h `shiftR` 0)  .&. 0xFF) / 255.0 - 0.5
                     v = fromIntegral ((h `shiftR` 8)  .&. 0xFF) / 255.0 - 0.5
                 in (u * 0.8, v * 0.8)

        -- Clamp offset so the base stays inside the tile.
        -- baseWidth is in pixels; tileWidth (32) is the full tile.
        -- Half the base in tile-units:
        halfBase = if baseWidth > 0.0
                   then (baseWidth / 2.0) / 32.0
                   else 0.0
        -- Maximum offset: half tile (0.5) minus half base
        maxOff = max 0.0 (0.5 - halfBase)
        offU = clamp (negate maxOff) maxOff rawU
        offV = clamp (negate maxOff) maxOff rawV

        variant = fromIntegral ((h `shiftR` 16) .&. 0x03)
    in FloraInstance
        { fiSpecies   = fid
        , fiTileX     = fromIntegral lx
        , fiTileY     = fromIntegral ly
        , fiOffU      = offU
        , fiOffV      = offV
        , fiZ         = surfZ
        , fiAge       = 0.0
        , fiHealth    = 1.0
        , fiVariant   = variant
        , fiBaseWidth = baseWidth }

-----------------------------------------------------------
-- Biome Matching
-----------------------------------------------------------

matchesBiome ∷ FloraWorldGen → Word8 → Word8 → Float → Float → Bool
matchesBiome wg matId slopeId temp precip =
       temp   ≥ fwMinTemp wg
    ∧ temp   ≤ fwMaxTemp wg
    ∧ precip ≥ fwMinPrecip wg
    ∧ precip ≤ fwMaxPrecip wg
    ∧ slopeId ≤ fwMaxSlope wg
    ∧ soilOk
  where
    soils = fwSoils wg
    soilOk = null soils ∨ matId `elem` soils

-----------------------------------------------------------
-- Climate Lookup (same as Vegetation.hs)
-----------------------------------------------------------

lookupLocalClimate ∷ ClimateState → Int → Int → Int
                   → (Float, Float, Float, Float)
lookupLocalClimate climate worldSize gx gy =
    let regions = cgRegions (csClimate climate)
        halfChunks = worldSize `div` 2
        w = worldSize * chunkSz
        halfW = w `div` 2
        u = gx - gy
        v = gx + gy
        wrappedU = ((u + halfW) `mod` w + w) `mod` w - halfW
        chunkU = floorDiv wrappedU chunkSz
        chunkV = floorDiv v chunkSz
        ru = (chunkU + halfChunks) `div` climateRegionSize
        rv = (chunkV + halfChunks) `div` climateRegionSize
    in case HM.lookup (ClimateCoord ru rv) regions of
        Just rc →
            let SeasonalClimate st wt = rcAirTemp rc
                temp = (st + wt) / 2.0
                SeasonalClimate sp wp = rcPrecipitation rc
                precip = (sp + wp) / 2.0
            in (temp, precip, rcHumidity rc, rcPrecipType rc)
        Nothing →
            (csGlobalTemp climate, 0.5, 0.5, 0.0)
  where
    chunkSz = chunkSize
    floorDiv a b
        | b > 0     = if a ≥ 0 then a `div` b
                       else negate ((negate a + b - 1) `div` b)
        | otherwise = 0

-----------------------------------------------------------
-- Hash
-----------------------------------------------------------

-- | Deterministic hash for flora placement.
--   Uses a different salt from vegetation to avoid correlation.
floraHash ∷ Word64 → Int → Int → Word64 → Word64
floraHash seed gx gy salt =
    let a = seed `xor` 0xF15A533D
        b = a `xor` (fromIntegral gx * 2654435761)
        c = b `xor` (fromIntegral gy * 2246822519)
        d = c `xor` (salt * 1640531527)
        e = d `xor` (d `shiftR` 16)
        f = e * 2246822519
        g = f `xor` (f `shiftR` 13)
    in g
