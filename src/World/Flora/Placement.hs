{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Flora.Placement
    ( computeChunkFlora
    ) where

import UPrelude
import Data.Bits (xor, shiftR, (.&.))
import Data.Word (Word8, Word16, Word64)
import Data.List (sortOn, foldl')
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import World.Types
import World.Chunk.Types (ChunkCoord(..), chunkSize)
import World.Fluid.Types (FluidCell(..))
import World.Vegetation (isBarrenMaterial)
import World.Weather.Types (ClimateState(..), ClimateGrid(..)
                           , RegionClimate(..), SeasonalClimate(..)
                           , ClimateCoord(..), climateRegionSize)
import World.Flora.Types
import System.IO.Unsafe (unsafePerformIO)

-----------------------------------------------------------
-- Chunk Flora Computation
-----------------------------------------------------------

-- | Place flora instances for an entire chunk.
--   Uses an occupancy grid to prevent overlapping footprints.
--   Each placed tree claims a radius of tiles around it based
--   on its fwFootprint (in pixels → tile radius).
computeChunkFlora
    ∷ Word64 → Int → ChunkCoord
    → VU.Vector Int → VU.Vector Word8 → VU.Vector Word8
    → V.Vector (Maybe FluidCell) → ClimateState → FloraCatalog
    → FloraChunkData
computeChunkFlora seed worldSize coord surfMap surfMats surfSlopes
                  fluidMap climate catalog =
    let ChunkCoord cx cy = coord
        chunkArea = chunkSize * chunkSize
        wgSpecies = worldGenSpecies catalog

        -- Shuffle tile indices so placement isn't biased by scan order.
        -- This ensures trees at the top-left of a chunk don't always
        -- claim space before trees at the bottom-right.
        tileOrder = shuffledIndices seed cx cy chunkArea

        -- Build instances with occupancy tracking.
        -- The occupancy vector is chunkSize×chunkSize of Bool (0/1).
        -- A tile marked occupied cannot have new flora placed on it.
        allInstances = unsafePerformIO $ do
            occupied ← VUM.replicate chunkArea (0 ∷ Word8)
            let go [] acc = return (reverse acc)
                go (idx:rest) acc = do
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

                    occ ← VUM.read occupied idx
                    if occ /= 0 ∨ hasFluid ∨ isBarrenMaterial matId ∨ surfZ ≡ minBound
                    then go rest acc
                    else do
                        let newInsts = placeTileFlora seed gx gy lx ly surfZ
                                          matId slopeId temp precip wgSpecies
                        case newInsts of
                            [] → go rest acc
                            insts → do
                                -- Mark tiles as occupied based on footprint
                                let baseW = case insts of
                                        (fi:_) → fiBaseWidth fi
                                        _      → 0.0
                                    -- Convert pixel footprint to tile radius:
                                    -- a 24px base on a 32px tile → radius 1
                                    -- (claims the center tile + immediate neighbors)
                                    tileRadius = if baseW > 0.0
                                                 then max 0 (ceiling (baseW / 32.0) - 1)
                                                 else 0 ∷ Int
                                markOccupied occupied lx ly tileRadius chunkSize
                                go rest (insts ++ acc)

            go tileOrder []

    in FloraChunkData allInstances

-- | Mark a radius of tiles as occupied in the mutable vector.
markOccupied ∷ VUM.IOVector Word8 → Int → Int → Int → Int → IO ()
markOccupied occupied cx cy radius chunkSz =
    forM_ [negate radius .. radius] $ \dx →
        forM_ [negate radius .. radius] $ \dy → do
            let nx = cx + dx
                ny = cy + dy
            when (nx >= 0 ∧ nx < chunkSz ∧ ny >= 0 ∧ ny < chunkSz) $
                VUM.write occupied (ny * chunkSz + nx) 1

-- | Generate a shuffled list of tile indices for a chunk.
--   Deterministic based on seed and chunk coord.
shuffledIndices ∷ Word64 → Int → Int → Int → [Int]
shuffledIndices seed cx cy n =
    map snd $ sortOn fst
        [ (floraHash seed (cx * 1000 + i) (cy * 1000) 0x51151551, i)
        | i ← [0 .. n - 1]
        ]

-----------------------------------------------------------
-- Per-Tile Placement
-----------------------------------------------------------

placeTileFlora
    ∷ Word64 → Int → Int → Int → Int → Int
    → Word8 → Word8 → Float → Float
    → [(FloraId, FloraWorldGen)]
    → [FloraInstance]
placeTileFlora seed gx gy lx ly surfZ matId slopeId temp precip wgSpecies =
    let h0 = floraHash seed gx gy 0
        tileRoll = fromIntegral (h0 .&. 0xFFFF) / 65535.0 ∷ Float

        sorted = sortOn (negate . fwDensity . snd) wgSpecies
        match = firstMatch sorted tileRoll
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
    firstMatch [] _ = Nothing
    firstMatch ((fid, wg):rest) roll
        | matchesBiome wg matId slopeId temp precip
        , roll < fwDensity wg
            = Just (fid, wg)
        | otherwise = firstMatch rest roll

-----------------------------------------------------------
-- Instance Construction
-----------------------------------------------------------

instanceCount ∷ Text → Word64 → Int
instanceCount cat h
    | cat ≡ "tree"      = 1
    | cat ≡ "shrub"     = 1
    | cat ≡ "bush"      = 1 + fromIntegral ((h `shiftR` 16) .&. 0x01)
    | cat ≡ "cactus"    = 1
    | otherwise          = 2 + fromIntegral ((h `shiftR` 16) .&. 0x01)

-- | Build one FloraInstance with deterministic sub-tile offset.
--   ALL instances get a random offset (including trees with count=1).
--   The offset is clamped by the footprint so the base stays on the tile.
mkInstance ∷ FloraId → Int → Int → Int → Word64 → Int → Int
           → Int → Int → Float → FloraInstance
mkInstance fid lx ly surfZ seed gx gy i _count baseWidth =
    let h = floraHash seed gx gy (fromIntegral i + 1)
        -- Always scatter — even single instances get a random position
        rawU = fromIntegral ((h `shiftR` 0)  .&. 0xFF) / 255.0 - 0.5
        rawV = fromIntegral ((h `shiftR` 8)  .&. 0xFF) / 255.0 - 0.5

        -- Clamp offset so the base stays inside the tile.
        halfBase = if baseWidth > 0.0
                   then (baseWidth / 2.0) / 96.0
                   else 0.0
        maxOff = max 0.0 (0.5 - halfBase)
        -- Scale down scatter range (0.7 keeps things off the very edge)
        offU = clamp (negate maxOff) maxOff (rawU * 0.7)
        offV = clamp (negate maxOff) maxOff (rawV * 0.7)

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
        , fiBaseWidth = baseWidth
        }

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
-- Climate Lookup
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
