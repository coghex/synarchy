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

        tileOrder = shuffledIndices seed cx cy chunkArea

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
                                          matId slopeId temp precip wgSpecies catalog
                        case newInsts of
                            [] → go rest acc
                            insts → do
                                let maxFP = maximum
                                        [ fiBaseWidth fi | fi ← insts ]
                                    tileRadius = if maxFP > 0.0
                                                 then max 0 (ceiling (maxFP / 32.0) - 1)
                                                 else 0 ∷ Int
                                markOccupied occupied lx ly tileRadius chunkSize
                                go rest (insts ++ acc)

            go tileOrder []

    in FloraChunkData allInstances

markOccupied ∷ VUM.IOVector Word8 → Int → Int → Int → Int → IO ()
markOccupied occupied cx cy radius chunkSz =
    forM_ [negate radius .. radius] $ \dx →
        forM_ [negate radius .. radius] $ \dy → do
            let nx = cx + dx
                ny = cy + dy
            when (nx >= 0 ∧ nx < chunkSz ∧ ny >= 0 ∧ ny < chunkSz) $
                VUM.write occupied (ny * chunkSz + nx) 1

shuffledIndices ∷ Word64 → Int → Int → Int → [Int]
shuffledIndices seed cx cy n =
    map snd $ sortOn fst
        [ (floraHash seed (cx * 1000 + i) (cy * 1000) 0x5A1F1E, i)
        | i ← [0 .. n - 1]
        ]

-----------------------------------------------------------
-- Per-Tile Placement
-----------------------------------------------------------

placeTileFlora
    ∷ Word64 → Int → Int → Int → Int → Int
    → Word8 → Word8 → Float → Float
    → [(FloraId, FloraWorldGen)]
    → FloraCatalog
    → [FloraInstance]
placeTileFlora seed gx gy lx ly surfZ matId slopeId temp precip wgSpecies catalog =
    concatMap (\(i, (fid, wg)) →
        let h = floraHash seed gx gy (fromIntegral i + 100)
            roll = fromIntegral (h .&. 0xFFFF) / 65535.0 ∷ Float
        in if matchesBiome wg matId slopeId temp precip ∧ roll < fwDensity wg
           then let cat   = fwCategory wg
                    baseW = fwFootprint wg
                    count = instanceCount cat h
                    maxAge = speciesMaxAge fid catalog
                in [ mkInstance fid lx ly surfZ seed gx gy
                         (i * 10 + j) count baseW maxAge
                   | j ← [0 .. count - 1]
                   ]
           else []
    ) (zip [0..] wgSpecies)

-----------------------------------------------------------
-- Max Age from Lifecycle Data
-----------------------------------------------------------

-- | Determine the maximum initial age for worldgen spawning
--   from the actual species lifecycle and phase definitions.
--
--   Strategy:
--     Perennial → use lcMaxLifespan (cap at that, they die)
--     Annual    → 360 game-days (one year)
--     Biennial  → 720 game-days (two years)
--     Evergreen → use the highest phase age, so we get a
--                 spread across all defined growth stages.
--                 If no phases defined, default to 1800 (~5yr).
speciesMaxAge ∷ FloraId → FloraCatalog → Float
speciesMaxAge fid catalog =
    case lookupSpecies fid catalog of
        Nothing → 360.0  -- fallback
        Just species →
            let lifecycle = fsLifecycle species
                phases    = fsPhases species
                -- Highest age threshold from any defined phase
                maxPhaseAge = if HM.null phases
                              then 0.0
                              else maximum [ lpAge lp | lp ← HM.elems phases ]
            in case lifecycle of
                Perennial minL maxL _ → maxL
                Annual                → 360.0
                Biennial              → 720.0
                Evergreen
                    | maxPhaseAge > 0.0 → maxPhaseAge * 1.5
                    | otherwise         → 1800.0

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
           → Int → Int → Float → Float → FloraInstance
mkInstance fid lx ly surfZ seed gx gy i _count baseWidth maxAge =
    let h = floraHash seed gx gy (fromIntegral i + 1)
        rawU = fromIntegral ((h `shiftR` 0)  .&. 0xFF) / 255.0 - 0.5
        rawV = fromIntegral ((h `shiftR` 8)  .&. 0xFF) / 255.0 - 0.5

        halfBase = if baseWidth > 0.0
                   then (baseWidth / 2.0) / 96.0
                   else 0.0
        maxOff = max 0.0 (0.5 - halfBase)
        offU = clamp (negate maxOff) maxOff (rawU * 0.7)
        offV = clamp (negate maxOff) maxOff (rawV * 0.7)

        variant = fromIntegral ((h `shiftR` 16) .&. 0x03)

        -- Randomize initial age from lifecycle data
        ageFrac = fromIntegral ((h `shiftR` 24) .&. 0xFF) / 255.0
        age = ageFrac * maxAge

    in FloraInstance
        { fiSpecies   = fid
        , fiTileX     = fromIntegral lx
        , fiTileY     = fromIntegral ly
        , fiOffU      = offU
        , fiOffV      = offV
        , fiZ         = surfZ
        , fiAge       = age
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
