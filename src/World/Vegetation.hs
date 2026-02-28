{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Vegetation
    ( -- * Chunk computation
      computeChunkVegetation
    , VegId(..)
      -- * Texture lookup
    , getVegTexture
      -- * Vegetation ID constants
    , vegNone
    , vegSparseGrass
    , vegMediumGrass
    , vegDenseGrass
    , vegTallGrass
    , vegThinMoss
    , vegThickMoss
    , vegLightIvy
    , vegHeavyIvy
    , vegLichen
    , vegDesertScrub
    , vegMarshGrass
    , vegDeadGrass
    , vegFallenLeaves
    , vegPineNeedles
    , vegMushroomPatch
    , vegWildflowers
      -- * Classification helpers
    , isBarrenMaterial
    , isWetlandSoil
    ) where

import UPrelude
import Data.Bits (xor, shiftR, (.&.))
import Data.Word (Word8, Word64)
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import World.Types
import World.Material (MaterialId(..), getMaterialProps, MaterialProps(..))
import World.Fluid.Types (FluidCell(..))
import World.Weather.Types (ClimateState(..), ClimateGrid(..)
                           , RegionClimate(..), SeasonalClimate(..)
                           , ClimateCoord(..), climateRegionSize)
import Engine.Asset.Handle (TextureHandle(..))
import World.Render.Textures.Types (WorldTextures(..))

-----------------------------------------------------------
-- Vegetation ID Constants
--
-- Each constant is the base of a 4-variant range.
-- Usage: vegSparseGrass + variant  (variant ∈ 0..3)
--
-- Layout:
--   0           none
--   1-4         sparse grass
--   5-8         medium grass
--   9-12        dense grass
--   13-16       tall grass (prairie/savanna)
--   17-20       thin moss
--   21-24       thick moss
--   25-28       light ivy
--   29-32       heavy ivy
--   33-36       lichen / tundra
--   37-40       desert scrub
--   41-44       marsh grass
--   45-48       dead grass
--   49-52       fallen leaves
--   53-56       pine needles
--   57-60       mushroom patch
--   61-64       wildflowers
--   65-99       (reserved groundcover)
--   100+        sprites (phase 2)
-----------------------------------------------------------

vegNone ∷ Word8
vegNone = 0

vegSparseGrass ∷ Word8
vegSparseGrass = 1

vegMediumGrass ∷ Word8
vegMediumGrass = 5

vegDenseGrass ∷ Word8
vegDenseGrass = 9

vegTallGrass ∷ Word8
vegTallGrass = 13

vegThinMoss ∷ Word8
vegThinMoss = 17

vegThickMoss ∷ Word8
vegThickMoss = 21

vegLightIvy ∷ Word8
vegLightIvy = 25

vegHeavyIvy ∷ Word8
vegHeavyIvy = 29

vegLichen ∷ Word8
vegLichen = 33

vegDesertScrub ∷ Word8
vegDesertScrub = 37

vegMarshGrass ∷ Word8
vegMarshGrass = 41

vegDeadGrass ∷ Word8
vegDeadGrass = 45

vegFallenLeaves ∷ Word8
vegFallenLeaves = 49

vegPineNeedles ∷ Word8
vegPineNeedles = 53

vegMushroomPatch ∷ Word8
vegMushroomPatch = 57

vegWildflowers ∷ Word8
vegWildflowers = 61

-- | Number of variants per vegetation type.
vegVariants ∷ Word8
vegVariants = 4

-----------------------------------------------------------
-- Texture Lookup
-----------------------------------------------------------

-- | Look up the texture handle for a vegetation ID.
--   Veg textures are stored in wtVegTextures keyed by
--   the exact vegId byte (base + variant).
--   Returns the blank texture for unknown / vegNone.
getVegTexture ∷ WorldTextures → Word8 → TextureHandle
getVegTexture _        0 = TextureHandle 0
getVegTexture textures vegId =
    case HM.lookup vegId (wtVegTextures textures) of
        Just h  → h
        Nothing → wtBlankTexture textures

-----------------------------------------------------------
-- Chunk Vegetation Computation
-----------------------------------------------------------

newtype VegId = VegId { unVegId ∷ Word8 }
    deriving (Show, Eq)

-- | Compute vegetation overlay for an entire chunk.
--   Pure, deterministic, runs after slopes are computed.
--   Returns a VU.Vector Word8 of length chunkSize*chunkSize
--   (one entry per column = the surface tile's veg ID).
computeChunkVegetation
    ∷ Word64                     -- ^ world seed
    → Int                        -- ^ worldSize
    → ChunkCoord                 -- ^ chunk coordinate
    → VU.Vector Int              -- ^ terrainSurfaceMap
    → VU.Vector Word8            -- ^ surface material IDs
    → VU.Vector Word8            -- ^ slope IDs
    → V.Vector (Maybe FluidCell) -- ^ fluid map
    → ClimateState               -- ^ climate for regional lookup
    → VU.Vector Word8            -- ^ veg IDs, one per column
computeChunkVegetation seed worldSize coord surfMap surfMats surfSlopes
                       fluidMap climate =
    let ChunkCoord cx cy = coord
        chunkArea = chunkSz * chunkSz
    in VU.generate chunkArea $ \idx →
        let lx = idx `mod` chunkSz
            ly = idx `div` chunkSz
            gx = cx * chunkSz + lx
            gy = cy * chunkSz + ly
            matId   = surfMats   VU.! idx
            slopeId = surfSlopes VU.! idx
            hasFluid = case fluidMap V.! idx of
                Just _  → True
                Nothing → False
            elev = surfMap VU.! idx
            -- Per-tile deterministic hash for variant selection
            h = vegHash seed gx gy
            roll    = fromIntegral (h .&. 0xFF) / 255.0 ∷ Float
            variant = fromIntegral ((h `shiftR` 8) .&. 0x03) ∷ Word8

            -- Regional climate
            (temp, precip, humid, snow) =
                lookupLocalClimate climate worldSize gx gy

        in selectVegetation matId slopeId hasFluid elev
                            temp precip humid snow roll variant
  where chunkSz = chunkSize

-----------------------------------------------------------
-- Vegetation Selection
-----------------------------------------------------------

-- | The core biome → vegetation mapping.
selectVegetation
    ∷ Word8   -- ^ surface material ID
    → Word8   -- ^ slope ID
    → Bool    -- ^ has fluid
    → Int     -- ^ elevation
    → Float   -- ^ temperature (°C)
    → Float   -- ^ precipitation (0-1)
    → Float   -- ^ humidity (0-1)
    → Float   -- ^ snow fraction (0-1)
    → Float   -- ^ random roll (0-1)
    → Word8   -- ^ variant (0-3)
    → Word8   -- ^ vegetation ID
selectVegetation matId slopeId hasFluid elev
                 temp precip humid snow roll variant

    -- === EXCLUSION RULES ===
    | hasFluid               = vegNone
    | isBarrenMaterial matId = vegNone
    | snow > 0.7             = vegNone

    -- === WETLAND ===
    | isWetlandSoil matId ∧ precip > 0.4
        = vegMarshGrass + variant

    -- === TUNDRA / ALPINE ===
    | temp < -5.0 ∨ (temp < 2.0 ∧ snow > 0.3)
        = if roll < 0.6
          then vegLichen + variant
          else vegNone

    -- === DESERT ===
    | temp > 25.0 ∧ precip < 0.15
        = if roll < 0.25
          then vegDesertScrub + variant
          else vegNone

    -- === ARID / SEMI-ARID ===
    | precip < 0.2
        = if roll < 0.3
          then vegSparseGrass + variant
          else if roll < 0.45
               then vegDeadGrass + variant
               else if roll < 0.55
                    then vegDesertScrub + variant
                    else vegNone

    -- === MOSS ON WET SLOPES ===
    | slopeId > 0 ∧ humid > 0.5
        = if precip > 0.6
          then vegThickMoss + variant
          else vegThinMoss + variant

    -- === IVY ON WARM SLOPES ===
    | slopeId > 0 ∧ temp > 10.0
        = if roll < 0.3
          then vegLightIvy + variant
          else vegThinMoss + variant

    -- === HOT + WET (tropical) ===
    | temp > 25.0 ∧ precip > 0.5
        = if roll < 0.6
          then vegDenseGrass + variant
          else if roll < 0.8
               then vegThickMoss + variant
               else vegMushroomPatch + variant

    -- === WARM + WET ===
    | temp > 15.0 ∧ precip > 0.4
        = if roll < 0.8
          then vegDenseGrass + variant
          else vegWildflowers + variant

    -- === TEMPERATE ===
    | temp > 5.0 ∧ precip > 0.3
        = if roll < 0.6
          then vegMediumGrass + variant
          else if roll < 0.85
               then vegDenseGrass + variant
               else vegWildflowers + variant

    -- === PRAIRIE / SAVANNA ===
    | temp > 15.0 ∧ precip > 0.2 ∧ precip < 0.4
        = if roll < 0.7
          then vegTallGrass + variant
          else vegDeadGrass + variant

    -- === COOL TEMPERATE ===
    | temp > 0.0
        = if roll < 0.4
          then vegMediumGrass + variant
          else if roll < 0.7
               then vegSparseGrass + variant
               else vegDeadGrass + variant

    -- === FALLBACK ===
    | otherwise
        = if roll < 0.3
          then vegSparseGrass + variant
          else vegNone

-----------------------------------------------------------
-- Material Classification
-----------------------------------------------------------

-- | Materials that never get vegetation.
isBarrenMaterial ∷ Word8 → Bool
isBarrenMaterial m
    | m ≡ 0               = True   -- air
    | m ≥ 1   ∧ m ≤ 16   = True   -- igneous rock
    | m ≥ 20  ∧ m ≤ 35   = True   -- sedimentary rock
    | m ≥ 40  ∧ m ≤ 45   = True   -- metamorphic rock
    | m ≥ 70  ∧ m ≤ 72   = True   -- coal
    | m ≥ 80  ∧ m ≤ 86   = True   -- ores
    | m ≥ 90  ∧ m ≤ 91   = True   -- impact
    | m ≥ 100 ∧ m ≤ 103  = True   -- volcanic active
    | m ≡ 250             = True   -- glacier
    | m ≡ 251             = True   -- mantle
    | m ≡ 255             = True   -- ocean
    | m ≡ 55              = True   -- pure sand (dunes)
    | m ≡ 67              = True   -- salt flat
    | otherwise           = False

-- | Wetland soils that produce marsh vegetation.
isWetlandSoil ∷ Word8 → Bool
isWetlandSoil 62 = True  -- peat
isWetlandSoil 63 = True  -- mucky peat
isWetlandSoil 64 = True  -- muck
isWetlandSoil _  = False

-----------------------------------------------------------
-- Climate Lookup
-----------------------------------------------------------

-- | Look up regional climate for a global tile coordinate.
--   Returns (temperature, precipitation, humidity, snowFraction).
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
        chunkU = floorDiv' wrappedU chunkSz
        chunkV = floorDiv' v chunkSz
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
    floorDiv' a b
        | b > 0     = if a ≥ 0 then a `div` b
                       else negate ((negate a + b - 1) `div` b)
        | otherwise = 0

-----------------------------------------------------------
-- Hash
-----------------------------------------------------------

-- | Deterministic hash for vegetation placement.
vegHash ∷ Word64 → Int → Int → Word64
vegHash seed gx gy =
    let a = seed `xor` 0xBE6E7A710
        b = a `xor` (fromIntegral gx * 2654435761)
        c = b `xor` (fromIntegral gy * 2246822519)
        d = c `xor` (c `shiftR` 16)
        e = d * 2246822519
        f = e `xor` (e `shiftR` 13)
    in f
