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
    , vegSnow
    , vegDesertSand
    , vegGravelTundra
    , vegVariants
      -- * Per-tile vegetation selection
    , selectVegetation
    , vegHash
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
import World.Fluid.Types (FluidCell(..), FluidType(..))
import World.Weather.Types (ClimateState(..))
import World.Weather.Lookup (lookupLocalClimate, LocalClimate(..))
import World.Geology.Hash (valueNoise2D)
import Engine.Asset.Handle (TextureHandle(..))
import World.Render.Textures.Types (WorldTextures(..))

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

vegSnow ∷ Word8
vegSnow = 65

vegDesertSand ∷ Word8
vegDesertSand = 69

vegGravelTundra ∷ Word8
vegGravelTundra = 73

-- | Number of variants per vegetation type.
vegVariants ∷ Word8
vegVariants = 4

-- * Texture Lookup

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

-- * Chunk Vegetation Computation

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
            elev = surfMap VU.! idx
            -- Fluid depth: how many tiles of water above terrain.
            -- Only deep water (≥3) fully suppresses vegetation.
            -- Shallow fresh water (1-2) gets marsh/wetland vegetation
            -- instead of bare dark terrain.
            fluidInfo = case fluidMap V.! idx of
                Just fc → let d = max 0 (fcSurface fc - elev)
                          in (d, fcType fc)
                Nothing → (0, Ocean)  -- dummy type, depth 0 = no fluid
            fluidDepth = fst fluidInfo
            fluidTy    = snd fluidInfo
            hasFluid   = fluidDepth ≥ 3

            -- Per-tile hash for texture variant selection only
            h = vegHash seed gx gy
            variant = fromIntegral ((h `shiftR` 8) .&. 0x03) ∷ Word8

            -- Spatially coherent noise for vegetation type selection.
            -- Cell size ~10 tiles creates natural-looking patches:
            -- a ~10-tile area of dense grass, then wildflowers, etc.
            -- instead of per-tile salt-and-pepper randomness.
            gxF = fromIntegral gx ∷ Float
            gyF = fromIntegral gy ∷ Float
            roll = valueNoise2D seed 47 gxF gyF 10.0 + 0.5

            -- Regional climate
            LocalClimate{lcTemp=temp, lcPrecip=precip
                        , lcHumidity=humid, lcSnow=snow} =
                lookupLocalClimate climate worldSize gx gy

            -- Shallow fresh water → marsh vegetation (not bare ground)
        in if fluidDepth ≥ 1 ∧ fluidDepth < 3 ∧ fluidTy ≢ Ocean
           then vegMarshGrass + variant
           else selectVegetation matId slopeId hasFluid elev
                                temp precip humid snow roll variant
  where chunkSz = chunkSize

-- * Vegetation Selection

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

    -- === SNOW COVER ===
    | snow > 0.7             = vegSnow + variant

    -- === TUNDRA / ALPINE (before barren check — covers bare rock) ===
    | temp < -5.0 ∨ (temp < 2.0 ∧ snow > 0.3)
        = if roll < 0.15
          then vegLichen + variant
          else vegGravelTundra + variant

    -- === DESERT (before barren check — covers bare rock) ===
    | temp > 25.0 ∧ precip < 0.15
        = if roll < 0.20
          then vegDesertScrub + variant
          else vegDesertSand + variant

    -- === BARREN MATERIALS ===
    | isBarrenMaterial matId ∧ not (isSnowableMaterial matId)
                             = vegNone

    -- === WETLAND ===
    | isWetlandSoil matId ∧ precip > 0.4
        = vegMarshGrass + variant

    -- === ARID / SEMI-ARID ===
    | precip < 0.2
        = if roll < 0.30
          then vegSparseGrass + variant
          else if roll < 0.50
               then vegDeadGrass + variant
               else if roll < 0.65
                    then vegDesertScrub + variant
                    else vegDesertSand + variant

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
        = if roll < 0.55
          then vegDenseGrass + variant
          else if roll < 0.80
               then vegThickMoss + variant
               else vegMushroomPatch + variant

    -- === WARM + WET ===
    | temp > 15.0 ∧ precip > 0.4
        = if roll < 0.75
          then vegDenseGrass + variant
          else vegWildflowers + variant

    -- === PRAIRIE / SAVANNA ===
    | temp > 15.0 ∧ precip > 0.2 ∧ precip < 0.4
        = if roll < 0.65
          then vegTallGrass + variant
          else if roll < 0.90
               then vegDeadGrass + variant
               else vegSparseGrass + variant

    -- === TEMPERATE ===
    | temp > 5.0 ∧ precip > 0.3
        = if roll < 0.55
          then vegMediumGrass + variant
          else if roll < 0.85
               then vegDenseGrass + variant
               else vegWildflowers + variant

    -- === COOL TEMPERATE ===
    | temp > 0.0
        = if roll < 0.45
          then vegMediumGrass + variant
          else if roll < 0.80
               then vegSparseGrass + variant
               else vegDeadGrass + variant

    -- === FALLBACK (near-polar) ===
    | otherwise
        = if roll < 0.4
          then vegSparseGrass + variant
          else if roll < 0.6
               then vegLichen + variant
               else vegNone

-- * Material Classification

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

-- | Materials that are barren but can still receive snow overlay.
isSnowableMaterial ∷ Word8 → Bool
isSnowableMaterial 250 = True   -- glacier
isSnowableMaterial _   = False

-- | Wetland soils that produce marsh vegetation.
isWetlandSoil ∷ Word8 → Bool
isWetlandSoil 62 = True  -- peat
isWetlandSoil 63 = True  -- mucky peat
isWetlandSoil 64 = True  -- muck
isWetlandSoil _  = False

-- * Climate Lookup


-- * Hash

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
