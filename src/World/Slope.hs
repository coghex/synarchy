{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Slope
    ( -- * Slope Computation
      computeChunkSlopes
    , recomputeNeighborSlopes
    , chunkNeighbors
      -- * Slope Face Map Generation
    , SlopeFaceMaps(..)
    , generateSlopeFaceMaps
      -- * Slope → Face Map Mapping
    , slopeToFaceMapIndex
      -- * Constants
    , slopeHardnessThreshold
    ) where

import UPrelude
import Data.Bits ((.&.), (.|.), shiftL, testBit, shiftR, shiftL, xor)
import Data.Word (Word8, Word32)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Vector.Storable as VS
import World.Types
import World.Material (getMaterialProps, MaterialProps(..), MaterialId(..))

-----------------------------------------------------------
-- Constants
-----------------------------------------------------------

-- | Materials with hardness above this threshold produce
--   vertical cliff faces instead of slopes.
slopeHardnessThreshold ∷ Float
slopeHardnessThreshold = 0.7

-- | Tile pixel dimensions (must match Grid config)
tilePixelWidth ∷ Int
tilePixelWidth = 96

tilePixelHeight ∷ Int
tilePixelHeight = 64

-- | Diamond top zone is rows 0–47, side wall zone is rows 48–63
diamondRows ∷ Int
diamondRows = 48

-----------------------------------------------------------
-- Slope Computation (Post-Processing Pass)
-----------------------------------------------------------

-- | Compute slope IDs for all surface tiles in a chunk.
--   Called after tiles, surfaceMap, and fluidMap are fully built.
--
--   For each surface tile, compares its z-level to 4 cardinal
--   neighbors. Uses marching-squares encoding:
--     bit 0 = north neighbor lower
--     bit 1 = east neighbor lower
--     bit 2 = south neighbor lower
--     bit 3 = west neighbor lower
--
--   Returns a HashMap mapping (lx, ly, z) to the new slopeId.
--   Only surface tiles get slopes; sub-surface tiles keep slopeId 0.
computeChunkSlopes ∷ Word64                          -- ^ World seed (for roughness hash)
                   → ChunkCoord                      -- ^ This chunk's coordinate
                   → HM.HashMap (Int, Int) Int        -- ^ Surface map (lx,ly → surfZ)
                   → HM.HashMap (Int, Int) FluidCell  -- ^ Fluid map
                   → Chunk                            -- ^ The chunk's tiles
                   → (ChunkCoord → Maybe (HM.HashMap (Int, Int) Int))
                      -- ^ Neighbor chunk surface map lookup
                   → Chunk                            -- ^ Updated tiles with slope IDs
computeChunkSlopes seed coord surfMap fluidMap tiles neighborLookup =
    HM.mapWithKey (\(lx, ly, z) tile →
        let surfZ = HM.lookupDefault minBound (lx, ly) surfMap
        in if z ≡ surfZ
           then tile { tileSlopeId = computeTileSlope
                         seed coord lx ly z surfMap fluidMap tiles neighborLookup }
           else tile
    ) tiles

-- | Compute the slope ID for a single surface tile.
computeTileSlope ∷ Word64 → ChunkCoord
                → Int → Int → Int
                → HM.HashMap (Int, Int) Int
                → HM.HashMap (Int, Int) FluidCell
                → Chunk
                → (ChunkCoord → Maybe (HM.HashMap (Int, Int) Int))
                → Word8
computeTileSlope seed coord lx ly z surfMap fluidMap tiles neighborLookup =
    let -- Get this tile's material
        matId = case HM.lookup (lx, ly, z) tiles of
            Just t  → tileType t
            Nothing → 0
        props = getMaterialProps (MaterialId matId)
        hardness = matHardness props

        -- Material hardness gate
        passesHardness = hardness < slopeHardnessThreshold

        -- Look up neighbor elevations (handling cross-chunk boundaries)
        neighN = neighborElev coord lx (ly - 1) surfMap neighborLookup  -- North = ly-1
        neighE = neighborElev coord (lx + 1) ly surfMap neighborLookup  -- East  = lx+1
        neighS = neighborElev coord lx (ly + 1) surfMap neighborLookup  -- South = ly+1
        neighW = neighborElev coord (lx - 1) ly surfMap neighborLookup  -- West  = lx-1

        -- Check each direction for valid slope conditions
        bitN = slopeBit z neighN lx (ly - 1) coord fluidMap neighborLookup
        bitE = slopeBit z neighE (lx + 1) ly coord fluidMap neighborLookup
        bitS = slopeBit z neighS lx (ly + 1) coord fluidMap neighborLookup
        bitW = slopeBit z neighW (lx - 1) ly coord fluidMap neighborLookup

        -- Marching squares: combine bits
        rawSlope = (if bitN then 1 else 0)
               .|. (if bitE then 2 else 0)
               .|. (if bitS then 4 else 0)
               .|. (if bitW then 8 else 0) ∷ Word8

    in if not passesHardness ∨ rawSlope ≡ 0
       then 0
       else applyRoughness seed coord lx ly hardness rawSlope

-- | Determine if a neighbor direction should contribute a slope bit.
--   Returns True if the neighbor is exactly 1 z-level lower.
--   Returns False for differences of 0, or 2+ (cliff face).
--   Returns False if the neighbor has fluid (no slopes at fluid boundaries).
slopeBit ∷ Int → Int → Int → Int → ChunkCoord
         → HM.HashMap (Int, Int) FluidCell
         → (ChunkCoord → Maybe (HM.HashMap (Int, Int) Int))
         → Bool
slopeBit myZ neighborZ nlx nly coord fluidMap neighborLookup =
    let diff = myZ - neighborZ
        -- Only slope for exactly 1 tile difference
        validDiff = diff ≡ 1

        -- Check for fluid at neighbor position
        -- Normalize to local coords for fluid check
        (normLx, normLy, neighborCoord) = normalizeCoord coord nlx nly
        hasFluid = case neighborCoord of
            c | c ≡ coord → HM.member (normLx, normLy) fluidMap
            _ → False  -- Cross-chunk fluid: conservative, assume no fluid
                       -- (fluid boundaries at chunk edges are rare)
    in validDiff ∧ not hasFluid

-- | Look up neighbor surface elevation, handling cross-chunk boundaries.
neighborElev ∷ ChunkCoord → Int → Int
             → HM.HashMap (Int, Int) Int
             → (ChunkCoord → Maybe (HM.HashMap (Int, Int) Int))
             → Int
neighborElev coord lx ly surfMap neighborLookup
    | lx ≥ 0 ∧ lx < chunkSize ∧ ly ≥ 0 ∧ ly < chunkSize =
        -- Within this chunk
        HM.lookupDefault minBound (lx, ly) surfMap
    | otherwise =
        -- Cross-chunk boundary
        let (neighborCoord, (nlx, nly)) = normalizeToChunk coord lx ly
        in case neighborLookup neighborCoord of
            Just neighSurf → HM.lookupDefault minBound (nlx, nly) neighSurf
            Nothing        → minBound  -- Unloaded chunk: default to same
                                       -- elevation (no slope) — minBound
                                       -- ensures diff ≠ 1

-- | Normalize local coords that may be outside [0..chunkSize-1]
--   back to a (ChunkCoord, (lx, ly)) pair.
normalizeToChunk ∷ ChunkCoord → Int → Int → (ChunkCoord, (Int, Int))
normalizeToChunk (ChunkCoord cx cy) lx ly =
    let cx' = cx + floorDiv' lx chunkSize
        cy' = cy + floorDiv' ly chunkSize
        lx' = floorMod' lx chunkSize
        ly' = floorMod' ly chunkSize
    in (ChunkCoord cx' cy', (lx', ly'))

normalizeCoord ∷ ChunkCoord → Int → Int → (Int, Int, ChunkCoord)
normalizeCoord coord lx ly =
    let (c, (lx', ly')) = normalizeToChunk coord lx ly
    in (lx', ly', c)

floorDiv' ∷ Int → Int → Int
floorDiv' a b = floor (fromIntegral a / fromIntegral b ∷ Double)

floorMod' ∷ Int → Int → Int
floorMod' a b = a - floorDiv' a b * b

-----------------------------------------------------------
-- Probabilistic Roughness
-----------------------------------------------------------

-- | Apply hash-based roughness to hard materials.
--   Hard rocky materials that pass the hardness check still
--   have a ~30% chance of their slope direction being randomized,
--   creating craggy cliff faces instead of smooth ramps.
--   Soft materials (hardness < 0.3) always slope correctly.
applyRoughness ∷ Word64 → ChunkCoord → Int → Int → Float → Word8 → Word8
applyRoughness seed (ChunkCoord cx cy) lx ly hardness rawSlope
    | hardness < 0.3  = rawSlope  -- Soft materials: always geometrically correct
    | otherwise =
        let -- Hash-based per-tile random value
            h = tileHash seed cx cy lx ly
            roll = fromIntegral (h .&. 0xFF) / 255.0 ∷ Float

            -- Roughness chance scales with hardness:
            -- hardness 0.3 → 0% chance, hardness 0.7 → 30% chance
            roughnessChance = (hardness - 0.3) * 0.75  -- 0.0 at 0.3, 0.3 at 0.7

        in if roll < roughnessChance
           then -- Randomize: pick a random cardinal direction
                let dirBits = (h `shiftR` 8) .&. 0x3
                    randomSlope = case dirBits of
                        0 → 1   -- N only
                        1 → 2   -- E only
                        2 → 4   -- S only
                        _ → 8   -- W only
                in fromIntegral randomSlope
           else rawSlope

-- | Simple hash for per-tile randomness.
--   Produces a deterministic pseudo-random Word64 from tile position.
tileHash ∷ Word64 → Int → Int → Int → Int → Word64
tileHash seed cx cy lx ly =
    let a = seed `xor` (fromIntegral cx * 2654435761)
        b = a `xor` (fromIntegral cy * 2246822519)
        c = b `xor` (fromIntegral lx * 3266489917)
        d = c `xor` (fromIntegral ly * 668265263)
        -- Mix
        e = d `xor` (d `shiftR` 16)
        f = e * 2246822519
        g = f `xor` (f `shiftR` 13)
    in g

-----------------------------------------------------------
-- Slope → Face Map Index Mapping
-----------------------------------------------------------

-- | Map a 4-bit marching-squares slope ID to one of 5 face maps:
--   0 = flat (default isoface)
--   1 = north ramp
--   2 = east ramp
--   3 = south ramp
--   4 = west ramp
--
--   For corner cases (2+ bits set), pick the dominant direction.
--   Single-bit cases map directly. Zero maps to flat.
slopeToFaceMapIndex ∷ Word8 → Int
slopeToFaceMapIndex 0  = 0  -- flat
slopeToFaceMapIndex 1  = 1  -- N only
slopeToFaceMapIndex 2  = 2  -- E only
slopeToFaceMapIndex 4  = 3  -- S only
slopeToFaceMapIndex 8  = 4  -- W only
-- Two-bit corners: pick the first cardinal in N→E→S→W priority
slopeToFaceMapIndex 3  = 1  -- N+E → N dominates
slopeToFaceMapIndex 5  = 1  -- N+S → N dominates (rare, saddle)
slopeToFaceMapIndex 9  = 1  -- N+W → N dominates
slopeToFaceMapIndex 6  = 2  -- E+S → E dominates
slopeToFaceMapIndex 10 = 2  -- E+W → E dominates (rare, saddle)
slopeToFaceMapIndex 12 = 3  -- S+W → S dominates
-- Three-bit cases
slopeToFaceMapIndex 7  = 1  -- N+E+S → N
slopeToFaceMapIndex 11 = 1  -- N+E+W → N
slopeToFaceMapIndex 13 = 3  -- N+S+W → S (N and S cancel, W wins... but use S for visual)
slopeToFaceMapIndex 14 = 2  -- E+S+W → E
-- All four: flat (surrounded by lower terrain = plateau)
slopeToFaceMapIndex 15 = 0
slopeToFaceMapIndex _  = 0  -- fallback

-----------------------------------------------------------
-- Procedural Face Map Generation
-----------------------------------------------------------

-- | The 5 face map textures: flat + 4 cardinal ramps.
data SlopeFaceMaps = SlopeFaceMaps
    { sfmFlat  ∷ !(VS.Vector Word8)  -- ^ 96×64 RGBA = 24576 bytes
    , sfmNorth ∷ !(VS.Vector Word8)
    , sfmEast  ∷ !(VS.Vector Word8)
    , sfmSouth ∷ !(VS.Vector Word8)
    , sfmWest  ∷ !(VS.Vector Word8)
    } deriving (Show)

-- | Generate all 5 face maps procedurally as 96×64 RGBA pixel data.
--   The flat map matches the existing isoface.png encoding:
--     R = right face weight, G = top face weight, B = left face weight.
--   Ramp maps gradually transition from green (top) to red/blue (side)
--   on the low side of the ramp.
generateSlopeFaceMaps ∷ SlopeFaceMaps
generateSlopeFaceMaps = SlopeFaceMaps
    { sfmFlat  = generateFlatFaceMap
    , sfmNorth = generateRampFaceMap RampNorth
    , sfmEast  = generateRampFaceMap RampEast
    , sfmSouth = generateRampFaceMap RampSouth
    , sfmWest  = generateRampFaceMap RampWest
    }

data RampDirection = RampNorth | RampEast | RampSouth | RampWest

-- | Generate the flat face map (equivalent to isoface.png).
--   Diamond zone (rows 0–47): pure green (top-facing).
--   Side zone (rows 48–63):
--     Left half  → blue (left face)
--     Right half → red (right face)
generateFlatFaceMap ∷ VS.Vector Word8
generateFlatFaceMap = VS.generate (tilePixelWidth * tilePixelHeight * 4) $ \i →
    let px  = i `div` 4
        col = px `mod` tilePixelWidth
        row = px `div` tilePixelWidth
        chan = i `mod` 4  -- 0=R, 1=G, 2=B, 3=A
    in if row < diamondRows
       then -- Diamond zone: pure green (top-facing)
            case chan of
                0 → 0     -- R
                1 → 255   -- G
                2 → 0     -- B
                _ → 255   -- A
       else -- Side wall zone
            let halfW = tilePixelWidth `div` 2
            in if col < halfW
               then case chan of  -- Left side → blue
                   0 → 0
                   1 → 0
                   2 → 255
                   _ → 255
               else case chan of  -- Right side → red
                   0 → 255
                   1 → 0
                   2 → 0
                   _ → 255

-- | Generate a ramp face map for one cardinal direction.
--   The diamond zone gradually transitions from green (top-facing)
--   on the high side to red+blue (side-facing) on the low side.
--   The side wall zone remains unchanged from the flat map.
generateRampFaceMap ∷ RampDirection → VS.Vector Word8
generateRampFaceMap dir = VS.generate (tilePixelWidth * tilePixelHeight * 4) $ \i →
    let px  = i `div` 4
        col = px `mod` tilePixelWidth
        row = px `div` tilePixelWidth
        chan = i `mod` 4
    in if row < diamondRows
       then -- Diamond zone: interpolate based on ramp direction
            let -- Normalize position within diamond to [0..1]
                -- where 0 = high side of ramp, 1 = low side
                t = rampGradient dir col row
                -- Green (top) decreases toward low side
                green = round (255.0 * (1.0 - t * 0.7)) ∷ Int
                -- Red+Blue (side) increase toward low side
                red   = round (255.0 * t * 0.5) ∷ Int
                blue  = round (255.0 * t * 0.5) ∷ Int
            in case chan of
                0 → clampByte red
                1 → clampByte green
                2 → clampByte blue
                _ → 255
       else -- Side wall zone: same as flat
            let halfW = tilePixelWidth `div` 2
            in if col < halfW
               then case chan of
                   0 → 0
                   1 → 0
                   2 → 255
                   _ → 255
               else case chan of
                   0 → 255
                   1 → 0
                   2 → 0
                   _ → 255

-- | Compute gradient [0..1] across the diamond for a ramp direction.
--   0 = high side (stays green/top-lit), 1 = low side (becomes side-lit).
rampGradient ∷ RampDirection → Int → Int → Float
rampGradient dir col row =
    let -- Diamond center
        cx = fromIntegral tilePixelWidth / 2.0 ∷ Float
        cy = fromIntegral diamondRows / 2.0 ∷ Float
        -- Normalized position relative to center [-1..1]
        nx = (fromIntegral col - cx) / cx
        ny = (fromIntegral row - cy) / cy
    in clamp01 $ case dir of
        -- North ramp: low side is toward -Y (top of sprite)
        -- High side is +Y (bottom of diamond)
        RampNorth → (1.0 - ny) / 2.0
        -- South ramp: low side is toward +Y
        RampSouth → (1.0 + ny) / 2.0
        -- East ramp: low side is toward +X (right of sprite)
        RampEast  → (1.0 + nx) / 2.0
        -- West ramp: low side is toward -X (left of sprite)
        RampWest  → (1.0 - nx) / 2.0

clampByte ∷ Int → Word8
clampByte x = fromIntegral (max 0 (min 255 x))

-- | After inserting new chunks into WorldTileData, recompute slopes
--   for any chunk that borders one of the newly-inserted chunks.
--   This patches edge tiles (lx=0/15, ly=0/15) that couldn't see
--   their cross-chunk neighbors during initial generation.
--
--   Also re-runs slopes on the new chunks themselves, since they
--   may now have neighbors that were already loaded.
recomputeNeighborSlopes ∷ Word64             -- ^ World seed
                        → [ChunkCoord]       -- ^ Newly inserted chunk coords
                        → WorldTileData      -- ^ Full tile data (post-insert)
                        → WorldTileData
recomputeNeighborSlopes seed newCoords wtd =
    let chunks = wtdChunks wtd
        -- All chunks that need slope recomputation:
        -- the new chunks themselves + any existing neighbor
        affected = HS.toList $ HS.fromList $
            newCoords <>
            [ neighbor
            | new ← newCoords
            , neighbor ← chunkNeighbors new
            , HM.member neighbor chunks
            ]
        -- Build neighbor lookup from the full chunk map
        neighborLookup coord = case HM.lookup coord chunks of
            Just lc → Just (lcTerrainSurfaceMap lc)
            Nothing → Nothing
        -- Re-run slope computation for each affected chunk
        updatedChunks = foldl' (\acc coord →
            case HM.lookup coord acc of
                Just lc →
                    let newTiles = computeChunkSlopes seed coord
                            (lcTerrainSurfaceMap lc) (lcFluidMap lc)
                            (lcTiles lc) neighborLookup
                    in HM.insert coord (lc { lcTiles = newTiles }) acc
                Nothing → acc
            ) chunks affected
    in wtd { wtdChunks = updatedChunks }

-- | The 4 cardinal neighbor chunk coordinates.
chunkNeighbors ∷ ChunkCoord → [ChunkCoord]
chunkNeighbors (ChunkCoord cx cy) =
    [ ChunkCoord (cx - 1) cy
    , ChunkCoord (cx + 1) cy
    , ChunkCoord cx (cy - 1)
    , ChunkCoord cx (cy + 1)
    ]
