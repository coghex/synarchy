{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Slope
    ( -- * Slope Computation
      computeChunkSlopes
    , recomputeNeighborSlopes
    , patchEdgeStrata
    , chunkNeighbors
      -- * Slope Face Map Generation
    , SlopeFaceMaps(..)
    , generateSlopeFaceMaps
    , generateSideFaceMapLeft
    , generateSideFaceMapRight
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
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V
import World.Types
import World.Material (getMaterialProps, MaterialProps(..), MaterialId(..)
                      , MaterialRegistry(..))

-- * Constants

slopeHardnessThreshold ∷ Float
slopeHardnessThreshold = 0.7

tilePixelWidth ∷ Int
tilePixelWidth = 96

tilePixelHeight ∷ Int
tilePixelHeight = 64

diamondRows ∷ Int
diamondRows = 48

-- * Slope Computation (Post-Processing Pass)

computeChunkSlopes ∷ Word64 → ChunkCoord → VU.Vector Int → MaterialRegistry
                   → V.Vector (Maybe FluidCell) → Chunk
                   → (ChunkCoord → Maybe (VU.Vector Int)) → Chunk
computeChunkSlopes seed coord surfMap registry fluidMap chunk neighborLookup =
    V.imap (\idx col →
        let lx = idx `mod` chunkSize
            ly = idx `div` chunkSize
            surfZ = surfMap VU.! idx
            i = surfZ - ctStartZ col
        in if i ≥ 0 ∧ i < VU.length (ctSlopes col)
           then let newSlope = computeTileSlope seed coord lx ly
                                                surfZ registry
                                                surfMap fluidMap
                                                chunk neighborLookup
                    slopes' = ctSlopes col VU.// [(i, newSlope)]
                in col { ctSlopes = slopes' }
           else col
    ) chunk

computeTileSlope ∷ Word64 → ChunkCoord
                → Int → Int → Int → MaterialRegistry
                → VU.Vector Int
                → V.Vector (Maybe FluidCell)
                → Chunk
                → (ChunkCoord → Maybe (VU.Vector Int))
                → Word8
computeTileSlope seed coord lx ly z registry surfMap fluidMap tiles neighborLookup =
    let col = tiles V.! columnIndex lx ly
        i = z - ctStartZ col
        matId = if i ≥ 0 ∧ i < VU.length (ctMats col)
                then ctMats col VU.! i
                else 0
        props = getMaterialProps registry (MaterialId matId)
        hardness = mpHardness props

        passesHardness = hardness < slopeHardnessThreshold

        neighN = neighborElev coord lx (ly - 1) surfMap neighborLookup
        neighE = neighborElev coord (lx + 1) ly surfMap neighborLookup
        neighS = neighborElev coord lx (ly + 1) surfMap neighborLookup
        neighW = neighborElev coord (lx - 1) ly surfMap neighborLookup

        bitN = slopeBit z neighN lx (ly - 1) coord fluidMap neighborLookup
        bitE = slopeBit z neighE (lx + 1) ly coord fluidMap neighborLookup
        bitS = slopeBit z neighS lx (ly + 1) coord fluidMap neighborLookup
        bitW = slopeBit z neighW (lx - 1) ly coord fluidMap neighborLookup

        rawSlope = (if bitN then 1 else 0)
               .|. (if bitE then 2 else 0)
               .|. (if bitS then 4 else 0)
               .|. (if bitW then 8 else 0) ∷ Word8
    in if not passesHardness ∨ rawSlope ≡ 0 ∨ rawSlope ≡ 15
       then 0
       else applyRoughness seed coord lx ly hardness rawSlope

slopeBit ∷ Int → Int → Int → Int → ChunkCoord
         → V.Vector (Maybe FluidCell)
         → (ChunkCoord → Maybe (VU.Vector Int))
         → Bool
slopeBit myZ neighborZ nlx nly coord fluidMap neighborLookup =
    let diff = myZ - neighborZ
        validDiff = diff ≡ 1

        (normLx, normLy, neighborCoord) = normalizeCoord coord nlx nly
        hasFluid = case neighborCoord of
            c | c ≡ coord ->
                case fluidMap V.! columnIndex normLx normLy of
                    Just _  → True
                    Nothing → False
            _ → False
    in validDiff ∧ not hasFluid

neighborElev ∷ ChunkCoord → Int → Int
             → VU.Vector Int
             → (ChunkCoord → Maybe (VU.Vector Int))
             → Int
neighborElev coord lx ly surfMap neighborLookup
    | lx ≥ 0 ∧ lx < chunkSize ∧ ly ≥ 0 ∧ ly < chunkSize
                       = surfMap VU.! columnIndex lx ly
    | otherwise =
        let (neighborCoord, (nlx, nly)) = normalizeToChunk coord lx ly
        in case neighborLookup neighborCoord of
            Just neighSurf → neighSurf VU.! columnIndex nlx nly
            Nothing        → minBound

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

-- * Probabilistic Roughness

applyRoughness ∷ Word64 → ChunkCoord → Int → Int → Float → Word8 → Word8
applyRoughness seed (ChunkCoord cx cy) lx ly hardness rawSlope
    | hardness < 0.3  = rawSlope
    | otherwise =
        let h = tileHash seed cx cy lx ly
            roll = fromIntegral (h .&. 0xFF) / 255.0 ∷ Float
            roughnessChance = (hardness - 0.3) * 0.75
        in if roll < roughnessChance
           then let dirBits = (h `shiftR` 8) .&. 0x3
                    randomSlope = case dirBits of
                        0 → 1
                        1 → 2
                        2 → 4
                        _ → 8
                in fromIntegral randomSlope
           else rawSlope

tileHash ∷ Word64 → Int → Int → Int → Int → Word64
tileHash seed cx cy lx ly =
    let a = seed `xor` (fromIntegral cx * 2654435761)
        b = a `xor` (fromIntegral cy * 2246822519)
        c = b `xor` (fromIntegral lx * 3266489917)
        d = c `xor` (fromIntegral ly * 668265263)
        e = d `xor` (d `shiftR` 16)
        f = e * 2246822519
        g = f `xor` (f `shiftR` 13)
    in g

-- * Slope → Face Map Index Mapping

slopeToFaceMapIndex ∷ Word8 → Int
slopeToFaceMapIndex  = fromIntegral

-- * Procedural Face Map Generation

data SlopeFaceMaps = SlopeFaceMaps
    { sfmFlat  ∷ !(VS.Vector Word8)
    , sfmNorth ∷ !(VS.Vector Word8)
    , sfmEast  ∷ !(VS.Vector Word8)
    , sfmSouth ∷ !(VS.Vector Word8)
    , sfmWest  ∷ !(VS.Vector Word8)
    } deriving (Show)

generateSlopeFaceMaps ∷ SlopeFaceMaps
generateSlopeFaceMaps = SlopeFaceMaps
    { sfmFlat  = generateFlatFaceMap
    , sfmNorth = generateRampFaceMap RampNorth
    , sfmEast  = generateRampFaceMap RampEast
    , sfmSouth = generateRampFaceMap RampSouth
    , sfmWest  = generateRampFaceMap RampWest
    }

data RampDirection = RampNorth | RampEast | RampSouth | RampWest

generateFlatFaceMap ∷ VS.Vector Word8
generateFlatFaceMap = VS.generate (tilePixelWidth * tilePixelHeight * 4) $ \i →
    let px  = i `div` 4
        col = px `mod` tilePixelWidth
        row = px `div` tilePixelWidth
        chan = i `mod` 4
    in if row < diamondRows
       then case chan of
                0 → 0
                1 → 255
                2 → 0
                _ → 255
       else let halfW = tilePixelWidth `div` 2
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

generateRampFaceMap ∷ RampDirection → VS.Vector Word8
generateRampFaceMap dir = VS.generate (tilePixelWidth * tilePixelHeight * 4) $ \i →
    let px  = i `div` 4
        col = px `mod` tilePixelWidth
        row = px `div` tilePixelWidth
        chan = i `mod` 4
    in if row < diamondRows
       then let t = rampGradient dir col row
                green = round (255.0 * (1.0 - t * 0.7)) ∷ Int
                red   = round (255.0 * t * 0.5) ∷ Int
                blue  = round (255.0 * t * 0.5) ∷ Int
            in case chan of
                0 → clampByte red
                1 → clampByte green
                2 → clampByte blue
                _ → 255
       else let halfW = tilePixelWidth `div` 2
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

rampGradient ∷ RampDirection → Int → Int → Float
rampGradient dir col row =
    let cx = fromIntegral tilePixelWidth / 2.0 ∷ Float
        cy = fromIntegral diamondRows / 2.0 ∷ Float
        nx = (fromIntegral col - cx) / cx
        ny = (fromIntegral row - cy) / cy
    in clamp01 $ case dir of
        RampNorth → (1.0 - ny) / 2.0
        RampSouth → (1.0 + ny) / 2.0
        RampEast  → (1.0 + nx) / 2.0
        RampWest  → (1.0 - nx) / 2.0

clampByte ∷ Int → Word8
clampByte x = fromIntegral (max 0 (min 255 x))

-- * Side Face Maps (left/right only, no top face)

-- | Left side face map: G=0 (no top), B=255 for left-side pixels only.
--   The left side face occupies the bottom-left portion of the tile sprite.
generateSideFaceMapLeft ∷ VS.Vector Word8
generateSideFaceMapLeft = VS.generate (tilePixelWidth * tilePixelHeight * 4) $ \i →
    let px  = i `div` 4
        col = px `mod` tilePixelWidth
        row = px `div` tilePixelWidth
        chan = i `mod` 4
        halfW = tilePixelWidth `div` 2
    in if row < diamondRows
       then 0  -- no top face at all
       else if col < halfW
            then case chan of  -- left side face
                0 → 0
                1 → 0
                2 → 255
                _ → 255
            else 0  -- no right side face

-- | Right side face map: G=0 (no top), R=255 for right-side pixels only.
generateSideFaceMapRight ∷ VS.Vector Word8
generateSideFaceMapRight = VS.generate (tilePixelWidth * tilePixelHeight * 4) $ \i →
    let px  = i `div` 4
        col = px `mod` tilePixelWidth
        row = px `div` tilePixelWidth
        chan = i `mod` 4
        halfW = tilePixelWidth `div` 2
    in if row < diamondRows
       then 0  -- no top face at all
       else if col ≥ halfW
            then case chan of  -- right side face
                0 → 255
                1 → 0
                2 → 0
                _ → 255
            else 0  -- no left side face

-- * Recompute Neighbor Slopes

recomputeNeighborSlopes ∷ Word64 → MaterialRegistry
                        → [ChunkCoord]
                        → WorldTileData
                        → WorldTileData
recomputeNeighborSlopes seed registry newCoords wtd =
    let chunks = wtdChunks wtd
        affected = HS.toList $ HS.fromList $
            newCoords <>
            [ neighbor
            | new ← newCoords
            , neighbor ← chunkNeighbors new
            , HM.member neighbor chunks
            ]
        neighborLookup coord = case HM.lookup coord chunks of
            Just lc → Just (lcTerrainSurfaceMap lc)
            Nothing → Nothing
        updatedChunks = foldl' (\acc coord →
            case HM.lookup coord acc of
                Just lc →
                    let newTiles = computeChunkSlopes seed coord
                            (lcTerrainSurfaceMap lc) registry
                            (lcFluidMap lc) (lcTiles lc) neighborLookup
                    in HM.insert coord (lc { lcTiles = newTiles }) acc
                Nothing → acc
            ) chunks affected
    in wtd { wtdChunks = updatedChunks }

-- | After neighbors load, extend edge column strata downward where
--   the actual cross-chunk neighbor terrain is lower than what the
--   bordered grid showed at generation time. This fixes cliff-face
--   voids at chunk boundaries caused by coastal erosion lowering
--   terrain in one chunk that the adjacent chunk couldn't see.
--
--   The extension repeats the bottom-most material in the existing
--   strata, so the cliff face looks like a natural continuation
--   rather than exposing underground geology.
patchEdgeStrata ∷ [ChunkCoord] → WorldTileData → WorldTileData
patchEdgeStrata newCoords wtd =
    let chunks = wtdChunks wtd
        affected = HS.toList $ HS.fromList $
            newCoords <>
            [ neighbor
            | new ← newCoords
            , neighbor ← chunkNeighbors new
            , HM.member neighbor chunks
            ]
        neighborLookup coord = case HM.lookup coord chunks of
            Just lc → Just (lcTerrainSurfaceMap lc)
            Nothing → Nothing
        updatedChunks = foldl' (\acc coord →
            case HM.lookup coord acc of
                Just lc →
                    let tiles = lcTiles lc
                        tSurf = lcTerrainSurfaceMap lc
                        newTiles = patchChunkEdges coord tSurf neighborLookup tiles
                    in HM.insert coord (lc { lcTiles = newTiles }) acc
                Nothing → acc
            ) chunks affected
    in wtd { wtdChunks = updatedChunks }

-- | Patch edge columns of a single chunk. For each column at the
--   chunk boundary, check if any cross-chunk neighbor is lower than
--   ctStartZ. If so, extend ctMats/ctSlopes/ctVeg downward by
--   repeating the bottom material.
patchChunkEdges ∷ ChunkCoord → VU.Vector Int
                → (ChunkCoord → Maybe (VU.Vector Int))
                → Chunk → Chunk
patchChunkEdges coord tSurf neighborLookup tiles =
    let ChunkCoord cx cy = coord
        -- Get neighbor terrain at a local position, crossing chunks if needed
        getNeighborZ lx ly
            | lx ≥ 0 ∧ lx < chunkSize ∧ ly ≥ 0 ∧ ly < chunkSize
                = tSurf VU.! (ly * chunkSize + lx)
            | otherwise =
                let cx' = cx + (if lx < 0 then -1 else if lx ≥ chunkSize then 1 else 0)
                    cy' = cy + (if ly < 0 then -1 else if ly ≥ chunkSize then 1 else 0)
                    nlx = ((lx `mod` chunkSize) + chunkSize) `mod` chunkSize
                    nly = ((ly `mod` chunkSize) + chunkSize) `mod` chunkSize
                in case neighborLookup (ChunkCoord cx' cy') of
                    Just nSurf → nSurf VU.! (nly * chunkSize + nlx)
                    Nothing → tSurf VU.! (max 0 (min (chunkSize-1) ly)
                                          * chunkSize
                                          + max 0 (min (chunkSize-1) lx))
    in V.imap (\idx col →
        let lx = idx `mod` chunkSize
            ly = idx `div` chunkSize
            isEdge = lx ≡ 0 ∨ lx ≡ chunkSize - 1
                   ∨ ly ≡ 0 ∨ ly ≡ chunkSize - 1
        in if not isEdge
           then col
           else let -- Find the actual minimum neighbor elevation.
                    -- Filter out minBound sentinels (beyond-glacier tiles)
                    -- to prevent integer overflow in the extension calc.
                    nN = getNeighborZ lx (ly - 1)
                    nS = getNeighborZ lx (ly + 1)
                    nE = getNeighborZ (lx + 1) ly
                    nW = getNeighborZ (lx - 1) ly
                    clampSentinel z = if z ≡ minBound then maxBound else z
                    actualMinZ = min (clampSentinel nN) (min (clampSentinel nS)
                                     (min (clampSentinel nE) (clampSentinel nW)))
                    currentStart = ctStartZ col
                in if actualMinZ ≥ currentStart ∨ VU.null (ctMats col)
                   then col  -- No extension needed
                   else let extension = currentStart - actualMinZ
                            bottomMat = ctMats col VU.! 0
                            -- Extend all vectors by repeating bottom values
                            extMats = VU.replicate extension bottomMat
                                      VU.++ ctMats col
                            extSlopes = VU.replicate extension 0
                                        VU.++ ctSlopes col
                            extVeg = VU.replicate extension 0
                                     VU.++ ctVeg col
                        in col { ctStartZ = actualMinZ
                               , ctMats   = extMats
                               , ctSlopes = extSlopes
                               , ctVeg    = extVeg
                               }
        ) tiles

chunkNeighbors ∷ ChunkCoord → [ChunkCoord]
chunkNeighbors (ChunkCoord cx cy) =
    [ ChunkCoord (cx - 1) cy
    , ChunkCoord (cx + 1) cy
    , ChunkCoord cx (cy - 1)
    , ChunkCoord cx (cy + 1)
    ]
