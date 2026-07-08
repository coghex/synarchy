{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Slope.EdgePatch
    ( patchEdgeStrata
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V
import World.Types
import World.Slope.Compute (chunkNeighbors)

-- * Edge Strata Patching

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
                    -- Neighbor chunk not loaded: clamp to own edge tile,
                    -- which returns this tile's own elevation. This is
                    -- intentional — it means a missing neighbor never
                    -- drives a downward extension. patchEdgeStrata re-runs
                    -- on existing neighbors when new chunks load, so the
                    -- extension will be applied once the neighbor arrives.
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
