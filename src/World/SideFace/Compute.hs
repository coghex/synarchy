{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.SideFace.Compute
    ( computeChunkSideDecos
    ) where

import UPrelude
import Data.Bits (xor, shiftR)
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V
import World.Chunk.Types (ChunkCoord(..), chunkSize)
import World.Fluid.Types (FluidCell(..))
import World.SideFace.Base (SideDecoType(..), sideDecoBase, sideDecoVariants)

-- | Compute side-face decorations for a chunk.
--   Pure function, called during chunk loading.
--   Checks each column for cliff faces (cardinal neighbor 2+ z-levels lower)
--   and probabilistically assigns vines/moss/mineral decorations.
computeChunkSideDecos ∷ Word64 → ChunkCoord → VU.Vector Int
                      → V.Vector (Maybe FluidCell)
                      → (ChunkCoord → Maybe (VU.Vector Int))
                      → VU.Vector Word8
computeChunkSideDecos seed coord terrainSurfMap fluidMap neighborLookup =
    VU.generate (chunkSize * chunkSize) $ \idx →
        let lx = idx `mod` chunkSize
            ly = idx `div` chunkSize
            myZ = terrainSurfMap VU.! idx
            -- Skip if this tile has fluid (no cliff face visible)
            hasFluid = case fluidMap V.! idx of
                Just _  → True
                Nothing → False
        in if hasFluid ∨ myZ ≤ minBound + 2
           then 0
           else computeColumnDeco seed coord lx ly myZ terrainSurfMap neighborLookup

computeColumnDeco ∷ Word64 → ChunkCoord → Int → Int → Int
                  → VU.Vector Int
                  → (ChunkCoord → Maybe (VU.Vector Int))
                  → Word8
computeColumnDeco seed (ChunkCoord cx cy) lx ly myZ terrainSurfMap neighborLookup =
    let -- Check cardinal neighbors for cliff faces (2+ z-level drop)
        getNeighborZ nx ny
            | nx ≥ 0 ∧ nx < chunkSize ∧ ny ≥ 0 ∧ ny < chunkSize
                = terrainSurfMap VU.! (ny * chunkSize + nx)
            | otherwise =
                let cx' = cx + (if nx < 0 then -1 else if nx ≥ chunkSize then 1 else 0)
                    cy' = cy + (if ny < 0 then -1 else if ny ≥ chunkSize then 1 else 0)
                    nlx = ((nx `mod` chunkSize) + chunkSize) `mod` chunkSize
                    nly = ((ny `mod` chunkSize) + chunkSize) `mod` chunkSize
                in case neighborLookup (ChunkCoord cx' cy') of
                    Just nSurf → nSurf VU.! (nly * chunkSize + nlx)
                    Nothing    → myZ  -- assume same height if neighbor not loaded

        nN = getNeighborZ lx (ly - 1)
        nE = getNeighborZ (lx + 1) ly
        nS = getNeighborZ lx (ly + 1)
        nW = getNeighborZ (lx - 1) ly

        -- Any neighbor 2+ levels lower means a cliff face exists
        hasCliff = (myZ - nN ≥ 2) ∨ (myZ - nE ≥ 2)
                 ∨ (myZ - nS ≥ 2) ∨ (myZ - nW ≥ 2)

    in if not hasCliff
       then 0
       else let -- Hash for deterministic selection
                h = tileHash seed cx cy lx ly
                roll = fromIntegral (h .&. 0xFF) / 255.0 ∷ Float
                -- ~40% chance of having a deco on a cliff face
            in if roll > 0.4
               then 0
               else let -- Select type based on hash bits
                        typeBits = (h `shiftR` 8) .&. 0x3
                        decoType = case typeBits of
                            0 → DecoVines
                            1 → DecoMoss
                            2 → DecoMineral
                            _ → DecoVines  -- bias toward vines
                        -- Variant from more hash bits
                        variant = fromIntegral ((h `shiftR` 10) `mod` fromIntegral sideDecoVariants)
                    in sideDecoBase decoType + variant

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
