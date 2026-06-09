{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, DeriveAnyClass #-}

-- | Tile-resolution ocean mask, propagated to chunk gen.
--
--   The ocean placed by 'World.Generate.Chunk.composeFluidMap' was
--   historically decided at CHUNK resolution ('chunkOrNeighborOceanic'
--   — a coarse chunk-flood with median-elevation gating). Where that
--   flood can't propagate through a chunk (a chunk-scale land sill, or
--   median gating) but the tiles are genuinely sub-sea and connected
--   to the open ocean, the whole chunk renders DRY — a sea area
--   stopping dead at a chunk boundary (a long-standing bug).
--
--   'computeWorldEdgeOcean' (in "World.Fluid.Lake.Identify") already
--   computes the authoritative TILE-resolution edge-connected ocean,
--   but only used it internally. This module packs that bool grid into
--   a per-chunk bitmask stored on the timeline ('gtWorldOcean') so
--   chunk gen can OR it into the ocean test — filling the dry chunks.
module World.Fluid.OceanMask
    ( WorldOceanMask(..)
    , emptyWorldOceanMask
    , buildWorldOceanMask
    , oceanBitInChunk
    ) where

import UPrelude hiding (get)
import Control.DeepSeq (NFData(..))
import GHC.Generics (Generic)
import qualified Data.HashMap.Strict as HM
import qualified Data.Serialize as Serialize
import Data.Serialize (Serialize(..))
import qualified Data.Vector.Unboxed as VU
import World.Chunk.Types (ChunkCoord(..), chunkSize)
import World.Fluid.Lake.Types (packBitmask, unpackBitmask)

-- | Per-chunk tile-ocean bitmask (chunk-local index @ly*chunkSize+lx@),
--   keyed by RAW chunk coords (the 'wlByChunk' convention). Chunks with
--   no ocean tile are absent.
newtype WorldOceanMask = WorldOceanMask
    { womByChunk ∷ HM.HashMap ChunkCoord (VU.Vector Bool) }
    deriving (Show, Eq, Generic, NFData)

instance Serialize WorldOceanMask where
    put (WorldOceanMask m) =
        Serialize.put [ (cc, packBitmask bm) | (cc, bm) ← HM.toList m ]
    get = do
        raw ← Serialize.get ∷ Serialize.Get [(ChunkCoord, [Word8])]
        pure $ WorldOceanMask $ HM.fromList
            [ (cc, unpackBitmask bytes) | (cc, bytes) ← raw ]

emptyWorldOceanMask ∷ WorldOceanMask
emptyWorldOceanMask = WorldOceanMask HM.empty

-- | Slice the stitched world-ocean bool grid (indexed
--   @(gy+half)*worldTiles + (gx+half)@, the 'stitchWorldTerrain'
--   convention) into per-chunk bitmasks. Only chunks with ≥1 ocean
--   tile are stored.
buildWorldOceanMask ∷ Int → VU.Vector Bool → WorldOceanMask
buildWorldOceanMask worldSize worldOcean =
    let worldTiles = worldSize * chunkSize
        half       = worldTiles `div` 2
        halfChunks = worldSize `div` 2
        chunkArea  = chunkSize * chunkSize
        entryFor (ChunkCoord cx cy) =
            VU.generate chunkArea $ \i →
                let lx = i `mod` chunkSize
                    ly = i `div` chunkSize
                    gxOff = cx * chunkSize + lx + half
                    gyOff = cy * chunkSize + ly + half
                in gxOff ≥ 0 ∧ gxOff < worldTiles
                   ∧ gyOff ≥ 0 ∧ gyOff < worldTiles
                   ∧ worldOcean VU.! (gyOff * worldTiles + gxOff)
    in WorldOceanMask $ HM.fromList
        [ (cc, bm)
        | cx ← [-halfChunks .. halfChunks - 1]
        , cy ← [-halfChunks .. halfChunks - 1]
        , let cc = ChunkCoord cx cy
              bm = entryFor cc
        , VU.or bm
        ]

-- | Per-chunk-local ocean bit lookup. Absent chunk ⇒ all False.
oceanBitInChunk ∷ WorldOceanMask → ChunkCoord → (Int → Bool)
oceanBitInChunk (WorldOceanMask m) coord =
    case HM.lookup coord m of
        Nothing → const False
        Just bm → \i → i ≥ 0 ∧ i < VU.length bm ∧ bm VU.! i
