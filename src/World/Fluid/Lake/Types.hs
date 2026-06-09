{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, DeriveAnyClass #-}

-- | First-class lakes identified by a global priority flood at
--   world init.
--
--   Every basin has ONE 'lkSurface' computed once globally. At chunk
--   gen, the composer reads this chunk's bitmask for each overlapping
--   lake — a tile is rendered as Lake at 'lkSurface' iff the bitmask
--   bit is set AND the chunk's real terrain is at or below 'lkSurface'.
--   Cross-chunk consistency is automatic because every chunk reads
--   the same global table.
module World.Fluid.Lake.Types
    ( LakeId
    , Lake(..)
    , WorldLakes(..)
    , LakeChunkEntry(..)
    , emptyWorldLakes
    , lakesInChunk
    , packBitmask
    , unpackBitmask
    ) where

import UPrelude
import Control.DeepSeq (NFData(..))
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.Word (Word8)
import GHC.Generics (Generic)
import qualified Data.HashMap.Strict as HM
import qualified Data.Serialize as Serialize
import Data.Serialize (Serialize(..))
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import World.Chunk.Types (ChunkCoord(..))

-- | Index into 'wlLakes'. 0-based.
type LakeId = Int

-- | One global lake. Surface, floor, and area are computed once at
--   world init from the global priority flood; every chunk that
--   touches this lake reads the same surface.
data Lake = Lake
    { lkSurface ∷ !Int
      -- ^ Water surface elevation. Equal to the priority-flood
      --   spillway (no cap applied in Phase 1 — basins fill to rim).
    , lkFloor   ∷ !Int
      -- ^ Lowest terrain elevation inside the lake's footprint.
      --   Recorded for subsurface queries and stats.
    , lkArea    ∷ !Int
      -- ^ Tile count in the lake's footprint.
    , lkBBoxMinX ∷ !Int
    , lkBBoxMinY ∷ !Int
    , lkBBoxMaxX ∷ !Int
    , lkBBoxMaxY ∷ !Int
      -- ^ Tile-coord bounding box (inclusive). Used to fast-filter
      --   which lakes can possibly touch a given chunk.
    } deriving (Show, Eq, Generic, NFData, Serialize)

-- | The lake's overlap with one chunk. 'lceBitmask' is length
--   @chunkSize * chunkSize@ (256), indexed @ly * chunkSize + lx@,
--   True iff the corresponding chunk-local tile is in the lake.
data LakeChunkEntry = LakeChunkEntry
    { lceLakeId  ∷ !LakeId
    , lceBitmask ∷ !(VU.Vector Bool)
    } deriving (Show, Eq, Generic, NFData)

-- | Serialised in bit-packed form: the 256-element 'lceBitmask' is
--   written as 32 bytes (8 tiles per byte), making per-entry overhead
--   ~32 bytes instead of ~256. Round-trips through 'unpackBitmask' /
--   'packBitmask' below.
instance Serialize LakeChunkEntry where
    put e = do
        Serialize.put (lceLakeId e)
        Serialize.put (packBitmask (lceBitmask e))
    get = do
        i     ← Serialize.get
        bytes ← Serialize.get ∷ Serialize.Get [Word8]
        pure LakeChunkEntry
            { lceLakeId  = i
            , lceBitmask = unpackBitmask bytes
            }

-- | Pack a 256-element @VU.Vector Bool@ into 32 'Word8's. Bit @i@
--   (LSB = 0) of byte @b@ encodes tile @b * 8 + i@. Last byte's
--   unused bits are zero. Vectors that aren't exactly 256 still
--   round-trip safely: leftover bits zero on pack, trailing tiles
--   default to False on unpack — but the caller is expected to
--   always pass a 256-element bitmask.
packBitmask ∷ VU.Vector Bool → [Word8]
packBitmask v =
    let n  = VU.length v
        nB = (n + 7) `div` 8
        bitAt i = if i < n ∧ v VU.! i then 1 else 0 ∷ Word8
        byte b =
            let base = b * 8
            in foldr (\i acc → acc `shiftL` 1 .|. bitAt (base + i)) 0 [0 .. 7]
    in [ byte b | b ← [0 .. nB - 1] ]

-- | Inverse of 'packBitmask'. Always produces 256-element output
--   (the on-disk byte list is expected to be 32 bytes); shorter
--   inputs are zero-padded, longer ones truncated.
unpackBitmask ∷ [Word8] → VU.Vector Bool
unpackBitmask bytes =
    let bsV      = VU.fromList (take 32 (bytes ⧺ replicate 32 0))
        byteAt b = bsV VU.! b
    in VU.fromList
        [ (byteAt b `shiftR` i) .&. 1 ≡ 1
        | b ← [0 .. 31]
        , i ← [0 .. 7]
        ]

-- | Global lake table for a world.
data WorldLakes = WorldLakes
    { wlLakes      ∷ !(V.Vector Lake)
      -- ^ All lakes in the world, indexed by 'LakeId'.
    , wlByChunk    ∷ !(HM.HashMap ChunkCoord (V.Vector LakeChunkEntry))
      -- ^ Per-chunk: lakes overlapping the chunk + their bitmasks.
      --   Chunks with no lakes are absent from the map.
    , wlCarveDelta ∷ !(HM.HashMap ChunkCoord (VU.Vector Int))
      -- ^ Per-chunk per-tile carve depth for coastal lakes that have
      --   been clamped to 'seaLevel'. Lowers the basin floor down to
      --   sub-sea so the clamped surface actually finds tiles to fill.
      --   Same shape and intent as 'wrCarveDelta' on 'WorldRivers';
      --   chunk gen combines the two via max per tile.
    } deriving (Show, Eq, Generic, NFData)

instance Serialize WorldLakes where
    put wl = do
        Serialize.put (V.toList (wlLakes wl))
        Serialize.put
            [ (cc, V.toList es)
            | (cc, es) ← HM.toList (wlByChunk wl)
            ]
        Serialize.put
            [ (cc, VU.toList dv)
            | (cc, dv) ← HM.toList (wlCarveDelta wl)
            ]
    get = do
        lks   ← Serialize.get
        rawBy ← Serialize.get ∷ Serialize.Get [(ChunkCoord, [LakeChunkEntry])]
        rawCv ← Serialize.get ∷ Serialize.Get [(ChunkCoord, [Int])]
        pure WorldLakes
            { wlLakes      = V.fromList lks
            , wlByChunk    = HM.fromList
                [ (cc, V.fromList es) | (cc, es) ← rawBy ]
            , wlCarveDelta = HM.fromList
                [ (cc, VU.fromList dv) | (cc, dv) ← rawCv ]
            }

emptyWorldLakes ∷ WorldLakes
emptyWorldLakes = WorldLakes V.empty HM.empty HM.empty

-- | All lake entries for a chunk. Empty vector if the chunk has none.
lakesInChunk ∷ WorldLakes → ChunkCoord → V.Vector LakeChunkEntry
lakesInChunk wl coord =
    case HM.lookup coord (wlByChunk wl) of
        Nothing  → V.empty
        Just es  → es
