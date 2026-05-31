{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, DeriveAnyClass #-}

-- | First-class rivers identified by a global flow-accumulation pass
--   at world init (see "World.Fluid.River.Identify").
--
--   Every river tile has ONE 'rcePerTileSurfZ' computed once globally
--   (quantised carve-z with noise-perturbed boundaries for natural-
--   looking waterfalls). Chunk gen reads this chunk's bitmasks for
--   each overlapping river — a tile renders as 'River' at the stored
--   surface iff the bitmask bit is set AND the chunk's real terrain
--   is at or below that surface. Cross-chunk consistency is automatic
--   because every chunk reads the same global table.
--
--   v1 is 1-tile-wide everywhere. The 'rivFlowRate' field is computed
--   and stored so variable width can be added later without redoing
--   the trace: a future per-tile width field can hang off the same
--   'RiverChunkEntry'.
module World.Fluid.River.Types
    ( RiverId
    , River(..)
    , WorldRivers(..)
    , RiverChunkEntry(..)
    , emptyWorldRivers
    , riversInChunk
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
import World.Fluid.Lake.Types (LakeId)

-- | Index into 'wrRivers'. 0-based.
type RiverId = Int

-- | One global river. A connected chain of D4 steepest-descent tiles
--   whose accumulated flow exceeds the river threshold. The surface
--   z and tile footprint live on the per-chunk entries; this header
--   carries whole-river stats.
data River = River
    { rivFlowRate   ∷ !Int
      -- ^ Peak accumulated flow along the river's path. v1 ignores
      --   this for rendering (every tile is 1 wide), but it's
      --   captured so variable-width can be added without redoing
      --   the trace.
    , rivSourceLake ∷ !(Maybe LakeId)
      -- ^ When the river is spawned by a lake spillway, the source
      --   lake's id. 'Nothing' for precipitation-fed sources.
    , rivSinkLake   ∷ !(Maybe LakeId)
      -- ^ When the river terminates by emptying into a lake (rather
      --   than the ocean or world boundary), the sink lake's id.
    , rivBBoxMinX ∷ !Int
    , rivBBoxMinY ∷ !Int
    , rivBBoxMaxX ∷ !Int
    , rivBBoxMaxY ∷ !Int
      -- ^ Tile-coord bounding box (inclusive). Mirrors 'Lake'.
    } deriving (Show, Eq, Generic, NFData, Serialize)

-- | The river's overlap with one chunk. 'rceBitmask' is length
--   @chunkSize * chunkSize@ (256), indexed @ly * chunkSize + lx@;
--   True iff that tile is part of this river. 'rcePerTileSurfZ' is
--   the same length; for in-bitmask tiles it carries the quantised
--   water surface elevation, for out-of-bitmask tiles its slot is
--   'minBound'.
data RiverChunkEntry = RiverChunkEntry
    { rceRiverId      ∷ !RiverId
    , rceBitmask      ∷ !(VU.Vector Bool)
    , rcePerTileSurfZ ∷ !(VU.Vector Int)
    } deriving (Show, Eq, Generic, NFData)

-- | Bit-packed bitmask form (32 bytes for the 256-tile bitmask, same
--   scheme as 'World.Fluid.Lake.Types.LakeChunkEntry'), plus a raw
--   list of the per-tile surface z values. The pack helpers below
--   mirror 'World.Fluid.Lake.Types' — they're inlined here to keep
--   the River module from depending on Lake internals.
instance Serialize RiverChunkEntry where
    put e = do
        Serialize.put (rceRiverId e)
        Serialize.put (packBitmask (rceBitmask e))
        Serialize.put (VU.toList (rcePerTileSurfZ e))
    get = do
        i      ← Serialize.get
        bytes  ← Serialize.get ∷ Serialize.Get [Word8]
        surfsL ← Serialize.get ∷ Serialize.Get [Int]
        pure RiverChunkEntry
            { rceRiverId      = i
            , rceBitmask      = unpackBitmask bytes
            , rcePerTileSurfZ = VU.fromList surfsL
            }

-- | Pack a 256-element @VU.Vector Bool@ into 32 'Word8's. Bit @i@
--   (LSB = 0) of byte @b@ encodes tile @b * 8 + i@. Mirrors the
--   helper of the same name in 'World.Fluid.Lake.Types'.
packBitmask ∷ VU.Vector Bool → [Word8]
packBitmask v =
    let n  = VU.length v
        nB = (n + 7) `div` 8
        bitAt i = if i < n ∧ v VU.! i then 1 else 0 ∷ Word8
        byte b =
            let base = b * 8
            in foldr (\i acc → acc `shiftL` 1 .|. bitAt (base + i)) 0 [0 .. 7]
    in [ byte b | b ← [0 .. nB - 1] ]

unpackBitmask ∷ [Word8] → VU.Vector Bool
unpackBitmask bytes =
    let bsV      = VU.fromList (take 32 (bytes ⧺ replicate 32 0))
        byteAt b = bsV VU.! b
    in VU.fromList
        [ (byteAt b `shiftR` i) .&. 1 ≡ 1
        | b ← [0 .. 31]
        , i ← [0 .. 7]
        ]

-- | Global river table for a world.
data WorldRivers = WorldRivers
    { wrRivers  ∷ !(V.Vector River)
      -- ^ All rivers, indexed by 'RiverId'.
    , wrByChunk ∷ !(HM.HashMap ChunkCoord (V.Vector RiverChunkEntry))
      -- ^ Per-chunk: rivers overlapping the chunk + their bitmasks
      --   and per-tile surface z. Chunks with no rivers are absent.
    } deriving (Show, Eq, Generic, NFData)

instance Serialize WorldRivers where
    put wr = do
        Serialize.put (V.toList (wrRivers wr))
        Serialize.put
            [ (cc, V.toList es)
            | (cc, es) ← HM.toList (wrByChunk wr)
            ]
    get = do
        rvs   ← Serialize.get
        rawBy ← Serialize.get ∷ Serialize.Get [(ChunkCoord, [RiverChunkEntry])]
        pure WorldRivers
            { wrRivers  = V.fromList rvs
            , wrByChunk = HM.fromList
                [ (cc, V.fromList es) | (cc, es) ← rawBy ]
            }

emptyWorldRivers ∷ WorldRivers
emptyWorldRivers = WorldRivers V.empty HM.empty

-- | All river entries for a chunk. Empty vector if none.
riversInChunk ∷ WorldRivers → ChunkCoord → V.Vector RiverChunkEntry
riversInChunk wr coord =
    case HM.lookup coord (wrByChunk wr) of
        Nothing → V.empty
        Just es → es
