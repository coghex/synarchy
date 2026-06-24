{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Structural pieces — walls, floors, ceilings — placed in the world by
--   the "structures" debug builder. A first, in-memory store (no save
--   support yet); the data model for the RCT-style edge-wall feature.
--
--   Walls sit on one of the 4 diamond edges of a tile, named by the wall
--   sprite suffix (ne/nw/se/sw). Floors and ceilings occupy the tile top.
--   Each piece is a full 96×64 sprite (the wall art is pre-positioned on
--   its edge within the canvas, transparent elsewhere) plus its own
--   facemap, drawn at the tile exactly like a terrain tile.
module Structure.Types
    ( StructureSlot(..)
    , slotFromText
    , StructurePiece(..)
    , StructureStore
    , emptyStructureStore
    , StructurePieceData(..)
    , ChunkStructures
    , emptyChunkStructures
    ) where

import UPrelude
import Control.DeepSeq (NFData(..))
import Data.Word (Word8)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import Engine.Asset.Handle (TextureHandle)

-- | Where a piece sits within a tile.
data StructureSlot
    = SFloor | SCeiling
    | SWallNE | SWallNW | SWallSE | SWallSW
    -- ^ Corner posts — one per tile VERTEX (N/E/S/W of the diamond), placed
    --   per-tile (inset toward the tile centre) so an underground wall pressed
    --   against terrain keeps its post inside the tile, not on the shared node.
    | SPostN | SPostE | SPostS | SPostW
    deriving (Show, Eq, Ord, Enum, Bounded)

-- | Parse the Lua-facing slot name. Matches the texture suffixes.
slotFromText ∷ Text → Maybe StructureSlot
slotFromText t = case T.toLower t of
    "floor"   → Just SFloor
    "ceiling" → Just SCeiling
    "wall_ne" → Just SWallNE
    "wall_nw" → Just SWallNW
    "wall_se" → Just SWallSE
    "wall_sw" → Just SWallSW
    "post_n"  → Just SPostN
    "post_e"  → Just SPostE
    "post_s"  → Just SPostS
    "post_w"  → Just SPostW
    _         → Nothing

data StructurePiece = StructurePiece
    { spTexture ∷ !TextureHandle   -- ^ the 96×64 sprite
    , spFaceMap ∷ !TextureHandle   -- ^ its facemap (sun shading)
    , spGridZ   ∷ !Int             -- ^ world z it sits at
    } deriving (Show, Eq)

-- | Global store, keyed by (gx, gy, slot-tag via fromEnum). One piece
--   per (tile, slot). Mirrors the building manager's flat instance map.
type StructureStore = HM.HashMap (Int, Int, Int) StructurePiece

emptyStructureStore ∷ StructureStore
emptyStructureStore = HM.empty

-- | PURE, per-chunk structure piece — texture identity by PALETTE ID (not a
--   runtime handle), so it can be produced by the pure replay (`applyEdit`) and
--   live inside a 'LoadedChunk'. The renderer resolves the ids → handles via
--   the texture palette. (Contrast 'StructurePiece', which holds resolved
--   handles for the old in-memory global store.)
data StructurePieceData = StructurePieceData
    { spdTexId  ∷ !Int   -- ^ palette id of the sprite texture
    , spdFaceId ∷ !Int   -- ^ palette id of the facemap
    , spdGridZ  ∷ !Int   -- ^ world z it sits at
    } deriving (Show, Eq)

instance NFData StructurePieceData where
    rnf (StructurePieceData a b c) = rnf a `seq` rnf b `seq` rnf c

-- | A chunk's structure overlay: piece keyed by (gx, gy, slot-tag = fromEnum
--   slot). Built by replaying the chunk's structure edits; lives in
--   'LoadedChunk' so it evicts and reloads with the chunk.
type ChunkStructures = HM.HashMap (Int, Int, Word8) StructurePieceData

emptyChunkStructures ∷ ChunkStructures
emptyChunkStructures = HM.empty
