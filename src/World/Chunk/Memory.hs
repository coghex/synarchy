-- | Logical resident-data estimates, NOT a retained-heap census. Each field
-- is costed independently, even if buffers/objects are shared. No estimate
-- here may be subtracted from RSS or added to GPU residency. The model and
-- excluded capacity/slack are documented in docs/chunk_memory_measurement.md.
module World.Chunk.Memory
    ( ChunkBytes(..), totalChunkBytes, chunkBytes, wordBytes
    , vectorBytes, boxedMaybeBytes, mapEntryBytes
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.Bits (finiteBitSize)
import World.Chunk.Types (LoadedChunk(..), ColumnTiles(..))
import World.Flora.Types (FloraChunkData(..))
import World.Magma.Overlay (MagmaOverlay(..))

data ChunkBytes = ChunkBytes
    { cbColumns ∷ !Integer
    , cbMaps ∷ !Integer
    , cbOverlays ∷ !Integer
    , cbContainers ∷ !Integer
    } deriving (Eq, Show)

instance Semigroup ChunkBytes where
    ChunkBytes a b c d <> ChunkBytes e f g h = ChunkBytes (a+e) (b+f) (c+g) (d+h)
instance Monoid ChunkBytes where
    mempty = ChunkBytes 0 0 0 0

totalChunkBytes ∷ ChunkBytes → Integer
totalChunkBytes (ChunkBytes a b c d) = a+b+c+d

wordBytes ∷ Integer
wordBytes = toInteger (finiteBitSize (0 ∷ Int) `div` 8)

-- Vector wrapper + array header + word-aligned visible payload. These
-- header allowances are an explicit model, not GHC closure introspection.
vectorBytes ∷ Integer → Int → Integer
vectorBytes elementBytes count = 7 * wordBytes + align (elementBytes * toInteger count)
  where align n = ((n + wordBytes - 1) `div` wordBytes) * wordBytes

-- Static Nothing costs only its vector slot. Present cells include a Just
-- wrapper and the stated record allowance; sharing is intentionally unknown.
boxedMaybeBytes ∷ Integer → V.Vector (Maybe α) → Integer
boxedMaybeBytes recordWords xs = vectorBytes wordBytes (V.length xs)
    + V.foldl' (\n x → n + maybe 0 (const ((recordWords+2)*wordBytes)) x) 0 xs

-- Amortized trie node/leaf/key allowance per map entry, excluding its value.
mapEntryBytes ∷ Integer
mapEntryBytes = 12 * wordBytes

chunkBytes ∷ LoadedChunk → ChunkBytes
chunkBytes lc = ChunkBytes columns maps overlays containers
  where
    columns = vectorBytes wordBytes (V.length (lcTiles lc))
        + V.foldl' (\n c → n + 5*wordBytes
            + vectorBytes 1 (VU.length (ctMats c))
            + vectorBytes 1 (VU.length (ctSlopes c))
            + vectorBytes 1 (VU.length (ctVeg c))) 0 (lcTiles lc)
    maps = sum [vectorBytes wordBytes (VU.length xs)
               | xs ← [lcSurfaceMap lc, lcTerrainSurfaceMap lc, lcWaterTableMap lc]]
        + vectorBytes 1 (VU.length (lcSideDeco lc))
        + boxedMaybeBytes 3 (lcFluidMap lc)
    overlays = boxedMaybeBytes 3 (lcIceMap lc)
        + 2*wordBytes + toInteger (length (fcdInstances (lcFlora lc))) * 20*wordBytes
        + toInteger (HM.size (lcStructures lc)) * (mapEntryBytes + 4*wordBytes)
        + maybe 0 magmaBytes (lcMagma lc)
    magmaBytes m = 5*wordBytes
        + toInteger (HM.size (moBasaltCap m)) * (mapEntryBytes + 2*wordBytes)
        + toInteger (HM.size (moRevealed m)) * mapEntryBytes
    containers = 16*wordBytes + mapEntryBytes
