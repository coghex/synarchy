{-# LANGUAGE Strict #-}
module World.Tile.Types
    ( Tile(..)
    , WorldTileData(..)
    , emptyWorldTileData
    , lookupChunk
    , insertChunk
    , evictDistantChunksWithReport
    ) where

import UPrelude
import Control.DeepSeq (NFData(..))
import Data.List (sortOn)
import qualified Data.HashMap.Strict as HM
import World.Chunk.Types (ChunkCoord(..), LoadedChunk(..))

data Tile = Tile
    { tileType ∷ Word8
    , tileSlopeId ∷ Word8
    } deriving (Show, Eq)
instance NFData Tile where
    rnf (Tile t s) = rnf t `seq` rnf s

data WorldTileData = WorldTileData
    { wtdChunks    ∷ !(HM.HashMap ChunkCoord LoadedChunk)
    , wtdMaxChunks ∷ !Int
    } deriving (Show, Eq)

emptyWorldTileData ∷ WorldTileData
emptyWorldTileData = WorldTileData
    { wtdChunks = HM.empty
    , wtdMaxChunks = 200
    }

lookupChunk ∷ ChunkCoord → WorldTileData → Maybe LoadedChunk
lookupChunk coord wtd = HM.lookup coord (wtdChunks wtd)

insertChunk ∷ LoadedChunk → WorldTileData → WorldTileData
insertChunk lc wtd =
    wtd { wtdChunks = HM.insert (lcCoord lc) lc (wtdChunks wtd) }

-- | Evict chunks that are far from the camera, keeping at most
--   'wtdMaxChunks'. Keeps every chunk within the keep radius and evicts
--   furthest-first beyond that, returning the coords of the evicted
--   chunks. Edited chunks evict freely — the world's edit log preserves
--   their changes, and replay on regeneration restores them.
evictDistantChunksWithReport ∷ ChunkCoord → Int → WorldTileData
                             → (WorldTileData, [ChunkCoord])
evictDistantChunksWithReport (ChunkCoord camCX camCY) keepRadius wtd =
    let chunks = wtdChunks wtd
        maxC   = wtdMaxChunks wtd
    in if HM.size chunks ≤ maxC
       then (wtd, [])
       else
         let keep = HM.filterWithKey (\coord _ →
                 let ChunkCoord cx cy = coord
                     dx = abs (cx - camCX)
                     dy = abs (cy - camCY)
                 in dx ≤ keepRadius ∧ dy ≤ keepRadius
                 ) chunks
             candidates = HM.filterWithKey (\coord _ → not (HM.member coord keep)) chunks
             candidateList = sortOn (\lc →
                 let ChunkCoord cx cy = lcCoord lc
                 in negate (abs (cx - camCX) + abs (cy - camCY))
                 ) (HM.elems candidates)
             roomLeft = max 0 (maxC - HM.size keep)
             kept = take roomLeft candidateList
             evicted = drop roomLeft candidateList
             keptMap = HM.fromList [(lcCoord lc, lc) | lc ← kept]
             evictedCoords = map lcCoord evicted
         in (wtd { wtdChunks = HM.union keep keptMap }, evictedCoords)
