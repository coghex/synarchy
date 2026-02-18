{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Tile.Types
    ( Tile(..)
    , WorldTileData(..)
    , emptyWorldTileData
    , lookupChunk
    , insertChunk
    , chunkCount
    , evictDistantChunks
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

chunkCount ∷ WorldTileData → Int
chunkCount = HM.size . wtdChunks

-- | Evict chunks that are far from the camera, keeping at most wtdMaxChunks.
--   Keeps all chunks within the keep radius, evicts furthest-first beyond that.
--   Never evicts modified chunks (future-proofing for when chunks can be edited).
evictDistantChunks ∷ ChunkCoord → Int → WorldTileData → WorldTileData
evictDistantChunks (ChunkCoord camCX camCY) keepRadius wtd =
    let chunks = wtdChunks wtd
        maxC   = wtdMaxChunks wtd
    in if HM.size chunks ≤ maxC
       then wtd
       else
         let -- Must-keep: modified or within keep radius
             keep = HM.filterWithKey (\coord lc →
                 let ChunkCoord cx cy = coord
                     dx = abs (cx - camCX)
                     dy = abs (cy - camCY)
                 in lcModified lc ∨ (dx ≤ keepRadius ∧ dy ≤ keepRadius)
                 ) chunks
             -- Everything else is a candidate for eviction
             candidates = HM.filterWithKey (\coord _ → not (HM.member coord keep)) chunks
             -- Sort candidates by distance (furthest first), keep only what fits
             candidateList = sortOn (\lc →
                 let ChunkCoord cx cy = lcCoord lc
                 in negate (abs (cx - camCX) + abs (cy - camCY))
                 ) (HM.elems candidates)
             roomLeft = max 0 (maxC - HM.size keep)
             kept = take roomLeft candidateList
             keptMap = HM.fromList [(lcCoord lc, lc) | lc ← kept]
         in wtd { wtdChunks = HM.union keep keptMap }
