{-# LANGUAGE Strict #-}
module World.Tile.Types
    ( Tile(..)
    , WorldTileData(..)
    , emptyWorldTileData
    , tileAvailable
    , lookupTileChunk
    , lookupChunk
    , insertChunk
    , evictDistantChunksWithReport
    ) where

import UPrelude
import Control.DeepSeq (NFData(..))
import Data.List (sortOn)
import qualified Data.HashMap.Strict as HM
import World.Chunk.Types (ChunkCoord(..), LoadedChunk(..))
import World.Arena.Bounds (ArenaBounds, arenaAdmitsTile)

data Tile = Tile
    { tileType ∷ Word8
    , tileSlopeId ∷ Word8
    } deriving (Show, Eq)
instance NFData Tile where
    rnf (Tile t s) = rnf t `seq` rnf s

data WorldTileData = WorldTileData
    { wtdChunks    ∷ !(HM.HashMap ChunkCoord LoadedChunk)
    , wtdMaxChunks ∷ !Int
    , wtdBounds    ∷ !(Maybe ArenaBounds)
      -- ^ The page's finite-arena bounds (#2708), carried WITH the terrain
      --   so every reader that holds only a tile snapshot — path
      --   planning, movement, placement, picking — judges availability
      --   against the same rectangle ('tileAvailable'). 'Nothing' is an
      --   expandable arena or a generated world.
      --
      --   A DERIVED CACHE, never an authority: the page's authoritative
      --   bounds are 'World.Generate.Types.wgpArenaBounds', which is what
      --   is persisted. Every site that installs a whole 'WorldTileData'
      --   for a page (fresh creation, staged restoration, a generated
      --   world's initial centre) copies them in before the data becomes
      --   visible to anything, so a tile snapshot can never be live
      --   without its page's rectangle. Chunk inserts, evictions and
      --   writebacks update the record and preserve the field.
    } deriving (Show, Eq)

emptyWorldTileData ∷ WorldTileData
emptyWorldTileData = WorldTileData
    { wtdChunks = HM.empty
    , wtdMaxChunks = 200
    , wtdBounds = Nothing
    }

-- | May terrain exist at this engine tile on this page? Always true
--   unless the page is a finite arena and the tile lies outside its
--   rectangle. This is the AVAILABILITY test; whether a chunk holding
--   the tile is resident is the separate 'lookupChunk' question.
tileAvailable ∷ WorldTileData → Int → Int → Bool
tileAvailable wtd = arenaAdmitsTile (wtdBounds wtd)

-- | The resident chunk holding an engine tile, or 'Nothing' when the
--   tile is outside the page's finite arena (#2708) — however resident
--   the chunk's absent padding is. The one read every tile-addressed
--   query and pick goes through, so none of them can answer about
--   terrain the map does not have.
lookupTileChunk ∷ Int → Int → ChunkCoord → WorldTileData → Maybe LoadedChunk
lookupTileChunk gx gy coord wtd
    | tileAvailable wtd gx gy = lookupChunk coord wtd
    | otherwise               = Nothing

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
