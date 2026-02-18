{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Ocean.Types
    ( OceanMap
    ) where

import UPrelude
import qualified Data.HashSet as HS
import World.Chunk.Types (ChunkCoord)

-- | Set of all chunk coordinates that are ocean-connected.
--   Computed once at world init via BFS from ocean plate centers.
type OceanMap = HS.HashSet ChunkCoord
