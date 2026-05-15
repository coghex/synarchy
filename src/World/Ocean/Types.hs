{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Ocean.Types
    ( OceanMap
    , OceanDistMap
    , oceanDistAt
    ) where

import UPrelude
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import World.Chunk.Types (ChunkCoord)

-- | Set of all chunk coordinates that are ocean-connected.
--   Computed once at world init via BFS from ocean plate centers.
type OceanMap = HS.HashSet ChunkCoord

-- | Distance from ocean per chunk (BFS hops). Ocean = 0, coast = 1, etc.
type OceanDistMap = HM.HashMap ChunkCoord Int

-- | Look up distance from ocean for a chunk.
--   Returns maxBound for chunks not reached by BFS.
oceanDistAt ∷ OceanDistMap → ChunkCoord → Int
oceanDistAt dm coord = HM.lookupDefault maxBound coord dm
