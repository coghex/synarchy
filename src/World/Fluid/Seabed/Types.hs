{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, DeriveAnyClass #-}

-- | Global seabed table — the output of
--   'World.Fluid.Seabed.identifySeabed', which runs ONCE at world init
--   on the stitched terrain and records, per chunk, how the ocean-floor
--   pass reshaped each sub-sea tile: a depth-from-shore ramp + gentle
--   noise replacing the flat @seaLevel − 1@ basin carve, plus seabed
--   materials (sand → coarse sand → silt → muck by depth) and
--   occasional bedrock outcrops. Chunk gen applies the table the same
--   way it applies the coastal table, so every chunk agrees on the
--   seabed. Same global-authority pattern as
--   'World.Geology.Coastal.Types.CoastalTable'.
module World.Fluid.Seabed.Types
    ( SeabedTable(..)
    , emptySeabedTable
    ) where

import UPrelude hiding (get)
import Control.DeepSeq (NFData(..))
import GHC.Generics (Generic)
import qualified Data.HashMap.Strict as HM
import qualified Data.Serialize as Serialize
import Data.Serialize (Serialize(..))
import qualified Data.Vector.Unboxed as VU
import World.Chunk.Types (ChunkCoord(..))

-- | Per-chunk seabed deltas, keyed by RAW chunk coords (the
--   'wrCarveDelta' convention). Chunks with no sub-sea tiles are
--   absent from both maps.
data SeabedTable = SeabedTable
    { sbElevDelta   ∷ !(HM.HashMap ChunkCoord (VU.Vector Int))
      -- ^ @seabedTarget − preCarveTerrain@ per tile (chunk-local
      --   indexing, length @chunkSize²@). Signed but almost always
      --   ≤ 0 (only-lower ramp); an outcrop bump in a shallow basin
      --   reads as a smaller carve, not a positive delta. 0 = land /
      --   already-deep tile left untouched.
    , sbMatOverride ∷ !(HM.HashMap ChunkCoord (VU.Vector Word8))
      -- ^ Seabed surface material per tile (sand / coarse sand / silt
      --   / muck / bedrock). 0 = no override (matAir is never a seabed
      --   material, so 0 is a safe sentinel).
    } deriving (Show, Eq, Generic, NFData)

instance Serialize SeabedTable where
    put st = do
        Serialize.put
            [ (cc, VU.toList dv)
            | (cc, dv) ← HM.toList (sbElevDelta st) ]
        Serialize.put
            [ (cc, VU.toList mv)
            | (cc, mv) ← HM.toList (sbMatOverride st) ]
    get = do
        rawDv ← Serialize.get ∷ Serialize.Get [(ChunkCoord, [Int])]
        rawMv ← Serialize.get ∷ Serialize.Get [(ChunkCoord, [Word8])]
        pure SeabedTable
            { sbElevDelta   = HM.fromList
                [ (cc, VU.fromList dv) | (cc, dv) ← rawDv ]
            , sbMatOverride = HM.fromList
                [ (cc, VU.fromList mv) | (cc, mv) ← rawMv ]
            }

emptySeabedTable ∷ SeabedTable
emptySeabedTable = SeabedTable HM.empty HM.empty
