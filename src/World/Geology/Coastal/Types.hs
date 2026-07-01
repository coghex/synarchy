{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, DeriveAnyClass #-}

-- | Global coastal-erosion table — the output of
--   'World.Geology.Coastal.identifyCoastalErosion', which runs ONCE at
--   world init on the stitched pre-coastal terrain and records, per
--   chunk, how coastal processing (contour smoothing + erosion +
--   beach smoothing) changed each tile.  Chunk generation applies the
--   table instead of re-running the windowed coastal pass — adjacent
--   chunks read the same global answer, so the cross-window coastline
--   divergence (12-pass smoother + chained BFS needed ~36 tiles of
--   context against a 14-tile shared border; seam cliffs up to ~18z)
--   is impossible by construction.  Same global-authority pattern as
--   'wrCarveDelta' / 'wlCarveDelta'.
module World.Geology.Coastal.Types
    ( CoastalTable(..)
    , emptyCoastalTable
    ) where

import UPrelude hiding (get)
import Control.DeepSeq (NFData(..))
import GHC.Generics (Generic)
import qualified Data.HashMap.Strict as HM
import qualified Data.Serialize as Serialize
import Data.Serialize (Serialize(..))
import qualified Data.Vector.Unboxed as VU
import World.Chunk.Types (ChunkCoord(..))

-- | Per-chunk coastal deltas, keyed by RAW chunk coords (the
--   'wrCarveDelta' convention — no u-wrap on the keys).  Chunks the
--   coastal pass never touched are absent from both maps.
data CoastalTable = CoastalTable
    { coElevDelta   ∷ !(HM.HashMap ChunkCoord (VU.Vector Int))
      -- ^ @finalElev − preCoastElev@ per tile (chunk-local indexing,
      --   length @chunkSize²@).  Signed: erosion lowers, contour
      --   smoothing can raise a noise dip.  0 = untouched.
    , coMatOverride ∷ !(HM.HashMap ChunkCoord (VU.Vector Word8))
      -- ^ Coastal material rewrite per tile (sand / gravel / wetland /
      --   delta soils).  0 = no override (matAir is never a coastal
      --   material, so 0 is a safe sentinel).
    } deriving (Show, Eq, Generic, NFData)

instance Serialize CoastalTable where
    put ct = do
        Serialize.put
            [ (cc, VU.toList dv)
            | (cc, dv) ← HM.toList (coElevDelta ct) ]
        Serialize.put
            [ (cc, VU.toList mv)
            | (cc, mv) ← HM.toList (coMatOverride ct) ]
    get = do
        rawDv ← Serialize.get ∷ Serialize.Get [(ChunkCoord, [Int])]
        rawMv ← Serialize.get ∷ Serialize.Get [(ChunkCoord, [Word8])]
        pure CoastalTable
            { coElevDelta   = HM.fromList
                [ (cc, VU.fromList dv) | (cc, dv) ← rawDv ]
            , coMatOverride = HM.fromList
                [ (cc, VU.fromList mv) | (cc, mv) ← rawMv ]
            }

emptyCoastalTable ∷ CoastalTable
emptyCoastalTable = CoastalTable HM.empty HM.empty
