{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Render.Camera.Types
    ( WorldCameraSnapshot(..)
    , WorldQuadCache(..)
    , WorldCamera(..)
    ) where

import UPrelude
import qualified Data.Map as Map
import qualified Data.Vector as V
import Engine.Scene.Base (LayerId)
import Engine.Scene.Types.Batch (SortableQuad(..))
import Engine.Graphics.Camera (CameraFacing(..))

-- | Snapshot of camera state used to generate cached quads.
--   If the current camera matches this, we can reuse the quads.
data WorldCameraSnapshot = WorldCameraSnapshot
    { wcsPosition ∷ !(Float, Float)
    , wcsZoom     ∷ !Float
    , wcsZSlice   ∷ !Int
    , wcsFbSize   ∷ !(Int, Int)
    , wcsFacing   ∷ !CameraFacing
    } deriving (Show, Eq)

data WorldQuadCache = WorldQuadCache
    { wqcGen    ∷ !Int                       -- ^ Invalidation generation this cache was built at
    , wqcCamera ∷ !WorldCameraSnapshot
    , wqcQuads  ∷ !(Map.Map LayerId (V.Vector SortableQuad))
      -- ^ Pre-grouped by layer, each run pre-sorted by sqSortKey — the
      --   sort happens once here (world thread, cache rebuild) so the
      --   frame loop only linear-merges dynamic quads in (#446).
    } deriving (Show)

data WorldCamera = WorldCamera
    { wcX ∷ Float
    , wcY ∷ Float
    } deriving (Show, Eq)
