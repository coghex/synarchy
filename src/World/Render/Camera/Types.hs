{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Render.Camera.Types
    ( WorldCameraSnapshot(..)
    , WorldQuadCache(..)
    , WorldCamera(..)
    ) where

import UPrelude
import qualified Data.Vector as V
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
    { wqcCamera ∷ !WorldCameraSnapshot
    , wqcQuads  ∷ !(V.Vector SortableQuad)
    } deriving (Show)

data WorldCamera = WorldCamera
    { wcX ∷ Float
    , wcY ∷ Float
    } deriving (Show, Eq)
