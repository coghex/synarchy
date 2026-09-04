{-# LANGUAGE Strict, DeriveGeneric, DeriveAnyClass #-}
module Engine.Graphics.Camera
    ( Camera2D(..)
    , CameraFacing(..)
    , rotateCW
    , rotateCCW
    , UICamera(..)
    , defaultCamera
    , repairCameraView
    , defaultUICamera
    , createViewMatrix
    , createProjectionMatrix
    , createUIViewMatrix
    , createUIProjectionMatrix
    ) where

import UPrelude
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Linear (M44, V4(..), identity)
import Linear.Matrix ((!*!))

-- | Four camera facings, 90° apart.
-- FaceSouth is the default (current) viewing direction.
data CameraFacing = FaceSouth | FaceWest | FaceNorth | FaceEast
    deriving (Show, Eq, Enum, Bounded, Generic, Serialize)

rotateCW ∷ CameraFacing → CameraFacing
rotateCW FaceSouth = FaceWest
rotateCW FaceWest  = FaceNorth
rotateCW FaceNorth = FaceEast
rotateCW FaceEast  = FaceSouth

rotateCCW ∷ CameraFacing → CameraFacing
rotateCCW FaceSouth = FaceEast
rotateCCW FaceEast  = FaceNorth
rotateCCW FaceNorth = FaceWest
rotateCCW FaceWest  = FaceSouth

data Camera2D = Camera2D
    { camPosition   ∷ (Float, Float)
    , camVelocity   ∷ (Float, Float)
    , camZoom       ∷ Float
    , camZoomVelocity ∷ Float
    , camRotation   ∷ Float
    , camFacing     ∷ CameraFacing
    , camDragging   ∷ Bool
    , camDragOrigin ∷ (Double, Double)
    , camZSlice     ∷ Int
    , camZTracking  ∷ Bool
    } deriving (Show, Eq)

defaultCamera ∷ Camera2D
defaultCamera = Camera2D
    { camPosition = (0, 0)
    , camVelocity = (0, 0)
    , camZoom     = 64.0
    , camZoomVelocity = 0.0
    , camRotation = 0.0
    , camFacing   = FaceSouth
    , camDragging = False
    , camDragOrigin = (0, 0)
    , camZSlice = 0
    , camZTracking = True
    }

-- | Replace a restored camera view whose x, y or zoom is not finite
--   with 'defaultCamera''s, reporting whether it did (#2337).
--
--   All three move together. A view with one poisoned component is not
--   somewhere the player was ever looking — @wrapCoord@ and @clampF@
--   both pass a NaN through unchanged, so a saved non-finite coordinate
--   is the blank view the session was saved in — and keeping the two
--   survivors would frame a place they never chose. Taking the shipped
--   default for the whole view is the one answer that is a position.
--
--   The caller owns the rest of the camera: the saved facing and every
--   value staging derives (z-slice, the zeroed velocities) are untouched
--   here, and a finite view is returned byte-for-byte with @False@, so a
--   healthy save is silent.
repairCameraView ∷ (Float, Float, Float) → ((Float, Float, Float), Bool)
repairCameraView view@(x, y, zoom)
    | all finite [x, y, zoom] = (view, False)
    | otherwise               = ((dx, dy, camZoom defaultCamera), True)
  where
    finite v = not (isNaN v ∨ isInfinite v)
    (dx, dy) = camPosition defaultCamera

data UICamera = UICamera
    { uiCamWidth  ∷ Float
    , uiCamHeight ∷ Float
    } deriving (Show, Eq)

defaultUICamera ∷ Float → Float → UICamera
defaultUICamera width height = UICamera
    { uiCamWidth  = width
    , uiCamHeight = height
    }

-- | UI camera view matrix (identity - no transformation)
createUIViewMatrix ∷ UICamera → M44 Float
createUIViewMatrix _ = 
    V4 (V4 1 0 0 0)
       (V4 0 1 0 0)
       (V4 0 0 1 0)
       (V4 0 0 0 1)

-- | UI camera projection matrix (pixel coordinates, origin at top-left, Y down - Vulkan style)
-- Vulkan NDC: X [-1,1] left to right, Y [-1,1] top to bottom
createUIProjectionMatrix ∷ UICamera → M44 Float
createUIProjectionMatrix uiCam
    -- Zero-size framebuffer (minimized window): the 2/(right-left) and
    -- 2/(bottom-top) scales below are 2/width and 2/height, which would
    -- write Infinity/NaN into the per-frame UBO. Hand back identity until
    -- a real size arrives on restore.
    | uiCamWidth uiCam ≤ 0 ∨ uiCamHeight uiCam ≤ 0 = identity
createUIProjectionMatrix uiCam =
    let width  = uiCamWidth uiCam
        height = uiCamHeight uiCam

        -- For Vulkan: Y=0 (top) -> NDC -1, Y=height (bottom) -> NDC +1
        -- So we use top=0, bottom=height but need to flip the sign
        left   = 0
        right  = width
        top    = 0
        bottom = height
        near   = -1
        far    = 1
        
        -- Column-major: each V4 is a COLUMN
    in V4 (V4 (2/(right-left))  0                   0   0)   -- Column 0
          (V4  0                (2/(bottom-top))    0   0)   -- Column 1: positive Y scale
          (V4  0                 0                  (2/(far-near))   0)   -- Column 2
          (V4 (-(right+left)/(right-left))  (-(bottom+top)/(bottom-top))  (-(far+near)/(far-near))  1)  -- Column 3

createViewMatrix ∷ Camera2D → M44 Float
createViewMatrix camera =
    let (px, py) = camPosition camera
        rot = camRotation camera
        cosθ = cos rot
        sinθ = sin rot
        
        -- First create translation matrix with rotated position
        translateMat = V4 (V4 1 0 0 0)
                        (V4 0 1 0 0)
                        (V4 0 0 1 0)
                        (V4 (-px * cosθ + py * sinθ)    -- Rotated X translation
                            (-py * cosθ - px * sinθ)    -- Rotated Y translation
                            0 1)
        
        -- Then create rotation matrix
        rotationMat = V4 (V4  cosθ  (-sinθ) 0 0)
                        (V4  sinθ   cosθ    0 0)
                        (V4  0      0       1 0)
                        (V4  0      0       0 1)
                        
        -- Apply rotation then translation
    in rotationMat !*! translateMat

createProjectionMatrix ∷ Camera2D → Float → Float → M44 Float
createProjectionMatrix _camera width height
    -- Zero-size framebuffer (minimized window): aspect = width/height and
    -- the 2/(right-left) scale would feed Infinity/NaN into the per-frame
    -- UBO (height 0 → NaN, width 0 → a degenerate centerline projection).
    -- Hand back identity until a real size arrives on restore.
    | width ≤ 0 ∨ height ≤ 0 = identity
createProjectionMatrix camera width height =
    let aspect = width / height
        zoom = max 0.1 (camZoom camera)  -- Prevent zero or negative zoom
        left   = -zoom * aspect
        right  = zoom * aspect
        bottom = -zoom
        top    = zoom
        near   = -1
        far    = 1
        
    in V4 (V4 (2/(right-left))  0                  0                (-(right+left)/(right-left)))
          (V4  0                (2/(top-bottom))    0                (-(top+bottom)/(top-bottom)))
          (V4  0                 0                 (2/(far-near))    (-(far+near)/(far-near)))
          (V4  0                 0                  0                 1)
