-- src/Engine/Graphics/Camera.hs
{-# LANGUAGE Strict, UnicodeSyntax #-}
module Engine.Graphics.Camera
    ( Camera2D(..)
    , CameraFacing(..)
    , rotateCW
    , rotateCCW
    , UICamera(..)
    , defaultCamera
    , defaultUICamera
    , createViewMatrix
    , createProjectionMatrix
    , createUIViewMatrix
    , createUIProjectionMatrix
    ) where

import UPrelude
import Linear (M44, V3(..), V4(..), identity)
import qualified Data.Text as T
import Linear.Matrix ((!*!))
import qualified Graphics.UI.GLFW as GLFW
import Engine.Input.Types (InputState(..))

-- | Four camera facings, 90° apart.
-- FaceSouth is the default (current) viewing direction.
data CameraFacing = FaceSouth | FaceWest | FaceNorth | FaceEast
    deriving (Show, Eq, Enum, Bounded)

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
    , camZoom     = 1.0
    , camRotation = 0.0
    , camFacing   = FaceSouth
    , camDragging = False
    , camDragOrigin = (0, 0)
    , camZSlice = 0
    , camZTracking = True
    }

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
