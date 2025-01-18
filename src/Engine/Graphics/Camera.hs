-- src/Engine/Graphics/Camera.hs
{-# LANGUAGE Strict #-}
module Engine.Graphics.Camera
    ( Camera2D(..)
    , defaultCamera
    , createViewMatrix
    , createProjectionMatrix
    ) where

import UPrelude
import Linear (M44, V3(..), V4(..), identity)
import qualified Data.Text as T
import Linear.Matrix ((!*!))
import qualified Graphics.UI.GLFW as GLFW
import Engine.Input.Types (InputState(..))

data Camera2D = Camera2D
    { camPosition ∷ (Float, Float)
    , camZoom     ∷ Float
    , camRotation ∷ Float
    } deriving (Show, Eq)

defaultCamera ∷ Camera2D
defaultCamera = Camera2D
    { camPosition = (0, 0)
    , camZoom     = 1.0
    , camRotation = 0.0
    }

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
                            (py * cosθ + px * sinθ)    -- Rotated Y translation
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
