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
        rotationMat = V4 (V4 cosθ (-sinθ) 0 0)
                        (V4 sinθ cosθ  0 0)
                        (V4 0    0     1 0)
                        (V4 0    0     0 1)
        translateMat = V4 (V4 1 0 0 (-px))
                        (V4 0 1 0 (-py))
                        (V4 0 0 1 0)
                        (V4 0 0 0 1)
    in rotationMat !*! translateMat

createProjectionMatrix ∷ Camera2D → Float → Float → M44 Float
createProjectionMatrix camera width height =
    let aspect = width / height
        zoom = camZoom camera
        left   = -zoom * aspect
        right  = zoom * aspect
        bottom = -zoom
        top    = zoom
        near   = 0.1
        far    = 100.0
        x =  2.0 / (right - left)
        y =  2.0 / (top - bottom)
        z = -2.0 / (far - near)
        tx = -(right + left) / (right - left)
        ty = -(top + bottom) / (top - bottom)
        tz = -(far + near) / (far - near)
    in V4 (V4 x   0   0   tx)
          (V4 0   y   0   ty)
          (V4 0   0   z   tz)
          (V4 0   0   0   1)
