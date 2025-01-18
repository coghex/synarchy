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
--        rotationMat = V4 (V4 cosθ (-sinθ) 0 0)
--                        (V4 sinθ cosθ  0 0)
--                        (V4 0    0     1 0)
--                        (V4 0    0     0 1)
--        translateMat = V4 (V4 1 0 0 (-px))
--                        (V4 0 1 0 (-py))
--                        (V4 0 0 1 0)
--                        (V4 0 0 0 1)
--    in translateMat !*! rotationMat
        -- Create a pure 2D transformation matrix
        -- This combines rotation and translation in 2D space
    in V4 (V4  cosθ     sinθ    0  (-px * cosθ - py * sinθ))
          (V4 (-sinθ)    cosθ    0  (px * sinθ - py * cosθ))
          (V4  0         0       1   0)
          (V4  0         0       0   1)

createProjectionMatrix ∷ Camera2D → Float → Float → M44 Float
createProjectionMatrix camera width height =
    let aspect = width / height
        zoom = camZoom camera
        left   = -zoom * aspect
        right  = zoom * aspect
        bottom = -zoom
        top    = zoom
        near   = -1
        far    = 1
        -- Simplified ortho projection for 2D
    in V4 (V4 (2/(right-left)) 0                0              (-(right+left)/(right-left)))
          (V4  0               (2/(top-bottom)) 0              (-(top+bottom)/(top-bottom)))
          (V4  0               0                (-2/(far-near)) (-(far+near)/(far-near)))
          (V4  0               0                0               1)
