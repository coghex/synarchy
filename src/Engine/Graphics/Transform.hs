-- src/Engine/Graphics/Transform.hs
{-# LANGUAGE Strict #-}
module Engine.Graphics.Transform
    ( createModelMatrix
    , applyTransform
    , combineTransforms
    , Transform2D(..)
    , defaultTransform
    ) where

import UPrelude
import Linear
import qualified Data.Text as T
import Engine.Scene.Base (Transform2D(..))

-- | Create model matrix from 2D transform
createModelMatrix ∷ Transform2D → M44 Float
createModelMatrix transform =
    let (px, py) = position transform
        (sx, sy) = scale transform
        rot = rotation transform
        z = zIndex transform
        
        -- Create individual transformation matrices
        translate = mkTranslation $ V3 px py z
        rotate = mkRotation rot
        scale' = mkScale $ V3 sx sy 1
        
        -- Combine transforms in correct order: Scale -> Rotate -> Translate
    in translate !*! rotate !*! scale'
  where
    mkTranslation (V3 x y z) = V4 (V4 1 0 0 x)
                                  (V4 0 1 0 y)
                                  (V4 0 0 1 z)
                                  (V4 0 0 0 1)
    
    mkRotation θ = V4 (V4 (cos θ) (-sin θ) 0 0)
                      (V4 (sin θ) (cos θ)  0 0)
                      (V4 0       0        1 0)
                      (V4 0       0        0 1)
    
    mkScale (V3 x y z) = V4 (V4 x 0 0 0)
                            (V4 0 y 0 0)
                            (V4 0 0 z 0)
                            (V4 0 0 0 1)

-- | Apply one transform to another (for parent-child relationships)
applyTransform ∷ Transform2D → Transform2D → Transform2D
applyTransform parentT childT =
    Transform2D
        { position = (px + cx * cos pr - cy * sin pr,
                     py + cx * sin pr + cy * cos pr)
        , rotation = pr + cr
        , scale = (psx * csx, psy * csy)
        , zIndex = pz + cz
        , parent = parent childT
        }
  where
    (px, py) = position parentT
    (cx, cy) = position childT
    pr = rotation parentT
    cr = rotation childT
    (psx, psy) = scale parentT
    (csx, csy) = scale childT
    pz = zIndex parentT
    cz = zIndex childT

-- | Combine multiple transforms in order
combineTransforms ∷ [Transform2D] → Transform2D
combineTransforms = foldr applyTransform defaultTransform

-- | Default transform with identity values
defaultTransform ∷ Transform2D
defaultTransform = Transform2D
    { position = (0, 0)
    , rotation = 0
    , scale = (1, 1)
    , zIndex = 0
    , parent = Nothing
    }
