{-# LANGUAGE Strict #-}
module Engine.Graphics.Sprite.Math
  ( Mat4(..)
  , createTransformMatrix
  , createViewMatrix
  , createProjectionMatrix
  , (⊗)  -- Matrix multiplication
  , Vec2(..)
  , Vec4(..)
  ) where

import UPrelude
import Engine.Graphics.Sprite.Types (Vec2(..), Vec4(..))

-- | 4x4 Matrix stored in column-major order (same as OpenGL/Vulkan)
data Mat4 = Mat4
  { m00 ∷ Float, m01 ∷ Float, m02 ∷ Float, m03 ∷ Float  -- First column
  , m10 ∷ Float, m11 ∷ Float, m12 ∷ Float, m13 ∷ Float  -- Second column
  , m20 ∷ Float, m21 ∷ Float, m22 ∷ Float, m23 ∷ Float  -- Third column
  , m30 ∷ Float, m31 ∷ Float, m32 ∷ Float, m33 ∷ Float  -- Fourth column
  } deriving (Show, Eq)

-- | Identity matrix
identity ∷ Mat4
identity = Mat4
  1 0 0 0
  0 1 0 0
  0 0 1 0
  0 0 0 1

-- | Matrix multiplication
(⊗) ∷ Mat4 → Mat4 → Mat4
(⊗) a b = Mat4
  (m00 a * m00 b + m10 a * m01 b + m20 a * m02 b + m30 a * m03 b)
  (m01 a * m00 b + m11 a * m01 b + m21 a * m02 b + m31 a * m03 b)
  (m02 a * m00 b + m12 a * m01 b + m22 a * m02 b + m32 a * m03 b)
  (m03 a * m00 b + m13 a * m01 b + m23 a * m02 b + m33 a * m03 b)
  
  (m00 a * m10 b + m10 a * m11 b + m20 a * m12 b + m30 a * m13 b)
  (m01 a * m10 b + m11 a * m11 b + m21 a * m12 b + m31 a * m13 b)
  (m02 a * m10 b + m12 a * m11 b + m22 a * m12 b + m32 a * m13 b)
  (m03 a * m10 b + m13 a * m11 b + m23 a * m12 b + m33 a * m13 b)
  
  (m00 a * m20 b + m10 a * m21 b + m20 a * m22 b + m30 a * m23 b)
  (m01 a * m20 b + m11 a * m21 b + m21 a * m22 b + m31 a * m23 b)
  (m02 a * m20 b + m12 a * m21 b + m22 a * m22 b + m32 a * m23 b)
  (m03 a * m20 b + m13 a * m21 b + m23 a * m22 b + m33 a * m23 b)
  
  (m00 a * m30 b + m10 a * m31 b + m20 a * m32 b + m30 a * m33 b)
  (m01 a * m30 b + m11 a * m31 b + m21 a * m32 b + m31 a * m33 b)
  (m02 a * m30 b + m12 a * m31 b + m22 a * m32 b + m32 a * m33 b)
  (m03 a * m30 b + m13 a * m31 b + m23 a * m32 b + m33 a * m33 b)

-- | Create transform matrix from position, scale, and rotation
createTransformMatrix ∷ Vec2   -- ^ Position
                     → Vec2   -- ^ Scale
                     → Float  -- ^ Rotation in radians
                     → Mat4
createTransformMatrix (Vec2 x y) (Vec2 sx sy) rot =
  let cosR = cos rot
      sinR = sin rot
  in Mat4
    (cosR * sx)  (sinR * sx)  0 0
    (-sinR * sy) (cosR * sy)  0 0
    0            0            1 0
    x            y            0 1

-- | Create view matrix from camera parameters
createViewMatrix ∷ Vec2   -- ^ Camera position
                → Float  -- ^ Camera zoom
                → Float  -- ^ Camera rotation in radians
                → Mat4
createViewMatrix (Vec2 x y) zoom rot =
  let cosR = cos (-rot)
      sinR = sin (-rot)
      iz = 1 / zoom
  in Mat4
    (cosR * iz) (-sinR * iz) 0 0
    (sinR * iz) (cosR * iz)  0 0
    0           0            1 0
    (-x)        (-y)         0 1

-- | Create orthographic projection matrix
createProjectionMatrix ∷ Int    -- ^ Viewport width
                      → Int    -- ^ Viewport height
                      → Mat4
createProjectionMatrix width height =
  let w = fromIntegral width
      h = fromIntegral height
      -- Convert to normalized device coordinates (-1 to 1)
      scaleX = 2 / w
      scaleY = -2 / h  -- Y is flipped in Vulkan
  in Mat4
    scaleX 0      0 0
    0      scaleY 0 0
    0      0      1 0
    -1     1      0 1

-- | Helper function to convert radians to degrees
radToDeg ∷ Float → Float
radToDeg r = r * 180 / pi

-- | Helper function to convert degrees to radians
degToRad ∷ Float → Float
degToRad d = d * pi / 180
