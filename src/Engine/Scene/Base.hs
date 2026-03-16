{-# LANGUAGE Strict #-}
module Engine.Scene.Base where

import UPrelude
import qualified Data.Text as T

newtype ObjectId = ObjectId Word32
  deriving (Show, Eq, Ord)

data Transform2D = Transform2D
    { position    ∷ (Float, Float)  -- ^ Position in 2D space
    , rotation    ∷ Float           -- ^ Rotation in radians
    , scale       ∷ (Float, Float)  -- ^ Scale in x and y
    , zIndex      ∷ Float           -- ^ Depth ordering
    } deriving (Show, Eq)

data NodeType
    = SpriteObject
    | TextObject
    | ParticleEmitter
    deriving (Show, Eq)

newtype LayerId = LayerId Word32
  deriving (Show, Eq, Ord)

defaultTransform ∷ Transform2D
defaultTransform = Transform2D
    { position = (0, 0)
    , rotation = 0
    , scale = (1, 1)
    , zIndex = 0
    }
