{-# LANGUAGE Strict #-}
module Engine.Scene.Base where

import UPrelude
import qualified Data.Text as T

-- | Unique identifier for scene objects
newtype ObjectId = ObjectId Word32
  deriving (Show, Eq, Ord)

-- | Basic transform for 2D objects
data Transform2D = Transform2D
    { position    ∷ (Float, Float)  -- ^ Position in 2D space
    , rotation    ∷ Float           -- ^ Rotation in radians
    , scale       ∷ (Float, Float)  -- ^ Scale in x and y
    , zIndex      ∷ Float           -- ^ Depth ordering
    } deriving (Show, Eq)

-- | Type of scene object
data ObjectType 
    = SpriteObject
    | ParticleEmitter
    deriving (Show, Eq)

-- | Scene layer identifiers
newtype LayerId = LayerId Word32
  deriving (Show, Eq, Ord)

-- | Helper function for default transform
defaultTransform ∷ Transform2D
defaultTransform = Transform2D
    { position = (0, 0)
    , rotation = 0
    , scale = (1, 1)
    , zIndex = 0
    }
