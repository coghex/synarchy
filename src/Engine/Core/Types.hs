module Engine.Core.Types
  ( EngineConfig(..)
  ) where

import UPrelude

-- | Engine configuration
data EngineConfig = EngineConfig
  { windowWidth     ∷ Int
  , windowHeight    ∷ Int
  , enableVSync     ∷ Bool
  , enableDebug     ∷ Bool
  , ecHeadless      ∷ Bool
  } deriving (Eq, Show)

