module Engine.Core.Types
  ( EngineConfig(..)
  ) where

import UPrelude

data EngineConfig = EngineConfig
  { windowWidth     ∷ Int
  , windowHeight    ∷ Int
  , enableVSync     ∷ Bool
  , enableDebug     ∷ Bool
  , ecHeadless      ∷ Bool
  , ecDebugPort     ∷ Int
  } deriving (Eq, Show)

