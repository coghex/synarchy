module Engine.Core.Types
  ( BootProfile(..)
  , bootProfileTag
  , EngineConfig(..)
  ) where

import UPrelude

data BootProfile
  = BootNormal
  | BootArena
  | BootPreview
  deriving (Eq, Show)

bootProfileTag ∷ BootProfile → Text
bootProfileTag BootNormal  = "normal"
bootProfileTag BootArena   = "arena"
bootProfileTag BootPreview = "preview"

data EngineConfig = EngineConfig
  { windowWidth     ∷ Int
  , windowHeight    ∷ Int
  , enableVSync     ∷ Bool
  , enableDebug     ∷ Bool
  , ecHeadless      ∷ Bool
  , ecDebugPort     ∷ Int
  , ecBootProfile   ∷ BootProfile
  -- | (category, item) requested via @--preview category[/item]@.
  --   'Nothing' outside 'BootPreview'; item is 'Nothing' for a bare
  --   category (e.g. @--preview icons@).
  , ecPreviewTarget ∷ Maybe (Text, Maybe Text)
  } deriving (Eq, Show)
