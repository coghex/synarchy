module Engine.Core.Types
  ( BootProfile(..)
  , bootProfileTag
  , EngineConfig(..)
  ) where

import UPrelude

data BootProfile
  = BootNormal
  | BootArena
  deriving (Eq, Show)

bootProfileTag ∷ BootProfile → Text
bootProfileTag BootNormal = "normal"
bootProfileTag BootArena  = "arena"

data EngineConfig = EngineConfig
  { windowWidth     ∷ Int
  , windowHeight    ∷ Int
  , enableVSync     ∷ Bool
  , enableDebug     ∷ Bool
  , ecHeadless      ∷ Bool
  , ecDebugPort     ∷ Int
  , ecBootProfile   ∷ BootProfile
  } deriving (Eq, Show)
