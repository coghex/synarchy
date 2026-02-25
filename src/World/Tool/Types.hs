{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module World.Tool.Types where

import UPrelude
import GHC.Generics (Generic)
import Data.Serialize (Serialize)

data ToolMode = DefaultTool | InfoTool | MineTool
                deriving (Eq, Show, Generic, Serialize)

textToToolMode ∷ Text → ToolMode
textToToolMode "tool_info" = InfoTool
textToToolMode "tool_mine" = MineTool
textToToolMode _      = DefaultTool
