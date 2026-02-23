module World.Tool.Types where

import UPrelude

data ToolMode = DefaultTool | InfoTool | MineTool deriving (Eq, Show)

textToToolMode ∷ Text → ToolMode
textToToolMode "tool_info" = InfoTool
textToToolMode "tool_mine" = MineTool
textToToolMode _      = DefaultTool
