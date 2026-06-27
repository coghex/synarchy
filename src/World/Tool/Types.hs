{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module World.Tool.Types where

import UPrelude
import GHC.Generics (Generic)
import Data.Serialize (Serialize)

-- NB: 'Generic Serialize' is positional by constructor tag, so this enum
-- is APPEND-ONLY — new tools go at the END (BuildTool = tag 3) to keep
-- existing 'sdToolMode' saves decoding correctly. See the enum schema
-- policy in CLAUDE.md.
data ToolMode = DefaultTool | InfoTool | MineTool | BuildTool
                deriving (Eq, Show, Generic, Serialize)

textToToolMode ∷ Text → ToolMode
textToToolMode "tool_info"  = InfoTool
textToToolMode "tool_mine"  = MineTool
textToToolMode "tool_build" = BuildTool
textToToolMode _      = DefaultTool
