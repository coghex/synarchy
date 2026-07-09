{-# LANGUAGE UnicodeSyntax #-}

-- | Cursor and designation-tool command handlers, split (issue #564)
--   into focused submodules under "World.Thread.Command.Cursor.*":
--
--     * "World.Thread.Command.Cursor.Common" — shared designation
--       constants.
--     * "World.Thread.Command.Cursor.Select" — raw cursor hover/select
--       state (zoom + world cursor) and direct tile-by-coord selection.
--     * "World.Thread.Command.Cursor.Mine" — mine designation tool.
--     * "World.Thread.Command.Cursor.Construct" — construction
--       designation tool (#95/#96).
--     * "World.Thread.Command.Cursor.Chop" — chop designation tool
--       (#97).
--     * "World.Thread.Command.Cursor.Till" — till designation tool
--       (#333).
--     * "World.Thread.Command.Cursor.Plant" — plant designation tool
--       (#335).
--
--   This module re-exports the public API unchanged.
module World.Thread.Command.Cursor
    ( handleWorldSetZoomCursorHoverCommand
    , handleWorldSetZoomCursorSelectCommand
    , handleWorldSetZoomCursorDeselectCommand
    , handleWorldSetZoomCursorSelectTextureCommand
    , handleWorldSetZoomCursorHoverTextureCommand
    , handleWorldSetWorldCursorHoverCommand
    , handleWorldSetWorldCursorSelectCommand
    , handleWorldSetWorldCursorDeselectCommand
    , handleWorldSetWorldCursorSelectTextureCommand
    , handleWorldSetWorldCursorHoverTextureCommand
    , handleWorldSetWorldCursorSelectBgTextureCommand
    , handleWorldSetWorldCursorHoverBgTextureCommand
    , handleWorldSelectTileByCoordCommand
    , handleWorldSetMineAnchorCommand
    , handleWorldClearMineAnchorCommand
    , handleWorldDesignateMineCommand
    , handleWorldSetMineDesignateTextureCommand
    , handleWorldSetConstructAnchorCommand
    , handleWorldClearConstructAnchorCommand
    , handleWorldDesignateConstructCommand
    , handleWorldCancelConstructCommand
    , handleWorldSetConstructStatusCommand
    , handleWorldAddConstructProgressCommand
    , handleWorldSetConstructDesignateTextureCommand
    , handleWorldSetConstructLineModeCommand
    , handleWorldSetChopAnchorCommand
    , handleWorldClearChopAnchorCommand
    , handleWorldDesignateChopCommand
    , handleWorldCancelChopCommand
    , handleWorldSetChopDesignateTextureCommand
    , handleWorldSetTillAnchorCommand
    , handleWorldClearTillAnchorCommand
    , handleWorldDesignateTillCommand
    , handleWorldCancelTillCommand
    , handleWorldSetTillDesignateTextureCommand
    , handleWorldDesignatePlantCommand
    , handleWorldCancelPlantCommand
    , handleWorldSetPlantDesignateTextureCommand
    ) where

import World.Thread.Command.Cursor.Select
    ( handleWorldSetZoomCursorHoverCommand
    , handleWorldSetZoomCursorSelectCommand
    , handleWorldSetZoomCursorDeselectCommand
    , handleWorldSetZoomCursorSelectTextureCommand
    , handleWorldSetZoomCursorHoverTextureCommand
    , handleWorldSetWorldCursorHoverCommand
    , handleWorldSetWorldCursorSelectCommand
    , handleWorldSetWorldCursorDeselectCommand
    , handleWorldSetWorldCursorSelectTextureCommand
    , handleWorldSetWorldCursorHoverTextureCommand
    , handleWorldSetWorldCursorSelectBgTextureCommand
    , handleWorldSetWorldCursorHoverBgTextureCommand
    , handleWorldSelectTileByCoordCommand
    )
import World.Thread.Command.Cursor.Mine
    ( handleWorldSetMineAnchorCommand
    , handleWorldClearMineAnchorCommand
    , handleWorldDesignateMineCommand
    , handleWorldSetMineDesignateTextureCommand
    )
import World.Thread.Command.Cursor.Construct
    ( handleWorldSetConstructAnchorCommand
    , handleWorldClearConstructAnchorCommand
    , handleWorldDesignateConstructCommand
    , handleWorldCancelConstructCommand
    , handleWorldSetConstructStatusCommand
    , handleWorldAddConstructProgressCommand
    , handleWorldSetConstructDesignateTextureCommand
    , handleWorldSetConstructLineModeCommand
    )
import World.Thread.Command.Cursor.Chop
    ( handleWorldSetChopAnchorCommand
    , handleWorldClearChopAnchorCommand
    , handleWorldDesignateChopCommand
    , handleWorldCancelChopCommand
    , handleWorldSetChopDesignateTextureCommand
    )
import World.Thread.Command.Cursor.Till
    ( handleWorldSetTillAnchorCommand
    , handleWorldClearTillAnchorCommand
    , handleWorldDesignateTillCommand
    , handleWorldCancelTillCommand
    , handleWorldSetTillDesignateTextureCommand
    )
import World.Thread.Command.Cursor.Plant
    ( handleWorldDesignatePlantCommand
    , handleWorldCancelPlantCommand
    , handleWorldSetPlantDesignateTextureCommand
    )
