{-# LANGUAGE UnicodeSyntax #-}

-- | Live world-edit command handlers, split (issue #563) into focused
--   submodules under "World.Thread.Command.Edit.*":
--
--     * "World.Thread.Command.Edit.Sync" — shared sim re-seed helper.
--     * "World.Thread.Command.Edit.Terrain" — add/delete tile, slope,
--       and arbitrary single-cell edits.
--     * "World.Thread.Command.Edit.Structure" — structure piece
--       set/clear/clear-all.
--     * "World.Thread.Command.Edit.Vegetation" — tile veg id + row-crop
--       planting.
--     * "World.Thread.Command.Edit.Dig" — dig progress + spoil pile
--       promotion.
--     * "World.Thread.Command.Edit.Fluid" — fluid tile placement.
--
--   This module re-exports the public API unchanged.
module World.Thread.Command.Edit
    ( handleWorldAddTileCommand
    , handleWorldDeleteTileCommand
    , handleWorldSetFluidTileCommand
    , handleWorldSetSlopeCommand
    , handleWorldSetVegCommand
    , handleWorldSetCellCommand
    , handleWorldSetStructureCommand
    , handleWorldClearStructureCommand
    , handleWorldClearAllStructuresCommand
    , handleWorldDigTileCommand
    , handleWorldPlantRowCropAtCommand
    ) where

import World.Thread.Command.Edit.Terrain
    ( handleWorldAddTileCommand
    , handleWorldDeleteTileCommand
    , handleWorldSetSlopeCommand
    , handleWorldSetCellCommand
    )
import World.Thread.Command.Edit.Structure
    ( handleWorldSetStructureCommand
    , handleWorldClearStructureCommand
    , handleWorldClearAllStructuresCommand
    )
import World.Thread.Command.Edit.Vegetation
    ( handleWorldSetVegCommand
    , handleWorldPlantRowCropAtCommand
    )
import World.Thread.Command.Edit.Dig (handleWorldDigTileCommand)
import World.Thread.Command.Edit.Fluid (handleWorldSetFluidTileCommand)
