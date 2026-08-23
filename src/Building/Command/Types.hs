{-# LANGUAGE Strict #-}
module Building.Command.Types
    ( BuildingCommand(..)
    ) where

import UPrelude
import Building.Types (BuildingId(..))
import World.Page.Types (WorldPageId(..))

data BuildingCommand
    = BuildingSpawn !BuildingId !Text !Int !Int !Int !WorldPageId
                    !(Maybe Word64)
        -- ^ pre-allocated id, defName, anchor gx, gy, gz, owning world
        --   page (stamped from the active world so the building is
        --   world-scoped, #76), and the expected page-SELECTION
        --   generation (#1602).
        --   Placement validation is the caller's responsibility — the
        --   handler trusts these coords. (We do this in the Lua API:
        --   spawn checks canPlaceAt before enqueuing.)
        --
        --   The generation is the exception, and it exists precisely
        --   because enqueuing is not committing: the check
        --   'building.spawn' runs answers the CALLER synchronously, but
        --   this command is applied later, by the building-command drain
        --   ('Building.Thread.Command', which the unit thread runs), and
        --   'wmVisible' can move in between. 'Nothing' means "not a
        --   page-bound placement" (location content-spawning, the AI's
        --   blueprint staking, power nodes) and is never checked; 'Just'
        --   is re-checked against the live 'wmSelectionGen' immediately
        --   before the instance is inserted, so a placement whose page
        --   selection moved after the click is DROPPED rather than
        --   landing on a page the player is no longer looking at.
    | BuildingDestroy !BuildingId
    | BuildingClearAll
        -- ^ Drop every building instance + selection. Enqueued by
        --   world.destroyAll so the clear is ordered AFTER any in-flight
        --   BuildingSpawns on this queue (#58).
    deriving (Show)
