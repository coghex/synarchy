{-# LANGUAGE Strict #-}
module Building.Command.Types
    ( BuildingCommand(..)
    ) where

import UPrelude
import Building.Types (BuildingId(..))
import World.Page.Types (WorldPageId(..))

data BuildingCommand
    = BuildingSpawn !BuildingId !Text !Int !Int !Int !WorldPageId
        -- ^ pre-allocated id, defName, anchor gx, gy, gz, owning world
        --   page (stamped from the active world so the building is
        --   world-scoped, #76).
        --   Placement validation is the caller's responsibility — the
        --   handler trusts these coords. (We do this in the Lua API:
        --   spawn checks canPlaceAt before enqueuing.)
        --
        --   A PAGE-BOUND placement (#1602) never reaches this queue at
        --   all: it goes to the world thread as
        --   'World.Command.Types.WorldSpawnBoundBuilding', which
        --   discharges the binding and inserts the instance itself,
        --   where page selection is actually owned. Both paths run the
        --   same 'Building.Thread.Command.applyBuildingSpawn' body, so
        --   this queue stays the route for every UNBOUND spawn without
        --   the two ever diverging.
    | BuildingDestroy !BuildingId
    | BuildingClearAll
        -- ^ Drop every building instance + selection. Enqueued by
        --   world.destroyAll so the clear is ordered AFTER any in-flight
        --   BuildingSpawns on this queue (#58).
    deriving (Show)
