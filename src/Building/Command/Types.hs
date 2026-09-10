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
    | BuildingClearPage !WorldPageId !BuildingId
        -- ^ #2476: the building half of 'Unit.Command.Types.UnitClearPage',
        --   with the same page + EXCLUSIVE-CUTOFF pair and the same
        --   producers (a single-page @world.destroy@; either init path
        --   replacing a registered page id). Retires only rows whose
        --   page matches and whose 'BuildingId' is strictly below the
        --   cutoff: instances, destruction effects, outstanding
        --   footprint reservations, and the selection when it names one
        --   of the removed instances.
        --
        --   Reservations are in that list on purpose (#2326): a claim
        --   admitted for the OLD incarnation would otherwise keep tiles
        --   held against the replacement forever, since the request
        --   holding it can no longer commit. A claim at or above the
        --   cutoff belongs to the replacement and is left alone —
        --   including one whose bound spawn already committed on the
        --   world thread ahead of this clear (#1602).
    | BuildingEndSession
        -- ^ The building half of the same Exit-to-Menu boundary
        --   'Unit.Command.Types.UnitEndSession' marks (#2291), and the
        --   same kind of value: a position, not work. Enqueued by
        --   world.destroyAll immediately behind its 'BuildingClearAll',
        --   and 'Building.Thread.Command.processAllBuildingCommands'
        --   stops draining when it takes this off the queue, so a
        --   building command queued after the boundary cannot be stamped
        --   on the outgoing session's clock in the same tick that resets
        --   it.
    deriving (Show)
