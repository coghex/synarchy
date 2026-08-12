{-# LANGUAGE Strict #-}
-- | The IO glue that drives "Power.Types"' pure registry transitions
--   from live engine state (#1206). Kept OUT of "Power.Types" so that
--   module stays pure and free of "World.State.Types" in its import
--   graph — the same split "Building.Knowledge.Live" makes for
--   container knowledge, which is the pattern this follows (#1087).
--
--   __A node's lifetime belongs to its building.__ "Power.Types" says
--   so outright: the building manager is the authority for a node's
--   position, page and lifetime, and the registry only adds the
--   power-specific role plus its parameter. Placement already honours
--   that (@power.placeNode@ spawns the building and registers the node
--   in one step); demolition is the other half, and it lands here.
module Power.Live
    ( retirePowerNodeEverywhere
    ) where

import UPrelude
import Data.IORef (IORef, readIORef, atomicModifyIORef')
import Building.Types (BuildingId(..))
import Power.Types
    (PowerNode(..), PowerNodes, nodeForBuilding, removePowerNode)
import World.State.Types (WorldManager(..), WorldState(..))

-- | Demolition: retire the power node riding @bid@, if it has one.
--
--   Applied to EVERY live page rather than the building's own, for the
--   same two reasons 'Building.Knowledge.Live.forgetContainerEverywhere'
--   is: the caller ("Building.Thread.Command") has already removed the
--   instance by the time this runs, so there is no @biPage@ left to
--   resolve — and a 'BuildingId' comes from one session-global
--   allocator, so it can name a building on at most one page anyway.
--   Page-correct by construction, with no dependence on read\/delete
--   ordering, and nodes on every other page are untouched because no
--   node there rides this id.
--
--   Takes the world-manager ref alone rather than a capability record:
--   retiring a node needs no clock, no items and no acting unit.
--
--   The counter ('Power.Types.pnsNextId') is deliberately left where it
--   is — 'removePowerNode' never rewinds it, so a later placement mints
--   a fresh id rather than reusing a demolished node's.
retirePowerNodeEverywhere ∷ IORef WorldManager → BuildingId → IO ()
retirePowerNodeEverywhere worldsRef bid = do
    wm ← readIORef worldsRef
    forM_ (wmWorlds wm) $ \(_, ws) →
        atomicModifyIORef' (wsPowerNodesRef ws) $ \nodes →
            (retireFor nodes, ())
  where
    -- Loops rather than removing once, so "gone from every observable
    -- surface" holds by construction: @power.getNodeForBuilding@ is
    -- 'nodeForBuilding', so draining until IT reports nothing is
    -- exactly the postcondition, however many rows happen to ride the
    -- id. One node per building is the design, so this normally runs
    -- at most one removal. Terminates because each step deletes a key.
    retireFor ∷ PowerNodes → PowerNodes
    retireFor nodes = case nodeForBuilding bid nodes of
        Nothing   → nodes
        Just node → retireFor (fst (removePowerNode (pnId node) nodes))
