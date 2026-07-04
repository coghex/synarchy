{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, DeriveAnyClass #-}
-- | Power-node registry (#358): the data + pure transitions behind the
--   placeable solar panel / battery. A node is a small record riding a
--   placed 'Building.Types.BuildingId' — the building manager stays the
--   authority for position/page/lifetime, this registry only adds the
--   power-specific role (source/storage) + its parameter (peak watts /
--   capacity Wh). Mirrors 'Craft.Bills': a per-world, id-keyed registry
--   persisted as its own 'WorldPageSave' field, pruned to live buildings
--   on load.
--
--   Wire adjacency / connected-components + energy balance live in
--   'Power.Network' (#360); consumer drain (#361) is still deferred.
--   This registry additionally carries each storage node's own charge
--   ('pnStoredWh'), the one piece of #360 state that must survive save/
--   load — connectivity and the tick's generation/drain numbers are all
--   recomputed fresh from wire + node positions, never persisted.
module Power.Types
    ( PowerRole(..)
    , PowerNodeId(..)
    , PowerNode(..)
    , PowerNodes(..)
    , emptyPowerNodes
    , powerNodeSpecFor
    , addPowerNode
    , removePowerNode
    , lookupPowerNode
    , nodeForBuilding
    , allNodes
    , pruneToBuildings
    ) where

import UPrelude
import GHC.Generics (Generic)
import Data.Hashable (Hashable)
import Data.List (find, sortOn)
import Data.Serialize (Serialize)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Building.Types (BuildingId(..))

-- | What a placed power node does on its future network. Sources
--   generate (scaled by time-of-day, #360); storage banks charge and
--   discharge. Consumers (#361) aren't a node role — they hang
--   requires_power/power_drain off ordinary building defs instead.
data PowerRole = PowerSource | PowerStorage
    deriving (Show, Eq, Generic, Serialize)

-- | Node ids start at 1 (see 'emptyPowerNodes'), same convention as
--   'Craft.Bills.BillId' — 0 never names a real node.
newtype PowerNodeId = PowerNodeId { unPowerNodeId ∷ Word32 }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (Hashable, Serialize)

-- | One placed source/storage node. Field order is load-bearing
--   (positional Generic Serialize — append, don't reorder).
data PowerNode = PowerNode
    { pnId         ∷ !PowerNodeId
    , pnBuilding   ∷ !BuildingId
      -- ^ The placed building this node rides on.
    , pnRole       ∷ !PowerRole
    , pnPeakWatts  ∷ !Float
      -- ^ Nominal output at full sun (#360 scales it by sunAngle).
      --   Meaningful for 'PowerSource'; 0 for storage nodes.
    , pnCapacityWh ∷ !Float
      -- ^ Meaningful for 'PowerStorage'; 0 for source nodes.
    , pnStoredWh   ∷ !Float
      -- ^ Current charge (#360). Meaningful for 'PowerStorage'; always 0
      --   for source nodes (a panel has no charge of its own to report).
      --   Appended field — save v75.
    } deriving (Show, Eq, Generic, Serialize)

-- | The per-world node set. The id counter lives inside so it persists
--   with the nodes — a loaded save can't mint an id that collides with
--   a saved node.
data PowerNodes = PowerNodes
    { pnsNodes  ∷ !(HM.HashMap PowerNodeId PowerNode)
    , pnsNextId ∷ !Word32
    } deriving (Show, Eq, Generic, Serialize)

emptyPowerNodes ∷ PowerNodes
emptyPowerNodes = PowerNodes HM.empty 1

-- | The fixed catalogue of placeable power items: item def name → the
--   node role + its one meaningful parameter (peak watts for a source,
--   capacity Wh for storage). #358's whole placeable set; a future
--   power item just adds a case here, no other wiring changes.
powerNodeSpecFor ∷ Text → Maybe (PowerRole, Float)
powerNodeSpecFor "solar_panel"          = Just (PowerSource,  400)
powerNodeSpecFor "high_voltage_battery" = Just (PowerStorage, 5000)
powerNodeSpecFor _                      = Nothing

-- | Register a new node riding an already-placed building. A freshly
--   placed battery starts empty (pnStoredWh = 0) — it charges up from
--   the network like any other newly-wired storage, rather than
--   starting pre-filled.
addPowerNode ∷ BuildingId → PowerRole → Float → PowerNodes
             → (PowerNodes, PowerNodeId)
addPowerNode bid role param nodes =
    let nid  = PowerNodeId (pnsNextId nodes)
        node = PowerNode
            { pnId         = nid
            , pnBuilding   = bid
            , pnRole       = role
            , pnPeakWatts  = if role ≡ PowerSource  then param else 0
            , pnCapacityWh = if role ≡ PowerStorage then param else 0
            , pnStoredWh   = 0
            }
    in ( nodes { pnsNodes  = HM.insert nid node (pnsNodes nodes)
               , pnsNextId = pnsNextId nodes + 1 }
       , nid )

removePowerNode ∷ PowerNodeId → PowerNodes → (PowerNodes, Bool)
removePowerNode nid nodes
    | HM.member nid (pnsNodes nodes) =
        (nodes { pnsNodes = HM.delete nid (pnsNodes nodes) }, True)
    | otherwise = (nodes, False)

lookupPowerNode ∷ PowerNodeId → PowerNodes → Maybe PowerNode
lookupPowerNode nid = HM.lookup nid . pnsNodes

-- | The node riding a given building, if any (one node per building in
--   this design).
nodeForBuilding ∷ BuildingId → PowerNodes → Maybe PowerNode
nodeForBuilding bid = find ((≡ bid) . pnBuilding) . HM.elems . pnsNodes

-- | Every node, oldest first — the #358 headless-verification surface
--   and a future network-scan's enumeration point.
allNodes ∷ PowerNodes → [PowerNode]
allNodes = sortOn pnId . HM.elems . pnsNodes

-- | Drop nodes whose building isn't in the given id set — the same
--   save-load defense as 'Craft.Bills.pruneToStations' (a node whose
--   building's def was deregistered between sessions would otherwise
--   point at nothing forever).
pruneToBuildings ∷ HS.HashSet BuildingId → PowerNodes → PowerNodes
pruneToBuildings buildings nodes = nodes
    { pnsNodes = HM.filter ((`HS.member` buildings) . pnBuilding)
                           (pnsNodes nodes) }
