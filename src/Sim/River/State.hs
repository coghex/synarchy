{-# LANGUAGE Strict, UnicodeSyntax #-}
module Sim.River.State
    ( -- * River simulation world
      RiverSimWorld(..)
    , emptyRiverSimWorld
    , initRiverSimWorld
      -- * Per-reach runtime state
    , ReachState(..)
    , defaultReachState
      -- * Boundary conditions
    , UpstreamCondition(..)
    , DownstreamCondition(..)
      -- * Chunk registration
    , registerChunk
    , unregisterChunk
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Vector as V
import World.Chunk.Types (ChunkCoord(..))
import World.River.Graph
    ( RiverGraph(..), RiverRoute(..), RiverNodeId(..)
    , RiverNode(..), NodeKind(..), SinkType(..) )
import World.River.Types (RiverMask, RiverTileInfo(..))
import World.Base (GeoFeatureId(..))

-- * Boundary conditions

-- | What feeds the upstream end of a loaded reach.
data UpstreamCondition
    = TrueHeadwater
        -- ^ The actual headwater is loaded. Use nominal discharge.
    | FrontierSource !ChunkCoord
        -- ^ The real upstream is not loaded. Synthesize inflow
        --   at this chunk's edge crossing. Used when the camera
        --   cuts through a river mid-stream.
    | ConfluenceInflow ![Int]
        -- ^ Upstream routes (indices into rgRoutes) merge here.
        --   Total inflow = sum of upstream discharges.
    deriving (Show, Eq)

-- | What the downstream end of a loaded reach drains into.
data DownstreamCondition
    = OceanDrain
        -- ^ Drains into the ocean (infinite sink).
    | LakeDrain !GeoFeatureId
        -- ^ Drains into a specific lake.
    | FrontierOutflow !ChunkCoord
        -- ^ Drains into an unloaded downstream chunk.
        --   Water leaves freely (no back-pressure).
    | LoadedContinuation !Int
        -- ^ Continues into another loaded reach (index into rgRoutes).
    deriving (Show, Eq)

-- * Per-reach runtime state

-- | Runtime state for one river route that is at least
--   partially within the loaded chunk window.
data ReachState = ReachState
    { rsActive        ∷ !Bool
        -- ^ True if any chunk containing this route is loaded.
    , rsDischarge     ∷ !Float
        -- ^ Current flow rate through this reach.
    , rsUpstream      ∷ !UpstreamCondition
    , rsDownstream    ∷ !DownstreamCondition
    , rsLoadedChunks  ∷ !(HS.HashSet ChunkCoord)
        -- ^ Which loaded chunks contain tiles from this route.
    } deriving (Show, Eq)

defaultReachState ∷ ReachState
defaultReachState = ReachState
    { rsActive       = False
    , rsDischarge    = 0.0
    , rsUpstream     = FrontierSource (ChunkCoord 0 0)
    , rsDownstream   = FrontierOutflow (ChunkCoord 0 0)
    , rsLoadedChunks = HS.empty
    }

-- * River simulation world

-- | The runtime river simulation object. Owns live river
--   water state, separate from per-chunk fluid maps.
--   The graph is immutable (from worldgen); the reach states
--   are mutable (updated as chunks load/unload and the sim
--   ticks).
data RiverSimWorld = RiverSimWorld
    { rswGraph       ∷ !RiverGraph
        -- ^ Immutable river network topology.
    , rswReaches     ∷ !(HM.HashMap Int ReachState)
        -- ^ Per-route runtime state. Key = index into rgRoutes.
    , rswLoadedChunks ∷ !(HS.HashSet ChunkCoord)
        -- ^ All chunks currently loaded in the sim.
    , rswChunkRoutes ∷ !(HM.HashMap ChunkCoord (HS.HashSet Int))
        -- ^ For each loaded chunk, which route indices it contains.
        --   Built from RiverMask data on chunk load.
    } deriving (Show, Eq)

emptyRiverSimWorld ∷ RiverSimWorld
emptyRiverSimWorld = RiverSimWorld
    { rswGraph       = emptyGraph
    , rswReaches     = HM.empty
    , rswLoadedChunks = HS.empty
    , rswChunkRoutes = HM.empty
    }
  where
    emptyGraph = RiverGraph HM.empty V.empty HM.empty HM.empty

-- | Initialize from a river graph. All reaches start inactive.
initRiverSimWorld ∷ RiverGraph → RiverSimWorld
initRiverSimWorld graph = RiverSimWorld
    { rswGraph       = graph
    , rswReaches     = HM.empty
    , rswLoadedChunks = HS.empty
    , rswChunkRoutes = HM.empty
    }

-- * Chunk registration

-- | Register a newly loaded chunk. Scans its RiverMask to
--   find which routes intersect it, then activates those
--   reaches and recomputes their boundary conditions.
registerChunk ∷ ChunkCoord → RiverMask → RiverSimWorld → RiverSimWorld
registerChunk coord mask rsw =
    let -- Find which route indices appear in this chunk's mask
        routeIndices = extractRouteIndices mask
        -- Update chunk tracking
        rsw' = rsw
            { rswLoadedChunks = HS.insert coord (rswLoadedChunks rsw)
            , rswChunkRoutes  = HM.insert coord routeIndices (rswChunkRoutes rsw)
            }
        -- Activate/update each affected reach
        rsw'' = HS.foldl' (\w ri → activateReach ri coord w) rsw' routeIndices
    in rsw''

-- | Unregister a chunk that was evicted. Removes it from
--   all affected reaches and deactivates reaches that no
--   longer have any loaded chunks.
unregisterChunk ∷ ChunkCoord → RiverSimWorld → RiverSimWorld
unregisterChunk coord rsw =
    let affectedRoutes = HM.lookupDefault HS.empty coord (rswChunkRoutes rsw)
        rsw' = rsw
            { rswLoadedChunks = HS.delete coord (rswLoadedChunks rsw)
            , rswChunkRoutes  = HM.delete coord (rswChunkRoutes rsw)
            }
        rsw'' = HS.foldl' (\w ri → deactivateReach ri coord w) rsw' affectedRoutes
    in rsw''

-- * Internal helpers

-- | Scan a RiverMask and collect the set of route indices.
extractRouteIndices ∷ RiverMask → HS.HashSet Int
extractRouteIndices mask =
    V.foldl' (\acc mInfo →
        case mInfo of
            Just info → HS.insert (rtiRouteIndex info) acc
            Nothing   → acc
    ) HS.empty mask

-- | Activate a reach (or add a chunk to an already-active reach).
--   Recomputes boundary conditions based on which chunks are loaded.
activateReach ∷ Int → ChunkCoord → RiverSimWorld → RiverSimWorld
activateReach routeIdx coord rsw
    | routeIdx < 0 ∨ routeIdx ≥ V.length (rgRoutes (rswGraph rsw)) = rsw
    | otherwise =
        let graph  = rswGraph rsw
            route  = rgRoutes graph V.! routeIdx
            existing = HM.lookupDefault defaultReachState routeIdx (rswReaches rsw)
            updated = existing
                { rsActive       = True
                , rsDischarge    = rrFlowRate route
                , rsLoadedChunks = HS.insert coord (rsLoadedChunks existing)
                }
            withBounds = recomputeBoundary routeIdx updated rsw
        in rsw { rswReaches = HM.insert routeIdx withBounds (rswReaches rsw) }

-- | Remove a chunk from a reach. Deactivate if no chunks remain.
deactivateReach ∷ Int → ChunkCoord → RiverSimWorld → RiverSimWorld
deactivateReach routeIdx coord rsw =
    case HM.lookup routeIdx (rswReaches rsw) of
        Nothing → rsw
        Just existing →
            let remaining = HS.delete coord (rsLoadedChunks existing)
            in if HS.null remaining
               then rsw { rswReaches = HM.delete routeIdx (rswReaches rsw) }
               else
                   let updated = existing { rsLoadedChunks = remaining }
                       withBounds = recomputeBoundary routeIdx updated rsw
                   in rsw { rswReaches = HM.insert routeIdx withBounds (rswReaches rsw) }

-- | Recompute upstream/downstream boundary conditions for a reach
--   based on the currently loaded chunk set.
recomputeBoundary ∷ Int → ReachState → RiverSimWorld → ReachState
recomputeBoundary routeIdx state rsw =
    let graph = rswGraph rsw
        route = rgRoutes graph V.! routeIdx
        upNode   = rrUpstream route
        downNode = rrDownstream route
        -- Check upstream: is the source node's chunk loaded?
        upCond = classifyUpstream upNode graph (rswLoadedChunks rsw)
        -- Check downstream: is the sink node's chunk loaded?
        downCond = classifyDownstream downNode graph (rswLoadedChunks rsw)
    in state { rsUpstream = upCond, rsDownstream = downCond }

-- | Classify the upstream condition for a reach.
classifyUpstream ∷ RiverNodeId → RiverGraph → HS.HashSet ChunkCoord → UpstreamCondition
classifyUpstream nodeId graph _loadedChunks =
    case HM.lookup nodeId (rgNodes graph) of
        Nothing → TrueHeadwater  -- shouldn't happen
        Just node → case rnKind node of
            Headwater   → TrueHeadwater
            Confluence  →
                -- Check which upstream routes feed into this node
                let upRoutes = HM.lookupDefault [] nodeId (rgUpstream graph)
                in ConfluenceInflow upRoutes
            LakeOutlet  → TrueHeadwater  -- lake outflow = source
            _           → TrueHeadwater

-- | Classify the downstream condition for a reach.
classifyDownstream ∷ RiverNodeId → RiverGraph → HS.HashSet ChunkCoord → DownstreamCondition
classifyDownstream nodeId graph _loadedChunks =
    case HM.lookup nodeId (rgNodes graph) of
        Nothing → FrontierOutflow (ChunkCoord 0 0)
        Just node → case rnSink node of
            Just OceanSink     → OceanDrain
            Just (LakeSink lid) → LakeDrain lid
            Just InlandSink    → FrontierOutflow (ChunkCoord 0 0)
            Nothing → case rnKind node of
                Mouth → OceanDrain
                _     → FrontierOutflow (ChunkCoord 0 0)
