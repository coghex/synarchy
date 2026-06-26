{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, DeriveAnyClass #-}
module World.River.Graph
    ( -- * Types
      RiverGraph(..)
    , RiverRoute(..)
    , RiverNodeId(..)
    , RiverNode(..)
    , NodeKind(..)
    , SinkType(..)
      -- * Construction
    , buildRiverGraph
    , emptyRiverGraph
      -- * Classification (exposed for tests)
    , classifyMouth
    ) where

import UPrelude
import GHC.Generics (Generic)
import Data.Serialize (Serialize)
import Control.DeepSeq (NFData(..))
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import World.Base (GeoCoord(..), GeoFeatureId(..))
import World.Hydrology.Types
    ( RiverParams(..), RiverSegment(..)
    , LakeParams(..), HydroFeature(..) )
import World.Geology.Timeline.Types
    ( GeoTimeline(..), PersistentFeature(..)
    , FeatureShape(..), FeatureActivity(..) )
import Data.Hashable (Hashable(..))
import World.Constants (seaLevel)
import World.Geology.Hash (wrappedDeltaUV)

-- * Node identifiers

-- | Unique identifier for a node in the river graph.
newtype RiverNodeId = RiverNodeId Int
    deriving (Show, Eq, Ord, Generic, Serialize, NFData, Hashable)

-- * Node kinds

-- | What role a node plays in the river network.
data NodeKind
    = Headwater             -- ^ Source of a river (no upstream)
    | Confluence            -- ^ Two or more rivers merge here
    | LakeInlet             -- ^ River enters a lake
    | LakeOutlet            -- ^ River exits a lake
    | Mouth                 -- ^ River reaches ocean or terminal basin
    deriving (Show, Eq, Generic, Serialize, NFData, Hashable)

-- | What kind of terminal drain a mouth connects to.
data SinkType
    = OceanSink             -- ^ Drains into the ocean
    | LakeSink GeoFeatureId -- ^ Drains into a specific lake
    | InlandSink            -- ^ Drains into dry land (river ends)
    deriving (Show, Eq, Generic, Serialize, NFData, Hashable)

-- * River nodes

-- | A node in the river graph. Nodes exist at headwaters,
--   confluences, lake boundaries, and mouths.
data RiverNode = RiverNode
    { rnId       ∷ !RiverNodeId
    , rnKind     ∷ !NodeKind
    , rnCoord    ∷ !GeoCoord     -- ^ Location in global tile coords
    , rnElevation ∷ !Int          -- ^ Terrain elevation at this node
    , rnSink     ∷ !(Maybe SinkType) -- ^ Only set for Mouth nodes
    } deriving (Show, Eq, Generic, Serialize, NFData, Hashable)

-- * River routes (edges between nodes)

-- | A route is a section of river between two nodes.
--   It carries the geometric segments and flow data needed
--   for the sim to compute water surfaces along the route.
data RiverRoute = RiverRoute
    { rrFeatureId  ∷ !GeoFeatureId   -- ^ Which PersistentFeature this came from
    , rrUpstream   ∷ !RiverNodeId    -- ^ Source end of this route
    , rrDownstream ∷ !RiverNodeId    -- ^ Drain end of this route
    , rrSegments   ∷ !(V.Vector RiverSegment)  -- ^ Ordered source→mouth
    , rrFlowRate   ∷ !Float          -- ^ Nominal discharge
    , rrSource     ∷ !GeoCoord       -- ^ First segment start
    , rrMouth      ∷ !GeoCoord       -- ^ Last segment end
    } deriving (Show, Eq, Generic, Serialize, NFData, Hashable)

-- * The graph

-- | The immutable river network graph, built once from worldgen.
--   Contains all active rivers as routes between nodes, plus
--   topology (which routes connect to which nodes).
data RiverGraph = RiverGraph
    { rgNodes      ∷ !(HM.HashMap RiverNodeId RiverNode)
    , rgRoutes     ∷ !(V.Vector RiverRoute)
    , rgUpstream   ∷ !(HM.HashMap RiverNodeId [Int])
        -- ^ Node → indices of routes that DRAIN INTO this node
    , rgDownstream ∷ !(HM.HashMap RiverNodeId [Int])
        -- ^ Node → indices of routes that FLOW FROM this node
    } deriving (Show, Eq, Generic, Serialize, NFData)

-- * Empty graph

emptyRiverGraph ∷ RiverGraph
emptyRiverGraph = RiverGraph HM.empty V.empty HM.empty HM.empty

-- * Construction

-- | Build the river graph from a completed GeoTimeline.
--   Extracts all active river and lake features, identifies
--   nodes (headwaters, mouths, confluences), and links them
--   into routes.
buildRiverGraph ∷ GeoTimeline → RiverGraph
buildRiverGraph timeline =
    let features = gtFeatures timeline
        worldSize = gtWorldSize timeline

        -- Extract active rivers and lakes
        rivers = [ (pfId pf, rp)
                 | pf ← features
                 , isActiveFeature pf
                 , HydroShape (RiverFeature rp) ← [pfFeature pf]
                 ]

        lakes = [ (pfId pf, lk)
                | pf ← features
                , isActiveFeature pf
                , HydroShape (LakeFeature lk) ← [pfFeature pf]
                ]

        -- Build parent→children map for tributary detection
        parentMap = HM.fromListWith (⧺)
            [ (pid, [pfId pf])
            | pf ← features
            , isActiveFeature pf
            , HydroShape (RiverFeature _) ← [pfFeature pf]
            , Just pid ← [pfParentId pf]
            ]

        -- Lake lookup by feature ID for mouth classification
        lakeById = HM.fromList lakes

        -- Assign node IDs. Each river gets a headwater node
        -- and a mouth node. Confluences are detected when a
        -- tributary's mouth is near the parent river's path.
        (nodes, routes, nextId) = foldl' addRiver ([], [], 0) rivers

        addRiver (ns, rs, nid) (fid, rp) =
            let headNode = RiverNode
                    { rnId        = RiverNodeId nid
                    , rnKind      = Headwater
                    , rnCoord     = rpSourceRegion rp
                    , rnElevation = if V.null (rpSegments rp) then 0
                                   else rsStartElev (V.head (rpSegments rp))
                    , rnSink      = Nothing
                    }
                mouthCoord = rpMouthRegion rp
                -- Classify the mouth: ocean, lake, or inland
                mouthElev = if V.null (rpSegments rp) then 0
                            else rsEndElev (V.last (rpSegments rp))
                sink = classifyMouth worldSize mouthElev mouthCoord lakeById
                mouthKind = case sink of
                    OceanSink   → Mouth
                    LakeSink _  → LakeInlet
                    InlandSink  → Mouth
                -- Check if this river has children (tributaries
                -- that merge into it). If so, the mouth of each
                -- tributary is a Confluence node — but that's
                -- handled when we process the tributary itself.
                -- Here we just check if this river IS a tributary.
                isConfluence = HM.member fid parentMap
                headKind = if isConfluence then Confluence else Headwater
                headNode' = headNode { rnKind = headKind }

                mouthNode = RiverNode
                    { rnId        = RiverNodeId (nid + 1)
                    , rnKind      = mouthKind
                    , rnCoord     = mouthCoord
                    , rnElevation = mouthElev
                    , rnSink      = Just sink
                    }
                route = RiverRoute
                    { rrFeatureId  = fid
                    , rrUpstream   = RiverNodeId nid
                    , rrDownstream = RiverNodeId (nid + 1)
                    , rrSegments   = rpSegments rp
                    , rrFlowRate   = rpFlowRate rp
                    , rrSource     = rpSourceRegion rp
                    , rrMouth      = rpMouthRegion rp
                    }
            in (headNode' : mouthNode : ns, route : rs, nid + 2)

        nodeMap = HM.fromList [ (rnId n, n) | n ← nodes ]
        routeVec = V.fromList (reverse routes)

        -- Build adjacency: for each node, which route indices
        -- drain into it (upstream map) and flow from it (downstream map)
        upMap = foldl' (\m (i, r) →
            HM.insertWith (⧺) (rrDownstream r) [i] m
            ) HM.empty (zip [0..] (V.toList routeVec))

        downMap = foldl' (\m (i, r) →
            HM.insertWith (⧺) (rrUpstream r) [i] m
            ) HM.empty (zip [0..] (V.toList routeVec))

    in RiverGraph
        { rgNodes      = nodeMap
        , rgRoutes     = routeVec
        , rgUpstream   = upMap
        , rgDownstream = downMap
        }

-- * Helpers

isActiveFeature ∷ PersistentFeature → Bool
isActiveFeature pf = case pfActivity pf of
    FActive  → True
    FDormant → True
    _        → False

-- | Slack factor applied to a lake's nominal radius when testing
--   whether a river mouth drains into it. A river mouth is recorded at
--   the shoreline (≈ one radius from the lake centre), so we accept a
--   little beyond the nominal radius.
lakeMouthSlack ∷ Float
lakeMouthSlack = 1.25
{-# INLINE lakeMouthSlack #-}

-- | Classify what a river mouth drains into.
--
--   A mouth at or below sea level is an 'OceanSink'. Otherwise we test
--   the mouth against every active lake's footprint (centre + radius,
--   wrap-aware via 'wrappedDeltaUV'): if it lands inside the (slack-
--   scaled) radius of one or more lakes it is a 'LakeSink' draining into
--   the nearest of them. With no lake match an above-sea mouth is an
--   'InlandSink'. This mirrors the centre+radius proximity test used by
--   the lake bowl carve ('World.Hydrology.Event').
classifyMouth ∷ Int → Int → GeoCoord → HM.HashMap GeoFeatureId LakeParams
              → SinkType
classifyMouth worldSize mouthElev mouthCoord lakeById
    | mouthElev ≤ seaLevel = OceanSink
    | otherwise = case findNearestLake of
        Just (_, lid) → LakeSink lid
        Nothing       → InlandSink
  where
    GeoCoord mx my = mouthCoord
    -- Nearest lake (smallest squared distance) whose slack-scaled
    -- radius contains the mouth. foldl' keeps this single-pass.
    findNearestLake = foldl' pickCloser Nothing (HM.toList lakeById)
    pickCloser acc (lid, lk) =
        let GeoCoord lx ly = lkCenter lk
            (dx, dy)       = wrappedDeltaUV worldSize mx my lx ly
            d2             = dx * dx + dy * dy
            r              = round (fromIntegral (lkRadius lk) * lakeMouthSlack) ∷ Int
        in if d2 ≤ r * r
             then case acc of
                    Just (bestD2, _) | bestD2 ≤ d2 → acc
                    _                              → Just (d2, lid)
             else acc
