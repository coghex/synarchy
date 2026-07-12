{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, DeriveAnyClass #-}
module World.Geology.Timeline.Event
    ( GeoEvent(..)
    , isRiverCarveEvent
    , OreSheetParams(..)
    , RiverSegmentCarve(..)
    , RiverDeltaParams(..)
    , explodeRiverEvent
    ) where

import UPrelude hiding (get)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.Serialize (Serialize(..))
import Data.Hashable (Hashable)
import World.Base (GeoFeatureId(..))
import World.Geology.Ore.Types (OreSheetParams(..))
import World.Geology.Timeline.Feature
    ( CraterParams(..)
    , FeatureShape(..)
    , FeatureEvolution(..)
    , LandslideParams(..)
    , FloodParams(..)
    )
import World.Hydrology.Types
    ( HydroFeature(..)
    , HydroEvolution
    , GlacierParams(..)
    , GlacierMoraineParams(..)
    , RiverSegment(..)
    )

-- * Per-segment river carving event data

-- | A single river segment's carving parameters.
--   Replaces the old whole-river HydroEvent (RiverFeature river)
--   at the event level. Each segment gets its own tight bbox.
data RiverSegmentCarve = RiverSegmentCarve
    { rscSegment     ∷ !RiverSegment
    } deriving (Show, Eq, Generic, Serialize, Hashable, NFData)

-- | Delta deposit at the river mouth. Extracted from the last
--   segment so it gets its own small bbox around the mouth.
data RiverDeltaParams = RiverDeltaParams
    { rdpLastSegment ∷ !RiverSegment  -- ^ Last segment (for mouth position + direction)
    , rdpFlowRate    ∷ !Float         -- ^ Total river flow (drives delta size)
    } deriving (Show, Eq, Generic, Serialize, Hashable, NFData)

-- * GeoEvent — now with per-segment river events

data GeoEvent
    = CraterEvent !CraterParams
    | VolcanicEvent !FeatureShape
    | VolcanicModify !GeoFeatureId !FeatureEvolution
    | LandslideEvent !LandslideParams
    | GlaciationEvent !GlacierParams
    | FloodEvent !FloodParams
    | HydroEvent !HydroFeature
    | HydroModify !GeoFeatureId !HydroEvolution
    | RiverSegmentEvent !RiverSegmentCarve
    | RiverDeltaEvent   !RiverDeltaParams
    | OreSheetEvent     !OreSheetParams
      -- ^ Flow-routed sedimentary ore deposit (appended in save v30:
      --   'Generic Serialize' is positional by constructor tag).
    | GlacierMoraineEvent !GlacierMoraineParams
      -- ^ Retreat/melt moraine deposit from pre-change glacier geometry.
      --   Appended last (save v53): 'Generic Serialize' is positional by
      --   constructor tag.
    deriving (Show, Eq, Generic, Serialize, Hashable, NFData)

-- * Explode a river HydroEvent into per-segment events

-- | Previously split rivers into per-segment events for tighter bbox
--   filtering, but this caused gaps at waypoint joints where adjacent
--   segments don't overlap. Now keeps rivers as a single HydroEvent
--   so applyRiverCarve's findDeepestCarve considers all segments
--   simultaneously, producing gap-free valleys.
explodeRiverEvent ∷ GeoEvent → [GeoEvent]
explodeRiverEvent evt = [evt]

-- * River-carve event predicate

-- | True for events that carve river/glacier valleys into terrain.
--   Single source shared by the timeline build (cache pre-filter,
--   "World.Geology.Timeline") and the chunk apply pass (final river
--   re-carve, "World.Generate.Timeline") so the two consumers can't
--   diverge as 'GeoEvent' grows.
isRiverCarveEvent ∷ GeoEvent → Bool
isRiverCarveEvent (HydroEvent (RiverFeature _))   = True
isRiverCarveEvent (HydroEvent (GlacierFeature _)) = True
isRiverCarveEvent (RiverSegmentEvent _)           = True
isRiverCarveEvent (RiverDeltaEvent _)             = True
isRiverCarveEvent _                               = False
