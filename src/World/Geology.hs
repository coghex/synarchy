{-# LANGUAGE Strict #-}
module World.Geology
    ( -- * Timeline construction
      buildTimeline
      -- * Event application (called from generateChunk)
    , applyGeoEvent
      -- * Types re-export
    , GeoModification(..)
      -- * Logging
    , logTimeline
    , logTimelineSummary
    ) where

import World.Geology.Timeline (buildTimeline)
import World.Geology.Event (applyGeoEvent)
import World.Geology.Crater (applyCrater, generateCraters)
import World.Geology.Types (GeoModification(..))
import World.Geology.Log (logTimeline, logTimelineSummary)
