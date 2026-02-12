{-# LANGUAGE Strict #-}
module World.Geology
    ( -- * Timeline construction
      buildTimeline
      -- * Event application (called from generateChunk)
    , applyGeoEvent
    , applyErosion
      -- * Crater
    , applyCrater
    , generateCraters
      -- * Types re-export
    , GeoModification(..)
    ) where

import World.Geology.Timeline (buildTimeline)
import World.Geology.Event (applyGeoEvent)
import World.Geology.Erosion (applyErosion)
import World.Geology.Crater (applyCrater, generateCraters)
import World.Geology.Types (GeoModification(..))
