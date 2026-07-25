{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Every placed location's absolute bounds box, read straight off the
--   page's placed-location instance table (#911) — the values each
--   instance stored when it was placed, so a definition edited later
--   never silently moves an existing portal-exclusion box (#778) or
--   remote-start distance (#779). Factored out here so
--   'Building.Placement.canPlaceAt' can reuse it purely. The instance
--   table alone is authoritative — no need for a location's chunk to
--   have been visited or its geometry stamped.
module Location.Placement
    ( placedLocationBounds
    , nearestLocationDistance
    ) where

import UPrelude
import Location.Instance (LocationInstances, locationInstanceBounds)
import Location.Bounds (AbsBounds, nearestBoundsDistance)

-- | Absolute bounds for every placed location on this page, in instance
--   id order.
placedLocationBounds ∷ LocationInstances → [AbsBounds]
placedLocationBounds = locationInstanceBounds

-- | Nearest seam-aware footprint→placed-location distance across
--   every location placed on this page (#779); 'Nothing' when the page
--   has none.
nearestLocationDistance
    ∷ Int → LocationInstances → AbsBounds → Maybe Int
nearestLocationDistance worldSize instances footprint =
    nearestBoundsDistance worldSize footprint
        (placedLocationBounds instances)
