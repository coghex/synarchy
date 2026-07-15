{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Derives every placed location's absolute bounds box (#777) from the
--   world-gen overlay + registered defs (#778 portal exclusion) — the
--   same anchor (chunk-centre tile) + 'translateBounds' computation
--   'Engine.Scripting.Lua.API.WorldQuery.Location.worldListPlacedLocationsFn'
--   already exposes to Lua, factored out here so
--   'Building.Placement.canPlaceAt' can reuse it purely. The overlay
--   alone is authoritative — no need for a location's chunk to have
--   been visited or its geometry stamped.
module Location.Placement
    ( placedLocationBounds
    , nearestLocationDistance
    ) where

import UPrelude
import Location.Types (LocationRegistry, lookupLocation, ldBounds)
import Location.Overlay.Types (LocationOverlay, overlayToList)
import Location.Bounds (AbsBounds, translateBounds, nearestBoundsDistance)
import World.Chunk.Types (ChunkCoord(..), chunkSize)

-- | Absolute bounds for every placed location whose def is currently
--   registered. An overlay entry naming an unregistered id (its YAML
--   hasn't been (re)loaded this session) is silently skipped — the
--   same tolerance 'world.listPlacedLocations' already applies.
placedLocationBounds ∷ LocationRegistry → LocationOverlay → [AbsBounds]
placedLocationBounds registry overlay =
    [ translateBounds (gx, gy) (ldBounds def)
    | (ChunkCoord cx cy, lid) ← overlayToList overlay
    , Just def ← [lookupLocation lid registry]
    , let half = chunkSize `div` 2
          gx   = cx * chunkSize + half
          gy   = cy * chunkSize + half
    ]

-- | Nearest seam-aware footprint→placed-location distance across
--   every location placed on this page's overlay (#779); 'Nothing'
--   when the page has none.
nearestLocationDistance
    ∷ Int → LocationRegistry → LocationOverlay → AbsBounds → Maybe Int
nearestLocationDistance worldSize registry overlay footprint =
    nearestBoundsDistance worldSize footprint
        (placedLocationBounds registry overlay)
