{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Pure discovery-transition detection (#780): given a page's placed
--   locations (registry + overlay), its already-discovered chunk set,
--   and the positions of every player-controlled unit currently on that
--   page, decides which locations just transitioned from undiscovered
--   to discovered this tick. A location is discovered the instant a
--   player-faction unit's tile falls inside its absolute bounds (#777)
--   expanded by the definition's discovery margin — reusing the exact
--   seam-aware containment 'Location.Bounds.boundsContainsPoint'
--   already provides, mirroring 'Location.Placement's translate-then-
--   test pattern. This module only decides WHICH locations qualify;
--   the caller ('World.Thread.Discovery') owns persisting the
--   transition and emitting the player-facing event.
module Location.Discovery
    ( DiscoveryHit(..)
    , findDiscoveries
    ) where

import UPrelude
import Data.List (find)
import qualified Data.HashSet as HS
import Location.Types
    ( LocationRegistry, lookupLocation, ldBounds, ldDiscoveryMargin, ldLabel )
import Location.Overlay.Types (LocationOverlay, overlayToList)
import Location.Bounds (AbsBounds, translateBounds, expandBounds, boundsContainsPoint)
import World.Chunk.Types (ChunkCoord(..), chunkSize)

-- | One location that transitions to discovered this tick: the chunk
--   hosting it, its anchor tile (identifies WHICH placed instance — two
--   ruins of the same def have distinct anchors — and doubles as the
--   player event's clickable coordinate), its display label (the
--   player event's text names it), and the id of the unit whose
--   approach triggered the transition — the first qualifying unit
--   found inside its expanded bounds.
data DiscoveryHit uid = DiscoveryHit
    { dhCoord  ∷ !ChunkCoord
    , dhAnchor ∷ !(Int, Int)
    , dhLabel  ∷ !Text
    , dhUnit   ∷ !uid
    } deriving (Show, Eq)

-- | Every placed location's discovery-margin-expanded bounds, paired
--   with its chunk coord + anchor tile + label. An overlay entry naming
--   an unregistered id is silently skipped, the same tolerance
--   'Location.Placement.placedLocationBounds' applies.
discoverableLocations
    ∷ LocationRegistry → LocationOverlay
    → [(ChunkCoord, (Int, Int), Text, AbsBounds)]
discoverableLocations registry overlay =
    [ (coord, (gx, gy), ldLabel def, expandBounds (ldDiscoveryMargin def) ab)
    | (coord@(ChunkCoord cx cy), lid) ← overlayToList overlay
    , Just def ← [lookupLocation lid registry]
    , let half = chunkSize `div` 2
          gx   = cx * chunkSize + half
          gy   = cy * chunkSize + half
          ab   = translateBounds (gx, gy) (ldBounds def)
    ]

-- | Which placed locations transition to discovered THIS tick, given
--   the page's world size (chunks, same unit 'World.Generate.Types.
--   wgpWorldSize' already uses), its location registry + overlay, its
--   already-discovered chunk set, and every currently-known unit's
--   (id, faction id, gx, gy) on this page — hostile, wildlife, neutral,
--   and unrelated debug factions included; the player-control faction
--   contract (@factionId == "player"@ — the tag every player-spawn path,
--   including a portal-spawned unit, assigns) is applied HERE, so a
--   non-player unit standing inside a location's bounds never
--   contributes a hit. An already-discovered location never re-fires,
--   making the transition idempotent from the caller's point of view. A
--   location with more than one qualifying unit inside attributes to
--   the first match in @units@, so a caller wanting a deterministic
--   discoverer should pass units in a stable order (e.g. sorted by unit
--   id).
findDiscoveries
    ∷ Int → LocationRegistry → LocationOverlay → HS.HashSet ChunkCoord
    → [(uid, Text, Int, Int)] → [DiscoveryHit uid]
findDiscoveries worldSize registry overlay discovered units =
    let playerUnits =
            [ (uid, gx, gy) | (uid, factionId, gx, gy) ← units
                             , factionId ≡ "player" ]
    in [ DiscoveryHit coord anchor label uid
       | (coord, anchor, label, bounds) ← discoverableLocations registry overlay
       , not (HS.member coord discovered)
       , Just (uid, _, _) ← [ find (\(_, px, py) →
               boundsContainsPoint worldSize bounds (px, py)) playerUnits ]
       ]
