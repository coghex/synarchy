{-# LANGUAGE Strict #-}
-- | Pure discovery-transition detection (#780): given a page's placed
--   location instances (#911) and what every unit currently on that page
--   can SEE, decides which locations just transitioned from undiscovered
--   to discovered this tick. A location is discovered the instant a
--   PLAYER-OWNED unit's ('Unit.Faction.isPlayerOwned', #912) visible-tile
--   set touches its stored absolute bounds (#777) — reusing the exact
--   seam-aware containment 'Location.Bounds.boundsContainsPoint' already
--   provides. This module only decides WHICH locations qualify; the
--   caller ('World.Thread.Discovery') owns persisting the transition and
--   emitting the player-facing event.
--
--   #1230 replaced the old proximity trigger — a point-in-halo test
--   against the bounds expanded by a stored @discovery_margin@ — with
--   SIGHT. The margin is gone entirely; 'liBounds' is the authoritative
--   footprint, and the caller supplies each unit's visible tiles from
--   the one shared calculation 'Unit.LineOfSight.visibleTilesOnPage'
--   owns (perception range scaled by the page-local night factor, a
--   120° facing cone, and terrain-Z occlusion). Seeing ONE occupied tile
--   is enough — nothing here requires the unit to stand inside, and a
--   unit that IS inside always qualifies, since its own tile is always
--   in its visible set.
--
--   #915 added the second, independent knowledge layer
--   ('findAwareness'): which player-owned units can see which location
--   AT ALL, for the per-unit experiential memory
--   @scripts\/unit_ai_locations.lua@ keeps. Both layers are derived from
--   one shared sight enumeration ('sightContactsWhere'), so \"the player
--   has mapped it\" and \"this acolyte knows where it is\" can never
--   disagree about the geometry or about which units count.
--
--   Everything read here is stored ON the instance — bounds, display
--   name, and lifecycle — so neither the location registry nor the
--   overlay is a parameter any more (#911): a definition edited after
--   placement can no longer reshape a placed location's footprint.
module Location.Discovery
    ( UnitSight(..)
    , DiscoveryHit(..)
    , findDiscoveries
    , AwarenessHit(..)
    , findAwareness
    ) where

import UPrelude
import Data.Function (on)
import Data.List (groupBy)
import Location.Instance
    ( LocationInstance(..), LocationInstances, LocationInstanceId
    , LocationLifecycle(..), instancesToList, promoteLifecycle )
import Location.Bounds (boundsContainsPoint)
import Unit.Faction (Faction, isPlayerOwned)
import World.Chunk.Types (ChunkCoord)

-- | One unit's sight this tick, as both layers below consume it: its
--   id, its faction (the ownership filter is applied HERE, not by the
--   caller), and every tile it can currently see.
--
--   The tile set is computed ONCE per unit by the IO caller and shared
--   across every location on the page and across both layers (#1230):
--   sight is a radius-squared rasterization plus a per-tile terrain
--   raycast, so recomputing it per (unit, location) pair would scale
--   with the page's location count for no gain.
data UnitSight uid = UnitSight
    { usUnit    ∷ !uid
    , usFaction ∷ !Faction
    , usTiles   ∷ ![(Int, Int)]
    } deriving (Show, Eq)

-- | One location that transitions to discovered this tick: the instance
--   it names (#911 — the durable, page-local identity; two ruins in one
--   chunk are distinct hits), the chunk hosting it, its anchor tile
--   (the player event's clickable coordinate), its display name (the
--   event's text names it), and the id of the unit whose sight
--   triggered the transition — the first qualifying unit that can see
--   it.
data DiscoveryHit uid = DiscoveryHit
    { dhInstance ∷ !LocationInstanceId
    , dhCoord    ∷ !ChunkCoord
    , dhAnchor   ∷ !(Int, Int)
    , dhLabel    ∷ !Text
    , dhUnit     ∷ !uid
    } deriving (Show, Eq)

-- | Which placed locations transition to discovered THIS tick, given
--   the page's world size (chunks, same unit 'World.Generate.Types.
--   wgpWorldSize' already uses), its instance table, and every
--   currently-known unit's sight on this page — units of every faction
--   included, since the filter is applied HERE.
--
--   Discovery asks 'Unit.Faction.isPlayerOwned' — "is this the player's
--   OWN unit?" (#912) — which is deliberately NOT the same question as
--   "is this unit friendly to the player?". 'Unit.Faction.FactionDebug'
--   is allied with the player and takes player orders, yet is not
--   player-owned, so a debug unit still never discovers a location by
--   looking at it.
--
--   Only a transition 'promoteLifecycle' accepts is reported: an
--   instance already at 'LifecycleDiscovered' or beyond
--   ('LifecycleActive' / 'LifecycleCleared' / 'LifecycleDepleted') is
--   never re-discovered and never downgraded, making the transition
--   idempotent from the caller's point of view and keeping the "exactly
--   one @location_discovery@ event" contract intact. 'LifecycleUnknown'
--   and 'LifecycleHinted' both promote.
--
--   Instances are scanned in 'instancesToList' (id) order, so a tick
--   producing several hits produces them deterministically. A location
--   more than one unit can see attributes to the first match in
--   @sights@, so a caller wanting a deterministic discoverer should pass
--   units in a stable order (e.g. sorted by unit id).
--
--   Derived from 'sightContactsWhere' — the SAME enumeration
--   'findAwareness' reports in full (#915) — so the player-wide
--   transition and the per-unit memory can never disagree about who can
--   see which location. All this adds on top is the two things specific
--   to the player-wide layer: the promotable-lifecycle filter (passed
--   IN, so an already-discovered instance still short-circuits before
--   its units are scanned, exactly as before), and taking only the
--   FIRST qualifying unit per location (the attributed discoverer).
findDiscoveries
    ∷ Int → LocationInstances → [UnitSight uid] → [DiscoveryHit uid]
findDiscoveries worldSize instances sights =
    [ DiscoveryHit (liId inst) (liChunk inst) (liAnchor inst)
                   (liDisplayName inst) uid
    | ((inst, uid) : _) ← groupBy ((≡) `on` (liId . fst)) contacts
    ]
  where
    contacts = sightContactsWhere promotable worldSize instances sights
    promotable inst =
        isJust (promoteLifecycle (liLifecycle inst) LifecycleDiscovered)

-- | One player-owned unit that can SEE one placed location THIS tick
--   (#915): the durable page-local instance identity, the chunk hosting
--   it, its anchor tile (the coordinate the unit remembers), its display
--   name, and the unit that sees it.
--
--   Deliberately NOT the same thing as a 'DiscoveryHit': awareness is
--   reported for EVERY qualifying unit and regardless of lifecycle,
--   where a discovery is reported once per location for the first
--   qualifying unit and only while the location can still promote.
data AwarenessHit uid = AwarenessHit
    { ahInstance ∷ !LocationInstanceId
    , ahChunk    ∷ !ChunkCoord
    , ahAnchor   ∷ !(Int, Int)
    , ahLabel    ∷ !Text
    , ahUnit     ∷ !uid
    } deriving (Show, Eq)

-- | Every (location, player-owned unit) pair whose unit can see that
--   location right now (#915) — the per-unit EXPERIENTIAL layer's
--   acquisition predicate, as distinct from the player-wide
--   CARTOGRAPHIC transition 'findDiscoveries' reports.
--
--   Same world-size/instance-table/sight inputs, same
--   'Unit.Faction.isPlayerOwned' filter, and the same seam-aware
--   bounds containment — literally the same 'sightContactsWhere'
--   enumeration, so the two layers cannot drift apart. Two deliberate
--   differences, both required by #915:
--
--   * NO lifecycle filter. A unit arriving at a location the player has
--     already mapped still learns where it is — the memory is about
--     what THIS unit has seen, and nothing about it is gated on the
--     one-time player-wide promotion or on the event that promotion
--     emits.
--   * EVERY qualifying unit, not just the first. Two acolytes looking
--     at the same ruin both learn it; a location with one discoverer
--     does not leave its companions ignorant.
--
--   Ordered by instance ('instancesToList' id order), then by the
--   caller's unit order — deterministic for the same reason
--   'findDiscoveries' is.
findAwareness
    ∷ Int → LocationInstances → [UnitSight uid] → [AwarenessHit uid]
findAwareness worldSize instances sights =
    [ AwarenessHit (liId inst) (liChunk inst) (liAnchor inst)
                   (liDisplayName inst) uid
    | (inst, uid) ← sightContactsWhere (const True) worldSize instances sights
    ]

-- | The one sight enumeration both layers above are built from: every
--   (placed location, player-owned unit that can see it) pair, instances
--   in 'instancesToList' order and units in the caller's order.
--
--   "Can see it" is exactly requirement 7 of #1230: the unit's visible
--   tile set intersects the location's own inclusive occupied bounds
--   ('liBounds' — the authoritative footprint #777 established and #1230
--   left as the only one). One shared tile is enough, and the
--   containment is the seam-aware 'boundsContainsPoint' the rest of the
--   location stack already uses, so a location straddling the
--   cylindrical u-seam is seen from either side.
--
--   @keep@ is applied to the INSTANCE before its units are scanned, so a
--   caller that only cares about some instances ('findDiscoveries' and
--   its promotable-lifecycle filter) pays nothing for the rest.
sightContactsWhere
    ∷ (LocationInstance → Bool) → Int → LocationInstances
    → [UnitSight uid] → [(LocationInstance, uid)]
sightContactsWhere keep worldSize instances sights =
    [ (inst, usUnit s)
    | inst ← instancesToList instances
    , keep inst
    , s ← playerSights
    , any (boundsContainsPoint worldSize (liBounds inst)) (usTiles s)
    ]
  where
    playerSights = [ s | s ← sights, isPlayerOwned (usFaction s) ]
