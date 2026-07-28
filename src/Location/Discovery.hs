{-# LANGUAGE Strict #-}
-- | Pure discovery-transition detection (#780): given a page's placed
--   location instances (#911) and the positions of every unit currently
--   on that page, decides which locations just transitioned from
--   undiscovered to discovered this tick. A location is discovered the
--   instant a PLAYER-OWNED unit's ('Unit.Faction.isPlayerOwned', #912)
--   tile falls inside its stored absolute bounds (#777) expanded by its
--   stored discovery margin — reusing the exact seam-aware containment
--   'Location.Bounds.boundsContainsPoint' already provides. This module
--   only decides WHICH locations qualify; the caller
--   ('World.Thread.Discovery') owns persisting the transition and
--   emitting the player-facing event.
--
--   #915 added the second, independent knowledge layer
--   ('findAwareness'): which player-owned units are inside which
--   location's halo AT ALL, for the per-unit experiential memory
--   @scripts\/unit_ai_locations.lua@ keeps. Both layers are derived from
--   one shared containment enumeration ('haloContactsWhere'), so \"the
--   player has mapped it\" and \"this acolyte knows where it is\" can
--   never disagree about the geometry or about which units count.
--
--   Everything read here is stored ON the instance — bounds, margin,
--   display name, and lifecycle — so neither the location registry nor
--   the overlay is a parameter any more (#911): a definition edited
--   after placement can no longer reshape a placed location's discovery
--   halo.
module Location.Discovery
    ( DiscoveryHit(..)
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
import Location.Bounds (expandBounds, boundsContainsPoint)
import Unit.Faction (Faction, isPlayerOwned)
import World.Chunk.Types (ChunkCoord)

-- | One location that transitions to discovered this tick: the instance
--   it names (#911 — the durable, page-local identity; two ruins in one
--   chunk are distinct hits), the chunk hosting it, its anchor tile
--   (the player event's clickable coordinate), its display name (the
--   event's text names it), and the id of the unit whose approach
--   triggered the transition — the first qualifying unit found inside
--   its expanded bounds.
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
--   currently-known unit's (id, faction, gx, gy) on this page — units of
--   every faction included, since the filter is applied HERE.
--
--   Discovery asks 'Unit.Faction.isPlayerOwned' — "is this the player's
--   OWN unit?" (#912) — which is deliberately NOT the same question as
--   "is this unit friendly to the player?". 'Unit.Faction.FactionDebug'
--   is allied with the player and takes player orders, yet is not
--   player-owned, so a debug unit still never discovers a location by
--   walking through it. Answering the alliance question here instead
--   would be a silent, player-visible behavior change; that is exactly
--   why "Unit.Faction" keeps ownership and alliance apart.
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
--   with more than one qualifying unit inside attributes to the first
--   match in @units@, so a caller wanting a deterministic discoverer
--   should pass units in a stable order (e.g. sorted by unit id).
--
--   Derived from 'haloContactsWhere' — the SAME enumeration
--   'findAwareness' reports in full (#915) — so the player-wide
--   transition and the per-unit memory can never disagree about who is
--   inside which location's halo. All this adds on top is the two things
--   specific to the player-wide layer: the promotable-lifecycle filter
--   (passed IN, so an already-discovered instance still short-circuits
--   before its units are scanned, exactly as before), and taking only
--   the FIRST qualifying unit per location (the attributed discoverer).
findDiscoveries
    ∷ Int → LocationInstances → [(uid, Faction, Int, Int)] → [DiscoveryHit uid]
findDiscoveries worldSize instances units =
    [ DiscoveryHit (liId inst) (liChunk inst) (liAnchor inst)
                   (liDisplayName inst) uid
    | ((inst, uid) : _) ← groupBy ((≡) `on` (liId . fst)) contacts
    ]
  where
    contacts = haloContactsWhere promotable worldSize instances units
    promotable inst =
        isJust (promoteLifecycle (liLifecycle inst) LifecycleDiscovered)

-- | One player-owned unit standing inside one placed location's
--   discovery halo THIS tick (#915): the durable page-local instance
--   identity, the chunk hosting it, its anchor tile (the coordinate the
--   unit remembers), its display name, and the unit that is inside.
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

-- | Every (location, player-owned unit) pair whose unit is inside that
--   location's discovery halo right now (#915) — the per-unit
--   EXPERIENTIAL layer's acquisition predicate, as distinct from the
--   player-wide CARTOGRAPHIC transition 'findDiscoveries' reports.
--
--   Same world-size/instance-table/unit-list inputs, same
--   'Unit.Faction.isPlayerOwned' filter, and the same seam-aware
--   expanded-bounds containment — literally the same 'haloContacts'
--   enumeration, so the two layers cannot drift apart. Two deliberate
--   differences, both required by #915:
--
--   * NO lifecycle filter. A unit arriving at a location the player has
--     already mapped still learns where it is — the memory is about
--     what THIS unit has seen, and nothing about it is gated on the
--     one-time player-wide promotion or on the event that promotion
--     emits.
--   * EVERY qualifying unit, not just the first. Two acolytes standing
--     in the same halo both learn it; a location with one discoverer
--     does not leave its companions ignorant.
--
--   Ordered by instance ('instancesToList' id order), then by the
--   caller's unit order — deterministic for the same reason
--   'findDiscoveries' is.
findAwareness
    ∷ Int → LocationInstances → [(uid, Faction, Int, Int)] → [AwarenessHit uid]
findAwareness worldSize instances units =
    [ AwarenessHit (liId inst) (liChunk inst) (liAnchor inst)
                   (liDisplayName inst) uid
    | (inst, uid) ← haloContactsWhere (const True) worldSize instances units
    ]

-- | The one halo-containment enumeration both layers above are built
--   from: every (placed location, player-owned unit inside its expanded
--   bounds) pair, instances in 'instancesToList' order and units in the
--   caller's order.
--
--   @keep@ is applied to the INSTANCE before its units are scanned, so a
--   caller that only cares about some instances ('findDiscoveries' and
--   its promotable-lifecycle filter) pays nothing for the rest — the
--   short-circuit that keeps the per-page discovery tick cheap once a
--   world's locations have all been discovered.
haloContactsWhere
    ∷ (LocationInstance → Bool) → Int → LocationInstances
    → [(uid, Faction, Int, Int)] → [(LocationInstance, uid)]
haloContactsWhere keep worldSize instances units =
    [ (inst, uid)
    | inst ← instancesToList instances
    , keep inst
    , let halo = expandBounds (liDiscoveryMargin inst) (liBounds inst)
    , (uid, px, py) ← playerUnits
    , boundsContainsPoint worldSize halo (px, py)
    ]
  where
    playerUnits = [ (uid, gx, gy) | (uid, faction, gx, gy) ← units
                                   , isPlayerOwned faction ]
