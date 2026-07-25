{-# LANGUAGE Strict, UnicodeSyntax #-}
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
--   Everything read here is stored ON the instance — bounds, margin,
--   display name, and lifecycle — so neither the location registry nor
--   the overlay is a parameter any more (#911): a definition edited
--   after placement can no longer reshape a placed location's discovery
--   halo.
module Location.Discovery
    ( DiscoveryHit(..)
    , findDiscoveries
    ) where

import UPrelude
import Data.List (find)
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
findDiscoveries
    ∷ Int → LocationInstances → [(uid, Faction, Int, Int)] → [DiscoveryHit uid]
findDiscoveries worldSize instances units =
    let playerUnits =
            [ (uid, gx, gy) | (uid, faction, gx, gy) ← units
                             , isPlayerOwned faction ]
    in [ DiscoveryHit (liId inst) (liChunk inst) (liAnchor inst)
                      (liDisplayName inst) uid
       | inst ← instancesToList instances
       , isJust (promoteLifecycle (liLifecycle inst) LifecycleDiscovered)
       , let halo = expandBounds (liDiscoveryMargin inst) (liBounds inst)
       , Just (uid, _, _) ← [ find (\(_, px, py) →
               boundsContainsPoint worldSize halo (px, py)) playerUnits ]
       ]
