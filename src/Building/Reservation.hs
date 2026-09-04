{-# LANGUAGE Strict #-}
-- | Footprint exclusivity for building placement (#2326).
--
--   A building is admitted on one thread and inserted on another:
--   @building.spawn@ and @power.placeNode@ answer their caller from the
--   Lua thread, while the insertion happens later in
--   'Building.Thread.Command.applyBuildingSpawn' — drained from the unit
--   thread, or performed by the world thread for a page-BOUND placement
--   (#1602). Validating against a manager snapshot and inserting without
--   re-reading it is therefore a real window, not an instantaneous one:
--   two requests could read the same free footprint and both be told
--   yes.
--
--   This module is the single authority that closes it, in two halves
--   that are deliberately BOTH present:
--
--   * 'reserveFootprint' is the ADMISSION half. In one manager
--     transition it tests the candidate footprint against everything
--     that already holds tiles on that page — committed instances AND
--     outstanding reservations — allocates the 'BuildingId' only if it
--     is free, and records the reservation. A request that loses the
--     race allocates nothing and is refused SYNCHRONOUSLY, so its caller
--     learns the placement failed instead of receiving an id for a
--     building that will never appear.
--
--   * 'commitFootprint' is the COMMIT half, run inside the same
--     'Data.IORef.atomicModifyIORef'' that inserts the instance. It
--     consumes the reservation the request holds and re-tests the
--     footprint against the LIVE instance map, so the transition that
--     inserts a building is itself the authority on whether its tiles
--     are free — the property #2051 and #1686 established for the
--     location-stamp marker and for portal page activity.
--
--   Only OCCUPANCY is re-tested at commit, deliberately: terrain and
--   location constraints are 'Building.Placement.canPlaceAt's business
--   at admission, and re-running them here would make terrain changing
--   under a queued placement drop it, which #2326 excludes.
--
--   Every non-committing outcome releases the reservation instead
--   ('releaseReservation', 'clearReservations'), so the collection is
--   consumable rather than merely bounded and nothing survives a
--   teardown or a load replacement.
module Building.Reservation
    ( -- * Occupancy, in the canonical frame
      occupancyConflictReason
    , footprintCanonTiles
    , instanceTiles
    , reservationTiles
    , tilesHeldOnPage
    , footprintClear
      -- * Admission
    , reserveFootprint
      -- * Commit
    , commitFootprint
      -- * Release
    , releaseReservation
    , clearReservations
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Building.Types
import World.Generate.Coordinates (canonicalTile)
import World.Page.Types (WorldPageId)

-- | The one refusal a footprint collision ever produces, shared by the
--   pure admission check in 'Building.Placement.canPlaceAt' and by
--   'reserveFootprint'. One string so a caller — Lua or test — can tell
--   an occupancy conflict from every other placement refusal without
--   caring which of the two answered.
occupancyConflictReason ∷ Text
occupancyConflictReason = "tile already occupied"

-- | The tiles a footprint covers, each resolved into the canonical
--   frame (#1175).
--
--   A footprint is enumerated by stepping off its anchor, so even a
--   canonical anchor near the seam produces tiles past the canonical u
--   range, and an anchor restored from a pre-#1175 save can be an alias
--   outright. Canonicalising every tile on both sides of a comparison is
--   what makes "these two rectangles share a tile" true across the seam
--   rather than only away from it.
footprintCanonTiles ∷ Int → Int → Int → Int → Int → HS.HashSet (Int, Int)
footprintCanonTiles worldSize ax ay w h = HS.fromList
    [ canonicalTile worldSize x y | (x, y) ← footprintTiles ax ay w h ]

-- | The canonical tiles every one of these committed instances covers.
--   Page filtering is the CALLER's, because 'Building.Placement' is
--   handed a manager already scoped to one page (#76) while the
--   reservation transaction scopes its own.
instanceTiles
    ∷ Int → HM.HashMap BuildingId BuildingInstance → HS.HashSet (Int, Int)
instanceTiles worldSize instances = HS.unions
    [ footprintCanonTiles worldSize (biAnchorX b) (biAnchorY b)
                          (biTileW b) (biTileH b)
    | b ← HM.elems instances ]

-- | The canonical tiles every one of these outstanding reservations
--   holds. Page filtering is the caller's, as above.
reservationTiles
    ∷ Int → HM.HashMap BuildingId FootprintReservation → HS.HashSet (Int, Int)
reservationTiles worldSize reservations = HS.unions
    [ footprintCanonTiles worldSize (frAnchorX r) (frAnchorY r)
                          (frTileW r) (frTileH r)
    | r ← HM.elems reservations ]

-- | Every canonical tile held on ONE page, by a committed instance or
--   by an outstanding reservation.
--
--   Page-scoped because occupancy is (#76): the same anchor on two
--   different pages is two independent placements, and a building on
--   another page must never block one here.
tilesHeldOnPage
    ∷ Int                                              -- ^ world size in chunks
    → WorldPageId
    → HM.HashMap BuildingId BuildingInstance
    → HM.HashMap BuildingId FootprintReservation
    → HS.HashSet (Int, Int)
tilesHeldOnPage worldSize pid instances reservations =
    instanceTiles worldSize (buildingsOnPage pid instances)
        `HS.union`
    reservationTiles worldSize (HM.filter ((≡ pid) ∘ frPage) reservations)

-- | Does the candidate footprint share no tile with the given held set?
--   PARTIAL overlap counts: two multi-tile footprints conflict as soon
--   as one tile is common, not only when their anchors coincide.
footprintClear
    ∷ Int                     -- ^ world size in chunks
    → Int → Int → Int → Int   -- ^ candidate anchor x, y and tile w, h
    → HS.HashSet (Int, Int)   -- ^ tiles already held
    → Bool
footprintClear worldSize ax ay w h held =
    HS.null (HS.intersection (footprintCanonTiles worldSize ax ay w h) held)

-- | Admit a placement: take its footprint and allocate its id, or refuse.
--
--   ONE manager transition does both, and that is the whole point. The
--   id allocation this replaced advanced 'bmNextId' without recording
--   anything about the tiles, so a second request reading the same
--   snapshot saw the same free footprint. Here a losing request leaves
--   the manager EXACTLY as it found it — no id consumed, no reservation
--   left behind — so requirement 3's "a lost race leaks nothing" holds
--   by construction rather than by a cleanup path that could be missed.
--
--   Shaped for 'Data.IORef.atomicModifyIORef'', which is where the
--   test, the allocation and the record become one critical section.
--
--   The anchor must already be canonical ('World.Generate.Coordinates.
--   canonicalTile'), as both call sites resolve it before validating.
reserveFootprint
    ∷ Int              -- ^ world size in chunks
    → WorldPageId      -- ^ the page this placement lands on
    → BuildingDef
    → Int → Int        -- ^ canonical anchor
    → BuildingManager
    → (BuildingManager, Either Text BuildingId)
reserveFootprint worldSize pid def gx gy bm
    | not (footprintClear worldSize gx gy (bdTileW def) (bdTileH def)
              (tilesHeldOnPage worldSize pid (bmInstances bm)
                               (bmReservations bm)))
        = (bm, Left occupancyConflictReason)
    | otherwise =
        let (bid, bm') = nextBuildingId bm
            res = FootprintReservation
                { frPage    = pid
                , frAnchorX = gx
                , frAnchorY = gy
                , frTileW   = bdTileW def
                , frTileH   = bdTileH def
                }
        in ( bm' { bmReservations = HM.insert bid res (bmReservations bm') }
           , Right bid )

-- | The commit half, as a PURE manager transition so its caller can run
--   it inside the very 'Data.IORef.atomicModifyIORef'' that inserts the
--   instance — a check in a separate read would be exactly the window
--   this issue is about.
--
--   Reports whether the insert may proceed. Two conditions, both
--   required:
--
--   1. The request OWNS a reservation for this exact placement — same
--      id, same page, same rectangle. That is what makes the commit
--      verify the admission half rather than trust it: a spawn whose
--      reservation was never taken, or was released or replaced
--      underneath it (a teardown, a load), holds no claim on these
--      tiles and must not insert.
--   2. The footprint is STILL free of committed instances on that page.
--      Redundant while every producer goes through 'reserveFootprint' —
--      which is the point of a belt-and-braces check — and it is what
--      makes this transition, and not only its admission, the authority
--      requirement 1 asks for.
--
--   Note what is NOT consulted: other reservations. They describe
--   placements that have not landed, and a committing request must not
--   be refused by a footprint nobody occupies yet — its own admission
--   already proved it did not collide with any reservation outstanding
--   at the time. Nor is terrain: re-running all of
--   'Building.Placement.canPlaceAt' here would drop a placement whose
--   ground changed under it, which #2326 excludes.
--
--   The claim is retired by this transition whenever it MATCHES the
--   request, insert or no insert — a rejected commit must not leave one
--   holding tiles nothing will ever occupy. A claim that does NOT match
--   is left exactly as found: it belongs to some other placement of the
--   same id, and a replayed or mis-addressed command must not be able to
--   cancel it.
commitFootprint
    ∷ Int → WorldPageId → BuildingId → Int → Int → Int → Int
    → BuildingManager → (BuildingManager, Bool)
commitFootprint worldSize pid bid gx gy w h bm = (retired, accepted)
  where
    holdsClaim = case HM.lookup bid (bmReservations bm) of
        Nothing  → False
        Just res → frPage res ≡ pid ∧ frAnchorX res ≡ gx
                 ∧ frAnchorY res ≡ gy ∧ frTileW res ≡ w ∧ frTileH res ≡ h
    retired
        | holdsClaim = bm { bmReservations = HM.delete bid (bmReservations bm) }
        | otherwise  = bm
    accepted = holdsClaim ∧ footprintClear worldSize gx gy w h
        (instanceTiles worldSize (buildingsOnPage pid (bmInstances bm)))

-- | Drop one outstanding reservation, for a request that will never
--   commit: its world went away (#58), its def is unknown, or its page
--   binding went stale (#1602). Idempotent, and harmless for a request
--   that never held one.
releaseReservation ∷ BuildingId → BuildingManager → BuildingManager
releaseReservation bid bm =
    bm { bmReservations = HM.delete bid (bmReservations bm) }

-- | Drop every outstanding reservation — the teardown case. Runs with
--   @BuildingClearAll@, which is enqueued behind every pending spawn
--   (#58) precisely so nothing admitted before a teardown can still be
--   holding tiles in the session that replaces it.
clearReservations ∷ BuildingManager → BuildingManager
clearReservations bm = bm { bmReservations = HM.empty }
