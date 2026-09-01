{-# LANGUAGE Strict #-}
-- | Per-page location-discovery tick (#780). Runs beside the world
--   clock ('World.Thread.Time.tickWorldTime'), once per LOADED page per
--   world-thread iteration — including a hidden (non-visible) page, and
--   independent of the pause flag, since a freshly loaded save can come
--   up already looking straight at a location and must discover it
--   immediately rather than waiting for an unpause. Unlike the
--   visible-only calendar/flora/power ticks, this never reads
--   game-scaled dt: discovery is a check against whatever the unit and
--   world-manager threads have already published this instant, not
--   something that advances with simulated time.
module World.Thread.Discovery
    ( tickLocationDiscovery
    ) where

import UPrelude
import Engine.Core.Capability.UnitCombat
    (UnitCombatCapability(..), toUnitCombatCapability)
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), toWorldSimCapability)
import qualified Data.HashMap.Strict as HM
import Data.List (sortOn)
import Data.IORef (readIORef, atomicModifyIORef')
import Engine.Core.State (EngineEnv, activeWorldPageFrom)
import Engine.PlayerEvent.Emit (emitEventFullOnPage)
import Location.Discovery (DiscoveryHit(..), UnitSight(..), findDiscoveries)
import Location.Instance
    ( LocationEncounter(..), LocationEncounterOccupant(..)
    , LocationInstance(..), LocationLifecycle(..), instancesToList
    , lookupLocationInstance, encounterDiscoveryLifecycle
    , isDiscoveredLifecycle
    , markLocationEncounterCleared, markLocationEncounterClearEventEmitted
    , promoteLifecycle, setLocationLifecycle )
import Unit.Faction (isPlayerOwned)
import Unit.LineOfSight (visibleTilesOnPage)
import Unit.Types (UnitInstance(..), UnitManager(..), UnitId(..))
import World.Types (WorldGenParams(..), WorldPageId(..), WorldState(..))

-- | Check one page's placed locations against what every currently-known
--   PLAYER-OWNED unit on it can SEE, mark any newly-visible location
--   discovered, and emit one attributable player event per transition.
--   A no-op when the page has no live gen params yet (mirrors
--   'World.Thread.ItemTemp.tickItemTemperatures'). Which units count is
--   NOT decided here: this hands every unit's sight to
--   'findDiscoveries', which applies 'Unit.Faction.isPlayerOwned' — the
--   shared definition of "the player's own unit" (#912). Ownership is
--   narrower than alliance on purpose, so a debug unit (allied with the
--   player, commandable by them, but not owned) still never discovers a
--   location by looking at it.
--
--   Sight comes from 'Unit.LineOfSight.visibleTilesOnPage' against THIS
--   page's own 'WorldState' (#1230) — the same calculation the public
--   @unit.getVisibleTiles@ query runs, but without that query's
--   'wmVisible' gate, which is what keeps discovery working on a page
--   that is loaded but hidden.
--
--   The tick has TWO independent jobs, and #1990 keeps their costs
--   independent too: sight-based DISCOVERY (which locations a unit can
--   see) and encounter CLEARANCE (whether a defeated roster has been
--   wiped out). Three cost guards, all of which must stay:
--
--     * The whole page is skipped when NEITHER job has anything left
--       to do — no instance can still promote and none is awaiting
--       clearance. That is the steady state for a fully explored,
--       fully cleared world, and it short-circuits before the unit
--       manager is even read.
--     * Sight is rasterized ONLY when some instance can still promote.
--       An uncleared encounter alone admits the page (#916/PR #1900
--       widened the guard so clearance keeps being polled), but on a
--       page where every location is already discovered that is
--       clearance work, not discovery work: it needs the location
--       roster and the unit manager, never a line-of-sight raster. So
--       the clearance pass runs on its own and 'visibleTilesOnPage' is
--       never called. Any further non-sight clearance condition (#917's
--       significant-item rule, say) belongs on that same branch.
--     * When sight IS rasterized it is computed ONCE per unit and only
--       for units that could qualify at all — 'findDiscoveries' still
--       applies the ownership filter itself, so pre-filtering here
--       changes cost, never behaviour.
--
--   Splitting the two jobs never reorders them: clearance still runs
--   BEFORE the discovery promotions, and both decide from the SAME
--   pre-clearance instance snapshot, so a zero-occupant ruin still
--   lands on 'LifecycleCleared' from one tick's discovery hit. The
--   deferred clearance event (a roster defeated while the site was
--   still unknown) is likewise unaffected: it is reachable only for an
--   instance that is still promotable, which is exactly a page where
--   sight runs anyway.
--
--   Every emitted event names its 'peSourcePage' EXPLICITLY (#780)
--   since this tick runs on every loaded page, not just the active
--   one — and an explicit page is the one thing
--   'Engine.PlayerEvent.Emit.resolveEventPage' will not override, which
--   is precisely why this emitter passes it: the automatic
--   active-page snapshot would attribute a hidden page's discovery to
--   whichever page the player happens to be looking at.
--
--   A discovery on a hidden page additionally omits 'peCoords'. Since
--   #1588 that is an EDITORIAL choice, not a safety measure: a
--   coordinate now names its own page and the popup refuses to pan
--   until that page is active, so carrying one would be harmless. It
--   would just offer the player a location line that cannot act until
--   they switch worlds, for a place they have not seen — so the event
--   stays a plain "you found something" notice instead.
tickLocationDiscovery ∷ EngineEnv → WorldPageId → WorldState → IO ()
tickLocationDiscovery env pageId@(WorldPageId pageText) ws = do
    mParams ← readIORef (wsGenParamsRef ws)
    case mParams of
        Nothing → pure ()
        Just p → do
            -- ONE pre-clearance snapshot answers both questions, so the
            -- two passes can never disagree about what this tick found:
            -- 'needsSight' is decided from the very instances
            -- 'findDiscoveries' is then handed.
            let placed         = instancesToList (wgpLocationInstances p)
                needsSight     = any promotable placed
                needsClearance = any pendingClearance placed
            when (needsSight ∨ needsClearance) $ do
                -- Every discovery input — bounds, display name,
                -- lifecycle — is stored on the instance itself (#911), so
                -- this tick no longer reads the location-def registry at
                -- all. Both passes need these two: clearance reads the
                -- unit manager to ask whether a roster is dead, and its
                -- event needs the active page to decide whether to carry
                -- coords.
                um ← readIORef (ucUnitManagerRef (toUnitCombatCapability env))
                mActive ← activeWorldPageFrom
                              (wsWorldManagerRef (toWorldSimCapability env))
                let isActivePage = case mActive of
                        Just (activePageId, _) → activePageId ≡ pageId
                        Nothing → False
                -- Clearance first, then the promotions — never the other
                -- way round (see the note above).
                when needsClearance $ clearancePass isActivePage um placed
                when needsSight     $ discoveryPass isActivePage um p
  where
    -- | Retire any encounter whose roster is now wholly dead on this
    --   page. Reads the location roster and the unit manager and
    --   nothing else — no sight input of any kind — which is what lets
    --   the caller run it on a fully-discovered page without paying for
    --   a rasterization. A further non-sight clearance condition (#917's
    --   significant-item rule, say) belongs here.
    clearancePass isActivePage um placed =
        forM_ (filter (encounterDead um) placed) $ \inst → do
            cleared ← atomicModifyIORef' (wsGenParamsRef ws) $ \mP → case mP of
                Just p' → case markLocationEncounterCleared (liId inst)
                                    (wgpLocationInstances p') of
                    Just instances' →
                        (Just p' { wgpLocationInstances = instances' }, True)
                    Nothing → (mP, False)
                Nothing → (mP, False)
            when (cleared ∧ isDiscoveredLifecycle (liLifecycle inst)) $
                emitEventFullOnPage env "location_clearance"
                    "World.Thread.Discovery"
                    ("Cleared: " <> liDisplayName inst)
                    (if isActivePage then Just (liAnchor inst) else Nothing)
                    Nothing
                    (Just pageText)

    -- | Rasterize each eligible unit's sight once and promote whatever
    --   it reveals. The expensive half of the tick, and the reason the
    --   caller gates it on 'needsSight'.
    discoveryPass isActivePage um p = do
        let pageUnits =
                [ (uid, inst)
                | (uid, inst) ← sortOn fst (HM.toList (umInstances um))
                , uiPage inst ≡ pageId
                , isPlayerOwned (uiFactionId inst)
                ]
        sights ← forM pageUnits $ \(uid, inst) → do
            tiles ← visibleTilesOnPage ws inst
            pure UnitSight { usUnit    = uid
                           , usFaction = uiFactionId inst
                           , usTiles   = tiles }
        let hits = findDiscoveries (wgpWorldSize p)
                                   (wgpLocationInstances p)
                                   sights
        forM_ hits $ \hit → do
            -- The lifecycle promotion is the persisted transition
            -- (#911) — 'setLocationLifecycle' refuses a backward or
            -- same-state move, so an instance already discovered (or
            -- past it) neither changes nor re-emits. The event fires
            -- only on a promotion that actually landed, preserving
            -- #780's exactly-once contract even if two ticks raced.
            promoted ← atomicModifyIORef' (wsGenParamsRef ws) $ \mP →
              case mP of
                Just p' → case lookupLocationInstance (dhInstance hit)
                                    (wgpLocationInstances p') of
                    Just inst → case setLocationLifecycle (dhInstance hit)
                                    (encounterDiscoveryLifecycle inst)
                                    (wgpLocationInstances p') of
                        Just instances' →
                            ( Just p' { wgpLocationInstances = instances' }
                            , True )
                        Nothing → (mP, False)
                    Nothing → (mP, False)
                Nothing → (mP, False)
            when promoted $ do
                emitEventFullOnPage env "location_discovery"
                    "World.Thread.Discovery"
                    ("Discovered: " <> dhLabel hit)
                    (if isActivePage then Just (dhAnchor hit) else Nothing)
                    (Just (unUnitId (dhUnit hit)))
                    (Just pageText)
                deferredClear ← atomicModifyIORef'
                    (wsGenParamsRef ws) $ \mP → case mP of
                        Just p' → case markLocationEncounterClearEventEmitted
                                           (dhInstance hit)
                                           (wgpLocationInstances p') of
                            Just instances' →
                                (Just p' { wgpLocationInstances = instances' }
                                , True)
                            Nothing → (mP, False)
                        Nothing → (mP, False)
                when deferredClear $
                    emitEventFullOnPage env "location_clearance"
                        "World.Thread.Discovery"
                        ("Cleared: " <> dhLabel hit)
                        (if isActivePage then Just (dhAnchor hit) else Nothing)
                        (Just (unUnitId (dhUnit hit)))
                        (Just pageText)

    promotable inst =
        isJust (promoteLifecycle (liLifecycle inst) LifecycleDiscovered)
    pendingClearance inst = case liEncounter inst of
        Just encounter → leDeathOnlyClearance encounter
            ∧ leRolledCount encounter > 0
            ∧ leRosterComplete encounter
            ∧ not (leCleared encounter)
        Nothing → False
    encounterDead um inst = case liEncounter inst of
        Just encounter → pendingClearance inst
            ∧ length (leOccupants encounter) ≡ leRolledCount encounter
            ∧ all occupantDead (leOccupants encounter)
        Nothing → False
      where
        occupantDead occupant = case HM.lookup (leoUnitId occupant)
                                           (umInstances um) of
            Just unitInst → uiPage unitInst ≡ pageId ∧ uiPose unitInst ≡ "dead"
            Nothing → False
