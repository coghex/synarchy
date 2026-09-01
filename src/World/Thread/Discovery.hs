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
    , lookupLocationInstance, locationDiscoveryLifecycle
    , locationAuthorsClearance, locationClearanceSatisfied
    , markLocationEncounterCleared, resolveLocationClearance
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
--       to do — no instance can still promote, no roster is awaiting a
--       death check, and none is READY to clear. That last one is
--       narrow on purpose (#917): a location whose guaranteed item is
--       still on the floor is not ready, so it admits nothing, and a
--       fully explored world short-circuits before the unit manager is
--       even read exactly as it did before significant contents
--       existed. Nothing is lost by not polling an unsatisfied
--       location — only two things can satisfy one, an encounter
--       completing on this thread and an item being taken on the Lua
--       thread, and the next tick sees either.
--     * Sight is rasterized ONLY when some instance can still promote.
--       A pending roster check or a ready-to-clear location alone
--       admits the page (#916/PR #1900 widened the guard so clearance
--       keeps being polled, and #917's rule joined it there), but on a
--       page
--       where every location is already discovered that is clearance
--       work, not discovery work: it needs the location roster and the
--       unit manager, never a line-of-sight raster. So the clearance
--       pass runs on its own and 'visibleTilesOnPage' is never called.
--     * When sight IS rasterized it is computed ONCE per unit and only
--       for units that could qualify at all — 'findDiscoveries' still
--       applies the ownership filter itself, so pre-filtering here
--       changes cost, never behaviour.
--
--   Splitting the two jobs never reorders them: clearance still runs
--   BEFORE the discovery promotions, and both decide from the SAME
--   pre-clearance instance snapshot, so a ruin whose every condition is
--   already satisfied still lands on 'LifecycleCleared' from one tick's
--   discovery hit. The deferred clearance event (a location completed
--   while the site was still unknown) is likewise unaffected: it is
--   reachable only for an instance that is still promotable, which is
--   exactly a page where sight runs anyway.
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
                needsRoster    = any (encounterPending) placed
                needsNotice    = any awaitingNotice placed
                needsClearance = needsRoster ∨ needsNotice
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
                when needsClearance $
                    clearancePass isActivePage um needsRoster placed
                when needsSight     $ discoveryPass isActivePage um p
  where
    -- | The non-sight half of the tick, in two ordered steps. Reads the
    --   location roster and the unit manager and nothing else — no
    --   sight input of any kind — which is what lets the caller run it
    --   on a fully-discovered page without paying for a rasterization.
    --
    --   1. Retire any encounter whose roster is now wholly dead on this
    --      page. That records ENCOUNTER completion and nothing more
    --      (#917): it moves no lifecycle and emits no event.
    --   2. Re-evaluate the COMPOUND clearance predicate for every
    --      instance that still owes one. Since #917 an encounter's
    --      death is only one conjunct — a ruin's guaranteed significant
    --      item must also have been taken, and that latch is set on the
    --      Lua thread by the pickup boundary, with no edge of its own
    --      to hang an event on. So the tick polls, which is why step 2
    --      runs over every pending instance rather than only over the
    --      ones step 1 just touched: whichever conjunct lands last
    --      clears the location, and either one can land while the world
    --      thread is elsewhere.
    --
    --   Doing both here, in this order, is what makes a roster wiped
    --   out on the same tick as the last pickup clear once rather than
    --   twice or never. 'resolveLocationClearance' is the sole writer
    --   of that transition and of the one-shot notice, so a 'Just'
    --   result is exactly the caller's licence to emit.
    clearancePass isActivePage um needsRoster placed = do
        when needsRoster $
            forM_ (filter (encounterDead um) placed) $ \inst →
                atomicModifyIORef' (wsGenParamsRef ws) $ \mP → case mP of
                    Just p' → case markLocationEncounterCleared (liId inst)
                                        (wgpLocationInstances p') of
                        Just instances' →
                            (Just p' { wgpLocationInstances = instances' }, ())
                        Nothing → (mP, ())
                    Nothing → (mP, ())
        -- Step 2 re-reads the table rather than reusing @placed@: step 1
        -- may have just completed an encounter, and that instance is
        -- then satisfied in the live table while the pre-clearance
        -- snapshot still says otherwise. Reading once here keeps a
        -- roster wiped out this tick clearing on this tick.
        settled ← maybe [] (instancesToList ∘ wgpLocationInstances)
                      <$> readIORef (wsGenParamsRef ws)
        forM_ (filter awaitingNotice settled) $ \inst → do
            cleared ← atomicModifyIORef' (wsGenParamsRef ws) $ \mP → case mP of
                Just p' → case resolveLocationClearance (liId inst)
                                    (wgpLocationInstances p') of
                    Just instances' →
                        (Just p' { wgpLocationInstances = instances' }, True)
                    Nothing → (mP, False)
                Nothing → (mP, False)
            when cleared $
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
                                    (locationDiscoveryLifecycle inst)
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
                        Just p' → case resolveLocationClearance
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
    -- The instance is ready to CLEAR right now: it authors at least one
    -- clearance condition, every one of them is satisfied, and its one
    -- notice is unspent. That is exactly — and only —
    -- 'resolveLocationClearance' can act on, which is what keeps
    -- #1990's cost guard honest: a ruin whose guaranteed item is still
    -- on the floor is NOT satisfied, so it admits nothing and a
    -- fully-explored page short-circuits the whole tick exactly as it
    -- did before #917. Nothing is missed by not polling it: the two
    -- things that can satisfy it are an encounter completing (step 1
    -- above, on this thread) and an item being taken (the Lua thread's
    -- latch), and the very next tick sees the result. A location
    -- authoring NO condition is never admitted at all.
    awaitingNotice inst =
        locationAuthorsClearance inst
            ∧ not (liClearEventEmitted inst)
            ∧ locationClearanceSatisfied inst
    -- The separate, narrower question step 1 asks: is this a death-only
    -- encounter whose complete roster is now wholly dead? Kept
    -- deliberately distinct from 'pendingClearance' — #916's clearance
    -- POLICY is unchanged by #917, and a collapsed, crawling, missing
    -- or driven-away occupant still does not satisfy it.
    encounterPending inst = case liEncounter inst of
        Just encounter → leDeathOnlyClearance encounter
            ∧ leRolledCount encounter > 0
            ∧ leRosterComplete encounter
            ∧ not (leCleared encounter)
        Nothing → False
    encounterDead um inst = case liEncounter inst of
        Just encounter → encounterPending inst
            ∧ length (leOccupants encounter) ≡ leRolledCount encounter
            ∧ all occupantDead (leOccupants encounter)
        Nothing → False
      where
        occupantDead occupant = case HM.lookup (leoUnitId occupant)
                                           (umInstances um) of
            Just unitInst → uiPage unitInst ≡ pageId ∧ uiPose unitInst ≡ "dead"
            Nothing → False
