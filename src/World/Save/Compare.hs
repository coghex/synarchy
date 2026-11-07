{-# LANGUAGE Strict #-}
-- | The structural comparison of a chain of saves taken across loads: what
--   @synarchy-save-codec compare@ (and so
--   @tools\/persistence_snapshot.compare_session_files@, the persistence
--   probes' fixed-point check) counts as a difference (#2936).
--
--   __The chain.__ The first save is the LIVE capture of a running session
--   (the probes' @gen1@); every later one is the re-save taken right after a
--   load of the one before it (@gen2@, @gen3@, ...). A load is not the
--   identity on a running session: it deliberately resets runtime-only state
--   (@docs\/persistence_contract.md@ §2). So two different questions are
--   asked, and they are not the same question:
--
--   * __Is a LOAD a fixed point?__ Every re-save must equal the first
--     re-save, field for field, apart from the two physiology rebuilds
--     'World.Save.Physiology.withoutRebuiltCaches' already excuses. This is
--     the property that a persistence defect (a field a load fails to read
--     back, or writes nondeterministically) breaks, and it keeps the
--     animation fields below compared EXACTLY.
--
--   * __Does the load keep the durable state of the live session?__ The
--     first re-save must equal the live capture, apart from the playback of
--     a unit whose animation name the load re-resolved
--     ('withLoadedPlayback').
module World.Save.Compare
    ( withLoadedPlayback
    , divergentGenerations
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import World.Save.Physiology (withoutRebuiltCaches)
import World.Save.Snapshot (PageSnapshot(..), SessionSnapshot(..))
import World.Save.Types (UnitInstanceSnapshot(..), UnitSnapshot(..))

-- | The live capture with the playback of every unit whose animation the load
--   RESOLVED differently taken from the re-save: where a unit's
--   @uisCurrentAnim@ in the live capture differs from the re-save's, both
--   @uisCurrentAnim@ and @uisAnimStart@ are replaced by the re-save's.
--
--   @uiAnimOverride@ is "Runtime-only … always cleared post-load"
--   (@World.Save.Types.fromUnitInstanceSnapshot@; the inventory's
--   @uiAnimOverride@ row), while @Unit.Thread.publishToRender@ resolves the
--   unit's current animation every tick as @chooseAnim pose override
--   stateAnim@ and restarts playback (@uiAnimStart@ = now) exactly when that
--   name changes. So a unit that the Lua combat AI had in an override
--   stance (@combat_idle_RH_dagger@, an @attack_*@ swing) when the live
--   session was saved resolves to its state animation (@idle@) on the first
--   tick after the load, restarted at the loaded game time, and the AI
--   re-sets the override only once it runs again. That name, and the start
--   that follows it, are not something a load can or should restore.
--
--   The exception is per unit and conditional on the name changing. A unit
--   whose animation name is unchanged keeps its @uisAnimStart@ compared
--   exactly: the load restores it verbatim and @publishToRender@ retains it
--   while the playback is the same, so a timestamp-only difference is a lost
--   playback position, not an override reset. The rest of the animation block
--   (facing, the sim mirrors @uisActivity@ \/ @uisPose@ \/ @uisAnimStride@ \/
--   @uisAnimReverse@, which @unit-sim@ persists) and all durable state stay
--   compared.
withLoadedPlayback ∷ SessionSnapshot → SessionSnapshot → SessionSnapshot
withLoadedPlayback loadedSnap live = live { snapPages = HM.mapWithKey onPage (snapPages live) }
  where
    onPage pid pg = case HM.lookup pid (snapPages loadedSnap) of
        Nothing → pg
        Just lp → pg { pgsUnits = onUnits (pgsUnits lp) (pgsUnits pg) }
    onUnits lu us = us { usnInstances = HM.mapWithKey (onUnit (usnInstances lu)) (usnInstances us) }
    onUnit lus uid u = case HM.lookup uid lus of
        Just l | uisCurrentAnim l ≢ uisCurrentAnim u →
            u { uisCurrentAnim = uisCurrentAnim l, uisAnimStart = uisAnimStart l }
        _ → u

-- | The positions (0-based, never 0) of the saves in a chain that diverge,
--   in order. Position @i ≥ 1@ diverges when it differs from the LIVE
--   capture in anything but the playback 'withLoadedPlayback' reconciles, or — for
--   @i ≥ 2@ — from the first re-save in anything but the rebuilt physiology
--   caches (so the playback fields stay exact between loaded generations).
--   An empty answer means the chain is a fixed point.
divergentGenerations ∷ [SessionSnapshot] → [Int]
divergentGenerations (live : first : rest) =
    [ i | (i, s) ← zip [1 ..] (first : rest)
        , withLoadedPlayback s liveDurable ≢ durable s
          ∨ (i ≥ 2 ∧ durable s ≢ durable first) ]
  where
    durable     = withoutRebuiltCaches
    liveDurable = durable live
divergentGenerations _ = []
