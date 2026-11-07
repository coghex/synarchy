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
--     first re-save must equal the live capture, apart from the
--     override-driven playback fields ('withoutOverrideDrivenPlayback').
module World.Save.Compare
    ( withoutOverrideDrivenPlayback
    , divergentGenerations
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import World.Save.Physiology (withoutRebuiltCaches)
import World.Save.Snapshot (PageSnapshot(..), SessionSnapshot(..))
import World.Save.Types (UnitInstanceSnapshot(..), UnitSnapshot(..))

-- | A session snapshot without the playback fields the engine derives from
--   the runtime-only animation override: @uisCurrentAnim@ and @uisAnimStart@.
--
--   @uiAnimOverride@ is "Runtime-only … always cleared post-load"
--   (@World.Save.Types.fromUnitInstanceSnapshot@; the inventory's
--   @uiAnimOverride@ row), while @Unit.Thread.publishToRender@ resolves the
--   unit's current animation every tick as @chooseAnim pose override
--   stateAnim@ and restarts playback (@uiAnimStart@ = now) exactly when that
--   name changes. So a unit that the Lua combat AI had in an override
--   stance (@combat_idle_RH_dagger@, an @attack_*@ swing) when the live
--   session was saved resolves to its state animation (@idle@) on the first
--   tick after the load, and the AI re-sets the override only once it runs
--   again. The name, and the start that follows it, are therefore not
--   something a load can or should restore.
--
--   Everything else the animation block carries is kept: facing, the sim
--   mirrors @uisActivity@ / @uisPose@ / @uisAnimStride@ / @uisAnimReverse@
--   (the sim state is persisted by @unit-sim@) and all of a unit's durable
--   state.
withoutOverrideDrivenPlayback ∷ SessionSnapshot → SessionSnapshot
withoutOverrideDrivenPlayback snap = snap { snapPages = HM.map onPage (snapPages snap) }
  where
    onPage pg = pg { pgsUnits = onUnits (pgsUnits pg) }
    onUnits us = us { usnInstances = HM.map onUnit (usnInstances us) }
    onUnit u = u { uisCurrentAnim = "", uisAnimStart = 0 }

-- | The positions (0-based, never 0) of the saves in a chain that diverge,
--   in order. Position @i ≥ 1@ diverges when it differs from the LIVE
--   capture in anything but the override-driven playback fields, or — for
--   @i ≥ 2@ — from the first re-save in anything but the rebuilt physiology
--   caches (so the playback fields stay exact between loaded generations).
--   An empty answer means the chain is a fixed point.
divergentGenerations ∷ [SessionSnapshot] → [Int]
divergentGenerations (live : first : rest) =
    [ i | (i, s) ← zip [1 ..] (first : rest)
        , durable s ≢ durable live
          ∨ (i ≥ 2 ∧ withoutRebuiltCaches s ≢ withoutRebuiltCaches first) ]
  where
    durable = withoutOverrideDrivenPlayback ∘ withoutRebuiltCaches
divergentGenerations _ = []
