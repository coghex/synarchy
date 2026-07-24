{-# LANGUAGE UnicodeSyntax #-}

-- | Per-tick wound subsystem. Runs at ~10 Hz from the combat thread
--   (the combat thread itself runs at 60 Hz but only ticks wounds on
--   every 6th iteration — finer resolution buys nothing visible to
--   the player). Mutates `uiWounds` (severity decay) and `uiBlood`
--   (bleeding drain), promotes alive→Collapsed at the 30%-blood
--   threshold, and alive→Dead at ≤0 blood, emitting an
--   "exsanguination" CombatEvent.
--
-- ## Formula summary (tunable constants at top of file)
--
-- bleed_per_sec(w, part, constitution) =
--     severity^2 × kind_factor × part.bleed_factor × bleedScale
--     / clamp(constitution, 0.5, 2.0)
--
-- heal_per_sec(w, constitution) =
--     healBaseRate × clotFactor(woundClot) × restMult ×
--     clamp(constitution, 0.3, 3.0) × infectionMult × calorieMult
--   clotFactor ramps from healClotFloor (open wound) up to 1.0 (fully
--   clotted); woundClot itself advances elsewhere in this module
--   (bandages/dressings/time), and infection/calorie state gate the
--   rate further.
--
-- Wounds with severity < woundCleanupThreshold are dropped from the
-- list to keep the per-tick scan cheap.
--
-- All blood values are litres; bodyMass-derived max_blood is
-- recomputed live so wasting/regrowth carries through naturally.
--
-- Split (issue #566) into focused submodules under "Combat.Wounds.*":
--
--     * "Combat.Wounds.Constants" — bleed scale, cleanup/unconscious
--       thresholds, and the clotting model.
--     * "Combat.Wounds.Healing" — heal-rate tuning and calorie gating.
--     * "Combat.Wounds.Infection" — infection/necrosis/immunity tuning
--       and the pure infection-type selection logic.
--     * "Combat.Wounds.Bleed" — per-kind bleed factors and the current
--       bleed-rate query (info panel).
--     * "Combat.Wounds.Sever" — the destroyed-part severing cascade.
--     * "Combat.Wounds.Tick" — the per-tick orchestration entry point
--       and the pure per-unit wound tick.
--
--   This module re-exports the public API unchanged.
module Combat.Wounds
    ( tickAllWounds
    , propagateSevering   -- exposed for unit testing
    , tickOneUnit         -- exposed for unit testing (pure per-unit wound tick)
    , bleedRateFor        -- current L/sec blood loss (for the info panel)
    , kindBleedFactor     -- per-kind bleed multiplier (treat-action ranking)
    , isExternallyBleedingKind  -- external- vs internal-only wound kinds (#882)
    , externalBleedRateFor      -- current external-only L/sec (#882)
    , dominantExternalBleedWound -- representative externally-bleeding wound (#882)
    , destroyThreshold    -- the "structurally destroyed" severity (#607 impact blood)
    ) where

import Combat.Wounds.Tick (tickAllWounds, tickOneUnit)
import Combat.Wounds.Sever (propagateSevering, destroyThreshold)
import Combat.Wounds.Bleed
    ( bleedRateFor, kindBleedFactor, isExternallyBleedingKind
    , externalBleedRateFor, dominantExternalBleedWound
    )
