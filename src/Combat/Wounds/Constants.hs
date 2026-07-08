{-# LANGUAGE Strict, UnicodeSyntax, OverloadedStrings #-}

-- | Foundational per-tick wound tuning constants shared across the
--   "Combat.Wounds" submodules — bleed scaling, wound cleanup /
--   unconsciousness thresholds, and the clotting model. See
--   "Combat.Wounds" for the overall formula summary.
module Combat.Wounds.Constants
    ( bleedScale
    , woundCleanupThreshold
    , unconsciousFraction
    , clotBaseRate
    , clotSeed
    , clotPressureK
    , kindClotFactor
    ) where

import UPrelude

-- ----- Tuning constants -----

bleedScale ∷ Float
bleedScale = 1.2
-- Calibration check:
--   sev-1.0 slash on neck (bleed_factor 3.0, kind 1.0) of a bear
--   with constitution 2.5:
--     bleed = 1.0 × 1.0 × 3.0 × 1.2 / 2.5 = 1.44 L/s
--     → 7 L bear drains in ~5 seconds. Matches the user's spec
--   sev-0.05 scratch on finger (bleed_factor 1.0):
--     bleed = 0.0025 × 1.0 × 1.0 × 1.2 / 1.0 = 0.003 L/s — bleeds
--   sev-0.02 scratch on finger:
--     bleed = 0.0004 × 1.2 = 0.00048 L/s → below 0.001 clot
--     threshold, heals naturally.

woundCleanupThreshold ∷ Float
woundCleanupThreshold = 0.01   -- EFFECTIVE severity below this = healed, removed

unconsciousFraction ∷ Float
unconsciousFraction = 0.30   -- pose flips to Collapsed below this

-- ----- Clotting -----
-- Each wound's `woundClot` (0..1) fills over time; the bleed is scaled
-- by (1 − clot), so a fully-clotted wound has stopped bleeding. The
-- rate ACCELERATES as the clot forms and scales DOWN with severity and
-- hard-to-clot kinds, so a scratch seals in well under a minute while a
-- deep arterial bleed barely clots at all (it outpaces clotting and
-- kills first unless a medic intervenes). A bandage's pressure boosts
-- the rate, so treatment both cuts the immediate bleed AND drives the
-- wound to a full clot far sooner.
clotBaseRate ∷ Float
clotBaseRate = 0.14
-- Calibration (untreated, body constitution 1.0):
--   sev-0.15 slash:  resist = 1·(0.85)² = 0.72; accel ramps 0.08→1;
--     clots (→1.0) in ~25 game-s, bleed gone well before.
--   sev-0.5 slash:   resist = 1·0.25 = 0.25 — clots in ~2-3 min.
--   sev-0.5 arterial: resist = 0.05·0.25 = 0.0125 — effectively never
--     self-clots; bleeds out in seconds without treatment.
--   bandaged (seep 0.1): bandBoost = 1+4·0.9 = 4.6× → clot finishes
--     ~5× faster AND bleed already cut to 10%.

clotSeed ∷ Float
clotSeed = 0.08   -- initial clot-accel floor (a fresh wound at clot 0
                  -- still begins to clot; the term grows from here)

clotPressureK ∷ Float
clotPressureK = 4.0   -- bandage pressure boost: rate ×(1 + K·(1−seep))

-- Per-kind self-clot ability (multiplies the clot rate). Ordinary
-- soft-tissue wounds clot normally; a stump clots slowly; internal and
-- (especially) arterial bleeds barely clot on their own. Fracture /
-- concussion barely bleed anyway, so their clot value is moot — leave
-- them at the 1.0 default.
kindClotFactor ∷ Text → Float
kindClotFactor "arterial" = 0.05
kindClotFactor "severed"  = 0.15
kindClotFactor "internal" = 0.10
kindClotFactor _          = 1.0
