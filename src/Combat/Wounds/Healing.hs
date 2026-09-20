{-# LANGUAGE Strict #-}

-- | Wound-healing rate tuning and calorie-store gating. See
--   "Combat.Wounds" for the overall formula summary.
module Combat.Wounds.Healing
    ( healBaseRate
    , healClotFloor
    , sleepHealMult
    , scarSeverityThreshold
    , calorieHealMultiplier
    , gameDaySeconds
    , bloodRecoveryFraction
    , bloodRecoveryDays
    , bloodRecoveryHydrationFloor
    , bloodRecoveryBaseRate
    , bloodRecoveryNutritionMultiplier
    , bloodRecoveryConstitutionMultiplier
    , bloodRecoveryDelta
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM

-- ----- Healing -----
-- A separate progress bar (woundHeal 0..1) from clotting. It fills
-- SLOWLY once the wound has clotted; advancing it eases the wound's
-- effective severity — 'Unit.Types.Wound.woundEffSeverity', which is
-- max (woundSeverity × (1 − heal)) woundNecrosis — so pain/impairment/
-- residual bleed all ease with it, down to the necrosis floor that
-- healing cannot cross. Cleanup is driven by that effective severity,
-- not by this bar: the wound is removed once it falls below
-- woundCleanupThreshold, leaving a scar if it was severe — so a wound
-- whose necrosis reaches that threshold can hit full heal and still not
-- be removed, because the floor holds it there. UNIFORM rate across
-- wound kinds (the user's call) — only severed is excluded (a lost limb
-- can't regrow). Constitution scales it gently (the existing healCon),
-- and clot gates it (an open wound barely mends). The base rate is
-- deliberately slow.
healBaseRate ∷ Float
healBaseRate = 0.0016
-- Calibration (clotted, constitution 1.0, and no necrosis — necrosis at or
-- above woundCleanupThreshold floors effective severity there, so such a
-- wound never heals out however far woundHeal advances; below the threshold
-- the floor is invisible to cleanup): a sev-0.5 wound reaches effective
-- severity < 0.01 at heal ≈ 0.98 — about 0.98 / 0.0016 ≈ 600 s ≈ 10 min of
-- clotted time. A scratch (sev 0.05) heals out at heal ≈ 0.8 → faster.

healClotFloor ∷ Float
healClotFloor = 0.05   -- an un-clotted wound heals at 5 % of the rate
                       -- (clot scales it from this floor up to full)

sleepHealMult ∷ Float
sleepHealMult = 4.0    -- rest/sleep heal speed-up, applied via restMult
                        -- below when uiPose == "sleeping"

scarSeverityThreshold ∷ Float
scarSeverityThreshold = 0.3   -- wounds milder than this heal scar-free

-- Calorie gating: a starving body heals slower (the food system's
-- "starving units heal significantly slower"). The unit's calories/
-- max_calories fraction (the ENERGY STORE, not the stomach meter — a
-- unit with an empty stomach but a fed store heals fine) drives a
-- heal-rate multiplier — full above calorieHealFloorFrac of the pool,
-- ramping down to calorieHealMin at an empty store.
calorieHealFloorFrac ∷ Float
calorieHealFloorFrac = 0.5

calorieHealMin ∷ Float
calorieHealMin = 0.25

-- | Heal-rate multiplier from the unit's calorie-store state. Gated on
--   the PRESENCE of a live "calories" stat — that's the real "this unit
--   runs on calories" signal. max_calories alone isn't: it's a body-
--   derived stat seeded for any unit with a body block (wildlife
--   included), but only food-system units (acolytes) ever get a draining
--   "calories" pool. A unit without it — wildlife, or an acolyte before
--   its first resource tick — heals ungated rather than being mistaken
--   for starving.
calorieHealMultiplier ∷ HM.HashMap Text Float → Float
calorieHealMultiplier stats =
    case (HM.lookup "calories" stats, HM.lookup "max_calories" stats) of
        (Just cur, Just maxH)
            | maxH > 0 →
                let frac = cur / maxH
                in if frac ≥ calorieHealFloorFrac then 1.0
                   else calorieHealMin
                      + (1 - calorieHealMin) * (frac / calorieHealFloorFrac)
        _ → 1.0

-- ----- Blood recovery (#2639) -----
-- A living unit whose wounds have stopped bleeding entirely rebuilds its
-- blood volume. Before this, blood only ever went DOWN: the wound tick
-- subtracted drain and nothing but spawn seeding and save restoration
-- ever wrote it back up, so a unit that collapsed from blood loss and was
-- then fully stabilized stayed collapsed forever — the revive gate in
-- scripts/unit_resource_tick.lua needs 50 % of maximum and collapse fires
-- below 30 %, a band nothing could ever cross.
--
-- The policy introduces no new skill, stat or persisted state: it scales
-- the unit's existing maximum blood volume by its live nutrition pools
-- and its constitution stat. Eligibility (zero aggregate post-tick bleed
-- rate, alive, body_mass present) belongs to the caller — see
-- @recoveredBloodVolume@ in "Combat.Wounds.Tick"; everything here is rate.

-- | One game day in seconds at time scale 1, matching @scripts\/unit_stats.lua@.
gameDaySeconds ∷ Float
gameDaySeconds = 1440

-- | Share of maximum blood volume recovered over 'bloodRecoveryDays' at
--   full nutrition and constitution 1.0.
bloodRecoveryFraction ∷ Float
bloodRecoveryFraction = 0.70

bloodRecoveryDays ∷ Float
bloodRecoveryDays = 3

-- | Hydration fraction at or above which blood recovery is permitted.
--   Matches the dehydration survival alert in
--   @scripts\/unit_resource_alerts.lua@, which fires below 0.25 of
--   max_hydration: a unit in survival-critical thirst rebuilds no blood.
--   EXACTLY 0.25 permits recovery (at factor 0.25), so the alert boundary
--   and this gate agree on which side of it the unit is on.
bloodRecoveryHydrationFloor ∷ Float
bloodRecoveryHydrationFloor = 0.25

-- | Litres per second recovered by an eligible unit at full nutrition and
--   constitution 1.0. Calibration: 30 % → 50 % of maximum takes
--   @0.20 × 4320 \/ 0.70 ≈ 1234 s@, about 6\/7 of a game day, and 30 % →
--   100 % takes the full three days. Nutrition and constitution
--   deliberately spread the real times around those figures.
bloodRecoveryBaseRate ∷ Float → Float
bloodRecoveryBaseRate maxBlood =
    maxBlood * bloodRecoveryFraction / (bloodRecoveryDays * gameDaySeconds)

-- | Nutrition scaling: the LOWER of an independent calorie factor and
--   hydration factor, so whichever pool is worse governs.
--
--   An ABSENT live pool reads 1.0, preserving the same distinction
--   'calorieHealMultiplier' already draws — wildlife and acolytes before
--   their first resource tick have body-derived maxima but no draining
--   pool, and must not be mistaken for starving. A PRESENT pool with a
--   missing or non-positive maximum cannot be evaluated and reads 0.
--
--   With a valid pool, calories scale linearly with the clamped
--   current\/maximum fraction (so an empty store blocks recovery
--   outright), while hydration blocks recovery below
--   'bloodRecoveryHydrationFloor' and scales linearly at or above it.
bloodRecoveryNutritionMultiplier ∷ HM.HashMap Text Float → Float
bloodRecoveryNutritionMultiplier stats =
    min (poolFactor "calories" "max_calories" 0)
        (poolFactor "hydration" "max_hydration" bloodRecoveryHydrationFloor)
  where
    poolFactor curKey maxKey floorFrac = case HM.lookup curKey stats of
        Nothing  → 1.0
        Just cur → case HM.lookup maxKey stats of
            Just maxV
                | maxV > 0 →
                    let frac = cur / maxV
                    in if frac < floorFrac then 0 else max 0 (min 1 frac)
            _ → 0

-- | Constitution scaling, clamped to [0.75, 1.5] so a hardy constitution
--   helps noticeably without dominating recovery. Absent reads 1.0.
bloodRecoveryConstitutionMultiplier ∷ HM.HashMap Text Float → Float
bloodRecoveryConstitutionMultiplier stats =
    let con = HM.lookupDefault 1.0 "constitution" stats
    in max 0.75 (min 1.5 (1 + 0.25 * (con - 1)))

-- | Litres recovered over @dt@ seconds by an ELIGIBLE unit with the given
--   maximum blood volume. Never negative; the maximum-volume clamp is the
--   caller's, because only it knows the unit's current volume.
bloodRecoveryDelta ∷ HM.HashMap Text Float → Float → Float → Float
bloodRecoveryDelta stats maxBlood dt =
    max 0 ( bloodRecoveryBaseRate maxBlood
          * bloodRecoveryNutritionMultiplier stats
          * bloodRecoveryConstitutionMultiplier stats
          * dt )
