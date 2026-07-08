{-# LANGUAGE Strict, UnicodeSyntax, OverloadedStrings #-}

-- | Wound-healing rate tuning and calorie-store gating. See
--   "Combat.Wounds" for the overall formula summary.
module Combat.Wounds.Healing
    ( healBaseRate
    , healClotFloor
    , sleepHealMult
    , scarSeverityThreshold
    , calorieHealFloorFrac
    , calorieHealMin
    , calorieHealMultiplier
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM

-- ----- Healing -----
-- A separate progress bar (woundHeal 0..1) from clotting. It fills
-- SLOWLY once the wound has clotted; the wound's effective severity is
-- woundSeverity × (1 − heal), so pain/impairment/residual bleed all ease
-- as it heals. At full heal the wound is removed, leaving a scar if it
-- was severe. UNIFORM rate across wound kinds (the user's call) — only
-- severed is excluded (a lost limb can't regrow). Constitution scales
-- it gently (the existing healCon), and clot gates it (an open wound
-- barely mends). The base rate is deliberately slow.
healBaseRate ∷ Float
healBaseRate = 0.0016
-- Calibration (clotted, constitution 1.0): a sev-0.5 wound reaches
-- effSev < 0.01 at heal ≈ 0.98 — about 0.98 / 0.0016 ≈ 600 s ≈ 10 min of
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
