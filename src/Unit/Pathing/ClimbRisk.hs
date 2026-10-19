{-# LANGUAGE Strict #-}
-- | The risk of a cliff ascent, and which ascents a hazard-PROTECTED
--   (@avoid_falls@) movement request may take (#2866).
--
--   Two rules, both owner decisions on #2866:
--
--     * __Height cap.__ Ambient movement never climbs more than ONE
--       z-level in one ascent ('maxAmbientClimbRise'), whatever the
--       unit's ability. The rise is the full destination-minus-source
--       terrain delta, mantle included.
--     * __Per-unit slip bound.__ It also refuses an ascent whose
--       whole-climb slip probability 'climbSlipProbability' — the SAME
--       calculation "Unit.Thread.Movement.Climb"'s @rollClimbSlips@ rolls
--       against, from the SAME per-z chance 'slipChancePerZ' — exceeds the
--       unit's acceptable risk 'slipRiskThreshold', which falls as its
--       awareness ((intelligence + perception) / 2) rises:
--
--       > T = clamp(0.20 − 0.2375 · (awareness − 0.6), 0.01, 0.20)
--
--       So a smart, perceptive unit refuses climbs it would likely fail,
--       limits differ by physical ability (skill, dexterity × strength,
--       mass) and by awareness, and even the most careful unit still
--       accepts a climb at or below 1 % risk.
--
--   Admission is a pure read of the mover's stats: it draws no RNG and
--   changes neither the physical slip chance, climb XP, nor fall damage.
--   Fall-permitted movement (player commands, survival, medic, panic,
--   mental-state movement) never consults it.
module Unit.Pathing.ClimbRisk
    ( slipChancePerZ
    , climbSlipProbability
    , ClimbAbility(..)
    , climbAbilityOf
    , slipRiskThreshold
    , maxAmbientClimbRise
    , ambientClimbAdmissible
    ) where

import UPrelude
import Unit.Thread.Movement.Types (UnitMoveStats(..))

-- | Per-z-level slip chance. Probability rises sharply with body
--   weight relative to strength × dexterity, and drops sharply with
--   climbing skill (squared so high skill effectively eliminates
--   slips). Clamped so even a perfect climber has a non-zero risk
--   and a terrible one isn't guaranteed to fall every z.
--
--     base        = 0.05
--     skill_mod   = (1 - skill/100)²
--     control_mod = clamp(1.5 / (dex × str), 0.1, 5)
--     weight_mod  = max(1, mass/70)
--     per_z       = base × skill_mod × control_mod × weight_mod
--     clamp [0.001, 0.5]
slipChancePerZ ∷ UnitMoveStats → Float
slipChancePerZ s =
    let base       = 0.05
        skillMod   = (1 - umsClimbing s / 100.0) ** 2
        dxs        = max 0.05 (umsDexterity s * umsStrength s)
        controlMod = max 0.1 (min 5.0 (1.5 / dxs))
        weightMod  = max 1.0 (umsBodyMass s / 70.0)
        raw        = base * skillMod * controlMod * weightMod
    in max 0.001 (min 0.5 raw)

-- | The probability that a climb of @rise@ z-levels slips at least once,
--   given a per-z slip chance: @1 − (1 − perZ)^rise@. This is the ONE
--   formula — @rollClimbSlips@ rolls against it and the ambient admission
--   rule judges against it, so the risk a unit refuses is exactly the
--   risk it would have run.
climbSlipProbability ∷ Float → Int → Float
climbSlipProbability perZ rise = 1 - (1 - perZ) ** fromIntegral rise

-- | What the admission rule needs to know about a mover: its per-z slip
--   chance and its awareness. Derived from the mover's stats on every
--   tick and never stored, so a changed stat applies to the next ascent.
data ClimbAbility = ClimbAbility
    { caSlipPerZ  ∷ !Float   -- ^ 'slipChancePerZ' of the mover
    , caAwareness ∷ !Float   -- ^ (intelligence + perception) / 2
    } deriving (Show, Eq)

-- | The mover's climb ability, from the same 'UnitMoveStats' snapshot
--   its physical climb and slip roll use. Absent stats read the movement
--   layer's baseline 1.0 (see "Unit.Thread.Movement"'s @statsForInst@).
climbAbilityOf ∷ UnitMoveStats → ClimbAbility
climbAbilityOf s = ClimbAbility
    { caSlipPerZ  = slipChancePerZ s
    , caAwareness = (umsIntelligence s + umsPerception s) / 2
    }

-- | The highest slip probability a protected ascent may carry for a unit
--   of this awareness:
--   @clamp(0.20 − 0.2375 · (awareness − 0.6), 0.01, 0.20)@ — 20 % at
--   awareness 0.6 or below, falling to the 1 % floor at 1.4 and above.
--   A non-finite awareness yields NaN, which no probability is ≤, so
--   such a unit takes no protected ascent at all (fail closed).
slipRiskThreshold ∷ Float → Float
slipRiskThreshold a
    | isNaN a ∨ isInfinite a = 0 / 0
    | otherwise              = max 0.01 (min 0.20 (0.20 - 0.2375 * (a - 0.6)))

-- | The tallest ascent, in z-levels, ambient movement ever takes.
maxAmbientClimbRise ∷ Int
maxAmbientClimbRise = 1

-- | May a protected request climb @rise@ z-levels (the full terrain
--   delta of a cliff step)? Only within the height cap AND at or below
--   the unit's acceptable risk; the bound is inclusive (@P ≤ T@).
ambientClimbAdmissible ∷ ClimbAbility → Int → Bool
ambientClimbAdmissible ab rise =
    rise ≤ maxAmbientClimbRise
    ∧ climbSlipProbability (caSlipPerZ ab) rise ≤ slipRiskThreshold (caAwareness ab)
