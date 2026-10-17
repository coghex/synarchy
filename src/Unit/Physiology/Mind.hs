{-# LANGUAGE Strict #-}
-- | The brain's pure rules (#2838, S11a; design
--   @docs\/designs\/physiology_stat_modes_design.md@ S11.2): the functions
--   that turn a unit's already-resolved inputs into @consciousness@,
--   @concentration@, @state_of_mind@ and the mood-drift target.
--
--   They are ports of @scripts\/brain.lua@'s equations and nothing else: the
--   constants, the clamps and the order of the factors are the ones Lua had,
--   so every number it produced is still the number produced. What changed is
--   WHO computes them. These rules are the one production implementation:
--   "Unit.Physiology" gathers their inputs from a unit snapshot (each read
--   through the mode-aware accessor) and @unit.readPhysio@, the facade, the
--   combat thread and the craft transaction all call the same function, so
--   none of them can observe a cache that lags the state it was derived from.
--   They store nothing, draw nothing and advance nothing.
module Unit.Physiology.Mind
    ( ConsciousnessInputs(..)
    , consciousnessRule
    , concentrationRule
    , stateOfMindRule
    , MoodTargetInputs(..)
    , moodTargetRule
    , euphoricState
    , euphoriaLift
    ) where

import UPrelude

-- | Core temperature, blood oxygen and salt concentration, each already read
--   through its own system's mode (a Nominal or Unsupported system supplies
--   its healthy value).
data ConsciousnessInputs = ConsciousnessInputs
    { cnCore ∷ !Float
    , cnOxygen ∷ !Float
    , cnSaltConcentration ∷ !Float
    } deriving (Show, Eq)

-- | Consciousness @0..1@: the WORST of the temperature, oxygen and salt
--   factors. Unchanged from the original @brain.lua@.
--
--     * temperature: @clamp ((core − 30) / 4)@ below 34 °C,
--       @clamp ((42 − core) / 2.5)@ above 39.5 °C, otherwise 1;
--     * oxygen: @clamp ((o2 − 0.5) / 0.3)@;
--     * salt: @clamp (1 − (|conc − 1| − 0.25) / 0.35)@.
consciousnessRule ∷ ConsciousnessInputs → Float
consciousnessRule i = minimum [tempFactor, oxygenFactor, saltFactor]
  where
    core = cnCore i
    tempFactor
        | core < 34.0  = clamp 0 1 ((core - 30.0) / 4.0)
        | core > 39.5  = clamp 0 1 ((42.0 - core) / 2.5)
        | otherwise    = 1.0
    oxygenFactor = clamp 0 1 ((cnOxygen i - 0.5) / 0.3)
    saltFactor   = clamp 0 1
        (1.0 - (abs (cnSaltConcentration i - 1.0) - 0.25) / 0.35)

-- | The code of the EUPHORIC mental state (@mental.EUPHORIC@ in
--   @scripts\/mental_state.lua@ and Combat's @mentalEffectiveness@: change
--   all three in lockstep).
euphoricState ∷ Float
euphoricState = 3.0

-- | The clamped concentration lift a euphoric episode gives, applied exactly
--   once, in 'concentrationRule' (@mental.TUNE.EUPHORIA_CONCENTRATION_BONUS@
--   no longer exists: the rule owns it).
euphoriaLift ∷ Float
euphoriaLift = 0.10

-- | Concentration @0..1@, no integration: consciousness gates it hard, pain
--   (@0..1@, 'Combat.Resolution.Common.painFor') and a low stamina fraction
--   erode it without ever zeroing it, caffeine adds up to 0.15, and a
--   euphoric episode lifts it by 'euphoriaLift' (clamped).
concentrationRule ∷ Float → Float → Float → Float → Bool → Float
concentrationRule consciousness pain staminaFraction caffeine euphoric =
    if euphoric then clamp 0 1 (base + euphoriaLift) else base
  where
    staminaFactor = 0.4 + 0.6 * staminaFraction
    base = clamp 0 1
        (consciousness * (1.0 - 0.6 * pain) * staminaFactor + 0.15 * caffeine)

-- | The unified state of mind: the worse of consciousness and wellbeing
--   (mood dragged down by lingering emotional pain). Under a Nominal mind
--   mood is 1 and emotional pain 0, so it equals consciousness.
stateOfMindRule ∷ Float → Float → Float → Float
stateOfMindRule consciousness mood emotionalPain =
    min consciousness (clamp 0 1 (mood - 0.5 * emotionalPain))

-- | What the mood drifts toward, every input already normalized @0..1@.
data MoodTargetInputs = MoodTargetInputs
    { mtPain ∷ !Float
    , mtHungerFraction ∷ !Float
    , mtStaminaFraction ∷ !Float
    , mtConsciousness ∷ !Float
    , mtAwareness ∷ !Float
    } deriving (Show, Eq)

-- | The mood-drift target: content (1) minus weighted deficits. The weights
--   are @scripts\/brain.lua@'s @MOOD_W_*@; a test pins this equal to the Lua
--   target for the same inputs.
moodTargetRule ∷ MoodTargetInputs → Float
moodTargetRule i = clamp 0 1
    (1.0 - (0.45 * mtPain i
          + 0.20 * (1.0 - mtHungerFraction i)
          + 0.20 * (1.0 - mtStaminaFraction i)
          + 0.35 * (1.0 - mtConsciousness i)
          + 0.15 * (1.0 - mtAwareness i)))
