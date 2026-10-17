{-# LANGUAGE Strict #-}
-- | The circulation rule (#2836, S9; design
--   @docs\/designs\/physiology_stat_modes_design.md@ S9.2, SD-21): the pure
--   function that turns a unit's blood, body, sickness, core temperature and
--   heart rate into how well it perfuses its tissues, @0.05..1@.
--
--   It is a port of the Lua @circulation.compute@ it replaces and nothing
--   else: the constants, the clamps and the order of the factors are the
--   ones that function had, so every number it produced is still the number
--   produced. What changed is WHO computes it. 'circulationRule' is the one
--   production implementation: "Unit.Physiology" gathers its inputs from a
--   unit snapshot (the mode-aware core temperature and heart rate, the
--   effective stats, "Unit.Blood"'s fraction) and @unit.readPhysio@ hands
--   the answer to Lua, so the facade, thermo, cardio and the Status row all
--   read the same on-demand value, never a stored or lagged copy.
--
--   The rule takes the resting heart rate as an argument rather than
--   spelling 70, because the registry's @RESTING_HR@ constant is the
--   authority for it.
module Unit.Physiology.Circulation
    ( CirculationInputs(..)
    , circulationRule
    ) where

import UPrelude

-- | Everything the rule reads, already resolved by the caller: the effective
--   values of the unit's stats (with their missing-value fallbacks applied),
--   the clamped blood fraction and the mode-aware core temperature.
data CirculationInputs = CirculationInputs
    { ciBloodFraction ∷ !Float
      -- ^ blood volume over its maximum, @0..1@ ("Unit.Blood").
    , ciConstitution  ∷ !Float
    , ciFatMass       ∷ !Float
    , ciBodyMass      ∷ !Float
    , ciSepsis        ∷ !Float
    , ciCore          ∷ !Float
      -- ^ core temperature in degrees C; the thermo baseline for a Nominal
      --   or Unsupported thermo.
    , ciHeartRate     ∷ !(Maybe Float)
      -- ^ the effective heart rate, or 'Nothing' under a Nominal cardio,
      --   whose heart-rate factor is exactly 1.0 (SD-21: only the heart
      --   rate reads healthy, every other factor stays live).
    } deriving (Show, Eq)

-- | Circulation @0.05..1@ (1 = perfect), the product of five factors:
--
--     * blood volume (haemorrhagic shock): @clamp 0.1 1 (1.1 × fraction)@;
--     * fitness, constitution times body composition (obesity hurts):
--       @clamp 0.3 1.1 ((0.5 + 0.5 con) × (1 − 0.7 × max 0 (fatFrac −
--       0.20)))@, where the fat fraction is fat over body mass, or 0.2 for
--       a non-positive body mass;
--     * sickness (sepsis drives septic shock): @clamp 0.4 1 (1 − 0.5
--       sepsis)@;
--     * cold vasoconstriction: @clamp 0.45 1 (1 − 0.06 × max 0 (36 −
--       core))@;
--     * heart rate (a slow heart perfuses poorly, a fast one earns no
--       bonus): @clamp 0.5 1 (hr \/ resting)@, or exactly 1.0 when 'ciHeartRate'
--       is 'Nothing'.
circulationRule ∷ Float → CirculationInputs → Float
circulationRule resting i = clamp 0.05 1.0
    (bloodFactor * fitness * sickness * vaso * hrFactor)
  where
    bloodFactor = clamp 0.1 1.0 (ciBloodFraction i * 1.1)
    fatFrac | ciBodyMass i > 0 = ciFatMass i / ciBodyMass i
            | otherwise        = 0.2
    fitness = clamp 0.3 1.1
        ((0.5 + 0.5 * ciConstitution i)
         * (1 - 0.7 * max 0 (fatFrac - 0.20)))
    sickness = clamp 0.4 1.0 (1 - 0.5 * ciSepsis i)
    vaso = clamp 0.45 1.0 (1 - 0.06 * max 0 (36.0 - ciCore i))
    hrFactor = case ciHeartRate i of
        Nothing → 1.0
        Just hr → clamp 0.5 1.0 (hr / resting)
