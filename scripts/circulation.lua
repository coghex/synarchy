-- Circulation — how well the unit perfuses blood to its tissues (0..1, 1 =
-- perfect). A DERIVED measure, not an integrated meter and (since #2836, S9)
-- not a stored one either: the cardio system derives it on demand through the
-- shared pure rule (Unit.Physiology.Circulation.circulationRule, reached
-- through the physiology facade), so every reader sees the current blood, stats, core
-- temperature, heart rate and active modifiers with no tick, cache or lag.
--
-- It is the second layer of the homeostasis sim (after core body temperature)
-- and feeds a growing list of effects:
--   * thermo: cold vasoconstriction (poor circulation conserves core heat but
--     starves the extremities — the setup for frostbite).
--   * frostbite: poor circulation + cold → extremities freeze.
--   * cardio: the oxygen delivery that sets blood_oxygen.
--
-- Factors (multiplicative; the rule's constants and clamps live in Haskell):
-- blood volume (shock), fitness (constitution + body composition — the
-- obese/frail circulate worse), sickness (sepsis → septic shock), cold
-- vasoconstriction and the heart-rate factor. Under a Nominal cardio only the
-- heart-rate factor is substituted (exactly 1.0): blood loss, sepsis, fitness
-- and cold still act (SD-21). "Old" has no stat yet → constitution stands in
-- for frailty; add an age input to the rule when it exists.
--
-- This module is only the Lua name for that read; it holds no formula.

local physiology = require("scripts.physiology")

local circulation = {}

-- The unit's circulation now, or nil when cardio is Unsupported for it (a
-- unit with no body has none). Never stored, so nothing here can go stale.
function circulation.get(uid)
    return (physiology.get(uid, "circulation"))
end

-- Same answer as `get`: the retained name of the old explicit-core entry
-- point. The core temperature is the unit's own mode-aware reading (37.0 for
-- a Nominal or Unsupported thermo), read inside the rule.
circulation.compute = circulation.get

return circulation
