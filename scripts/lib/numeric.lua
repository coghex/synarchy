-- Shared pure-numeric helpers (issue #1158).
--
-- `clamp` was copy-pasted character-for-character into all eleven
-- physiology/mental-state modules (brain, cardio, circulation,
-- consumable, exhaustion, mental_state, movement_speed, salts,
-- starvation, thermo, thoughts), which have no other shared utility
-- module between them. One definition here is the whole point: a
-- divergence in one copy silently gave that one meter a different
-- saturation rule from the other ten.
--
-- Deliberately depends on NOTHING but the Lua standard library -- no
-- consumer module, no engine global. `scripts/unit_resources.lua` pulls
-- the physiology graph in eagerly at load, so a require() edge from
-- here back into that graph would be a load-order cycle; keeping this
-- leaf pure is what makes it safe to require from any of them.

local numeric = {}

-- Clamp `x` into [lo, hi]. Byte-identical semantics to the eleven
-- copies this replaces, degenerate cases included: with lo > hi the
-- max wins and `lo` comes back, and a non-number argument raises from
-- math.min/math.max exactly as before.
function numeric.clamp(x, lo, hi)
    return math.max(lo, math.min(hi, x))
end

return numeric
