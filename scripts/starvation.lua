-- Calorie-store threshold effects (#806).
--
-- PR #296 closed #92 while implementing the calorie-store's heat and
-- healing couplings, but omitted the two threshold effects #92 also
-- specified: a HUNGRY band (movement/fatigue) and a lower, more severe
-- STARVING band (strength). Both are gated on the "calories" ENERGY
-- STORE fraction (calories / max_calories) — NOT the "hunger" stomach
-- meter #93 introduced — so they track the same store the heat/healing
-- couplings already key on, and recover the instant the store refills.
--
-- This is the one place the two fraction thresholds + both multipliers
-- live, so movement_speed.lua (sprint), the per-tick strength refresh
-- (scripts/unit_resources.lua), unit_info_v2_status.lua's Hungry/
-- Starving condition rows, and tools/physiology_probe.py all read
-- through here and can't drift onto different numbers — mirrors
-- scripts/exhaustion.lua's shape exactly (a dedicated module holding a
-- fatigue-style band's thresholds + a speedMultiplier consumed by
-- movement_speed.lua).
--
-- Distinct from unit_resource_alerts.lua's "is starving" survival
-- popup: that fires only once the store is fully EMPTY (calories <= 0,
-- 25%-of-max rearm) as a narrower death-clock warning tied to Phase 4
-- organ failure, and is left untouched here — see the comment there.

local M = {}

local function clamp(x, lo, hi) return math.max(lo, math.min(hi, x)) end

-- Fraction 0..1 of the calorie ENERGY STORE. nil (not a band member —
-- callers all treat this as "unaffected") when the unit has no live
-- "calories"/"max_calories" pair, e.g. wildlife with no food system
-- (requirement 4: units without a live calorie pool must remain
-- unaffected).
function M.fraction(uid)
    local cur = unit.getStat(uid, "calories")
    local mx  = unit.getStat(uid, "max_calories")
    if cur and mx and mx > 0 then return clamp(cur / mx, 0, 1) end
    return nil
end

-- Hungry band (movement/fatigue): starts at HUNGRY_START, floors at
-- HUNGRY_MIN_MULT at a fully empty store. Mirrors exhaustion.lua's
-- FATIGUE_START/FATIGUE_MIN_MULT shape exactly, so a hungry unit reads
-- as comparably sluggish to a fatigued one.
local HUNGRY_START    = 0.35
local HUNGRY_MIN_MULT = 0.60

-- Starving band (strength): a narrower, more severe band nested inside
-- the hungry range — only the last stretch toward an empty store costs
-- raw power. Floors at STARVING_MIN_MULT (half strength), a serious
-- but non-zero handicap, matching this codebase's other "still
-- functional, just badly handicapped" floors (exhaustion 0.55, salts'
-- cramp floor 0.40).
local STARVING_START    = 0.15
local STARVING_MIN_MULT = 0.50

-- Movement multiplier. Continuous over the WHOLE depleted range (not
-- just down to STARVING_START) — a unit that's also starving is worse
-- off for movement than one that's merely hungry, not capped at the
-- hungry floor.
function M.speedMultiplier(uid)
    local f = M.fraction(uid)
    if not f or f >= HUNGRY_START then return 1.0 end
    local t = clamp((HUNGRY_START - f) / HUNGRY_START, 0, 1)
    return 1.0 - (1.0 - HUNGRY_MIN_MULT) * t
end

-- Strength multiplier. Full (1.0) above STARVING_START — a merely
-- hungry unit is slower but not weaker.
function M.strengthMultiplier(uid)
    local f = M.fraction(uid)
    if not f or f >= STARVING_START then return 1.0 end
    local t = clamp((STARVING_START - f) / STARVING_START, 0, 1)
    return 1.0 - (1.0 - STARVING_MIN_MULT) * t
end

-- "full" | "hungry" | "starving" — the one place a human-facing label
-- is derived from the fraction, so UI status can't quote a threshold
-- that drifts from the multipliers above.
function M.band(uid)
    local f = M.fraction(uid)
    if not f then return "full" end
    if f < STARVING_START then return "starving" end
    if f < HUNGRY_START then return "hungry" end
    return "full"
end

-- Re-derive the engine "strength" stat every calorie tick from the
-- untainted "strength_body" anchor (Unit.Thread.Command.Body.
-- recomputeBodyDerivedStats writes both, see Body.hs) scaled by the
-- current starving multiplier. recomputeBody itself only re-runs on a
-- body-composition CHANGE (catabolism/regrowth), far rarer than the
-- calorie store draining — so scripts/unit_resources.lua calls this
-- every physiology pass to keep "strength" (the value BOTH the engine
-- combat resolver's statOr "strength" and every Lua strength consumer
-- actually read) fresh as the store crosses the band, independent of
-- any mass change. Always re-derived from strength_body rather than
-- strength itself, so repeated calls never compound the discount and
-- refilling the store restores the value exactly (never touches
-- strength_base). No-op for a unit with no strength_body yet (no body
-- block, or a save from before this recompute existed).
function M.refreshStrength(uid)
    local base = unit.getStat(uid, "strength_body")
    if not base then return end
    local target = base * M.strengthMultiplier(uid)
    local current = unit.getStat(uid, "strength")
    if not current or math.abs(target - current) > 1e-4 then
        unit.setStat(uid, "strength", target)
    end
end

return M
