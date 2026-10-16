-- The stored BASE of a stat whose EFFECTIVE value is a target (#2835).
--
-- A unit's stat reads as `(base + sum delta) * (1 + sum percent)` over its
-- ACTIVE modifiers (Unit.Stats.effectiveStat; a modifier is active while
-- `now < expiry`, and one with no expiry always is), clamped at zero. A
-- script that works in EFFECTIVE values -- the resource tick integrating a
-- pool, a facade contribution -- and writes the result with `unit.setStat`
-- (which stores a BASE) is therefore wrong by the modifiers the moment the
-- stat carries one: a +50% modifier would turn a debit of 1 from an effective
-- 10 into 13.5. `baseFor` solves the other way, `t / (1 + sum p) - sum d`,
-- the same rule physiology transitions seed with (invertEffectiveStatWhy), so
-- every writer of a pool lands its effective value on the value it meant.
--
-- A stat with no active modifier answers the target itself, unchanged and
-- bit for bit, so the unmodified path (every shipped pool today) is exactly
-- what it was.

local M = {}

-- (sum of active flat deltas, sum of active percents) of a stat's modifiers.
function M.totals(uid, statName)
    local now, delta, percent = engine.gameTime(), 0, 0
    for _, m in ipairs(unit.getModifiers(uid, statName) or {}) do
        if m.expiry == nil or now < m.expiry then
            delta   = delta   + (m.delta   or 0)
            percent = percent + (m.percent or 0)
        end
    end
    return delta, percent
end

-- The base whose effective value is `target`, or nil when none is
-- representable: the percent multiplier is not positive, or the base would be
-- negative or not finite (a zero base is representable).
function M.baseFor(uid, statName, target)
    local delta, percent = M.totals(uid, statName)
    if delta == 0 and percent == 0 then return target end
    local multiplier = 1 + percent
    if not (multiplier > 0) then return nil end
    local base = target / multiplier - delta
    if base ~= base or base == math.huge or base == -math.huge or base < 0 then
        return nil
    end
    return base
end

return M
