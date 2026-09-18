-- Sub-precision carry for scripts/unit_resource_tick.lua (#2633).
--
-- A unit's resource pools live in the unit manager as binary32
-- (`uiStats :: HashMap Text Float`, src/Unit/Types/Instance.hs), while
-- the tick computes in Lua doubles. Two things used to fall out of that
-- seam, and both made the simulated OUTCOME depend on how the update
-- was partitioned rather than on simulated time:
--
--   * The tick wrote a resource back only when the change cleared
--     1e-4 and kept no remainder for the write it skipped. That turned
--     1e-4 per tick into a minimum drain/regen RATE: at the shipped
--     0.1 s cadence (scripts/init_loader.lua) a small pool -- a red
--     squirrel's sleep pressure at endurance 0.3 drains 8.3e-5 per
--     tick -- never moved at all, however long the game ran, while the
--     same configuration integrated at dt = 1.0 depleted normally.
--   * Even writing unconditionally, every write rounds to binary32, so
--     the rounding error of ten times as many writes accumulates. Over
--     1,800 game-seconds of a constant 1/3600 drain on a pool of 3.0,
--     dt = 0.1 lands on 1.4986802 where dt = 1.0 lands on 1.5000372.
--
-- So this module owns both halves of one contract: the tick writes
-- EVERY tick (nothing is deferred, so nothing can be lost), and the
-- part of the intended value binary32 could not represent is kept here
-- in double precision and folded into the next tick's starting value.
-- Integration is then exact in doubles at any dt, and storage is always
-- the correctly rounded image of it.
--
-- What this module deliberately does NOT do is bank a change outside
-- storage for later:
--
--   * The remainder is only ever the last write's sub-binary32 residue,
--     never a whole pending change, so a caller that stops ticking a
--     resource loses at most half an ulp.
--   * It is only reused when storage still holds exactly what this
--     module last wrote there. ANY intervening write -- a drink, a
--     debug setStat, a load restoring a snapshot, a reused unit id --
--     discards it rather than replaying stale arithmetic over it.
--   * The remainder is taken from the value that was WRITTEN, which is
--     already clamped, so a change the pool could not absorb is
--     discarded at the bound rather than saved up and repaid the moment
--     the pool moves off it. Ticking a drain against an empty pool for
--     an hour leaves nothing owed.
--
-- Both reads go through the same `unit.getStat` the tick itself reads
-- `current` with -- the EFFECTIVE value -- so the stored value compared
-- against and the remainder taken from it are the same quantity. No
-- resource declares a stat modifier today; if one ever did, the
-- comparison below would simply see the shifted value as somebody
-- else's write and stop carrying, which is the safe direction.
--
-- The state is transient by construction (it is meaningless without the
-- stored value it is a remainder OF) and is never serialized:
-- scripts/unit_resources.lua clears it from its save_modules RESET
-- hook, which runs on every load. Entries for a unit that stops being
-- ticked without dying linger, exactly as unit_resource_alerts' own
-- per-uid cache does and for the same reason: they are tiny, bounded by
-- the units ever spawned, and a reused id cannot inherit one, because
-- the stored value it names has to still be there for it to be used.
local M = {}

-- uid → resourceName → { stored = <what we wrote, as storage read it
-- back>, residue = <intended double minus that> }.
local pending = {}

-- Drop every remainder held for one unit (`resourceName` nil), or for
-- one of its resources. Called when a pool is (re)initialised to full
-- and when the unit dies, so nothing survives to be re-applied to a
-- pool this module is no longer the author of.
function M.forget(uid, resourceName)
    if resourceName == nil then
        pending[uid] = nil
        return
    end
    local byUnit = pending[uid]
    if byUnit then byUnit[resourceName] = nil end
end

-- Save-load reset hook. Cleared IN-PLACE: `pending` is an upvalue of
-- the closures above, so reassigning it would orphan them. A load
-- replaces the unit manager wholesale and can rewind umNextId, so a
-- surviving entry could otherwise attach to a different unit that
-- happens to reuse the id AND happens to hold the same stored value.
function M.resetOnLoad()
    for k in pairs(pending) do pending[k] = nil end
end

-- Integrate one tick of `amount` onto `current` and commit the result.
--
-- `current` is what the caller read from storage this tick; `maxVal` is
-- the pool's upper bound. Returns the value storage ACTUALLY holds
-- afterwards -- binary32, not the Lua double that produced it -- so a
-- threshold decision in the same tick judges the committed value and
-- never one the rounding moved to the other side of the line. Returns
-- nil if the unit went away under the write, leaving nothing to decide.
function M.integrate(uid, resourceName, current, amount, maxVal)
    local byUnit = pending[uid]
    local entry  = byUnit and byUnit[resourceName]
    local exact  = current
    if entry then
        if entry.stored == current then
            exact = current + entry.residue
        else
            -- Somebody else owns this value now.
            byUnit[resourceName] = nil
        end
    end

    -- Clamp BEFORE the write, and take the remainder from the clamped
    -- value below: that is what keeps overflow past a bound out of the
    -- remainder entirely, rather than a second rule about bounds.
    local value = exact + amount
    if value < 0 then
        value = 0
    elseif value > maxVal then
        value = maxVal
    end

    unit.setStat(uid, resourceName, value)
    -- Read back rather than predicting the rounding: this is the one
    -- value that is true by construction, and it is also what makes the
    -- residue below exact instead of a model of binary32.
    local stored = unit.getStat(uid, resourceName)
    if stored == nil then
        M.forget(uid, resourceName)
        return nil
    end

    byUnit = pending[uid]
    if byUnit == nil then
        byUnit = {}
        pending[uid] = byUnit
    end
    byUnit[resourceName] = { stored = stored, residue = value - stored }
    return stored
end

return M
