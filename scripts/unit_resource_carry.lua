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
--   * Ownership is established two independent ways, because neither
--     alone is complete. A WRITE BARRIER (installWriteBarrier below)
--     wraps every `unit.*` verb that can write a stat this module
--     carries, so any other writer -- a drink, a debug console
--     setStat, a feed, an AI sip -- discards the remainder as it
--     writes, INCLUDING one that happens to commit the identical
--     Float, which a value comparison cannot see. On top of that, the
--     remainder is reused only while storage still holds exactly what
--     this module last wrote there, which catches an engine-side write
--     that reaches `uiStats` without passing a wrapped verb.
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
    if uid == nil then return end
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

-----------------------------------------------------------
-- Write barrier
--
-- A remainder belongs to the last value THIS module wrote. Comparing
-- against the stored value catches a writer that moved it, but not one
-- that re-committed the same Float -- and "the same Float" is exactly
-- what a debug `unit.setStat(uid, name, <what it already is>)`, or a
-- sip that rounds to no change, produces. So the write itself has to
-- say so, and the only Lua-reachable writes into `uiStats` are these
-- verbs.
--
-- Each wrapper drops ONLY what its verb actually writes. Nothing here
-- may invalidate more than that: a verb that runs every cadence and
-- clears the whole unit would discard the previous cadence's remainder
-- before it could ever be used, silently restoring the very
-- cadence-dependence this module exists to remove. `recoverStance` is
-- exactly that shape -- unit_resource_injury.tickStance calls it on
-- every unit on every tick, before the resources tick -- which is why
-- it is scoped to the one stat it writes rather than to the unit.
--
-- The verbs deliberately absent, and why none of them can carry a
-- remainder away:
--
--   * `commitStamina` -- stamina never takes this path at all
--     (unit_resource_tick.lua), so it never has an entry.
--   * `recoverStance` -- writes only `stance`, which no resource
--     config ticks, so there is never an entry under that name.
--   * `recomputeBody` -- writes `strength`, `strength_base`,
--     `strength_body`, `carrying_capacity` and the `max_*` pool SIZES
--     (Unit.Thread.Command.Body.recomputeBodyDerivedStats). A pool's
--     maximum is not the pool, and none of those names is a ticked
--     resource.
--   * `spawn` -- a new unit's pool is filled by tickResource's own
--     first-tick branch, which writes through `setStat` and therefore
--     through this barrier.
--
-- Read verbs are NOT wrapped, for the same reason: over-invalidating
-- quietly turns the carry back off.
-----------------------------------------------------------

--: The verbs this barrier covers, in the order it reports them. The
--: installs below are this list's implementation; they name each verb
--: LITERALLY because tools/lua_registration_audit.py refuses a
--: computed key on an engine namespace -- `unit[verb]` would name a
--: verb it cannot check against the registrar -- and spelling them out
--: is also what lets that audit prove each one still exists.
local DECLARED_WRITERS = { "setStat", "addXP", "feed" }

--: verb name → true once wrapped. Also the assertable record of what
--: the barrier actually covers in a live VM.
local wrapped = {}

-- `unit.<verb>(uid, statName, ...)`: the caller names the stat, so only
-- that resource's remainder goes.
local function nameScoped(raw)
    return function(uid, statName, ...)
        M.forget(uid, statName)
        return raw(uid, statName, ...)
    end
end

-- `unit.<verb>(uid, ...)`: always writes one stat the caller does not
-- name, so that one goes.
local function fixedStat(raw, statName)
    return function(uid, ...)
        M.forget(uid, statName)
        return raw(uid, ...)
    end
end

-- Idempotent, and safe to call before the engine API exists: it reports
-- false and leaves `wrapped` empty, so a later call can still install.
-- `integrate` calls it, which is what covers a VM that registers the
-- `unit` table after this module is required.
function M.installWriteBarrier()
    if type(unit) ~= "table" then return false end
    if not wrapped.setStat and type(unit.setStat) == "function" then
        unit.setStat = nameScoped(unit.setStat)
        wrapped.setStat = true
    end
    if not wrapped.addXP and type(unit.addXP) == "function" then
        unit.addXP = nameScoped(unit.addXP)
        wrapped.addXP = true
    end
    -- unit.feed credits `hunger`
    -- (Engine.Scripting.Lua.API.Units.Survival), a resource the acolyte
    -- config ticks.
    if not wrapped.feed and type(unit.feed) == "function" then
        unit.feed = fixedStat(unit.feed, "hunger")
        wrapped.feed = true
    end
    return true
end

-- What the barrier declares it must cover, and what it has covered in
-- this VM. Public so the two can be asserted against each other rather
-- than only through the arithmetic they protect.
function M.writeBarrierStatus()
    local declared, missing = {}, {}
    for _, verb in ipairs(DECLARED_WRITERS) do
        declared[#declared + 1] = verb
        if not wrapped[verb] then missing[#missing + 1] = verb end
    end
    return { declared = declared, missing = missing }
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
    M.installWriteBarrier()
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

-- Best effort at load: in the engine the `unit` table is registered
-- before any script runs, so the barrier is up before anything can
-- write. A VM that has not registered it yet simply gets the install
-- from the first `integrate`, before any remainder exists to lose.
M.installWriteBarrier()

return M
