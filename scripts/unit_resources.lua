-- Unit Resources
--
-- Phase D of the stat system: time-varying resources that drain or
-- regenerate per tick and can trigger state transitions when they
-- cross thresholds. Built entirely on top of Phases A/B/C — stamina
-- is just a regular stat (set via setStat), with this module's
-- update loop applying drain/regen each tick and checking for the
-- collapse threshold.
--
-- To add a resource:
--   1. Add an entry under `config[defName].resourceName` with
--      drain/regen/threshold values.
--   2. Ensure `max_resourceName` exists either as a derived formula
--      in scripts/unit_stats.lua or as an attribute in the YAML.
--   3. Add a branch in `tickResource` if the trigger differs from
--      stamina's "collapse when below threshold".
--
-- The first tick a unit is observed, its current value is set to the
-- max (full tank on spawn). After that, the loop just nudges the
-- current value up or down.

local stats = require("scripts.unit_stats")

local unitResources = {}

-- Per-def resource config. Drain is a per-second constant. Regen
-- factors are multiplied by endurance: a high-endurance unit recovers
-- much faster than a weak one (and may net-regen even while walking).
-- collapse_threshold is the fraction-of-max below which the unit
-- transitions to Collapsed (via unit.collapse). Set to 0 to disable.
local config = {
    acolyte = {
        stamina = {
            max_from               = "max_stamina",
            drain_walking          = 0.1,    -- per second
            -- These four are multiplied by current endurance.
            -- For endurance 1.0: walking net = +0.02/s (regens),
            -- idle = +0.5/s, collapsed = +0.3/s, reviving = +0.3/s.
            -- For endurance 0.5: walking net = -0.04/s (slow drain).
            regen_factor_walking   = 0.12,
            regen_factor_idle      = 0.5,
            regen_factor_collapsed = 0.3,
            regen_factor_reviving  = 0.3,
            collapse_threshold     = 0.1,
            -- When current/max climbs past this, a Collapsed unit
            -- auto-revives. Hysteresis vs collapse_threshold keeps a
            -- borderline unit from flapping between states each tick.
            revive_threshold       = 0.5,
        },
        hydration = {
            max_from        = "max_hydration",
            -- Constant per-second drain in any activity. Tuned so an
            -- average pool (~43 L) empties in 3 game-days. At
            -- timeScale 1.0 (1 game-minute per real-second), 3 days =
            -- 4320 real-seconds → 43 / 4320 ≈ 0.01 L/s. Real-world
            -- baseline metabolism + sweat is ~2.5 L/day; here we run
            -- a bit higher to make the gameplay loop visible without
            -- accelerating into the "constantly drinking" zone.
            drain_constant  = 0.01,
            -- No regen: hydration only restored by drinking events
            -- (separate API, not yet wired).
            collapse_threshold = 0.2,
            -- Revive requires drinking to bring hydration back up to
            -- 50% — phase 3 work. Until drinking exists, a collapsed-
            -- from-thirst unit stays collapsed.
            revive_threshold   = 0.5,
        },
    },
}

-----------------------------------------------------------
-- Init
-----------------------------------------------------------
function unitResources.init(scriptId)
    engine.logInfo("Unit resources tick initializing...")
end

-----------------------------------------------------------
-- Per-resource tick. Returns nothing; side-effects on the unit.
-----------------------------------------------------------
local function tickResource(uid, defName, resourceName, params, activity, dt)
    local maxVal = stats.get(uid, params.max_from)
    if not maxVal or maxVal <= 0 then return end

    -- First-tick init: if the unit has no value yet, fill it.
    local current = unit.getStat(uid, resourceName)
    if current == nil then
        unit.setStat(uid, resourceName, maxVal)
        return
    end

    -- Pick the regen factor for the current activity. Unknown activities
    -- contribute zero — safe fallback if a new state appears later.
    local regenFactor
    if     activity == "walking"   then regenFactor = params.regen_factor_walking
    elseif activity == "idle"      then regenFactor = params.regen_factor_idle
    elseif activity == "collapsed" then regenFactor = params.regen_factor_collapsed
    elseif activity == "reviving"  then regenFactor = params.regen_factor_reviving
    end
    regenFactor = regenFactor or 0

    -- Regen scales with endurance — a strong unit recovers faster.
    -- If endurance is undefined for this unit type, we just skip regen.
    local endurance = unit.getStat(uid, "endurance") or 0
    local regen = regenFactor * endurance

    -- Drain has two parts: an always-on constant (drain_constant) and
    -- an activity-specific drain (currently only drain_walking).
    -- Resources like hydration use the constant; stamina uses the
    -- activity-specific.
    local drainActivity = (activity == "walking" and params.drain_walking) or 0
    local drainConstant = params.drain_constant or 0
    local drain = drainActivity + drainConstant

    local next = current + (regen - drain) * dt

    if next < 0     then next = 0     end
    if next > maxVal then next = maxVal end

    -- Only write if it actually changed by a meaningful amount.
    -- Avoids hammering the unit manager IORef for sub-pixel updates.
    if math.abs(next - current) > 1e-4 then
        unit.setStat(uid, resourceName, next)
    end

    -- Collapse trigger. Only fires on non-collapsed/non-reviving
    -- units (otherwise a unit regenerating slowly through the
    -- threshold would re-stamp the collapse anim every tick, or
    -- interrupt an in-progress revive).
    if activity ~= "collapsed" and activity ~= "reviving"
       and params.collapse_threshold and params.collapse_threshold > 0
       and next / maxVal < params.collapse_threshold then
        unit.collapse(uid)
    end

    -- NOTE: auto-revive is checked once per unit in checkRevive after
    -- all resources for that unit have ticked, not here. The per-
    -- resource version would race-revive a unit collapsed from thirst
    -- as soon as stamina recovered, even if hydration was still low.
end

-----------------------------------------------------------
-- Cross-resource revive check (called once per unit per tick).
-- Only revives if EVERY resource with a revive_threshold > 0 is
-- at-or-above its own threshold. Resources with revive_threshold = 0
-- don't gate revive.
-----------------------------------------------------------
local function checkRevive(uid, defConfig)
    -- Re-read activity here: a tickResource earlier in this update
    -- pass may have just called unit.collapse, so the activity passed
    -- into the per-resource loop is stale.
    local activity = unit.getActivity(uid)
    if activity ~= "collapsed" then return end

    for resourceName, params in pairs(defConfig) do
        local rt = params.revive_threshold
        if rt and rt > 0 then
            local maxVal = stats.get(uid, params.max_from)
            local cur    = unit.getStat(uid, resourceName)
            if not maxVal or maxVal <= 0 or not cur then
                return    -- can't evaluate; play it safe, no revive
            end
            if cur / maxVal < rt then
                return    -- at least one resource still below threshold
            end
        end
    end
    unit.revive(uid)
end

-----------------------------------------------------------
-- Update (called at tick interval by engine.loadScript)
-----------------------------------------------------------
function unitResources.update(dt)
    if require("scripts.pause").isPaused() then return end
    local ids = unit.getAllIds()
    if not ids or #ids == 0 then return end

    for _, uid in ipairs(ids) do
        local info = unit.getInfo(uid)
        if info and info.defName then
            local defConfig = config[info.defName]
            if defConfig then
                local activity = unit.getActivity(uid) or "idle"
                for resourceName, params in pairs(defConfig) do
                    tickResource(uid, info.defName, resourceName,
                                 params, activity, dt)
                end
                checkRevive(uid, defConfig)
            end
        end
    end
end

-----------------------------------------------------------
-- Shutdown
-----------------------------------------------------------
function unitResources.shutdown()
    engine.logInfo("Unit resources tick shut down")
end

-- Exposed for debug console: inspect what a unit's drain/regen
-- numbers are. Returns nil if the def isn't configured.
function unitResources.getConfig(defName)
    return config[defName]
end

return unitResources
