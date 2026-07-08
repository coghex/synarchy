-- Unit Resources
--
-- Phase D of the stat system: time-varying resources that drain or
-- regenerate per tick and can trigger state transitions when they
-- cross thresholds. Built entirely on top of Phases A/B/C — stamina
-- is just a regular stat (set via setStat), with this module's
-- update loop orchestrating the per-tick physiology passes below.
--
-- This is now an entry/orchestration module — the domain logic lives
-- in sibling scripts/unit_resource_*.lua modules (#541):
--   * unit_resource_config.lua   — per-def resource config table
--   * unit_resource_alerts.lua   — survival warning/death alerts, unit labeling
--   * unit_resource_energy.lua  — surplus regrowth, digestion, starvation
--   * unit_resource_tick.lua    — per-resource drain/regen + cross-resource revive
--   * unit_resource_injury.lua  — stance recovery, disabled-hand drops, injury consequences
--   * unit_resource_failure.lua — delayed-death failure meters
--
-- To add a resource:
--   1. Add an entry under `config[defName].resourceName` in
--      unit_resource_config.lua with drain/regen/threshold values.
--   2. Ensure `max_resourceName` exists either as a derived formula
--      in scripts/unit_stats.lua or as an attribute in the YAML.
--   3. Add a branch in unit_resource_tick.lua's tickResource if the
--      trigger differs from stamina's "collapse when below threshold".
--
-- The first tick a unit is observed, its current value is set to the
-- max (full tank on spawn). After that, the loop just nudges the
-- current value up or down.

local cardio   = require("scripts.cardio")
local thermo   = require("scripts.thermo")
local salts    = require("scripts.salts")
local brain    = require("scripts.brain")
local thoughts = require("scripts.thoughts")

local resourceConfig = require("scripts.unit_resource_config")
local alerts          = require("scripts.unit_resource_alerts")
local energy          = require("scripts.unit_resource_energy")
local resourceTick    = require("scripts.unit_resource_tick")
local injuryTick      = require("scripts.unit_resource_injury")
local failureMeters   = require("scripts.unit_resource_failure")

-- Self-register in package.loaded so engine.loadScript (which uses
-- dofile and creates a fresh chunk) and require return the same
-- instance — otherwise a test harness that requires this module (e.g.
-- movement_probe neutralising the resource tick, mirroring its unit_ai
-- treatment) would mutate a dead copy while the engine ticks the real
-- one. Same pattern as scripts/unit_ai.lua and scripts/debug.lua.
local unitResources = package.loaded["scripts.unit_resources"] or {}
package.loaded["scripts.unit_resources"] = unitResources

-----------------------------------------------------------
-- Init
-----------------------------------------------------------
function unitResources.init(scriptId)
    engine.logInfo("Unit resources tick initializing...")
    -- Save hook: this module (via unit_resource_alerts) keeps a
    -- uid-keyed cache of transient per-unit alert-debounce state, with
    -- nothing worth persisting, so we register NO serializer (no blob
    -- is written into the save). The deserializer is what matters: a
    -- load replaces unitManagerRef from the snapshot and can rewind
    -- umNextId, so the same uid can be reused by a different unit.
    -- Clearing the cache on every load (deserializeAll invokes
    -- registered deserializers with a nil blob when no blob is present)
    -- prevents stale suppression state from attaching to a reused id.
    local saveMods = require("scripts.lib.save_modules")
    saveMods.register("unit_resources", nil, function(_blob)
        alerts.resetOnLoad()
    end)
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
            local pose0 = unit.getPose(uid) or "standing"
            -- A corpse ticks NOTHING. Without this guard, a failure-meter
            -- death leaves the driving wound pinned on the corpse (the
            -- engine wound tick skips dead units, so it never heals), so
            -- tickFailureMeters would re-hit emitDeathAlert + unit.kill
            -- every tick — spamming the death feed. Resource/starvation
            -- drains on the dead are pointless for the same reason. (Note
            -- tickInjuries returns false for the dead, so the `and` chain
            -- below would otherwise fall straight through to the meters.)
            if pose0 ~= "dead" then
                injuryTick.tickStance(uid, dt)
                -- Physiology order: cardio (heart rate + blood oxygen) FIRST so
                -- circulation reads this tick's heart rate; then thermo (core
                -- temp + circulation + sweat-salt-drain + frostbite); then salt
                -- balance. The failure meters read all of these afterward.
                cardio.tick(uid, dt)
                thermo.tick(uid, info, dt)
                salts.tick(uid, dt)
                -- Brain consciousness reads the above (core_temp, blood_oxygen,
                -- salt_conc), so it ticks last. Low consciousness → collapse
                -- (in tickInjuries); checkRevive keeps the unit down until
                -- lucid. Same call also drifts the psychological mental
                -- values (mood / emotional_pain / concentration / the
                -- unified state_of_mind) — dt-driven, unlike consciousness.
                brain.tick(uid, dt)
                -- Thoughts read this tick's freshly-computed mood/pain and
                -- may nudge mood back (#351) — after brain.tick so next
                -- tick's drift treats any nudge as "prev", fading it
                -- naturally toward the physiological target.
                thoughts.tick(uid, info, dt)
                -- Injuries first: a lethal injury kills the unit (skip the
                -- rest of its tick); a disabling one collapses it.
                if not injuryTick.tickInjuries(uid, info, pose0)
                   and not failureMeters.tickFailureMeters(uid, dt) then
                    local defConfig = resourceConfig[info.defName]
                    if defConfig then
                        local activity = unit.getActivity(uid) or "idle"
                        local pose     = unit.getPose(uid) or "standing"
                        for resourceName, params in pairs(defConfig) do
                            resourceTick.tickResource(uid, info.defName, resourceName,
                                         params, activity, pose, dt)
                        end
                        -- After resources have ticked (and the calorie
                        -- store has drained), digestion refills the store
                        -- from the stomach, THEN catabolism eats body mass
                        -- if the store is still empty. Respiratory failure
                        -- (lean ≤ min_lean) is the direct kill path here;
                        -- the gradual organ-failure path lives in the
                        -- stamina branch of unit_resource_tick.
                        energy.tickDigestion(uid, dt)
                        energy.tickStarvation(uid, dt)
                        resourceTick.checkRevive(uid, defConfig)
                    end
                end
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
    return resourceConfig[defName]
end

-- Exposed for the unit-info panel (unit_info_v2.lua): per-meter
-- readout of the delayed-death failure meters. See
-- unit_resource_failure.lua for the underlying math.
function unitResources.meterInfo(uid)
    return failureMeters.meterInfo(uid)
end

return unitResources
