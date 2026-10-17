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
--   * unit_resource_params.lua   — which pools a physiology system gates, and
--                                  the registry defaults a species falls back to
--   * unit_resource_upkeep.lua   — Nominal-nutrition tissue recovery (#2831)
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
local physiology = require("scripts.physiology")
local brain    = require("scripts.brain")
local thoughts = require("scripts.thoughts")
local mental   = require("scripts.mental_state")

local resourceConfig = require("scripts.unit_resource_config")
local resourceParams  = require("scripts.unit_resource_params")
local upkeep          = require("scripts.unit_resource_upkeep")
local alerts          = require("scripts.unit_resource_alerts")
local energy          = require("scripts.unit_resource_energy")
local resourceTick    = require("scripts.unit_resource_tick")
local resourceCarry   = require("scripts.unit_resource_carry")
local regrowth        = require("scripts.unit_resource_regrowth")
local injuryTick      = require("scripts.unit_resource_injury")
local failureMeters   = require("scripts.unit_resource_failure")
local starvation      = require("scripts.starvation")

-- Self-register in package.loaded so engine.loadScript (which uses
-- dofile and creates a fresh chunk) and require return the same
-- instance — otherwise a test harness that requires this module (e.g.
-- movement_probe neutralising the resource tick, mirroring its unit_ai
-- treatment) would mutate a dead copy while the engine ticks the real
-- one. Same pattern as scripts/unit_ai.lua and scripts/debug.lua.
-- Faults already reported by `update`, so a unit that faults every tick
-- (10 Hz) logs once. Cleared with the other per-session caches.
local reported = {}

local unitResources = package.loaded["scripts.unit_resources"] or {}
package.loaded["scripts.unit_resources"] = unitResources

-----------------------------------------------------------
-- Init
-----------------------------------------------------------
function unitResources.init(scriptId)
    engine.logDebug("Unit resources tick initializing...")
    -- Reset hook, not a save component (issue #761, requirement 4):
    -- this module (via unit_resource_alerts) keeps a uid-keyed cache of
    -- transient per-unit alert-debounce state, with nothing worth
    -- persisting. A load replaces unitManagerRef from the snapshot and
    -- can rewind umNextId, so the same uid can be reused by a different
    -- unit -- clearing the cache on every load (registerResetHook runs
    -- its hook unconditionally on every applyAll, regardless of what
    -- the save contains) prevents stale suppression state from
    -- attaching to a reused id.
    -- unit_resource_carry rides the same hook for the same reason
    -- (#2633): its per-uid sub-binary32 remainders are meaningless
    -- once a load has replaced the stored values they are remainders
    -- of, and a rewound umNextId can hand the same uid to a different
    -- unit. unit_resource_regrowth (#2637) is the same kind of cache
    -- for fat/lean/body residues plus the implicit organ baseline:
    -- each residue is at most half an ulp, and the baseline is
    -- reconstructed from the stored masses on the next regrowth tick,
    -- so a load owes nothing across the boundary.
    local function clearSessionCaches()
        alerts.resetOnLoad()
        resourceCarry.resetOnLoad()
        regrowth.resetOnLoad()
        physiology.resetSessionState()
        reported = {}
    end
    local saveMods = require("scripts.lib.save_modules")
    saveMods.registerResetHook("unit_resources", clearSessionCaches)

    -- Exit to Menu is a SECOND session-replacement path and it runs
    -- NONE of the load machinery above (#1610): no applyAll, so no
    -- reset hook, and no onSaveLoaded broadcast. It replaces the entity
    -- managers and can rewind umNextId exactly as a load does, so these
    -- caches have precisely the same reason to be cleared there, and
    -- the same clear does it. Registered with the id these caches
    -- already use; the two registries are independent namespaces
    -- (scripts/lib/session_teardown.lua).
    require("scripts.lib.session_teardown")
        .register("unit_resources", clearSessionCaches)
end

-----------------------------------------------------------
-- Physiology gates (#2831, S4)
--
-- A registry system ticks, stores and kills ONLY while its mode for the unit
-- is Simulated (scripts/physiology.lua). The orchestration below reads every
-- system's mode once per unit per pass and gates each owner on it:
--
--   cardio (Simulated only; it integrates heart rate and blood oxygen from the
--   derived, never stored circulation) -> the Simulated systems that tick
--   themselves, in REGISTRY order (thermo before salt, so sweat debits arrive
--   before the salt tick sees them) ->
--   brain / thoughts / mental -> injuries and the failure meters -> the per-def
--   pools (each only while its system is Simulated: hydration, hunger and
--   calories, stamina, exhaustion and sleep pressure) -> nutrition's
--   digestion and catabolism, or its Nominal upkeep.
--
-- The pool systems (hydration, nutrition) keep their pre-activation position
-- in the pass: they ran after the brain before and still do.
-----------------------------------------------------------

-- The systems that tick THEMSELVES, by id. A pool system is absent: its
-- pools tick with the per-def resources.
local SYSTEM_TICKS = {
    thermo = function(uid, info, dt) thermo.tick(uid, info, dt) end,
    salt   = function(uid, _info, dt) salts.tick(uid, dt) end,
}

-- The pools a unit ticks this pass: its species entry's resources whose
-- system (if any) is Simulated, plus the registry default for a Simulated
-- pool system the species has no entry for. `visit(name, params)` per pool.
local function eachActivePool(defName, modes, visit)
    local species = resourceConfig[defName]
    if species then
        for resourceName, params in pairs(species) do
            local sys = resourceParams.system(resourceName)
            if not sys or modes[sys] == "simulated" then
                visit(resourceName, params)
            end
        end
    end
    for resourceName, sys in pairs(resourceParams.SYSTEM_OF) do
        if modes[sys] == "simulated" and not (species and species[resourceName]) then
            visit(resourceName, resourceParams.DEFAULTS[resourceName])
        end
    end
end

local function hasSupportedSystem(modes)
    for _, mode in pairs(modes) do
        if mode ~= nil then return true end
    end
    return false
end

-- The per-def pools, digestion/catabolism or Nominal upkeep, and the revive
-- check, for one unit whose injuries and failure meters did not end its tick.
local function tickPools(uid, info, modes, dt)
    local activity = unit.getActivity(uid) or "idle"
    local pose     = unit.getPose(uid) or "standing"
    eachActivePool(info.defName, modes, function(resourceName, params)
        -- An OWNED pool is seeded when its system turns Simulated (spawn,
        -- load, transition): a missing one is a broken invariant, reported
        -- and skipped, never quietly refilled to full.
        if resourceParams.system(resourceName)
           and unit.getStat(uid, resourceName) == nil then
            error("Simulated '" .. resourceParams.system(resourceName)
                  .. "' holds no '" .. resourceName .. "'")
        end
        resourceTick.tickResource(uid, info.defName, resourceName,
                                  params, activity, pose, dt)
    end)
    if modes.nutrition == "simulated" then
        -- After resources have ticked (and the calorie store has drained),
        -- digestion refills the store from the stomach, THEN catabolism eats
        -- body mass if the store is still empty. Respiratory failure
        -- (lean <= min_lean) is the direct kill path here; the gradual
        -- organ-failure path lives in the stamina branch of
        -- unit_resource_tick.
        energy.tickDigestion(uid, dt)
        if energy.tickStarvation(uid, dt) then upkeep.markCatabolism(uid) end
        upkeep.syncFlag(uid)
        -- Calorie-store threshold effects (#806): re-derive "strength" from
        -- the fresh calorie fraction every pass, independent of
        -- recomputeBody's own mass-change-only cadence (see
        -- scripts/starvation.lua).
        starvation.refreshStrength(uid)
    elseif modes.nutrition == "nominal" and pose ~= "dead" then
        -- No food system, so nothing digests or starves; a unit wasted below
        -- its spawn margin recovers toward its healthy tissue, outside any
        -- resource config (the exempt nomad has none).
        upkeep.tick(uid, activity, dt)
    end
    -- Revive. Only the Simulated pools gate it (a Nominal pool is healthy by
    -- definition); a unit with neither a species config nor any supported
    -- system never had one and still does not (#2754's way back up for an
    -- exempt unit with no pools is the second case).
    if resourceConfig[info.defName] or hasSupportedSystem(modes) then
        local cfg = {}
        eachActivePool(info.defName, modes, function(resourceName, params)
            cfg[resourceName] = params
        end)
        resourceTick.checkRevive(uid, cfg)
    end
end

-- One unit's whole pass. Split from `update` so a fault in one unit (a
-- violated physiology invariant a read raises) is reported and skipped
-- instead of aborting the pass for every unit after it.
local function tickUnit(uid, info, dt)
    local pose0 = unit.getPose(uid) or "standing"
    -- A corpse ticks NOTHING. Without this guard, a failure-meter death
    -- leaves the driving wound pinned on the corpse (the engine wound tick
    -- skips dead units, so it never heals), so tickFailureMeters would re-hit
    -- emitDeathAlert + unit.kill every tick — spamming the death feed.
    -- Resource/starvation drains on the dead are pointless for the same
    -- reason. (Note tickInjuries returns false for the dead, so the `and`
    -- chain below would otherwise fall straight through to the meters.)
    if pose0 == "dead" then return end
    -- A PENDING combat engagement (SD-20, #2834) is applied at the start of
    -- the pass, before the modes are read below, so a unit that just
    -- transitioned ticks its new stamina pool this pass. The corpse branch
    -- above is why a unit a strike killed is never revisited. The marker is
    -- an unowned stat, so this read is no physiology access.
    if unit.getStat(uid, "combat_engaged") == 1 then
        physiology.applyEngagement(uid)
    end
    injuryTick.tickStance(uid, dt)
    -- Physiology order: cardio (heart rate + blood oxygen) FIRST, so thermo and
    -- the oxygen delivery read circulation derived from this tick's heart rate;
    -- then every Simulated system that ticks itself, in registry order. Cardio's
    -- registry position is not its tick position: it is ticked here, ahead of
    -- the loop, and only while Simulated (a bodyless unit's cardio is
    -- Unsupported and never ticks). The failure meters read all of these
    -- afterward.
    local modes = physiology.modes(uid)
    if modes.cardio == "simulated" then cardio.tick(uid, dt) end
    for _, sid in ipairs(physiology.systemOrder()) do
        local tickSystem = SYSTEM_TICKS[sid]
        if tickSystem and modes[sid] == "simulated" then
            tickSystem(uid, info, dt)
        end
    end
    -- Brain consciousness reads the above (core_temp, blood_oxygen,
    -- salt_conc), so it ticks last. Low consciousness → collapse (in
    -- tickInjuries); checkRevive keeps the unit down until lucid. Same call
    -- also drifts the psychological mental values (mood / emotional_pain /
    -- concentration / the unified state_of_mind) — dt-driven, unlike
    -- consciousness.
    brain.tick(uid, dt)
    -- Thoughts read this tick's freshly-computed mood/pain and may nudge mood
    -- back (#351) — after brain.tick so next tick's drift treats any nudge as
    -- "prev", fading it naturally toward the physiological target.
    thoughts.tick(uid, info, dt)
    -- Mental states (#352): the threshold machine over the state_of_mind
    -- brain.tick just computed — the stressed label plus break/euphoria
    -- episodes. (A thought's mood nudge reaches state_of_mind on the NEXT
    -- brain.tick, per the drift contract above — fine for a machine whose
    -- bands are minutes-scale.)
    mental.tick(uid, dt)
    -- Injuries first: a lethal injury kills the unit (skip the rest of its
    -- tick); a disabling one collapses it.
    if not injuryTick.tickInjuries(uid, info, pose0)
       and not failureMeters.tickFailureMeters(uid, dt) then
        tickPools(uid, info, modes, dt)
    end
end

-----------------------------------------------------------
-- Update (called at tick interval by engine.loadScript)
-----------------------------------------------------------
function unitResources.update(dt)
    -- #1610's drain window: between Exit to Menu and the next session
    -- the engine's UnitClearAll is still draining, so unit.getAllIds
    -- below still reports the destroyed session's units. Ticking them
    -- would re-enter the caches the teardown just cleared -- a fresh
    -- alert-debounce row and a fresh sub-binary32 remainder, per unit,
    -- against a manager that is about to be replaced. unitAi.update and
    -- buildingSpawn.update hold off on the same latch for the same
    -- reason.
    if require("scripts.lib.session_teardown").isTornDown() then return end
    if require("scripts.pause").isPaused() then return end
    local ids = unit.getAllIds()
    if not ids or #ids == 0 then return end

    for _, uid in ipairs(ids) do
        local info = unit.getInfo(uid)
        if info and info.defName then
            local ok, err = pcall(tickUnit, uid, info, dt)
            if not ok then
                local key = tostring(uid) .. ":" .. tostring(err)
                if not reported[key] then
                    reported[key] = true
                    engine.logError("unit_resources: unit " .. tostring(uid)
                        .. " (" .. tostring(info.defName) .. ") tick failed: "
                        .. tostring(err))
                end
            end
        end
    end
end

-----------------------------------------------------------
-- Shutdown
-----------------------------------------------------------
function unitResources.shutdown()
    engine.logDebug("Unit resources tick shut down")
end

-- Backfill "strength_body" (#806) for units loaded from a save
-- predating it: recomputeBodyDerivedStats only ever ran again on a
-- body-composition CHANGE, so a unit that was never mass-changed after
-- load would otherwise carry no strength_body and starvation.
-- refreshStrength would silently no-op on it forever. Recomputing here
-- is idempotent (height/body_mass/lean_mass/strength_base are all
-- persisted, so it re-derives the SAME "strength" a unit already had)
-- and safely no-ops on a bodyless unit, so it's simplest to run for
-- every survivor rather than special-case the missing-key check. A
-- loaded unit that was saved mid-discount briefly reads full strength
-- until the next physiology tick reapplies its current calorie band.
function unitResources.onSaveLoaded(survUnitIds, _survBuildingIds)
    for _, uid in ipairs(survUnitIds or {}) do
        unit.recomputeBody(uid)
        -- The load boundary (#2831, D-6): the engine has already normalized
        -- every unit's stored physiology before publishing it (a legacy
        -- stored circulation is scrubbed there), so what is left is the cache
        -- that READS it. Rebuild the brain's derived values (dt 0: nothing
        -- drifts) NOW, so a paused session reads what its first physiology
        -- tick would produce. Circulation is derived on demand, so it is
        -- already canonical. A Nominal unit reads healthy at once; an
        -- occupant saved with a drifted core temperature or salt no longer
        -- carries either.
        if unit.getPose(uid) ~= "dead" then physiology.refresh(uid) end
    end
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
