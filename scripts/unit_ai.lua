-- Unit AI
--
-- Utility-AI based per-unit decision loop. Each unit type defines a
-- config table with a thought interval, jitter, and a list of actions.
-- An action is a {utility, execute} pair: `utility(uid, aiState)`
-- returns a number; the highest score wins and `execute` is called.
-- Decision cadence is per-unit: each unit holds a `nextActionAt`
-- timestamp set on every decision (interval + ± jitter); update(dt)
-- only acts on units whose nextActionAt has elapsed, distributing
-- load across ticks without a global "act every N ticks" gate.
--
-- Player commands flow through `unit_ai.commandMove(uid, tx, ty, speed)`
-- rather than `unit.moveTo` directly, so the follow_command action can
-- evaluate the task as a candidate and resume it once a higher-utility
-- need (thirst, combat, ...) is satisfied.
--
-- Self-registers in package.loaded so engine.loadScript (dofile, a
-- fresh chunk) and require return the same instance — same pattern as
-- scripts/debug.lua and unit_drag_select.lua.
--
-- #538: this file is an entry/orchestration module — the singleton,
-- tunables/registry wiring, per-unit dispatch loop (tickOne), and the
-- init/update/shutdown/onSaveLoaded lifecycle. Every domain's utility/
-- execute bodies (survival needs, water-seeking, combat, logistics,
-- construction, crafting, dig/chop/till/plant/harvest, repair, pickup,
-- medic) live in scripts/unit_ai_*.lua submodules, required below and
-- wired into the action registry. Shared plumbing (per-unit state,
-- goal layer, distance/footprint geometry, water-source memory) lives
-- in scripts/unit_ai_core.lua; the materials-sourcing ladder is in
-- scripts/unit_ai_fetch.lua.

local unitAi = package.loaded["scripts.unit_ai"] or {}
package.loaded["scripts.unit_ai"] = unitAi

-- Derived roles (#265): skill-derived labels that weight work-action
-- ENTRY utilities (locks stay untouched — see unit_roles.lua header).
local roles = require("scripts.unit_roles")

local core = require("scripts.unit_ai_core")
local aiState = core.aiState

local config = require("scripts.unit_ai_tunables")

local needs        = require("scripts.unit_ai_needs")
local water         = require("scripts.unit_ai_water")
local combat        = require("scripts.unit_ai_combat")
local combatAttack  = require("scripts.unit_ai_combat_attack")
local notify        = require("scripts.unit_ai_notify")
local deliver       = require("scripts.unit_ai_deliver")
local logistics     = require("scripts.unit_ai_logistics")
local construct     = require("scripts.unit_ai_construct")
local craft_        = require("scripts.unit_ai_craft")
local dig           = require("scripts.unit_ai_dig")
local chop          = require("scripts.unit_ai_chop")
-- Attaches unitAi.till / unitAi.plant / unitAi.harvest (#333 convention).
require("scripts.unit_ai_farm")
local repairMod     = require("scripts.unit_ai_repair")
local pickup        = require("scripts.unit_ai_pickup")
local medic         = require("scripts.unit_ai_medic")
local sleepGoal     = require("scripts.unit_ai_sleep")
local mentalAi      = require("scripts.unit_ai_mental")
-- Persistent save-component registration (issue #761) + the raw-id
-- reference field lists scrubStaleRefs below needs -- split out to
-- stay under the #538 module line budget.
local unitAiSave    = require("scripts.unit_ai_save")

-----------------------------------------------------------
-- Action registry per unit type
-----------------------------------------------------------
-- Per-species action lists. Populated below via registerActions so
-- the universal combat candidates (retreat / engage / attack_target)
-- are prepended uniformly — species lists only declare their ambient
-- actions.
-----------------------------------------------------------
local actions = {}

-----------------------------------------------------------
-- Public registration API (for satellite AI scripts)
--
-- A wildlife or species-specific script (bear_ai.lua, future
-- panda_ai.lua, …) declares its own ambient candidates +
-- config block, then calls these to wire itself into the
-- dispatch loop. The universal combat candidates (retreat /
-- engage / attack_target) are auto-prepended to every
-- registered ambient list so each species automatically picks
-- up combat behavior without restating it.
--
-- Goal helpers are exposed below so satellite scripts can
-- read/write the activeGoal layer without poking the state
-- struct directly.
-----------------------------------------------------------

local UNIVERSAL_COMBAT_ACTIONS = {
    { name = "retreat",        utility = combat.retreatUtility,
      execute = combat.retreatExecute,
      forceExecute = true },
    { name = "engage",         utility = combat.engageUtility,
      execute = combat.engageExecute },
    { name = "attack_target",  utility = combatAttack.attackTargetUtility,
      execute = combatAttack.attackTargetExecute,
      forceExecute = true },
}

function unitAi.setConfig(defName, cfg)
    config[defName] = cfg
end

function unitAi.registerActions(defName, ambientActions)
    local list = {}
    for _, a in ipairs(UNIVERSAL_COMBAT_ACTIONS) do
        table.insert(list, a)
    end
    for _, a in ipairs(ambientActions or {}) do
        table.insert(list, a)
    end
    actions[defName] = list
end

-- Expose goal-layer helpers so satellite scripts can read/write
-- s.activeGoal through the canonical API.
unitAi.isGoalActive         = core.isGoalActive
unitAi.setGoal               = core.setGoal
unitAi.markGoalAccomplished  = core.markGoalAccomplished

-- Register acolyte's ambient action list. Combat candidates are
-- prepended by registerActions so the universal-combat invariant
-- holds for acolytes the same way it does for bears.
unitAi.registerActions("acolyte", {
    { name = "idle", utility = needs.idleUtility, execute = needs.idleExecute },
    { name = "wander", utility = needs.wanderUtility, execute = needs.wanderExecute },
    { name = "follow_command", utility = combat.followCommandUtility, execute = combat.followCommandExecute },
    { name = "treat_ally", utility = medic.treatAllyUtility, execute = medic.treatExecute },
    { name = "drink_from_canteen", utility = needs.drinkUtility, execute = needs.drinkExecute },
    { name = "eat_from_inventory", utility = needs.eatUtility, execute = needs.eatExecute },
    { name = "forage", utility = needs.forageUtility, execute = needs.forageExecute },
    { name = "refill_canteen", utility = water.refillUtility, execute = water.refillExecute },
    { name = "search_for_water", utility = water.searchUtility, execute = water.searchExecute },
    { name = "drink_from_source", utility = water.drinkFromSourceUtility, execute = water.drinkFromSourceExecute },
    { name = "go_to_sleep", utility = sleepGoal.sleepUtility, execute = sleepGoal.sleepExecute },
    { name = "notify_allies", utility = notify.notifyAlliesUtility, execute = notify.notifyAlliesExecute },
    { name = "build_nearby", utility = logistics.buildNearbyUtility, execute = logistics.buildNearbyExecute },
    { name = "deliver_to_build_site", utility = deliver.deliverUtility, execute = deliver.deliverExecute },
    { name = "construct_job", utility = construct.constructUtility, execute = construct.constructExecute, onExit = construct.constructOnExit },
    { name = "craft_job", utility = craft_.craftUtility, execute = craft_.craftExecute, onExit = craft_.craftOnExit },
    { name = "store_materials", utility = logistics.storeMaterialsUtility, execute = logistics.storeMaterialsExecute },
    { name = "dig_designation", utility = dig.digUtility, execute = dig.digExecute, onExit = dig.digOnExit },
    { name = "chop_designation", utility = chop.chopUtility, execute = chop.chopExecute, onExit = chop.chopOnExit },
    { name = "till_designation", utility = unitAi.till.utility, execute = unitAi.till.execute, onExit = unitAi.till.onExit },
    { name = "plant_designation", utility = unitAi.plant.utility, execute = unitAi.plant.execute, onExit = unitAi.plant.onExit },
    { name = "auto_harvest", utility = unitAi.harvest.utility, execute = unitAi.harvest.execute },
    { name = "repair_job", utility = repairMod.utility, execute = repairMod.execute, onExit = repairMod.onExit },
    { name = "pickup_ground", utility = pickup.pickupUtility, execute = pickup.pickupExecute },
})

-- Technomule: player pack unit. Stands by the colony's materials
-- (wander self-disables — the def has no stamina stat, and that's
-- intentional: a pack animal that drifts away from the build site
-- defeats its purpose) but follows player move orders, and the
-- universal combat candidates give it retreat when wolves come.
-- Acolytes pull build materials off it via the deliver fetch phase.
unitAi.setConfig("technomule", {
    thought_interval = 1.0,
    thought_jitter   = 0.5,
    combat_thought_interval = 0.1,
    wander_radius    = 3.0,
    base_wander_utility          = 0.3,
    wander_stamina_weight        = 0.0,
    wander_time_penalty          = 0.1,
    wander_min_stamina_fraction  = 0.0,
})

unitAi.registerActions("technomule", {
    { name = "idle", utility = needs.idleUtility, execute = needs.idleExecute },
    { name = "wander", utility = needs.wanderUtility, execute = needs.wanderExecute },
    { name = "follow_command", utility = combat.followCommandUtility, execute = combat.followCommandExecute },
})

-- Load species satellite scripts. Each one defines its candidates
-- and calls unitAi.registerActions + unitAi.setConfig to plug into
-- the dispatch loop. Done at load time so all defs are wired by
-- the time the first tick runs. Bear-specific candidates live in
-- scripts/bear_ai.lua; future wildlife scripts (panda_ai,
-- polar_bear_ai, …) plug in the same way.
require("scripts.bear_ai")
require("scripts.red_squirrel_ai")

-----------------------------------------------------------
-- Decide + execute for one unit
-----------------------------------------------------------
local function tickOne(uid, defName)
    local params  = config[defName]
    local actList = actions[defName]
    if not params or not actList then return end

    -- Short-circuit:
    --   * Collapsed pose: the unit is unconscious. Auto-revive lives
    --     in unit_resources; AI doesn't run.
    --   * Dead pose: terminal. No AI, no resources, no revival.
    --   * Transitioning / drinking / pickup: engine is mid-animation,
    --     we'd clobber the state by issuing new commands.
    -- Crouching/Crawling pose with idle activity DOES run AI — that's
    -- how multi-phase actions (e.g. source-drink) advance.
    local pose     = unit.getPose(uid)
    local activity = unit.getActivity(uid)
    if pose == "collapsed" or pose == "dead" then return end
    if activity == "drinking" or activity == "eating"
       or activity == "pickup" or activity == "transitioning" then return end

    local s = core.ensureState(uid)
    core.seedInitialGoal(s, defName)
    core.maintainTask(uid, s)

    -- Delirium (physiological) and mental break (psychological, #352):
    -- a unit in either can't act purposefully — no goals/work/combat,
    -- and entry preempts the running action (its onExit fires). The
    -- behaviours live in scripts/unit_ai_mental.lua.
    if mentalAi.shortCircuit(uid, s, params, activity, actList) then return end

    -- Stuck-walk watchdog. A unit stuck in walking/running with no
    -- position progress never returns to idle, and the execute gate
    -- below (switch-or-idle) then never re-fires its action — it
    -- hangs forever (seen with the water-search spiral walking at an
    -- unpathable waypoint). Force a stop after N seconds without
    -- movement so the AI re-decides from idle. Engine-side root cause
    -- (path stall) tracked separately; this is the safety net.
    --
    -- watchX/Y only advances on real progress (>0.1 tiles), so the
    -- 0.01 (squared-tiles) check is CUMULATIVE since the last progress
    -- point, not a single ~0.1s sample delta — the old per-tick
    -- version force-stopped any sufficiently slow (meander-speed) walk
    -- regardless of real progress, since one tick's delta never alone
    -- cleared the threshold (#612, surfaced by the sleep goal's longer
    -- walk-to-spot leg).
    do
        local wi = unit.getInfo(uid)
        if wi then
            local moving = (activity == "walking" or activity == "running")
            if moving then
                if not s.watchX then
                    s.watchX, s.watchY = wi.gridX, wi.gridY
                    s.lastProgressAt = engine.gameTime()
                else
                    local moved = (wi.gridX - s.watchX) ^ 2
                                + (wi.gridY - s.watchY) ^ 2
                    if moved > 0.01 then
                        s.watchX, s.watchY = wi.gridX, wi.gridY
                        s.lastProgressAt = engine.gameTime()
                    elseif engine.gameTime() - (s.lastProgressAt or engine.gameTime())
                           > (params.stuck_walk_timeout or 6.0) then
                        engine.logDebug("unitAi: stuck-walk watchdog stopped unit "
                            .. tostring(uid))
                        unit.stop(uid)
                        core.reportFailure(uid, "Stuck — can't reach destination")
                        s.watchX, s.watchY = wi.gridX, wi.gridY
                        s.lastProgressAt = engine.gameTime()
                    end
                end
            else
                s.watchX, s.watchY = nil, nil
                s.lastProgressAt = engine.gameTime()
            end
        end
    end
    local newSources = water.scanForWater(uid, s, params)
    -- First-time discovery while pursuing find_water: flip the goal
    -- chain. The next active goal is notify_allies, which fires the
    -- broadcast / walk-notify action defined below. Subsequent finds
    -- (already on notify_allies or past it) just add to the source
    -- list without re-triggering — markGoalAccomplished is idempotent.
    if newSources > 0 and core.isGoalActive(s, "find_water") then
        core.markGoalAccomplished(s, "find_water")
        core.setGoal(s, "notify_allies")
    end

    if engine.gameTime() < s.nextActionAt then return end

    -- Re-derive the unit's role (#265) once per thought tick, before
    -- scoring — the work entry utilities below read s.role via
    -- roles.weight.
    roles.update(uid, s)

    -- Score every action; pick the highest. Ties → first in list.
    local bestAction, bestScore = nil, -math.huge
    for _, a in ipairs(actList) do
        local u = a.utility(uid, s, params)
        if u > bestScore then
            bestScore  = u
            bestAction = a
        end
    end

    if bestAction then
        local switching = bestAction.name ~= s.currentAction
        if switching then
            -- Give the outgoing action a chance to drop its visuals
            -- (anim overrides etc.). Persistent state — claims, phase
            -- machines — stays, so preempted work resumes later.
            if s.currentAction then
                for _, a in ipairs(actList) do
                    if a.name == s.currentAction then
                        if a.onExit then a.onExit(uid, s, params) end
                        break
                    end
                end
            end
            s.currentAction   = bestAction.name
            s.actionStartedAt = engine.gameTime()
        end
        -- Re-execute conditions:
        --   * On a switch: always (need to set up the new action).
        --   * On the same action: only if the unit is currently idle
        --     — meaning its previous walk arrived or failed. We do
        --     NOT want to re-issue moveTo while it's actively walking
        --     because that wipes `usLocalPath` engine-side and the
        --     unit barely makes progress between AI ticks.
        --   * UNLESS the action sets `forceExecute = true`. Combat's
        --     attack_target needs this so it can react to entering
        --     range mid-walk (stop, then swing on the next idle tick)
        --     instead of marching through the target.
        if switching or activity == "idle" or bestAction.forceExecute then
            bestAction.execute(uid, s, params)
        end
    end

    core.scheduleNext(s, params)
end

-----------------------------------------------------------
-- Init / Update / Shutdown
-----------------------------------------------------------
function unitAi.init(scriptId)
    engine.logInfo("Unit AI initializing...")
    unitAiSave.register(unitAi, aiState)
end

-- Clear any ref that doesn't point at a surviving loaded-page entity out
-- of one surviving state entry. Setting a field to nil is exactly the
-- self-heal the AI already runs when a target legitimately vanishes, so
-- the next tick re-decides cleanly. Nested claim tables are dropped
-- wholesale when their target didn't survive (the execute paths treat a
-- nil claim as "release"). Returns #fields cleared.
local function scrubStaleRefs(s, liveUnitSet, liveBuildingSet)
    local cleared = 0
    for _, f in ipairs(unitAiSave.AI_UNIT_REF_FIELDS) do
        local v = s[f]
        if v ~= nil and not liveUnitSet[v] then s[f] = nil; cleared = cleared + 1 end
    end
    for _, f in ipairs(unitAiSave.AI_BUILDING_REF_FIELDS) do
        local v = s[f]
        if v ~= nil and not liveBuildingSet[v] then s[f] = nil; cleared = cleared + 1 end
    end
    if s.treatClaim and not liveUnitSet[s.treatClaim.patient] then
        s.treatClaim = nil; cleared = cleared + 1
    end
    if s.treatPending and not liveUnitSet[s.treatPending.uid] then
        s.treatPending = nil; cleared = cleared + 1
    end
    if s.deliveryClaim and not liveBuildingSet[s.deliveryClaim.bid] then
        s.deliveryClaim = nil; cleared = cleared + 1
    end
    if s.deliveryPendingTarget and not liveBuildingSet[s.deliveryPendingTarget.bid] then
        s.deliveryPendingTarget = nil; cleared = cleared + 1
    end
    return cleared
end

-- Broadcast from the engine once a save has finished loading (#195).
-- The Lua blob is a global singleton serialized WHOLESALE, so the restore
-- (deserializer above) clobbered aiState with save-time state for units on
-- EVERY page. But the engine load only restores the saved page and
-- PRESERVES other live pages' units (#191), so a load must touch only the
-- loaded page's AI state and leave other pages' state exactly as it was.
--
-- survUnitIds are the loaded page's survivors. We rebuild aiState as:
--   * loaded-page survivor → its restored (blob) state — the save is
--     authoritative for the page it loaded;
--   * every other still-live unit → its PRE-LOAD state (the off-page
--     entity's CURRENT state, NOT the blob's stale snapshot). This both
--     stops an older save from rolling back live off-page AI memory, and
--     means any stale/dropped/colliding loaded-page id resolves to the
--     live entity's own state rather than the blob's — no misattribution;
--   * everything else (orphans, dead, gone-before-save) → dropped.
-- Nested refs are then scrubbed on loaded-page survivor entries against the
-- survivor set: a loaded-page unit can only validly reference a page-mate.
-- Off-page entries keep their pre-load refs (they weren't reloaded).
function unitAi.onSaveLoaded(survUnitIds, survBuildingIds)
    local survUnitSet, survBuildingSet = {}, {}
    for _, uid in ipairs(survUnitIds or {})     do survUnitSet[uid] = true end
    for _, bid in ipairs(survBuildingIds or {}) do survBuildingSet[bid] = true end

    local pre = unitAi._preLoadState or {}
    unitAi._preLoadState = nil
    local blob = aiState   -- current contents = the just-restored blob

    local rebuilt = {}
    for uid in pairs(survUnitSet) do
        if blob[uid] ~= nil then rebuilt[uid] = blob[uid] end
    end
    for uid, s in pairs(pre) do
        if not survUnitSet[uid] and unit.exists(uid) then
            rebuilt[uid] = s          -- live off-page unit: keep current state
        end
    end

    -- Swap into the singleton in place (preserve table identity).
    local kept = 0
    for k in pairs(aiState) do aiState[k] = nil end
    for k, v in pairs(rebuilt) do aiState[k] = v; kept = kept + 1 end

    local scrubbed = 0
    for uid, s in pairs(aiState) do
        if survUnitSet[uid] then
            scrubbed = scrubbed + scrubStaleRefs(s, survUnitSet, survBuildingSet)
        end
    end
    engine.logInfo("Unit AI: reconciled AI state after load ("
        .. kept .. " kept, " .. scrubbed .. " stale ref(s) scrubbed)")
end

function unitAi.update(dt)
    if require("scripts.pause").isPaused() then return end
    local ids = unit.getAllIds()
    if not ids or #ids == 0 then return end

    -- All unit types now use the same utility-AI tickOne. Each def
    -- needs an entry in `config[defName]` + `actions[defName]`;
    -- bears + acolytes are registered above. Unknown defs are
    -- silently skipped by tickOne (params/actList lookup fails).
    for _, uid in ipairs(ids) do
        local info = unit.getInfo(uid)
        if info and info.defName then
            tickOne(uid, info.defName)
        end
    end
end

function unitAi.shutdown()
    -- Empty the singleton state in-place so all references see it
    -- (reassigning the local would orphan the package.loaded copy).
    for k in pairs(aiState) do aiState[k] = nil end
    engine.logInfo("Unit AI shut down")
end

return unitAi
