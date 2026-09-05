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
-- transfer, medic) live in scripts/unit_ai_*.lua submodules, required
-- below and wired into the action registry. Shared plumbing (per-unit
-- state, goal layer, distance/footprint geometry, water-source memory)
-- lives in scripts/unit_ai_core.lua; the materials-sourcing ladder is
-- in scripts/unit_ai_fetch.lua; per-unit location knowledge (#915) is
-- in scripts/unit_ai_locations.lua.
--
-- Designation job coordinates (#1175 requirement 4, audit result). The
-- job coords the AI stores across ticks -- s.digJob / s.chopJob /
-- s.tillJob / s.plantJob / s.constructJob, in unit_ai_dig.lua,
-- unit_ai_chop.lua, unit_ai_farm.lua and unit_ai_construct.lua -- are
-- CANONICAL, and nothing in those modules had to change to keep them so:
--
--   * They are only ever PRODUCED by an engine query (nearest*Designation,
--     get*DesignationAt, construction.getPendingJobs), each reporting the
--     canonical stored key -- see World/Render/HitTest.hs's contract. The
--     AI never derives one from a pick or from arithmetic over two.
--   * They are only ever CONSUMED by point verbs, all of which accept
--     any u-alias -- not just the designation reads/cancels but every
--     verb that FINISHES a job: world.getDigInfoAt/digTile,
--     harvestFlora, setVegAt, plantCropAt/plantRowCropAt,
--     structure.place/hasAt/floorZAt/clear, and building.spawn/
--     canPlaceAt for a CtBuilding stake. That set is what lets a coord
--     persisted by a pre-#1175 save (lua.unit_ai v1-v4, possibly an
--     alias) run to COMPLETION after a load with no migration;
--     resolving the designation but editing terrain raw is a half-fix.
--   * The Lua-side claim tables (digKey / chopKey / till.key) key off
--     those same coords, PAGE-qualified since #1329, so one physical
--     tile has one claim key per page -- two aliases held it twice.
--   * Job SELECTION range gates are the one place a canonical coord is
--     the wrong number: from a worker, a seam-side job measures a whole
--     world away and is rejected before it can be claimed. So
--     construction.getPendingJobs also reports lx/ly (the same tile in
--     the scan region's frame); unit_ai_construct.lua measures with
--     those, calling every verb with the canonical x/y.
--
-- Deliberately NOT in that contract: unit MOVEMENT. Unit positions and
-- pathing use the plain unwrapped global frame with no seam handling, so
-- unit.moveTo(job.x + 0.5, ...) walks the long way round rather than
-- across the seam -- a pre-existing pathing limit, not a frame mismatch:
-- the canonical coord still names the right tile there.

local unitAi = package.loaded["scripts.unit_ai"] or {}
package.loaded["scripts.unit_ai"] = unitAi

-- Derived roles (#265): skill-derived labels that weight work-action
-- ENTRY utilities (locks stay untouched — see unit_roles.lua header).
local roles = require("scripts.unit_roles")

local core = require("scripts.unit_ai_core")
local aiState = core.aiState
local hold = require("scripts.unit_ai_hold")
-- #1769: the order stall accounting, for the watchdog's report
-- de-duplication below. A leaf module (no requires at all), so this is
-- not a cycle; core re-exports maintainTask but is at its line budget.
local stall = require("scripts.unit_ai_stall")

local config = require("scripts.unit_ai_tunables")

local needs        = require("scripts.unit_ai_needs")
local water         = require("scripts.unit_ai_water")
local combat        = require("scripts.unit_ai_combat")
local combatAttack  = require("scripts.unit_ai_combat_attack")
local encounter     = require("scripts.unit_ai_encounter")
-- The lunge state machine (#1713). Required HERE and not only through
-- the attack module because tickOne calls its airborne observer before
-- the transition short-circuit returns; see the note at that call.
local lunge         = require("scripts.unit_ai_combat_lunge")
local notify        = require("scripts.unit_ai_notify")
local deliver       = require("scripts.unit_ai_deliver")
local logistics     = require("scripts.unit_ai_logistics")
local construct     = require("scripts.unit_ai_construct")
local craft_        = require("scripts.unit_ai_craft")
local dig           = require("scripts.unit_ai_dig")
local chop          = require("scripts.unit_ai_chop")
-- Attaches unitAi.till / unitAi.plant (#333 convention), and pulls in
-- scripts.unit_ai_harvest for unitAi.harvest (#1582).
require("scripts.unit_ai_farm")
local repairMod     = require("scripts.unit_ai_repair")
local pickup        = require("scripts.unit_ai_pickup")
local transfer      = require("scripts.unit_ai_transfer")
local medic         = require("scripts.unit_ai_medic")
local sleepGoal     = require("scripts.unit_ai_sleep")
local mentalAi      = require("scripts.unit_ai_mental")
-- Per-unit location knowledge (#915): the experiential layer beside the
-- player-wide cartographic discovery state. Own submodule because
-- unit_ai_core.lua is at its line budget.
local locations     = require("scripts.unit_ai_locations")
-- Persistent save-component registration (issue #761), over
-- unit_ai_ref_schema.lua's REF_SCHEMA -- the one declaration the wire
-- codec, the reference report, the tag validator and the post-load
-- reconcile all walk -- plus the #1610 session-teardown registration.
-- Split out to stay under the #538 module line budget.
local unitAiSave    = require("scripts.unit_ai_save")
-- Post-load reconciliation of aiState (#1589): orphan prune + the
-- schema-driven stale-reference scrub, and the module-owned release
-- paths that scrub drops a job through. Own submodule for the same
-- line-budget reason every other domain has one.
local reconcile     = require("scripts.unit_ai_reconcile")

-----------------------------------------------------------
-- Action registry per unit type. Per-species ambient action lists,
-- filled in below via registerActions — see its own block for the
-- UNIVERSAL prepend every one of them gets.
-----------------------------------------------------------
local actions, actionNames = {}, require("scripts.unit_ai_actions")

-----------------------------------------------------------
-- Public registration API (for satellite AI scripts)
--
-- A wildlife or species-specific script (bear_ai.lua, future
-- panda_ai.lua, …) declares its own ambient candidates + config block,
-- then calls these to wire itself into the dispatch loop. The UNIVERSAL
-- candidates below are auto-prepended to every registered ambient list,
-- so each species picks them up without restating them. Goal helpers
-- are exposed below too, so satellite scripts can read/write the
-- activeGoal layer without poking the state struct directly.
-----------------------------------------------------------

local UNIVERSAL_ACTIONS = {
    { name = "retreat", utility = combat.retreatUtility,
      execute = combat.retreatExecute, forceExecute = true },
    { name = "engage", utility = combat.engageUtility, execute = combat.engageExecute },
    { name = "attack_target", utility = combatAttack.attackTargetUtility,
      execute = combatAttack.attackTargetExecute, forceExecute = true },
    -- The TARGET side of a Mode A session (#1251): universal because a
    -- session's destination may be ANY player-commandable unit, while
    -- being its SOURCE is a per-species capability the source gate asks
    -- about — scripts/unit_ai_escort.lua's header has the asymmetry.
    transfer.escortHoldAction,
    -- #1216, universal for the same reason a move order is.
    hold.action,
}

function unitAi.setConfig(defName, cfg)
    config[defName] = cfg
end

function unitAi.registerActions(defName, ambientActions, options)
    local list = {}
    local excluded = (options and options.excludeUniversal) or {}
    for _, a in ipairs(UNIVERSAL_ACTIONS) do
        if not excluded[a.name] then table.insert(list, a) end
    end
    for _, a in ipairs(ambientActions or {}) do
        table.insert(list, a)
    end
    actions[defName] = actionNames.record(defName, list)
end

-- Goal-layer helpers: satellite scripts read/write s.activeGoal here.
unitAi.isGoalActive         = core.isGoalActive
unitAi.setGoal               = core.setGoal
unitAi.markGoalAccomplished  = core.markGoalAccomplished

-- Register acolyte's ambient action list. The universal candidates are
-- prepended by registerActions, so that invariant holds for acolytes
-- the same way it does for bears.
unitAi.registerActions("acolyte", {
    { name = "idle", utility = needs.idleUtility, execute = needs.idleExecute },
    { name = "wander", utility = needs.wanderUtility, execute = needs.ambientWanderExecute },
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
    { name = "auto_harvest", utility = unitAi.harvest.utility, execute = unitAi.harvest.execute, onExit = unitAi.harvest.onExit },
    { name = "repair_job", utility = repairMod.utility, execute = repairMod.execute, onExit = repairMod.onExit },
    { name = "pickup_ground", utility = pickup.pickupUtility, execute = pickup.pickupExecute },
    transfer.action, transfer.escortAction,
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
    { name = "wander", utility = needs.wanderUtility, execute = needs.ambientWanderExecute },
    { name = "follow_command", utility = combat.followCommandUtility, execute = combat.followCommandExecute },
    transfer.action, transfer.escortAction,
})

encounter.register(needs)

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
    -- Each return records an interruption boundary (#1291/#2332): none
    -- of it is charged to a pending order, or to any work clock.
    local pose     = unit.getPose(uid)
    local activity = unit.getActivity(uid)
    -- Lunge BOOKKEEPING, and nothing else, runs before those returns
    -- (#1713): a leap spends its whole airborne life on the
    -- `transitioning` path below (`activityLabel (TransitioningTo _)`,
    -- src/Unit/Thread.hs), so with no hook here no tick ever observes it
    -- and the landing strike's gate is unreachable by construction. NOT a
    -- widened execution window -- observeTick reads pose/activity and
    -- writes unit_ai_combat_lunge.lua's own `lunge*` fields, scoring no
    -- actions, issuing no commands and never calling combat.attack; the
    -- strike still fires only from an ordinary grounded execute tick, so
    -- the clobber protection these returns exist for is untouched. Run on
    -- EVERY tick so a lunge interrupted onto any path still reaches its
    -- terminal cleanup. Detail: that module's header.
    lunge.observeTick(uid, pose, activity)
    if pose == "collapsed" or pose == "dead" then return core.suspendOrders(uid) end
    if activity == "drinking" or activity == "eating" or activity == "pickup"
       or activity == "transitioning" then return core.suspendOrders(uid) end

    local s = core.ensureState(uid)
    core.seedInitialGoal(s, defName)
    combat.completeCommandedTask(uid, s, core.maintainTask(uid, s))
    -- Stamina-adaptive follow_command pacing (#999): runs unconditionally
    -- every tick, like maintainTask above, rather than through the
    -- switch/idle execute gate below — that gate deliberately avoids
    -- re-running an action mid-walk, which is exactly what continuous
    -- pacing feedback needs to do (via unit.setMoveSpeed, never moveTo).
    combat.followCommandPaceTick(uid, s)

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
                    -- Silence the later stall expiry of the order
                    -- that was actually walking this, if it is still
                    -- the current one (#1769).
                    stall.noteStuckReport(uid, s)
                    s.watchX, s.watchY = wi.gridX, wi.gridY
                    s.lastProgressAt = engine.gameTime()
                end
            end
        else
            s.watchX, s.watchY = nil, nil
            s.lastProgressAt = engine.gameTime()
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
    engine.logDebug("Unit AI initializing...")
    unitAiSave.register(aiState)
    locations.register(unitAi, aiState)
end

-- Broadcast from the engine once a save has finished loading (#195).
-- Since issue #900 the restore itself is per-entity: aiState already
-- holds exactly the rows the payload carried for units present in the
-- restored session, dropped-with-a-diagnostic otherwise (see
-- unit_ai_save.lua's apply). The pre-load snapshot/restore dance this
-- function used to run (`_preLoadState`, #195/#191) is retired along
-- with it, and so is its off-page-preservation branch — #763 replaced
-- the merge-based load path that made "another live page" a thing, so
-- survUnitIds/survBuildingIds name every unit/building in the whole new
-- session.
--
-- What remains here is the reconcile that apply-time ownership can't do:
--   * the ORPHAN PRUNE — the engine load path can still drop an entity
--     after the Lua restore (an unregistered def), so a row whose unit
--     isn't a survivor is dropped rather than left to be inherited by a
--     later id reuse;
--   * the NESTED-REF SCRUB on every surviving row, whose targets are
--     other entities that may not have survived. Since #1589 that
--     covers EVERY family unit_ai_ref_schema.lua's REF_SCHEMA declares
--     -- craftJob, repairJob, pickupOrder, a ground forageTarget and
--     the forageLoot/harvestLoot collections included, none of which
--     the original #195 scrub reached -- because the scrub walks that
--     one schema rather than a hand-maintained list beside it. That is
--     what makes the tolerated-dangling-reference promise both save
--     validators rely on actually true. knownLocations keeps the
--     specialized page-qualified scrub unit_ai_locations.lua owns.
--
-- Both live in scripts/unit_ai_reconcile.lua, along with the release
-- paths a dropped job goes out through (this file is at its line
-- budget).
--
-- `reconcileCtx` (#1589) is the restored session's item-instance /
-- unit-page / per-page bill + ground-item context, APPENDED by the
-- engine broadcast rather than inserted -- every other shipped
-- onSaveLoaded callback reads the two survivor arrays positionally and
-- simply never declares this one.
function unitAi.onSaveLoaded(survUnitIds, survBuildingIds, reconcileCtx)
    reconcile.reconcile(aiState, survUnitIds, survBuildingIds, reconcileCtx)
end


function unitAi.update(dt)
    -- #1610: nothing runs between Exit to Menu and the next session --
    -- the engine's UnitClearAll is still draining, so the entity queries
    -- below still report the destroyed session's units and ensureState
    -- would rebuild exactly the rows the teardown just cleared. See
    -- scripts/lib/session_teardown.lua.
    if require("scripts.lib.session_teardown").isTornDown() then return end
    -- Location awareness (#915) is recorded BEFORE the pause guard on
    -- purpose. Its engine-side source (the sight predicate #1230 gave
    -- World.Thread.Discovery) is pause-independent -- a freshly loaded,
    -- auto-paused save can come up with a unit already LOOKING AT a
    -- location -- and gating acquisition on unpause here would quietly
    -- reintroduce the dependency the engine side avoids.
    -- Recording a memory is not simulation: it mutates only aiState,
    -- never the world.
    locations.ingestAwareness(core.ensureState)
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
    engine.logDebug("Unit AI shut down")
end

return unitAi
