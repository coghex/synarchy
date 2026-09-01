-- Unit AI core (#538 split from unit_ai.lua).
--
-- Shared kernel required by every unit-AI submodule: per-unit AI
-- state storage, decision scheduling, distance/footprint geometry,
-- water-source memory, and the goal layer. Also carries a handful of
-- small public query/command entrypoints (getState, getRole,
-- countAdjacentBuilders, commandMove) that read/write that same
-- state and don't warrant their own module.
--
-- Self-registers nothing of its own — it extends the scripts.unit_ai
-- singleton (already registered in package.loaded by the time this
-- is required from scripts/unit_ai.lua) the same way till/plant/
-- harvest do, so `unitAi.aiState`, `unitAi.getState`, etc. keep
-- working for every existing external caller.

local unitAi = package.loaded["scripts.unit_ai"]

-- Commanded-order stall accounting + maintainTask (#1291). Re-exported
-- below, so every existing `core.maintainTask` / `core.TASK_*` caller
-- is unchanged.
local stall = require("scripts.unit_ai_stall")
-- Position hold after a completed player move order (#1216). Requires
-- stall only, never this module -- so the verbs below can clear a hold.
local hold = require("scripts.unit_ai_hold")
-- A row's transient runtime fields and their fresh-unit values, shared
-- with the save-restore path that normalizes sparse legacy rows (#2055).
local defaults = require("scripts.unit_ai_defaults")

local M = {}

local function reportFailure(uid, msg)
    local info = unit.getInfo(uid)
    if info and info.gridX then
        engine.emitEventForUnit("unit_warning", msg, uid, info.gridX, info.gridY)
    else
        engine.emitEventForUnit("unit_warning", msg, uid)
    end
end

-- Grant work-skill XP, seeding the key first when it's missing:
-- unit.addXP no-ops on a skill the unit doesn't have, and units from
-- saves that predate a skill (woodcutting/construction, #265) lack the
-- key. Seed at the acolyte.yaml base (25) so a legacy veteran starts a
-- new skill as the same novice a fresh spawn rolls around.
local WORK_SKILL_SEED = 25.0
local function grantWorkXP(uid, skill, amount)
    if amount <= 0 then return end
    if not unit.getSkill(uid, skill) then
        unit.setSkill(uid, skill, WORK_SKILL_SEED)
    end
    unit.addXP(uid, skill, amount)
end

-----------------------------------------------------------
-- Per-unit AI state, keyed by uid. Hung off the module table so it
-- survives reloads via the package.loaded singleton above.
-----------------------------------------------------------
-- aiState[uid] = {
--   currentAction      = "idle" | "wander" | "follow_command" | ...,
--   actionStartedAt    = <posix>,
--   nextActionAt       = <posix>,
--   commandedTask      = { x, y, speed, startedAt, bestDist,
--                          stalledFor, stallSeenAt } | nil,
--                                           -- last 3: the #920 stall
--                                           -- timer, charged in #1291
--                                           -- eligible time only
--                                           -- (unit_ai_stall.lua)
--   knownWaterSources  = { {x, y}, ... },   -- dedup'd by distance;
--                                           -- empty list = none known
--   knownLocations     = { {page,id,x,y} }, -- dedup'd by IDENTITY,
--                                           -- see unit_ai_locations
--   holdAnchor         = { x, y, ... }|nil, -- #1216: stand here until
--                                           -- an accepted player
--                                           -- command (unit_ai_hold)
--   role               = "miner"|"woodcutter"|"builder"|"smith"
--                        |"laborer"| nil,   -- derived each thought
--                                           -- tick (unit_roles.lua);
--                                           -- nil = not a worker
-- }
unitAi.aiState = unitAi.aiState or {}
local aiState = unitAi.aiState

-----------------------------------------------------------
-- Helpers
-----------------------------------------------------------

-- Built from unit_ai_defaults' ONE declaration, never an inline table:
-- this path and the restore path must not drift (#2055).
local function ensureState(uid)
    local s = aiState[uid]
    if not s then
        s = defaults.normalize({})
        aiState[uid] = s
    end
    return s
end

-- Schedule the next decision: interval + ±jitter, in seconds.
local function scheduleNext(s, params)
    -- Combat needs a much tighter cadence than ambient AI. At the
    -- 1.0s default thought_interval, two units charging each other
    -- at ~2 tiles/sec each close 4 tiles between ticks — easily
    -- enough to walk straight through the in-range window, overshoot,
    -- turn around, and oscillate without ever stopping to swing.
    --
    -- Active attack / retreat goals get combat_thought_interval
    -- instead (default 0.1s = 10 Hz). Same scheduling structure,
    -- just a shorter base. Jitter is suppressed for combat so two
    -- engaged units don't drift apart in their re-check phases.
    -- isGoalActive is declared further down, so inline its body
    -- here (`s.activeGoal == name`) — keeps scheduleNext usable
    -- by any tick path that fires before the goal helpers exist.
    local goal = s.activeGoal
    local interval = params.thought_interval
    local jitter
    if goal == "attack" or goal == "retreat" then
        interval = params.combat_thought_interval or 0.1
        jitter   = 0
    else
        jitter = (math.random() * 2 - 1) * params.thought_jitter
    end
    s.nextActionAt = engine.gameTime() + interval * (1 + jitter)
end

local function distance(ax, ay, bx, by)
    local dx = ax - bx
    local dy = ay - by
    return math.sqrt(dx * dx + dy * dy)
end

-- Chebyshev distance from a unit tile (utx, uty) to the nearest
-- footprint tile of a building anchored at (bx, by) with size tileW
-- × tileH. 0 = unit is on a footprint tile, 1 = adjacent, etc.
-- Module-scope so both the build_nearby and deliver_to_build_site
-- actions can see it (the latter is declared earlier in the file).
local function chebToFootprint(utx, uty, bx, by, tileW, tileH)
    local dx = 0
    if utx < bx then dx = bx - utx
    elseif utx >= bx + tileW then dx = utx - (bx + tileW - 1) end
    local dy = 0
    if uty < by then dy = by - uty
    elseif uty >= by + tileH then dy = uty - (by + tileH - 1) end
    return math.max(dx, dy)
end

-----------------------------------------------------------
-- Water-source memory (multi-slot, dedup'd by distance).
--   knownWaterSources is a list of {x, y} tile coords. Two
--   sources within WATER_SOURCE_DEDUP_TILES are treated as
--   the same body of water (so a long river adds at most a
--   few entries instead of dozens). Lookups pick nearest.
-----------------------------------------------------------
local WATER_SOURCE_DEDUP_TILES = 6.0

-- Returns true if added (genuinely new source), false if
-- folded into an existing entry.
local function addWaterSource(s, x, y)
    s.knownWaterSources = s.knownWaterSources or {}
    for _, src in ipairs(s.knownWaterSources) do
        if distance(x, y, src.x, src.y) <= WATER_SOURCE_DEDUP_TILES then
            return false
        end
    end
    table.insert(s.knownWaterSources, { x = x, y = y })
    return true
end

local function nearestWaterSource(s, fromX, fromY)
    if not s.knownWaterSources or #s.knownWaterSources == 0 then
        return nil
    end
    local best, bestD = nil, math.huge
    for _, src in ipairs(s.knownWaterSources) do
        local d = distance(fromX, fromY, src.x, src.y)
        if d < bestD then
            best, bestD = src, d
        end
    end
    return best
end

-- Drop a specific source (used when the tile we remembered is
-- no longer fluid — debug tool drained it, or it was a one-off
-- map mutation). Leaves the rest of the list intact.
local function forgetWaterSource(s, x, y)
    if not s.knownWaterSources then return end
    for i, src in ipairs(s.knownWaterSources) do
        if src.x == x and src.y == y then
            table.remove(s.knownWaterSources, i)
            return
        end
    end
end

local function hasKnownWaterSource(s)
    return s.knownWaterSources and #s.knownWaterSources > 0
end

-----------------------------------------------------------
-- Goal layer.
--   The AI is driven by per-tick utility selection over a flat list
--   of actions; the goal layer sits on top. Each unit holds an
--   activeGoal (string) and a goalStatus table mapping goal names
--   to "in_progress" | "accomplished". Action utilities can read
--   s.activeGoal to apply a goal-driven bonus, letting a long-range
--   pursuit (e.g. "find_water") outrank ambient behavior (wander)
--   even when the unit's local needs are already satisfied.
--   Goals are advisory — actions still own their own preconditions.
-----------------------------------------------------------
local GOALS = {
    find_water = {
        description = "Locate a source of fresh water.",
    },
    notify_allies = {
        description = "Inform every other acolyte where the water is.",
    },
    attack = {
        description = "Engage a specific hostile unit. Set by " ..
                      "unitAi.commandAttack from a player order.",
    },
}

-- Goal a freshly-spawned unit picks up. Indexed by unit defName;
-- unknown types get no auto-goal and behave as before.
local INITIAL_GOAL = {
    acolyte = "find_water",
}

local function setGoal(s, name)
    s.goalStatus = s.goalStatus or {}
    s.goalStatus[name] = "in_progress"
    s.activeGoal = name
end

local function markGoalAccomplished(s, name)
    s.goalStatus = s.goalStatus or {}
    s.goalStatus[name] = "accomplished"
    if s.activeGoal == name then
        s.activeGoal = nil
    end
end

local function isGoalActive(s, name)
    return s.activeGoal == name
end

local function seedInitialGoal(s, defName)
    s.goalStatus = s.goalStatus or {}
    if next(s.goalStatus) then return end   -- already seeded
    local first = INITIAL_GOAL[defName]
    if first then setGoal(s, first) end
end

-- #1291: the AI could not pursue this unit's orders over the interval
-- that just elapsed. Takes a uid so the tick paths that swallow a
-- whole update can call it before ensureState, and looks the state up
-- WITHOUT creating it -- a unit with no AI state has no order to
-- suspend.
local function suspendOrders(uid)
    stall.suspendOrders(aiState[uid])
end

-----------------------------------------------------------
-- Public: player-issued move command
-----------------------------------------------------------
-- Player code (e.g. right-click handler) calls this instead of
-- `unit.moveTo` so the AI can treat the command as a candidate
-- action and resume it after higher-priority interrupts.
--
-- `internal` marks a move the PLAYER did not ask for
-- (scripts/building_spawn.lua's portal walk-out): an ordinary commanded
-- task in every other respect, but completing it leaves no position
-- hold (#1216) and issuing it clears none — a newly spawned acolyte
-- must not be pinned where the roster walked it, and a spawn must not
-- release a hold the player set.
function unitAi.commandMove(uid, tx, ty, speed, internal)
    local s = ensureState(uid)
    if not internal then hold.clear(s) end
    s.commandedTask = {
        x         = tx,
        y         = ty,
        speed     = speed,
        startedAt = engine.gameTime(),
        -- Only a player-intent move may create a hold on arrival
        -- (unit_ai_stall.lua's maintainTask reads this).
        player    = (not internal) or nil,
    }
    -- Force a fresh decision on the next tick rather than waiting
    -- for the unit's natural cadence — feels more responsive.
    s.nextActionAt = 0
end

-----------------------------------------------------------
-- Debug: expose state for the console
-----------------------------------------------------------
function unitAi.getState(uid)
    return aiState[uid]
end

-- | Public: the unit's derived role (#265) — "miner" | "woodcutter" |
--   "builder" | "smith" | "laborer", or nil for non-workers (wildlife,
--   technomule) and units the AI hasn't ticked yet. The unit-info
--   panel maps it through unit_roles.display for the header Role row.
function unitAi.getRole(uid)
    local s = aiState[uid]
    return s and s.role or nil
end

-- | Public: how many acolytes are currently standing within Chebyshev
--   distance 1 of building bid with currentAction == "build_nearby"
--   targeting that bid. Used by the construction tick in
--   scripts/building_spawn.lua to drive worker-rate progress.
function unitAi.countAdjacentBuilders(bid)
    local binfo = building.getInfo(bid)
    if not binfo then return 0 end
    local tw = binfo.tileW or 1
    local th = binfo.tileH or 1
    local count = 0
    local ids = unit.getAllIds() or {}
    for _, uid in ipairs(ids) do
        local s = aiState[uid]
        if s and s.currentAction == "build_nearby"
           and s.buildTarget == bid then
            local info = unit.getInfo(uid)
            if info then
                local utx  = math.floor(info.gridX)
                local uty  = math.floor(info.gridY)
                local cheb = chebToFootprint(utx, uty,
                                             binfo.gridX, binfo.gridY,
                                             tw, th)
                if cheb <= 1 then count = count + 1 end
            end
        end
    end
    return count
end

-- 2e swarm: an ally counts toward "our" strength vs a threat if it's already
-- engaging it OR is a same-side unit within this many tiles of the threat (a
-- potential joiner). This breaks the chicken-and-egg where the FIRST attacker
-- sees only itself, judges the fight futile, and flees before a pack can form.
local SWARM_RALLY_RADIUS     = 8.0     -- chebyshev tiles from the threat

-- | Scalar combat-strength heuristic. Higher number = stronger in a
--   fight. Folds together physical stats, weapon-class skill, blood %,
--   pain, and a weapon-damage proxy (attack range — longer/sharper
--   weapons read as higher range). Tunable; treat the absolute value
--   as opaque and use it for ratios only.
--
--   Calibration target (healthy roll):
--     * Acolyte w/ dagger    ≈ 70
--     * Bear (natural claws) ≈ 150
--   Ratio ~2.1 → an acolyte who's taken damage triggers the futility
--   retreat (threshold 1.5×).
--
--   Returns 0 for dead / collapsed units; the retreat check treats a
--   collapsed friend as not contributing to a group's effectiveness.
function unitAi.combatEffectiveness(uid)
    if not unit.exists(uid) then return 0 end
    local pose = unit.getPose(uid)
    if pose == "dead" or pose == "collapsed" then return 0 end

    local function s(name, default)
        return unit.getStat(uid, name) or default
    end
    local strength     = s("strength",     1.0)
    local dexterity    = s("dexterity",    1.0)
    local agility      = s("agility",      1.0)
    local reflexes     = s("reflexes",     1.0)
    -- balance is a skill (0-100), not a stat. Normalise by /50 so
    -- a level-50 skill contributes the same as the previous
    -- stat-at-1.0 read. Read via getSkill (rather than getStat,
    -- which falls back to skills with the raw 0-100 value and
    -- would 4× the effectiveness number).
    local balance      = (unit.getSkill(uid, "balance") or 50.0) / 50.0
    local perception   = s("perception",   1.0)
    local toughness    = s("toughness",    1.0)
    local constitution = s("constitution", 1.0)

    -- Weapon-class skill of the *active* weapon. Reading via class
    -- lets unarmed creatures (bears) score by the right skill.
    local wepClass = unit.getWeaponClass(uid) or "unarmed"
    local skill    = unit.getSkill(uid, wepClass) or 0.0

    -- Weapon damage proxy: range stretches with both arm length and
    -- blade length. Bigger weapons = bigger threats.
    local range = unit.getAttackRange(uid) or 1.0
    local weaponFactor = math.max(0.5, range)

    -- Vitals.
    local blood = unit.getBlood(uid)
    local bloodFrac = blood
        and math.max(0.0, blood.current / math.max(0.01, blood.max))
        or 1.0
    local pain     = unit.getPain(uid) or 0
    local painNorm = math.min(1.0, pain / 5.0)

    -- Multiplicative stacking. The mid-values around 1.0 keep the
    -- numbers in a roughly comparable range across species.
    local base = 10.0
    return base
        * (1 + strength     * 0.3)
        * (1 + dexterity    * 0.2)
        * (1 + perception   * 0.1)
        * (1 + agility      * 0.2)
        * (1 + reflexes     * 0.15)
        * (1 + balance      * 0.05)
        * (1 + toughness    * 0.3)
        * (1 + constitution * 0.2)
        * (1 + skill / 100.0 * 0.5)
        * weaponFactor
        * bloodFrac
        * (1 - painNorm * 0.5)
end

-- | `subjectUid`'s side's combat strength against `threatUid`. Counts
--   every ALLY OF THE SUBJECT (faction relation, #912 — the subject
--   itself included) that is either already attacking the threat or
--   standing within SWARM_RALLY_RADIUS of it as a POTENTIAL joiner. Used
--   by the retreat candidate so a solo acolyte still sees a bear as
--   futile, while a pack near the threat reads as a winnable swarm and
--   commits together instead of each member fleeing before the swarm
--   forms.
--
--   The subject is REQUIRED, and the measure is "allied with the
--   subject" rather than "not the threat's faction", because those two
--   only coincide in a two-faction world. With a hostile ruin occupant
--   present, "not the threat's faction" counts a nearby squirrel toward
--   the swarm opposing a raider and the raider toward the swarm opposing
--   a bear — inflated group strength, so a lone acolyte commits to a
--   fight it should have fled. Non-allied co-belligerents are not our
--   side just because they happen to be fighting the same thing.
--
--   The threat itself is excluded explicitly: with a staged same-faction
--   fight (two debug units) the threat IS an ally of the subject, and
--   counting it would let a unit read its own opponent as backup.
function unitAi.groupEffectivenessVs(threatUid, subjectUid)
    if not unit.exists(threatUid) then return 0 end
    if not subjectUid or not unit.exists(subjectUid) then return 0 end
    local you = unit.getInfo(threatUid)
    local subjectFaction = unit.getFaction(subjectUid)
    local total = 0
    for _, uid in ipairs(unit.getAllIds() or {}) do
        if uid ~= threatUid
           and faction.areAllies(unit.getFaction(uid), subjectFaction) then
            local st = aiState[uid]
            local committed = st and st.attackTargetUid == threatUid
            local rallyable = false
            if not committed and you then
                local me = unit.getInfo(uid)
                if me then
                    local d = math.max(math.abs(me.gridX - you.gridX),
                                       math.abs(me.gridY - you.gridY))
                    rallyable = d <= SWARM_RALLY_RADIUS
                end
            end
            if committed or rallyable then
                total = total + unitAi.combatEffectiveness(uid)
            end
        end
    end
    return total
end

-- commandAttack(uid, targetUid [, committed]) — set the attack goal. When
-- `committed` is true (a player or scripted ORDER, vs the AI's own emergent
-- engage), the unit holds far longer before futility breaks it (soft override).
function unitAi.commandAttack(uid, targetUid, committed)
    if not targetUid then return end
    local s = ensureState(uid)
    -- #1216: the ORDER supersedes a position hold; the AI's own
    -- emergent engage (unit_ai_combat.lua, no flag) stays an interrupt.
    if committed then hold.clear(s) end
    s.attackTargetUid = targetUid
    s.committed = committed and true or nil
    setGoal(s, "attack")
    s.nextActionAt = 0
end

M.reportFailure         = reportFailure
M.grantWorkXP           = grantWorkXP
M.aiState               = aiState
M.TASK_ARRIVAL_TILES    = stall.TASK_ARRIVAL_TILES
M.TASK_TIMEOUT_SEC      = stall.TASK_TIMEOUT_SEC
M.ensureState           = ensureState
M.scheduleNext          = scheduleNext
M.distance              = distance
M.chebToFootprint       = chebToFootprint
M.addWaterSource        = addWaterSource
M.nearestWaterSource    = nearestWaterSource
M.forgetWaterSource     = forgetWaterSource
M.hasKnownWaterSource   = hasKnownWaterSource
M.setGoal               = setGoal
M.markGoalAccomplished  = markGoalAccomplished
M.isGoalActive          = isGoalActive
M.seedInitialGoal       = seedInitialGoal
M.maintainTask          = stall.maintainTask
M.suspendOrders         = suspendOrders

return M
