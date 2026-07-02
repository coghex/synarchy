-- Unit AI
--
-- Utility-AI based per-unit decision loop. Each unit type defines a
-- config table with a thought interval, jitter, and a list of actions.
-- An action is a {utility, execute} pair: `utility(uid, aiState)`
-- returns a number; the highest score wins and `execute` is called.
--
-- Decision cadence is per-unit: each unit holds a `nextActionAt`
-- timestamp set on every decision (interval + ± jitter). The module's
-- update(dt) tick iterates all units but only acts on those whose
-- nextActionAt has elapsed — this naturally distributes load across
-- ticks without a global "act every N ticks" gate.
--
-- Player commands flow through `unit_ai.commandMove(uid, tx, ty, speed)`
-- rather than `unit.moveTo` directly. The wrapper stores the task on
-- aiState so the follow_command action can evaluate it as a candidate
-- — letting higher-utility needs (later: thirst, hunger) interrupt and
-- the command resume once the need is satisfied.
--
-- Self-registers in package.loaded so engine.loadScript (which uses
-- dofile and creates a fresh chunk) and require return the same
-- instance — otherwise the engine would tick one module's aiState
-- while commandMove from init.lua writes to a different module's
-- aiState. Same pattern as scripts/debug.lua and unit_drag_select.lua.

local unitAi = package.loaded["scripts.unit_ai"] or {}
package.loaded["scripts.unit_ai"] = unitAi

-- Shared comfort/ordered/sprint speed model (see movement_speed.lua).
local mv = require("scripts.movement_speed")
local brain = require("scripts.brain")

-- Report a FAILED job/task to the player as a red unit_warning in the
-- event log (and the unit's per-unit Log). The engine coalesces
-- identical (category+text+uid) emits into one "…(xN)" line, so a unit
-- that keeps re-failing won't flood the log — no per-site rate-limiting
-- needed. Keep `msg` STABLE (no counts/changing numbers) so repeats
-- actually coalesce. `uid` is the unit the failure is ABOUT.
local function reportFailure(uid, msg)
    local info = unit.getInfo(uid)
    if info and info.gridX then
        engine.emitEventForUnit("unit_warning", msg, uid, info.gridX, info.gridY)
    else
        engine.emitEventForUnit("unit_warning", msg, uid)
    end
end

-----------------------------------------------------------
-- Tunables per unit def
-----------------------------------------------------------

-- Wander utility formula:
--   base + stamina_weight * (stamina / max_stamina)
--                         - time_penalty * (now - actionStartedAt)
-- Clamped to -inf if stamina < min_wander_stamina_fraction * max.
--
-- follow_command utility is constant 7.0 when a task is pending
-- (FOLLOW_COMMAND_UTILITY). The ladder (highest first, #306):
--   dire SELF survival (drink/eat ~10–15, derived from need)
--   combat / treatment        (engage·retreat 8.0+, treat_ally 8.0)
--   ───── player orders ─────  (follow_command 7.0, pickup 7.5,
--                               dry-canteen refill peak 7.5)
--   situational goals          (find_water 3.0–6.0 derived from thirst,
--                               notify_allies 4.0) — a routine goal
--                               yields to a command; it only wins on
--                               its own when need (not the goal) makes it
--   routine work / wander      (dig·deliver locks ≤6.0, wander ~0.8)
-- So: a fight or a wound supersedes a move order; a move order supersedes
-- a routine goal; and a goal only climbs above a command when the
-- underlying NEED (thirst→drink/refill) does, which is derived. See the
-- FOLLOW_COMMAND_UTILITY block and the per-action notes for the values.
local config = {
    acolyte = {
        thought_interval = 1.0,    -- seconds between decisions
        thought_jitter   = 0.5,    -- ± fraction of interval
        -- Override when active attack / retreat goal — combat needs
        -- 10× faster re-evaluation so two units charging each other
        -- catch the in-range moment instead of walking through it.
        combat_thought_interval = 0.1,
        wander_radius    = 5.0,    -- tiles
        -- Movement speeds come from the comfort/ordered/sprint regime
        -- (scripts/movement_speed.lua), not per-action fractions.
        base_wander_utility          = 0.5,
        wander_stamina_weight        = 0.3,
        wander_time_penalty          = 0.1,    -- per second in session
        wander_min_stamina_fraction  = 0.2,
        -- Drinking
        drink_sip_litres        = 0.5,    -- canteen water consumed per sip
        drink_min_thirst        = 0.2,    -- 1 - hydration/max; below this, no drink
        -- Drinking utility = thirst · drink_weight (thirst = 1 - hyd/max).
        -- At weight 15 it crosses follow_command (7.0) at thirst ≈ 0.47,
        -- so a unit whose hydration drops below ~half diverts to drink
        -- even under a move order (moderate thirst interrupts commands,
        -- #306), and a near-empty unit (thirst→1) tops out at ~15, well
        -- above every order/goal. Shared with drink_from_source.
        drink_weight            = 15.0,
        drink_canteen_def       = "canteen_steel_2l",
        -- One litre of canteen water restores N L of hydration. A 2 L
        -- canteen at K=11 gives 22 L of hydration ≈ 50% of the
        -- average max_hydration (~43 L), matching the design target
        -- "a full canteen should refill an average acolyte by ~50%".
        drink_hydration_per_litre = 11.0,
        -- Eating. Mirror of drink_from_canteen: only fires when hunger
        -- drops below eat_max_fraction (0.25) of max_hunger; utility =
        -- (1 - hungerFrac) · eat_weight. Because it only fires past the
        -- 0.25 threshold, the term is always ≥ 0.75·10 = 7.5 > command
        -- (7.0) — a hungry unit interrupts orders to eat (#306).
        -- Currently inventory-only — search_for_food (foraging from
        -- flora) is Phase 6.
        eat_max_fraction        = 0.25,
        eat_weight              = 10.0,
        -- Refilling. Utility ramps quadratically above the threshold:
        -- util = base + scale·x², x = (emptiness-0.25)/0.75 ∈ [0,1].
        --   25% empty (threshold): ~0.85 — beats wander, stays well
        --                          below follow_command (7.0). Tops off
        --                          only when nothing important is going on.
        --   50% empty:             ~1.6 — still far below command, so a
        --                          half-empty canteen won't interrupt
        --                          player orders.
        --   75% empty:             ~3.8 — climbing, but a busy/ordered
        --                          unit still finishes its task first.
        --   ~97% empty:            ~7.0 — crosses command. A near-dry
        --                          canteen now interrupts a move order.
        --   100% empty:            ~7.5 — above command. The canteen has
        --                          run dry; refilling pre-empts orders (#306).
        canteen_def              = "canteen_steel_2l",
        refill_min_emptiness     = 0.25,
        refill_base_weight       = 0.85,
        -- Peak (x=1, dry) = base + scale = 0.85 + 6.65 = 7.5, just above
        -- FOLLOW_COMMAND_UTILITY (7.0); the x² shape keeps partial
        -- canteens well under command (only a near-dry one interrupts).
        refill_urgency_scale     = 6.65,
        refill_arrival_tiles     = 1.5,
        -- Searching. Fires when canteen has headroom but no water is
        -- known. Walks a rosette: 8 compass waypoints per ring,
        -- distance = ring_index * search_spacing. Spacing roughly
        -- matches FOV radius so successive rings sweep new ground.
        -- Threshold matches refill — if we'd want to top up (had we
        -- known where water is), we want to spiral to find some.
        search_min_emptiness     = 0.25,
        search_base_weight       = 0.85,
        search_emptiness_weight  = 0.1,    -- → 0.85 at threshold, 0.95 at empty
        search_spacing           = 6,
        search_arrival_tiles     = 1.5,
        search_max_step          = 32,     -- ~4 rings; then re-anchor origin
        -- "find_water" goal urgency — DERIVED, not a flat weight (#306).
        -- A standing goal is not automatically more important than what
        -- the player just ordered; its priority should track how badly
        -- water is actually needed. So searchUtility returns
        --   goal_search_floor + goal_search_urgency · thirst
        -- where thirst = 1 - hydration/max ∈ [0,1]:
        --   * well-hydrated scout → ~3.0: above wander (so it searches
        --     when idle) but BELOW follow_command (7.0), combat, and
        --     treatment — a player move order, a fight, or a medic's
        --     patient all supersede a routine search;
        --   * as the searcher runs dry it climbs toward ~6.0, and its
        --     own thirst (drink/refill, thirst·15 / dry-canteen 7.5)
        --     takes over above command before the goal alone ever would.
        -- This is the "some goals are critical, some are not" rule: the
        -- criticality is derived from need rather than asserted by a
        -- constant. Capped below command by construction (floor+urgency<7).
        goal_search_floor        = 3.0,
        goal_search_urgency      = 3.0,
        -- Notify-allies (second goal). Radio branch: stand still N
        -- seconds, then push known sources to every other radio-
        -- bearing acolyte. Walk branch: pick an uninformed acolyte
        -- by rank-based split, walk to them, stand N seconds, transfer.
        notify_broadcast_seconds = 1.0,
        notify_transfer_seconds  = 1.0,
        notify_arrival_tiles     = 1.5,
        -- notify_allies is a routine, non-critical goal (sharing where
        -- water is) — it sits ABOVE wander but BELOW follow_command (7.0)
        -- so a player order supersedes it, and below combat/treatment so
        -- an attacked or injured notifier breaks off (#306). The phase
        -- lock (notifyAlliesUtility) only needs to out-rank ambient
        -- wander so a half-done broadcast isn't yanked by idle drift.
        goal_notify_weight       = 4.0,
        -- Construction (build_nearby). Utility shape:
        --   util = base · (1 − workers_present/saturation) · dist_factor
        -- workers_present EXCLUDES the unit asking, so the curve is
        -- 3.0 / 2.4 / 1.8 / 1.2 / 0.6 / 0 as 0..5 others have already
        -- joined. dist_factor is 1.0 at adjacent and linearly falls to
        -- 0 at build_scan_range tiles away.
        build_scan_range       = 30.0,
        build_arrival_tiles    = 1.5,
        build_base_utility     = 3.0,
        build_saturation_n     = 5,
        -- Material delivery (deliver_to_build_site). Higher than the
        -- build_nearby max so a unit with both materials and an
        -- adjacent build site will deliver first, then transition to
        -- building once empty.
        deliver_scan_range     = 30.0,
        deliver_utility        = 4.0,
        -- Fetching from the technomule. Construction materials live
        -- on the mule (technomule.yaml), so a deliverer whose own
        -- inventory lacks claimed materials walks to the mule first
        -- and takes the shortfall (unit.transferItemToUnit preserves
        -- the instances). Capacity-gated per item like pickup.
        mule_fetch_arrival     = 1.5,
        -- Auto-store materials. Utility curve = base · fill³ where
        -- fill = carrying_weight / carrying_capacity. Below ~65 %
        -- full the action sits under wander (0.8); past that it
        -- climbs to base at 100 %. Acolytes only auto-deposit items
        -- in the "Materials" category — supplies, tools, and worn
        -- equipment stay on them.
        store_scan_range       = 30.0,
        store_base_utility     = 3.0,
        -- Mining (dig_designation). Utility shape mirrors build:
        --   util = base · min(toolSpeed, 1) · dist_factor
        -- toolSpeed is the best dig-rate multiplier among carried
        -- tools for the tile's material (data/materials pick_speed /
        -- shovel_speed), so hard rock with only a shovel reads as
        -- high effort and scores near zero. No tool → no dig.
        -- Lock-in while a dig is underway is finite (dire needs
        -- still preempt; never math.huge).
        dig_scan_range       = 30.0,
        dig_arrival_tiles    = 0.4,   -- tight: stand AT the corner
        dig_base_utility     = 2.0,
        dig_lock_utility     = 6.0,
        dig_rate             = 0.5,   -- corner-units/sec at tool speed
                                      -- 1.0 AND unit factor 1.0 (a
                                      -- tile holds 4.0 total). Actual
                                      -- rate = dig_rate · toolSpeed ·
                                      -- strength · (0.5 + mining/100),
                                      -- so a strength-1.0 / mining-50
                                      -- unit is the baseline.
        dig_xp_per_tile      = 1.0,   -- mining XP per completed tile
                                      -- (diminishing returns via
                                      -- applySkillXP: gain ∝ 1/level²)
        dig_equip_seconds    = 1.0,
        dig_claim_timeout    = 30.0,  -- stale-claim expiry (seconds)
        dig_tools = {
            shovel = { defs = { shovel_steel = true },
                       equip_anim = "standing_to_holding_shovel",
                       work_anim  = "shoveling" },
            pick   = { defs = { pick_steel = true },
                       equip_anim = "standing_to_holding_pickaxe",
                       work_anim  = "using_pickaxe" },
        },
        -- Ground-item pickup (player order via right-click → Pick up).
        -- An explicit pickup is a player order peer to a move order, so
        -- it sits just ABOVE follow_command (7.0): commandMove and
        -- commandPickup set independent fields without clearing each
        -- other, so when both are pending the more-specific pickup must
        -- win the arbitration rather than time out behind the move (#306).
        -- 7.5 is just above follow_command (7.0) — a peer player order
        -- that wins the move-vs-pickup tie — but BELOW combat/treatment
        -- (≥8.0) and dire survival, so a fight or a fresh wound still
        -- interrupts it. Capacity is checked at the moment of pickup.
        pickup_utility       = 7.5,
        pickup_arrival_tiles = 1.2,
        pickup_timeout       = 30.0,
        -- Medic auto-treat (treat_ally, Phase D). A unit that KNOWS
        -- bleed-control (knowledge × intelligence = its capability)
        -- rushes to bandage a bleeding ally, fetching the first-aid
        -- kit from the technomule first. Base/lock utility sit ABOVE a
        -- player move order (follow_command 7.0) and the menial-work
        -- locks (dig/deliver = 6.0) so treating a bleeding ally is not
        -- cancelled by a stray move command or routine labour (#306) —
        -- but BELOW dire SELF survival (drink/eat ~10) and combat
        -- (engage/retreat 8.0, ties broken to combat by list order), so
        -- a medic still drinks when dying of thirst and defends itself
        -- when attacked (medicBusyInCombat then frees a lesser medic).
        -- Treating ANOTHER unit is high but, per design, below a unit's
        -- own survival/getting-treated. (A future refinement could scale
        -- this with the patient's bleed severity for a fully situational
        -- value; today it is a fixed above-command band.) Squad rule: the
        -- best available medic takes a patient; a lesser one only steps
        -- in when the best is tied up in combat and nobody else claimed it.
        treat_scan_range     = 60.0,
        treat_base_utility   = 8.0,
        treat_lock_utility   = 8.0,
        treat_arrival        = 1.5,   -- tiles to the patient to treat
        treat_min_seep       = 0.6,   -- a wound dressed below this seep
                                      -- is "good enough" — not re-treated
    },
    -- Species-specific config blocks (bear, future wildlife) are
    -- registered via unitAi.setConfig from their own AI scripts
    -- (scripts/bear_ai.lua, etc.). Keep `acolyte` here since the
    -- universal candidates + acolyte ambient candidates all live
    -- in this module.
}

-- 8 compass directions (E, SE, S, SW, W, NW, N, NE). Diagonals are
-- pre-normalized so all waypoints in a ring sit at the same physical
-- distance from origin — without this, diagonal waypoints would be
-- sqrt(2)× further out than cardinals.
local SEARCH_DIRECTIONS = {
    {  1,        0,        },  -- E
    {  0.707107, 0.707107  },  -- SE
    {  0,        1         },  -- S
    { -0.707107, 0.707107  },  -- SW
    { -1,        0         },  -- W
    { -0.707107, -0.707107 },  -- NW
    {  0,        -1        },  -- N
    {  0.707107, -0.707107 },  -- NE
}

-----------------------------------------------------------
-- Per-unit AI state, keyed by uid. Hung off the module table so it
-- survives reloads via the package.loaded singleton above.
-----------------------------------------------------------
-- aiState[uid] = {
--   currentAction      = "idle" | "wander" | "follow_command" | ...,
--   actionStartedAt    = <posix>,
--   nextActionAt       = <posix>,
--   commandedTask      = { x, y, speed, startedAt } | nil,
--   knownWaterSources  = { {x, y}, ... },   -- dedup'd by distance;
--                                           -- empty list = none known
-- }
unitAi.aiState = unitAi.aiState or {}
local aiState = unitAi.aiState

-- Constants for arrival detection on commanded tasks.
local TASK_ARRIVAL_TILES = 0.6
local TASK_TIMEOUT_SEC   = 60.0

-----------------------------------------------------------
-- Helpers
-----------------------------------------------------------

local function ensureState(uid)
    local s = aiState[uid]
    if not s then
        s = {
            currentAction   = "idle",
            actionStartedAt = engine.gameTime(),
            nextActionAt    = 0,      -- decide on first sight
            commandedTask   = nil,
        }
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

-----------------------------------------------------------
-- Action: idle
-----------------------------------------------------------
local function idleUtility(uid, s, params)
    return 0
end

local function idleExecute(uid, s, params)
    -- Clear any leftover movement target.
    unit.stop(uid)
end

-----------------------------------------------------------
-- Action: wander
-----------------------------------------------------------
local function wanderUtility(uid, s, params)
    local stamina = unit.getStat(uid, "stamina")
    local maxStam = require("scripts.unit_stats").get(uid, "max_stamina")
    if not stamina or not maxStam or maxStam <= 0 then return -math.huge end

    local fraction = stamina / maxStam
    if fraction < params.wander_min_stamina_fraction then
        return -math.huge
    end

    local timeInSession = 0
    if s.currentAction == "wander" then
        timeInSession = engine.gameTime() - s.actionStartedAt
    end

    return params.base_wander_utility
         + params.wander_stamina_weight * fraction
         - params.wander_time_penalty * timeInSession
end

local function wanderExecute(uid, s, params)
    local info = unit.getInfo(uid)
    if not info then return end

    local angle = math.random() * 2 * math.pi
    local dist  = math.random() * params.wander_radius
    local tx = info.gridX + math.cos(angle) * dist
    local ty = info.gridY + math.sin(angle) * dist

    -- Aimless wander is a slow meander — well below comfort, so the unit
    -- ambles (and recovers stamina) rather than cruising. Units only move
    -- at comfort/ordered/sprint when they have an actual purpose.
    unit.moveTo(uid, tx, ty, mv.meander(uid))
end

-----------------------------------------------------------
-- Action: drink_from_canteen
--
-- Utility scales linearly with thirst (1 - hydration/max) once it
-- crosses drink_min_thirst, weighted to dominate wander/command. Goes
-- to -inf if no canteen has water — keeps the unit from idling on a
-- "drink" decision it can't act on.
-----------------------------------------------------------
local function findCanteenWithWater(uid, defName)
    local inv = unit.getInventory(uid)
    if not inv then return nil end
    for _, it in ipairs(inv) do
        if it.defName == defName
           and it.currentFill and it.currentFill > 0 then
            return it
        end
    end
    return nil
end

local function drinkUtility(uid, s, params)
    local hyd = unit.getStat(uid, "hydration")
    local maxHyd = require("scripts.unit_stats").get(uid, "max_hydration")
    if not hyd or not maxHyd or maxHyd <= 0 then return -math.huge end

    local thirst = 1 - hyd / maxHyd
    if thirst < params.drink_min_thirst then return -math.huge end

    if not findCanteenWithWater(uid, params.drink_canteen_def) then
        return -math.huge
    end

    return thirst * params.drink_weight
end

local function drinkExecute(uid, s, params)
    -- Already drinking? Don't restart the anim mid-sip.
    if unit.getActivity(uid) == "drinking" then return end

    local canteen = findCanteenWithWater(uid, params.drink_canteen_def)
    if not canteen then return end

    local hyd    = unit.getStat(uid, "hydration") or 0
    local maxHyd = require("scripts.unit_stats").get(uid, "max_hydration") or 0
    local deficit = maxHyd - hyd
    if deficit <= 0 then return end

    local k = params.drink_hydration_per_litre
    -- Sip is in CANTEEN-LITRES. Hydration restored = sip * k.
    --   max sip from canteen:  canteen.currentFill
    --   max sip from deficit:  deficit / k    (so we don't overshoot max)
    --   max sip per action:    params.drink_sip_litres
    local sip = math.min(params.drink_sip_litres,
                         canteen.currentFill,
                         deficit / k)
    if sip <= 0 then return end

    -- Apply effects synchronously — engine drink command is just for
    -- the anim + state-block. If the anim is interrupted (it shouldn't
    -- be, since Drinking blocks movement) the unit still drank.
    unit.modifyItemFill(uid, params.drink_canteen_def, -sip)
    unit.setStat(uid, "hydration", hyd + sip * k)
    unit.drink(uid)
end

-----------------------------------------------------------
-- Action: eat_from_inventory
--
-- Mirror of drink_from_canteen: utility scales linearly with how empty
-- the hunger meter is, fires only when hunger < eat_max_fraction of
-- max_hunger AND inventory has at least one item with a `food` block.
-- The drink anim is reused for v1 (see acolyte.yaml standing-eat).
-----------------------------------------------------------
local function findFoodInInventory(uid)
    local inv = unit.getInventory(uid)
    if not inv then return nil end
    local best, bestN = nil, -math.huge
    for _, it in ipairs(inv) do
        local nut = it.food and it.food.nutrition
        -- Edible kcal this item can yield right now: whole-item value
        -- for discrete food (rations), remaining-fill value for bulk
        -- food (a quinoa sack — an eaten-dry sack scores 0 and is
        -- skipped; unit.feed removes those anyway).
        local cal = nut and nut.calories or 0
        local bulk = (nut and nut.caloriesPerKg or 0) * (it.currentFill or 0)
        local avail = math.max(cal, bulk)
        if avail > 0 and avail > bestN then
            best, bestN = it, avail
        end
    end
    return best
end

local function eatUtility(uid, s, params)
    local hun    = unit.getStat(uid, "hunger")
    local maxHun = unit.getStat(uid, "max_hunger")
    if not hun or not maxHun or maxHun <= 0 then return -math.huge end
    local hungerFrac = hun / maxHun
    if hungerFrac >= params.eat_max_fraction then return -math.huge end
    if not findFoodInInventory(uid) then return -math.huge end
    return (1 - hungerFrac) * params.eat_weight
end

local function eatExecute(uid, s, params)
    -- Already eating? Don't re-issue the anim mid-bite.
    if unit.getActivity(uid) == "eating" then return end

    -- A meal, not a bite (#93): keep feeding until the stomach meter is
    -- full or the inventory runs out of food. Bounded — each unit.feed
    -- consumes something real, but the cap keeps a pathological item
    -- list finite (never lock the AI into an unbounded loop).
    for _ = 1, 10 do
        local hun    = unit.getStat(uid, "hunger")
        local maxHun = unit.getStat(uid, "max_hunger")
        if hun and maxHun and hun >= maxHun * 0.99 then break end

        local food = findFoodInInventory(uid)
        if not food then break end

        -- unit.feed is the authoritative consume-and-credit primitive:
        -- discrete food is removed whole (credit clamped to max_hunger,
        -- overflow wasted by design); bulk food (quinoa sack) sheds just
        -- enough fill to top the stomach up. Plays the eat anim. Returns
        -- the kcal credited, or nil if the item couldn't be consumed.
        if not unit.feed(uid, food.defName) then break end

        -- Food carries salt — the only way to recover from sweat-driven
        -- hyponatremia (the kidneys can't make sodium). See scripts/salts.lua.
        require("scripts.salts").mealSalt(uid)
    end
end

-----------------------------------------------------------
-- Action: refill_canteen
--
-- Proactive maintenance: when the canteen is past the emptiness
-- threshold and the unit has a remembered water location, walk to it
-- and top up. The quadratic urgency ramp keeps a partly-empty canteen
-- BELOW follow_command (so a topping-off doesn't interrupt orders), but
-- a near-dry one crosses above it (a dry canteen interrupts orders,
-- #306). FOV memory is updated separately at the top of tickOne.
-----------------------------------------------------------
local function findCanteenWithHeadroom(uid, defName)
    local inv = unit.getInventory(uid)
    if not inv then return nil end
    for _, it in ipairs(inv) do
        if it.defName == defName and it.capacity
           and it.currentFill < it.capacity then
            return it
        end
    end
    return nil
end

local function refillUtility(uid, s, params)
    if not hasKnownWaterSource(s) then return -math.huge end
    local canteen = findCanteenWithHeadroom(uid, params.canteen_def)
    if not canteen then return -math.huge end
    local emptiness = 1 - (canteen.currentFill / canteen.capacity)
    if emptiness < params.refill_min_emptiness then return -math.huge end
    -- Quadratic ramp: x = normalised position in [threshold, 1.0],
    -- squared so urgency stays low while the canteen is mostly full
    -- and shoots up as it runs dry. At empty (x=1) → base+scale = 7.5,
    -- above the command priority (7.0) — a dry canteen interrupts orders.
    local x = (emptiness - params.refill_min_emptiness)
            / (1.0 - params.refill_min_emptiness)
    return params.refill_base_weight
         + params.refill_urgency_scale * x * x
end

-- 8 surrounding tile offsets. Cardinals first so they win ties on
-- equal distance — natural-looking approach (units prefer N/S/E/W
-- over diagonals when both are equally close).
local NEIGHBOR_OFFSETS = {
    {  1,  0 }, { -1,  0 }, {  0,  1 }, {  0, -1 },
    {  1,  1 }, {  1, -1 }, { -1,  1 }, { -1, -1 },
}

-- Find the closest tile adjacent to (waterX, waterY) that is NOT
-- itself fluid. Returns the tile coords + the unit's distance to it,
-- or nil if every neighbor is fluid (shouldn't happen on a single
-- placed source, but harmless to handle).
local function nearestNonFluidNeighbor(waterX, waterY, fromX, fromY)
    local bestX, bestY, bestD = nil, nil, math.huge
    for _, off in ipairs(NEIGHBOR_OFFSETS) do
        local nx, ny = waterX + off[1], waterY + off[2]
        if not world.getFluidAt(nx, ny) then
            local d = distance(fromX, fromY, nx + 0.5, ny + 0.5)
            if d < bestD then
                bestX, bestY, bestD = nx, ny, d
            end
        end
    end
    return bestX, bestY, bestD
end

local function refillExecute(uid, s, params)
    local info = unit.getInfo(uid)
    if not info then return end
    -- Aim at the nearest known source. Refill re-executes only on
    -- action switch or when the unit is idle, so this won't oscillate
    -- mid-walk even with multiple sources in memory.
    local loc = nearestWaterSource(s, info.gridX, info.gridY)
    if not loc then return end

    -- Don't walk into the water — pick a dry neighbor to stand on.
    -- Tile-adjacency check: only fire refill when the unit's actual
    -- TILE is one step from the water in 8-neighborhood, AND the unit
    -- isn't standing on fluid. Earlier "distance to nearest dry
    -- neighbor" with a 1.5 threshold fired mid-walk, ~2 tiles from
    -- water, which felt very wrong visually.
    local utx = math.floor(info.gridX)
    local uty = math.floor(info.gridY)
    local cheb = math.max(math.abs(utx - loc.x), math.abs(uty - loc.y))
    local onFluid = world.getFluidAt(utx, uty) ~= nil

    if cheb == 1 and not onFluid then
        -- Verify the tile still actually has water before drinking from
        -- it (someone could have removed it via the debug tool while we
        -- were en route). If gone, drop just THIS source from memory;
        -- other known sources stay.
        local fluidType = world.getFluidAt(loc.x, loc.y)
        if fluidType == "lake" or fluidType == "river" then
            local canteen = findCanteenWithHeadroom(uid, params.canteen_def)
            if canteen then
                local headroom = canteen.capacity - canteen.currentFill
                if headroom > 0 then
                    unit.modifyItemFill(uid, params.canteen_def, headroom)
                    -- Brief "picking up" animation; the fill itself is
                    -- instant (matches real-world canteen dipping).
                    unit.pickup(uid)
                end
            end
        else
            forgetWaterSource(s, loc.x, loc.y)
        end
        return
    end

    -- Not yet adjacent — pick a dry neighbor and walk toward the
    -- water *edge* on that side. Aiming at the neighbor center leaves
    -- the unit visibly a full tile away from the bank; pushing 45% of
    -- the way toward water places it right at the edge while keeping
    -- math.floor() of its position firmly on the neighbor tile
    -- (so the on-fluid check still rejects "standing in water").
    local nx, ny = nearestNonFluidNeighbor(loc.x, loc.y,
                                           info.gridX, info.gridY)
    if not nx then
        forgetWaterSource(s, loc.x, loc.y)
        return
    end
    local alpha = 0.45
    local tx = nx + 0.5 + alpha * (loc.x - nx)
    local ty = ny + 0.5 + alpha * (loc.y - ny)
    unit.moveTo(uid, tx, ty, mv.comfort(uid))  -- routine errand → comfort
end

-----------------------------------------------------------
-- Action: drink_from_source
--
-- No-canteen path. Three-phase sequence driven from Lua via a
-- per-unit phase flag (s.sourcePhase ∈ {"descending","drinking",
-- "ascending", nil}). Lock-in: utility returns math.huge while the
-- phase is non-nil so a half-completed sequence can't be pre-empted.
--
-- Pose descent + ascent are chained two-step (standing↔crouching↔
-- crawling) and use stride=2 so the visible duration is halved — the
-- player doesn't sit through every frame of two 9-frame transitions.
-- Hydration regen happens at pose == "crawling" (on all fours at the
-- water), keyed by regen_factor_crawling in unit_resources.
-----------------------------------------------------------
local STRIDE_DESCEND = 2
local STRIDE_ASCEND  = 2

local function drinkFromSourceUtility(uid, s, params)
    if s.sourcePhase then return math.huge end

    local hyd = unit.getStat(uid, "hydration")
    local maxHyd = require("scripts.unit_stats").get(uid, "max_hydration")
    if not hyd or not maxHyd or maxHyd <= 0 then return -math.huge end

    local thirst = 1 - hyd / maxHyd
    if thirst < params.drink_min_thirst then return -math.huge end
    if not hasKnownWaterSource(s) then return -math.huge end

    -- Mutually exclusive with drink_from_canteen.
    if findCanteenWithWater(uid, params.drink_canteen_def) then
        return -math.huge
    end

    return thirst * params.drink_weight
end

local function drinkFromSourceExecute(uid, s, params)
    local pose = unit.getPose(uid) or "standing"

    -- Descending: step one pose down per AI tick (between ticks, the
    -- engine plays the transition anim with stride 2).
    if s.sourcePhase == "descending" then
        if     pose == "standing"  then
            unit.transitionTo(uid, "crouching", STRIDE_DESCEND)
        elseif pose == "crouching" then
            unit.transitionTo(uid, "crawling", STRIDE_DESCEND)
        elseif pose == "crawling"  then
            s.sourcePhase = "drinking"
        end
        return
    end

    -- Drinking: at the water on all fours. unit_resources regens
    -- hydration. When mostly full, kick off the ascent.
    if s.sourcePhase == "drinking" then
        local hyd    = unit.getStat(uid, "hydration") or 0
        local maxHyd = require("scripts.unit_stats").get(uid, "max_hydration") or 0
        -- maxHyd <= 0 means the stat went missing mid-drink (debug
        -- edit, def change). Bail into the ascent rather than holding
        -- the math.huge phase lock forever on a condition that can
        -- never become true.
        if maxHyd <= 0 or hyd / maxHyd >= 0.95 then
            s.sourcePhase = "ascending"
            unit.transitionTo(uid, "crouching", STRIDE_ASCEND)
        end
        return
    end

    -- Ascending: step one pose up per tick. When we hit standing,
    -- we're done. The source itself stays in memory — the lake didn't
    -- vanish just because we drank from it, and we may want to refill
    -- a canteen here next.
    if s.sourcePhase == "ascending" then
        if     pose == "crawling"  then
            unit.transitionTo(uid, "crouching", STRIDE_ASCEND)
        elseif pose == "crouching" then
            unit.transitionTo(uid, "standing", STRIDE_ASCEND)
        elseif pose == "standing"  then
            s.sourcePhase = nil
        end
        return
    end

    -- No phase active: standing entry. Walk to the bank or kick off
    -- the descent if already adjacent. Source pick is by nearest, but
    -- execute only fires on switch/idle so it won't oscillate during
    -- the walk.
    local info = unit.getInfo(uid)
    if not info then return end
    local loc = nearestWaterSource(s, info.gridX, info.gridY)
    if not loc then return end

    local utx = math.floor(info.gridX)
    local uty = math.floor(info.gridY)
    local cheb = math.max(math.abs(utx - loc.x), math.abs(uty - loc.y))
    local onFluid = world.getFluidAt(utx, uty) ~= nil

    if cheb == 1 and not onFluid then
        local fluidType = world.getFluidAt(loc.x, loc.y)
        if fluidType == "lake" or fluidType == "river" then
            s.sourcePhase = "descending"
            unit.transitionTo(uid, "crouching", STRIDE_DESCEND)
        else
            forgetWaterSource(s, loc.x, loc.y)
        end
        return
    end

    -- Walk to the water edge. Same geometry as refill — push 45%
    -- toward water so the unit stops right at the bank.
    local nx, ny = nearestNonFluidNeighbor(loc.x, loc.y,
                                           info.gridX, info.gridY)
    if not nx then
        forgetWaterSource(s, loc.x, loc.y)
        return
    end
    local alpha = 0.45
    local tx = nx + 0.5 + alpha * (loc.x - nx)
    local ty = ny + 0.5 + alpha * (loc.y - ny)
    unit.moveTo(uid, tx, ty, mv.comfort(uid))  -- routine errand → comfort
end

-----------------------------------------------------------
-- Action: search_for_water
--
-- Fires when canteen has headroom AND no water is known. Walks the
-- unit through a rosette of waypoints (8 compass directions per ring,
-- expanding rings). FOV scans during the walk; if water is spotted
-- (scanForWater fires every tickOne), the refill action takes over
-- on the next decision and search drops to -inf.
-----------------------------------------------------------
-- Per-unit fan-out: the base rosette is rotated by angleOffset
-- (radians) and rings are scaled by spacingMult. Both seeded once per
-- search session in searchExecute so 5 acolytes spawning together
-- fan out across compass + ring depth instead of marching in lockstep.
local function searchWaypoint(originX, originY, step, spacing,
                              angleOffset, spacingMult)
    if step <= 0 then return originX, originY end
    local n = #SEARCH_DIRECTIONS
    local ring   = math.floor((step - 1) / n) + 1
    local dirIdx = ((step - 1) % n) + 1
    local d = SEARCH_DIRECTIONS[dirIdx]
    local cos_a, sin_a = math.cos(angleOffset), math.sin(angleOffset)
    local rx = d[1] * cos_a - d[2] * sin_a
    local ry = d[1] * sin_a + d[2] * cos_a
    local r = ring * spacing * spacingMult
    return originX + rx * r,
           originY + ry * r
end

local function searchUtility(uid, s, params)
    if hasKnownWaterSource(s) then return -math.huge end
    -- Goal-driven branch. With find_water active, the unit's mission
    -- is to locate water — search regardless of personal canteen
    -- state. Falls through to the personal-need branch below once the
    -- goal is accomplished (knownWaterSources still empty, but goal
    -- no longer pushes). DERIVED urgency (#306): floor + urgency·thirst
    -- (see goal_search_floor/_urgency). A hydrated scout searches at a
    -- low, below-command floor (a player order / fight / treatment all
    -- win); the value climbs as it runs dry, but never reaches command
    -- on its own — genuine thirst is carried by drink/refill, which
    -- scale past command themselves.
    if isGoalActive(s, "find_water") then
        local hyd    = unit.getStat(uid, "hydration")
        local maxHyd = require("scripts.unit_stats").get(uid, "max_hydration")
        local thirst = (hyd and maxHyd and maxHyd > 0)
                       and math.max(0, math.min(1, 1 - hyd / maxHyd)) or 0
        return params.goal_search_floor
             + params.goal_search_urgency * thirst
    end
    local canteen = findCanteenWithHeadroom(uid, params.canteen_def)
    if not canteen then return -math.huge end
    local emptiness = 1 - (canteen.currentFill / canteen.capacity)
    if emptiness < params.search_min_emptiness then return -math.huge end
    return params.search_base_weight
         + (emptiness - params.search_min_emptiness)
           * params.search_emptiness_weight
end

local function searchExecute(uid, s, params)
    local info = unit.getInfo(uid)
    if not info then return end

    -- Fresh search session? Each action-switch bumps actionStartedAt,
    -- so we use that as the session id. Anchoring on the unit's
    -- CURRENT position keeps the search local rather than re-starting
    -- from a stale origin. The angle offset + spacing jitter are
    -- seeded here so 5 acolytes starting in a cluster fan out across
    -- ~2π / 5 ≈ 72° on average, with rings of staggered radius.
    if not s.searchOrigin or s.searchSession ~= s.actionStartedAt then
        s.searchOrigin       = { x = info.gridX, y = info.gridY }
        s.searchStep         = 1
        s.searchSession      = s.actionStartedAt
        s.searchAngleOffset  = math.random() * 2 * math.pi
        -- ~U(0.8, 1.4): a few units take tighter rings, a few stride
        -- longer between waypoints. Widens the search-radius variance
        -- per ring instead of all units sweeping the same band.
        s.searchSpacingMult  = 0.8 + math.random() * 0.6
    end

    local wx, wy = searchWaypoint(s.searchOrigin.x, s.searchOrigin.y,
                                  s.searchStep, params.search_spacing,
                                  s.searchAngleOffset,
                                  s.searchSpacingMult)
    local d = distance(info.gridX, info.gridY, wx, wy)

    if d <= params.search_arrival_tiles then
        s.searchStep = s.searchStep + 1
        if s.searchStep > params.search_max_step then
            -- Spiral exhausted without finding water — re-anchor here
            -- and start over. Reseed fan-out so the next pass spreads
            -- differently from the first.
            s.searchOrigin       = { x = info.gridX, y = info.gridY }
            s.searchStep         = 1
            s.searchAngleOffset  = math.random() * 2 * math.pi
            s.searchSpacingMult  = 0.8 + math.random() * 0.6
        end
        wx, wy = searchWaypoint(s.searchOrigin.x, s.searchOrigin.y,
                                s.searchStep, params.search_spacing,
                                s.searchAngleOffset,
                                s.searchSpacingMult)
    end

    unit.moveTo(uid, wx, wy, mv.comfort(uid))  -- searching → comfort
end

-----------------------------------------------------------
-- Action: follow_command
-----------------------------------------------------------
-- An explicit player move order outranks routine autonomous behaviour:
-- above ambient wander, routine work (build/deliver/store/dig ≤6.0), and
-- the situational goals (find_water/notify) — so a right-click reliably
-- redirects a unit that is merely wandering, working, or scouting.
--
-- It is OUTRANKED by (the #306 ladder, re-derived against this 7.0
-- baseline — NOT the historical 1.0):
--   * dire SELF survival — drink (thirst·15, crosses 7.0 at thirst≈0.47)
--     and eat (≥7.5 whenever it fires), and a dry-canteen refill (peaks
--     7.5 near-empty): a unit tends to its own body first, then resumes;
--   * combat — engage/retreat (8.0+): a unit defends itself or flees a
--     hopeless fight rather than walking to a commanded tile;
--   * treatment — treat_ally (8.0): a medic finishes saving a life.
-- Peer to it: an explicit ground-item pickup (7.5) — also a player order,
-- nudged just above so it wins the move-vs-pickup tie.
-- commandedTask persists until maintainTask clears it on arrival/timeout,
-- so the unit resumes the move once the higher-priority action finishes.
local FOLLOW_COMMAND_UTILITY = 7.0

local function followCommandUtility(uid, s, params)
    if not s.commandedTask then return -math.huge end
    return FOLLOW_COMMAND_UTILITY
end

local function followCommandExecute(uid, s, params)
    local task = s.commandedTask
    if not task then return end
    -- Player-ordered move: a slight sustainable push above comfort
    -- (ordered regime), unless the command specified an explicit speed.
    unit.moveTo(uid, task.x, task.y, task.speed or mv.ordered(uid))
end

-----------------------------------------------------------
-- Action: retreat
--
-- Universal "this fight is futile, run" candidate. Outscores
-- attack_target so a wounded outmatched unit breaks contact.
--
-- Triggers when:
--   1. We're on an attack goal AND
--   2. We've been hit by our attack target (per uiLastAttackerUid) AND
--   3. Group combat effectiveness (us + anyone else engaging the same
--      target) is less than 1/1.5 of the threat's own effectiveness.
--
-- Once triggered, sets activeGoal = "retreat" + s.retreatThreatUid;
-- attack_target then naturally yields (its utility checks activeGoal
-- == "attack"). The candidate keeps re-executing while in retreat
-- to re-path away from a chasing threat.
--
-- Termination: threat dies, threat despawns, or we're more than
-- RETREAT_SAFE_DIST chebyshev tiles away. Then we clear the goal
-- and ambient candidates take back over.
-----------------------------------------------------------
local RETREAT_FUTILITY_RATIO = 1.5
-- 2e: a unit under a player/scripted ORDER is much braver — it weighs the
-- group by the same ratio but tolerates being outmatched far further before
-- breaking. SOFT override (the user's choice): a truly hopeless fight (beyond
-- this) still routs it; an order isn't a fight-to-the-last-cell compulsion.
local RETREAT_FUTILITY_RATIO_COMMITTED = 4.0
local RETREAT_SAFE_DIST      = 12.0    -- chebyshev tiles
local RETREAT_TARGET_DIST    = 8.0     -- how far to pick a new retreat tile
-- 2e swarm: an ally counts toward "our" strength vs a threat if it's already
-- engaging it OR is a same-side unit within this many tiles of the threat (a
-- potential joiner). This breaks the chicken-and-egg where the FIRST attacker
-- sees only itself, judges the fight futile, and flees before a pack can form.
local SWARM_RALLY_RADIUS     = 8.0     -- chebyshev tiles from the threat

local function selfWoundedByTarget(uid, s)
    if not s.attackTargetUid then return false end
    local att = unit.getLastAttacker(uid)
    if not att then return false end
    return att.uid == s.attackTargetUid
end

-----------------------------------------------------------
-- Combat animation helpers
--
-- The engine's publishToRender resolves animation names from a
-- (pose, activity) state-key lookup, which has no slot for combat
-- attack swings or weapon-class variants. We bypass that by writing
-- to uiAnimOverride directly via unit.setAnimOverride; the engine
-- preserves the override until clearAnimOverride flips it off.
--
-- Animation name conventions vary per species:
--   * acolyte: "<injured_>BASE<_RH_dagger | _unarmed>" — combat
--              anims carry the weapon-class suffix.
--   * bear:    "<injured_>BASE" — bears have no weapon variants
--              because their natural weapon (claws/fangs) is the
--              only one they ever fight with.
-- Add to COMBAT_ANIM_SUFFIX when new species ship with their own
-- combat anim sets.
-----------------------------------------------------------

local COMBAT_ANIM_SUFFIX = {
    acolyte = {
        dagger  = "_RH_dagger",
        unarmed = "_unarmed",
    },
    bear_brown = {
        unarmed = "",   -- bear anim files have no class suffix
    },
}

-- "Injured" for animation purposes = cumulative EFFECTIVE wound severity
-- > 1.0 (sum across all active wounds). A single bad slash or several
-- moderate ones flip the unit to the limp/struggling combat anim;
-- a couple of light scratches don't. Tunable via INJURED_THRESHOLD.
-- Sum the engine's effective severity (severityEffective = max(acute,
-- necrosis), from woundEffSeverity) so this stays in lockstep with the
-- engine-side injured-anim flag (Unit.Thread.publishToRender sums
-- woundEffSeverity); a rotting wound counts even once the cut has closed.
local INJURED_THRESHOLD = 1.0
local function isInjured(uid)
    local wounds = unit.getWounds(uid)
    if not wounds then return false end
    local total = 0
    for _, w in ipairs(wounds) do
        total = total + (w.severityEffective or w.severity or 0)
        if total > INJURED_THRESHOLD then return true end
    end
    return false
end

-- Compose a combat anim name from the base ("attack_quick" /
-- "attack_heavy" / "combat_idle" / "combat_hit_react") plus the
-- unit's def-specific weapon-class suffix plus an injured prefix
-- when the unit is wounded. Returns nil if the species has no
-- entry in COMBAT_ANIM_SUFFIX (which means we just don't override
-- and let the engine's state-driven anim play).
local function combatAnimName(uid, base)
    local info = unit.getInfo(uid)
    if not info or not info.defName then return nil end
    local suffixes = COMBAT_ANIM_SUFFIX[info.defName]
    if not suffixes then return nil end
    local class = unit.getWeaponClass(uid) or "unarmed"
    local suffix = suffixes[class]
    if not suffix then return nil end
    local injured = isInjured(uid) and "injured_" or ""
    return injured .. base .. suffix
end

-- True iff this unit's threat (per attack goal) significantly
-- outclasses everyone currently engaging it. Threshold defaults to
-- 1.5×.
local function futilityCheck(uid, s)
    local threatUid = s.attackTargetUid
    if not threatUid then return false, 0 end
    if not unit.exists(threatUid) then return false, 0 end
    if unit.getPose(threatUid) == "dead" then return false, 0 end
    local threatEff = unitAi.combatEffectiveness(threatUid)
    local groupEff  = unitAi.groupEffectivenessVs(threatUid)
    if groupEff <= 0 then return false, 0 end
    local ratio = threatEff / groupEff
    -- A unit under orders (player/scripted commandAttack) holds far longer.
    local cap = s.committed and RETREAT_FUTILITY_RATIO_COMMITTED
                            or  RETREAT_FUTILITY_RATIO
    return ratio > cap, ratio
end

local function retreatUtility(uid, s, params)
    -- Carry-through: as long as the unit is in retreat goal, keep
    -- the candidate dominant — 8.0 matches the engage floor, above a
    -- player move order (7.0) so fleeing for your life isn't cancelled
    -- by a stale move command (#306).
    if isGoalActive(s, "retreat") then return 8.0 end
    if not isGoalActive(s, "attack") then return -math.huge end
    if not selfWoundedByTarget(uid, s) then return -math.huge end
    local futile, ratio = futilityCheck(uid, s)
    if not futile then return -math.huge end
    -- Urgency grows with ratio from the combat floor (8.0). ratio=1.5
    -- → 8.0; ratio=3.0 → 11.0 — a hopeless fight outranks even dire
    -- needs so the unit commits to escaping.
    return 8.0 + (ratio - RETREAT_FUTILITY_RATIO) * 2.0
end

local function retreatExecute(uid, s, params)
    -- First entry into retreat: transfer the threat ref + switch goal.
    -- Clear any combat-anim override left over from attack_target so
    -- the engine's state-driven walking anim plays during the flight.
    if not isGoalActive(s, "retreat") then
        s.retreatThreatUid = s.attackTargetUid
        s.attackTargetUid  = nil
        s.committed        = nil   -- broke despite the order; drop commitment
        markGoalAccomplished(s, "attack")
        setGoal(s, "retreat")
        unit.clearAnimOverride(uid)
        engine.logDebug("retreat: " .. tostring(uid)
            .. " breaks from " .. tostring(s.retreatThreatUid))
    end

    local threat = s.retreatThreatUid
    if not threat or not unit.exists(threat)
       or unit.getPose(threat) == "dead" then
        -- Threat gone; we're safe.
        s.retreatThreatUid = nil
        markGoalAccomplished(s, "retreat")
        return
    end

    local me  = unit.getInfo(uid)
    local you = unit.getInfo(threat)
    if not me or not you then return end

    local dx = me.gridX - you.gridX
    local dy = me.gridY - you.gridY
    local dist = math.max(math.abs(dx), math.abs(dy))
    if dist > RETREAT_SAFE_DIST then
        engine.logDebug("retreat: " .. tostring(uid)
            .. " reached safe distance from " .. tostring(threat))
        s.retreatThreatUid = nil
        markGoalAccomplished(s, "retreat")
        return
    end

    -- Pick a tile in the direction away from the threat.
    local mag = math.sqrt(dx * dx + dy * dy)
    if mag < 0.001 then
        -- Co-located: pick an arbitrary direction.
        dx, dy = 1, 0
        mag = 1
    end
    local tx = me.gridX + (dx / mag) * RETREAT_TARGET_DIST
    local ty = me.gridY + (dy / mag) * RETREAT_TARGET_DIST

    -- Re-issue moveTo only when the unit is idle, OR the destination
    -- has drifted by > 0.5 tiles (threat moved): same heuristic as
    -- attack_target's pursuit moves. Avoids stomping usLocalPath.
    local last = s.retreatLastMoveTo
    local needRepath = unit.getActivity(uid) == "idle"
        or not last
        or math.abs(last.x - tx) > 0.5
        or math.abs(last.y - ty) > 0.5
    if needRepath then
        -- Fleeing → sprint: max_speed × agility, "as fast as they can".
        unit.moveTo(uid, tx, ty, mv.sprint(uid))
        s.retreatLastMoveTo = { x = tx, y = ty }
    end
end

-----------------------------------------------------------
-- Action: engage
--
-- Universal "should I pick a fight, and with whom?" decision layer.
-- THREAT_SOURCES is a table of independent threat detectors; each
-- returns a {uid, score} pair or nil. engage picks the highest-
-- scoring threat and triggers `commandAttack` — actual fighting
-- runs through the regular `attack_target` candidate from there.
--
-- Phase 2.2 ships with a single source (incoming_hit, replacing
-- the old standalone "retaliate" candidate). Phase 2.3+ adds:
--   * hostile_in_sight — visible unit of opposing faction.
--   * prey_hunger — hungry predator + viable prey visible.
--   * defend_ally — friendly being attacked nearby.
-- Each new source is a table entry; the picker stays the same.
--
-- Score is the natural utility-comparison number: 8.0 for fresh
-- incoming-hit retaliation, above a commanded move (follow_command
-- 7.0) but below dire-need candidates (thirst / hunger scale past it)
-- so a starving bear still drinks before fighting (#306).
-----------------------------------------------------------
local ENGAGE_WINDOW_SEC = 10.0
-- How recently a melee hit must have landed for the in-combat target swap
-- to round on the new attacker (shorter than the engage window so the bear
-- chases a fled target rather than ping-ponging to whoever poked it once).
local RETALIATE_WINDOW_SEC = 3.0

local THREAT_SOURCES = {
    -- Recently took damage from someone who is still alive. The
    -- 10s window resets on each new hit (Combat.Resolution stamps
    -- uiLastAttackerAt on every wound), so sustained combat keeps
    -- us engaged; a fled attacker stops being relevant after 10s.
    {
        name = "incoming_hit",
        score = function(uid, s, params)
            local att = unit.getLastAttacker(uid)
            if not att then return nil end
            if engine.gameTime() - (att.at or 0)
               > ENGAGE_WINDOW_SEC then return nil end
            if not unit.exists(att.uid) then return nil end
            if unit.getPose(att.uid) == "dead" then return nil end
            -- 8.0 sits above a player move order (follow_command 7.0)
            -- and well above the goal candidates (find_water/notify,
            -- ≤6): a unit defends itself when struck rather than
            -- walking off to a commanded tile or resuming a routine
            -- search. Dire SELF needs still beat us (drinking when
            -- near-empty / eating when starving scale past 8.0), the
            -- intended scale being: literally-dying > combat/treatment
            -- > player-issued moves > general goals > ambient (#306).
            return { uid = att.uid, score = 8.0 }
        end,
    },
    -- Future sources go here, e.g.:
    -- { name = "hostile_in_sight", score = function(uid, s, params)
    --       … FOV scan, faction filter, distance-weighted score … end },
}

-- Shared by utility + execute so they never disagree about which
-- target won. Returns (uid, score) or (nil, -math.huge).
local function pickThreat(uid, s, params)
    local bestUid, bestScore = nil, -math.huge
    for _, src in ipairs(THREAT_SOURCES) do
        local t = src.score(uid, s, params)
        if t and t.score > bestScore then
            bestUid, bestScore = t.uid, t.score
        end
    end
    return bestUid, bestScore
end

local function engageUtility(uid, s, params)
    -- Already on an attack goal: defer to attack_target. Engage
    -- only fires the *initial* target-selection; mid-fight target
    -- swaps land in a later slice.
    if isGoalActive(s, "attack") then return -math.huge end
    -- Don't re-engage the same threat we're actively fleeing from.
    -- retreat clears its goal once we're safe; until then we stay
    -- in flight even if the threat keeps landing hits.
    if isGoalActive(s, "retreat") then return -math.huge end
    local _, score = pickThreat(uid, s, params)
    return score
end

local function engageExecute(uid, s, params)
    local target, _ = pickThreat(uid, s, params)
    if not target then return end
    -- Reuse commandAttack so the goal + state are set identically
    -- to a player-issued order. attack_target wins on the next tick.
    unitAi.commandAttack(uid, target)
    engine.logDebug("engage: " .. tostring(uid)
        .. " engages " .. tostring(target))
end

-----------------------------------------------------------
-- Heavy / quick attack mode + dynamic recovery time.
--
-- Two attack modes, picked per swing. Heavy commits the body forward
-- and applies linear strength; quick is a controlled motion that
-- applies sqrt(strength). The damage gap between them therefore comes
-- entirely from the strength stat — a high-str unit gains a lot from
-- going heavy, a low-str unit gains nothing and never picks it. No
-- per-mode damage multiplier; the engine's Combat.Resolution makes
-- the differential stat-driven.
--
-- Recovery time (cooldown between swings) is computed live each tick
-- from:
--   * base attack_cooldown declared on the weapon / natural_weapon
--   * mode multiplier (heavy = 1.6× the quick base)
--   * weight factor: heavy weapons take longer to recover from,
--     scaled by the wielder's strength
--   * stamina factor: exhausted units recover slower
--   * injury factor: wounds on the weapon-arm slow you sharply
--   * stat factor: agility × dexterity make you faster
--
-- All inputs are stats the engine already tracks. No species-specific
-- knobs — a healthy acolyte and a healthy bear land at similar swings/
-- second despite very different stat profiles, because each species'
-- strengths and weaknesses cancel out through the formula.
-----------------------------------------------------------

-- Heavy mode is worth choosing when stamina is high AND the wielder
-- can actually deliver more damage (strength > 1.0; at str=1 heavy and
-- quick deal identical damage, so paying the extra stamina is a loss).
-- A wound on the weapon arm at severity ≥ 0.5 makes heavy unusable —
-- you can't put the body in to a swing if the arm holding the weapon
-- is torn up.
-- Stamina pct, robust to species that haven't been wired into
-- unit_resources.lua yet. A unit with no stamina stat at all is
-- treated as healthy (100%) — combat assumes "stamina works" so
-- the absence of config doesn't permanently lock the unit into
-- quick-mode. The unit_resources tick handles the drain regardless.
local function staminaPct(uid)
    local s  = unit.getStat(uid, "stamina")
    local ms = require("scripts.unit_stats").get(uid, "max_stamina")
    if s and ms and ms > 0 then
        return math.max(0, math.min(1, s / ms))
    end
    return 1.0
end

local function chooseAttackMode(uid)
    local pct = staminaPct(uid)
    if pct < 0.5 then return "quick" end

    local str = unit.getStat(uid, "strength") or 1.0
    if str <= 1.0 then return "quick" end

    local armPart = unit.getWeaponWieldedFrom(uid)
    if armPart then
        local armWound = unit.getWoundSeverityOn(uid, armPart) or 0
        if armWound >= 0.5 then return "quick" end
    end

    -- Soft utility curve: heavy preference rises from 0 at 50% stamina
    -- to 1.0 at 100% stamina. No flicker at any threshold.
    local heavyPref = (pct - 0.5) / 0.5
    if heavyPref > 0.5 then return "heavy" end
    return "quick"
end

local function computeAttackCooldown(uid, mode)
    local base = unit.getAttackCooldown(uid) or 1.5
    local modeMult = (mode == "heavy") and 1.6 or 1.0

    -- Weight factor: only weapons heavier than the reference (1 kg)
    -- add cost, and the cost scales inversely with strength. A
    -- powerful wielder isn't slowed by a heavy weapon they can muscle.
    local weight   = unit.getEquippedWeaponWeight(uid) or 1.0
    local str      = unit.getStat(uid, "strength") or 1.0
    local weightF  = 1 + math.max(0, weight - 1.0) / (5 * str)

    -- Stamina factor: 1.0 at full, 1.5 at empty. Exhausted units pay
    -- 50% longer recoveries. staminaPct returns 1.0 for species
    -- without resource config, so this is a no-op until the species
    -- is wired in — the formula still works for combat-only checks.
    local pct   = staminaPct(uid)
    local stamF = 1 + (1 - pct) * 0.5

    -- Injury factor: a severity-0.5 wound on the weapon arm doubles
    -- recovery. Severity-1.0 (vital arm, unit usually dead) triples.
    local armPart  = unit.getWeaponWieldedFrom(uid)
    local armWound = armPart
                     and (unit.getWoundSeverityOn(uid, armPart) or 0)
                     or 0
    local injuryF  = 1 + 2 * armWound

    -- Stat factor: agility and dexterity together govern raw motor
    -- speed. 1/sqrt(agi×dex) puts a 1.0/1.0 unit at the baseline and
    -- a 2.0/2.0 superhero at half the recovery.
    local agi = unit.getStat(uid, "agility")   or 1.0
    local dex = unit.getStat(uid, "dexterity") or 1.0
    local statF = 1 / math.sqrt(math.max(0.05, agi * dex))

    return base * modeMult * weightF * stamF * injuryF * statF
end

-----------------------------------------------------------
-- Action: attack_target
--
-- Combat candidate. Set via unitAi.commandAttack(uid, targetUid).
-- Goal-driven so dire-need candidates (thirst, hunger) preempt
-- by outscoring this candidate's 1.0.
--
-- State carried on `s`:
--   s.activeGoal       = "attack" (via setGoal)
--   s.attackTargetUid  = uid of the target
--   s.attackLastSwingAt = gameTime of last fired swing (cooldown gate)
--   s.attackLastMode   = "heavy" | "quick" — for cooldown math, since
--                        the recovery from the PREVIOUS swing is what
--                        gates the next one.
--
-- Each tick, until the target is dead or gone:
--   * Target missing / dead → clear goal.
--   * In range AND cooldown elapsed → pick mode, fire combat.attack
--     with it, stamp attackLastSwingAt + attackLastMode.
--   * In range AND on cooldown → stand still and wait.
--   * Out of range → re-pathfind toward target's CURRENT tile so
--     moving targets get tracked. tickOne gates re-issuing moveTo
--     on activity == idle, so we don't wipe usLocalPath while the
--     unit is mid-walk.
-----------------------------------------------------------
local function attackTargetUtility(uid, s, params)
    if not isGoalActive(s, "attack") then return -math.huge end
    if not s.attackTargetUid then return -math.huge end
    -- In the combat band (8.0), same as engage/retreat. commandAttack
    -- sets the attack goal but leaves any pending commandedTask intact,
    -- so the pursuit MUST out-rank follow_command (7.0) — otherwise a
    -- stale move order would yank the unit straight back off the fight
    -- the tick after engage hands over (#306). Dire SELF survival
    -- (drink/eat scaling past 8) still pre-empts and resumes, and the
    -- move resumes once the attack goal ends (target dead/gone/fled).
    return 8.0
end

-- Helper: pop the attack-target's anim override safely. Used when
-- the goal terminates (target dead, gone, mid-fight switch) so we
-- don't leave the unit frozen in a combat-idle stance forever.
local function clearAttackAnim(uid)
    unit.clearAnimOverride(uid)
end

-- Only very-short-reach attackers lunge (slice 2a: the squirrel, reach
-- ~0.11). Normal melee units (reach ≥ this) just close and swing. The
-- general "skilled/unintelligent fighters also lunge" gating is later (2e).
-- Abort a lunge that never resolves (e.g. interrupted mid-air) after this.
local LUNGE_TIMEOUT_SEC = 3.0

-- ----- Lunge decision (2e: rarity, split by intelligence + skill) -----
-- A lunge is a deliberate, committed move — not something a unit does every
-- time it's out of reach. WHO lunges, and how readily, depends on the mind:
--   * Unintelligent creatures (intelligence < LUNGE_INSTINCT_INTEL — squirrels,
--     bears, mules) lunge on INSTINCT. For a short-reach predator/prey it's
--     often the ONLY way to reach a tall target, so the propensity is high;
--     the leap's REACH is still naturally bounded by the jumping skill (the
--     engine's getJumpReach), so a clumsy animal simply can't leap far.
--   * Intelligent fighters (acolytes) treat the lunge as a trained TECHNIQUE,
--     gated by the `jumping` skill — a novice (skill 10) almost never lunges;
--     a skilled one occasionally does to close a gap. Otherwise they walk in
--     and fight normally.
-- Either way it costs commitment, so it's gated on stamina.
local LUNGE_INSTINCT_INTEL    = 0.7   -- below = instinct regime (animals)
local LUNGE_INSTINCT_P        = 0.85  -- animal propensity when out of reach
local LUNGE_TECH_MAX_P        = 0.5   -- ceiling for a trained lunger
local LUNGE_TECH_SKILL_K      = 0.6   -- jumping/100 × this = technique chance
local LUNGE_MIN_STAMINA_FRAC  = 0.25  -- too winded to commit below this

-- Decide whether an out-of-reach unit commits to a lunge THIS attempt.
-- Rolled only when already eligible (out of reach, off cooldown), so the
-- attack cooldown spaces the rolls — a "no" just means the unit pursues on
-- foot this cycle and may roll again next time it's off cooldown.
local function shouldLunge(uid, s)
    -- Stamina gate — a leap is a big spend.
    local stam = unit.getStat(uid, "stamina")
    if stam then
        local maxStam = require("scripts.unit_stats").get(uid, "max_stamina")
        if maxStam and maxStam > 0 and stam / maxStam < LUNGE_MIN_STAMINA_FRAC then
            return false
        end
    end
    local intel = unit.getStat(uid, "intelligence") or 1.0
    local p
    if intel < LUNGE_INSTINCT_INTEL then
        p = LUNGE_INSTINCT_P                       -- instinct: readily
    else
        local jumping = unit.getSkill(uid, "jumping") or 0.0
        p = math.min(LUNGE_TECH_MAX_P, (jumping / 100.0) * LUNGE_TECH_SKILL_K)
    end
    return math.random() < p
end

-- The lunge: a short-reach unit leaps to land ADJACENT to a target it
-- can't otherwise reach, then strikes on arrival with a reach BONUS equal
-- to the leap's strike-reach — so the engine's height-gated part picker
-- can select the now-reachable high parts (neck/throat). Multi-tick:
-- issue the leap, wait for the airborne→land transition, then strike.
-- Returns true if it handled this tick (caller skips normal attack/pursue).
local function tryLunge(uid, s, target, me, you, chebyshev)
    local range = unit.getAttackRange(uid) or 1.0
    local now = engine.gameTime()

    -- Phase 2: airborne — wait until we've actually left the ground and
    -- come back down, then deliver the strike.
    if s.lungePhase == "air" then
        if now - (s.lungeStartAt or now) > LUNGE_TIMEOUT_SEC then
            s.lungePhase = nil; return false        -- bail; resume normal logic
        end
        local pose = unit.getPose(uid)
        if pose == "falling" then s.lungeSawAir = true end
        if s.lungeSawAir and pose == "standing"
           and unit.getActivity(uid) ~= "transitioning" then
            if unit.exists(target) and unit.getPose(target) ~= "dead" then
                unit.setAnimOverride(uid, "attack_quick")
                -- reach bonus lets the strike hit a high part; impact speed
                -- folds the leap's full-body momentum into the damage.
                combat.attack(uid, target, s.lungeMode or "quick",
                              s.lungeReach or 0, s.lungeImpactSpeed or 0)
                s.attackSwingUntil  = now + (unit.getAnimDuration(uid, "attack_quick") or 0.4)
                s.attackLastSwingAt = now
                s.attackLastMode    = s.lungeMode or "quick"
            end
            s.lungePhase = nil; s.lungeSawAir = nil
            s.lungeTarget = nil; s.lungeReach = nil; s.lungeImpactSpeed = nil
        end
        return true   -- consume the tick while leaping / landing
    end

    -- Phase 1: decide to leap. Must be out of melee reach, off cooldown,
    -- and the mind/skill check (2e) must elect to commit — otherwise fall
    -- through to normal pursue (walk closer and fight).
    if chebyshev <= range then return false end
    local last = s.attackLastSwingAt or 0
    if now - last < computeAttackCooldown(uid, s.attackLastMode or "quick") then
        return false
    end
    if not shouldLunge(uid, s) then return false end
    local jr = unit.getJumpReach(uid)
    if not jr or not jr.dist or jr.dist <= 0 then return false end

    -- Land one tile from the target, on our side. getInfo gridX/Y are
    -- CONTINUOUS positions, so floor to integer TILE coords (unit.jump
    -- needs integers, or its tointeger silently rejects the command).
    local mtx, mty = math.floor(me.gridX),  math.floor(me.gridY)
    local ttx, tty = math.floor(you.gridX), math.floor(you.gridY)
    local sgx = (mtx > ttx and 1) or (mtx < ttx and -1) or 0
    local sgy = (mty > tty and 1) or (mty < tty and -1) or 0
    local landX, landY = ttx + sgx, tty + sgy
    local ldx, ldy = landX - mtx, landY - mty
    local d = math.sqrt(ldx * ldx + ldy * ldy)

    if d < 0.5 then
        -- Already adjacent: a vertical pounce in place — full strike-reach,
        -- no horizontal leap (which the engine would refuse at d≈0).
        unit.setAnimOverride(uid, "attack_quick")
        combat.attack(uid, target, "quick", jr.height or 0,
                      unit.lungeImpactSpeed(uid, 0))
        s.attackSwingUntil  = now + (unit.getAnimDuration(uid, "attack_quick") or 0.4)
        s.attackLastSwingAt = now
        s.attackLastMode    = "quick"
        return true
    end
    if d > jr.dist then return false end

    -- Strike-reach envelope at this leap distance.
    local frac  = d / jr.dist
    local reach = (jr.height or 0) * (1 - frac * frac)
    if unit.jump(uid, landX, landY) then
        s.lungePhase        = "air"
        s.lungeSawAir       = false
        s.lungeStartAt      = now
        s.lungeTarget       = target
        s.lungeMode         = "quick"
        s.lungeReach        = reach
        s.lungeImpactSpeed  = unit.lungeImpactSpeed(uid, d)
    end
    return true
end

local function attackTargetExecute(uid, s, params)
    local target = s.attackTargetUid
    if not target then
        markGoalAccomplished(s, "attack")
        clearAttackAnim(uid)
        return
    end
    -- Target existence + alive check. Phase 2 will layer in
    -- observed-status memory so a unit who hasn't seen the target
    -- doesn't blindly path toward it.
    if not unit.exists(target) then
        engine.logDebug("attack: target " .. tostring(target)
                        .. " gone, clearing goal")
        s.attackTargetUid = nil
        markGoalAccomplished(s, "attack")
        clearAttackAnim(uid)
        return
    end
    if unit.getPose(target) == "dead" then
        engine.logDebug("attack: target " .. tostring(target)
                        .. " is dead, clearing goal")
        s.attackTargetUid = nil
        markGoalAccomplished(s, "attack")
        clearAttackAnim(uid)
        return
    end

    -- Mid-fight RETALIATION: if someone other than the current target just
    -- hit us and they're within melee reach, turn on them. A predator
    -- chasing a fleeing victim will round on whoever's stabbing its flank
    -- instead of tunnel-visioning the runner. (The initial-engage path
    -- already handles first contact; this is the in-combat target swap.)
    do
        local att = unit.getLastAttacker(uid)
        if att and att.uid ~= target and unit.exists(att.uid)
           and unit.getPose(att.uid) ~= "dead"
           and (engine.gameTime() - (att.at or 0)) <= RETALIATE_WINDOW_SEC then
            local m = unit.getInfo(uid)
            local a = unit.getInfo(att.uid)
            if m and a then
                local d = math.max(math.abs(m.gridX - a.gridX),
                                   math.abs(m.gridY - a.gridY))
                if d <= (unit.getAttackRange(uid) or 1.0) + 0.5 then
                    s.attackTargetUid = att.uid
                    target = att.uid
                    clearAttackAnim(uid)
                end
            end
        end
    end

    local me  = unit.getInfo(uid)
    local you = unit.getInfo(target)
    if not me or not you then return end

    local dx = math.abs(me.gridX - you.gridX)
    local dy = math.abs(me.gridY - you.gridY)
    local chebyshev = (dx > dy) and dx or dy
    local range    = unit.getAttackRange(uid) or 1.0

    -- Short-reach units (the squirrel) lunge instead of futilely closing to
    -- a melee range they can never reach. Handles the whole leap→land→strike
    -- sequence over several ticks; if it acted, skip the normal path.
    if tryLunge(uid, s, target, me, you, chebyshev) then return end

    if chebyshev <= range then
        -- In range. If we were mid-walk, stop so the next AI tick
        -- sees activity == "idle" and we can settle into the
        -- cooldown loop. unit.stop is idempotent — fine to spam.
        if unit.getActivity(uid) == "walking" then
            unit.stop(uid)
        end
        -- Cooldown gate: recovery from the PREVIOUS swing is what
        -- governs when the next one can fire. So we read the
        -- cooldown using last swing's mode, not the upcoming one.
        local now  = engine.gameTime()
        local last = s.attackLastSwingAt or 0
        local prevMode = s.attackLastMode or "quick"
        local cooldown = computeAttackCooldown(uid, prevMode)
        -- Stance gate: you can't throw a swing you're not set for.
        -- Costs mirror Combat.Resolution (heavy 0.5, quick 0.25). If
        -- not set for heavy, downgrade to quick; if not even set for
        -- quick, hold the guard and recover (no swing this tick).
        local stance = unit.getStat(uid, "stance") or 1.0
        if now - last >= cooldown and stance >= 0.25 then
            -- Pick mode by current stamina + wounds + strength.
            -- Damage differential comes from the engine's strength
            -- application (sqrt(str) for quick, str for heavy); we
            -- just decide which swing to throw.
            local mode = chooseAttackMode(uid)
            if mode == "heavy" and stance < 0.5 then mode = "quick" end
            local base = (mode == "heavy") and "attack_heavy"
                                             or "attack_quick"
            local anim = combatAnimName(uid, base)
            if anim then
                unit.setAnimOverride(uid, anim)
                -- Hold the swing override for the swing animation's real
                -- length; otherwise the very next AI tick (still mid-
                -- cooldown) overwrites it with combat_idle before a
                -- single frame shows. 0.5s fallback if the duration is
                -- unknown.
                local dur = unit.getAnimDuration(uid, anim) or 0.5
                s.attackSwingUntil = now + dur
            end
            combat.attack(uid, target, mode)
            s.attackLastSwingAt = now
            s.attackLastMode    = mode
            engine.logDebug("attack: " .. tostring(uid)
                .. " " .. mode .. " at " .. tostring(target)
                .. " (cd=" .. string.format("%.2f", cooldown)
                .. "s, stance=" .. string.format("%.2f", stance)
                .. ", anim=" .. tostring(anim) .. ")")
        else
            -- Mid-cooldown — show the combat-idle stance instead of
            -- falling back to regular idle. But NOT while the last
            -- swing's animation is still playing (attackSwingUntil),
            -- or we'd cut the swing short. setAnimOverride is cheap to
            -- call every tick; engine treats same-anim writes as a
            -- no-op for playback timing.
            if now >= (s.attackSwingUntil or 0) then
                local anim = combatAnimName(uid, "combat_idle")
                if anim then unit.setAnimOverride(uid, anim) end
            end
        end
    else
        -- Out of range — clear the override so the engine's state-
        -- driven walking anim plays while we close on the target.
        unit.clearAnimOverride(uid)
        local last = s.attackLastMoveTo
        local dxLast = last and math.abs(last.x - you.gridX) or math.huge
        local dyLast = last and math.abs(last.y - you.gridY) or math.huge
        if unit.getActivity(uid) == "idle"
           or dxLast > 0.5 or dyLast > 0.5 then
            -- Close on the enemy at a STAMINA-AWARE pace. Sprint only while
            -- we have the wind for it; cruise when winded. Charging an empty
            -- tank just collapses us two tiles short — only fleeing for our
            -- lives (retreat) runs to exhaustion.
            local sp      = mv.sprint(uid)
            local stam    = unit.getStat(uid, "stamina")
            local maxStam = require("scripts.unit_stats").get(uid, "max_stamina")
            if stam and maxStam and maxStam > 0 then
                local frac = stam / maxStam
                if frac < 0.30 then sp = mv.comfort(uid)      -- winded: cruise
                elseif frac < 0.55 then sp = mv.ordered(uid)  -- tiring: push
                end
            end
            unit.moveTo(uid, you.gridX, you.gridY, sp)
            s.attackLastMoveTo = { x = you.gridX, y = you.gridY }
        end
    end
end

-----------------------------------------------------------
-- Action: notify_allies
--
-- Fires when activeGoal == "notify_allies" (set by scanForWater's
-- discovery branch, or by being notified in person). Two paths:
--
--  * Radio branch (unit carries a "radio" item): stand still for
--    notify_broadcast_seconds, then push knownWaterSources to every
--    other radio-bearing acolyte, marking their find_water goal
--    accomplished. The broadcaster's own notify_allies then completes.
--    In v1, every acolyte spawns with a radio, so this branch
--    informs the whole group in one hop.
--
--  * Walk branch (no radio): coordinated rank-based split. Each tick
--    we enumerate all current walk-notifiers sorted by uid, find this
--    unit's rank, and target uninformed[rank]. Two notifiers always
--    pick two different targets; the cascade doubles each round
--    (1 → 2 → 4 → ...) until no uninformed remain. When this unit
--    transfers, notifyPhase is reset rather than the goal completed —
--    we re-evaluate next tick and pick a new target if any remain.
--    Completion happens when uninformed is empty, or when rank
--    exceeds #uninformed (more walkers than work).
--
-- Phase state on s:
--   notifyPhase           = "walking" | "transferring" | "broadcasting" | nil
--   notifyPhaseStartedAt  = game-time when phase began (for timed phases)
--   notifyTarget          = uid (walk variant only)
-----------------------------------------------------------
local function hasRadio(uid)
    local inv = unit.getInventory(uid)
    if not inv then return false end
    for _, it in ipairs(inv) do
        if it.defName == "radio" then return true end
    end
    return false
end

local function isAcolyteUid(uid)
    local info = unit.getInfo(uid)
    return info ~= nil and info.defName == "acolyte"
end

-- Acolytes whose AI state has been initialized but who don't yet have
-- a known water source. Sorted by uid so every observer agrees on
-- the same ordering. excludeUid lets the caller drop themselves out
-- (a walker shouldn't target itself).
local function getUninformedAcolytes(excludeUid)
    local result = {}
    local ids = unit.getAllIds()
    if not ids then return result end
    for _, uid in ipairs(ids) do
        if uid ~= excludeUid and isAcolyteUid(uid) then
            local s = aiState[uid]
            if s and not hasKnownWaterSource(s) then
                table.insert(result, uid)
            end
        end
    end
    table.sort(result)
    return result
end

-- Acolytes currently in the walk-notify pool: notify_allies active,
-- no radio. Sorted by uid for the rank split.
local function getWalkNotifiers()
    local result = {}
    local ids = unit.getAllIds()
    if not ids then return result end
    for _, uid in ipairs(ids) do
        if isAcolyteUid(uid) then
            local s = aiState[uid]
            if s and isGoalActive(s, "notify_allies") and not hasRadio(uid) then
                table.insert(result, uid)
            end
        end
    end
    table.sort(result)
    return result
end

-- Transfer-of-knowledge effect. Dedup-merges sources into the
-- target's memory and advances their goal chain (find_water →
-- notify_allies). Idempotent on already-informed targets.
local function transferSourcesTo(sources, targetUid)
    local targetS = aiState[targetUid]
    if not targetS then return end
    for _, src in ipairs(sources) do
        addWaterSource(targetS, src.x, src.y)
    end
    if isGoalActive(targetS, "find_water") then
        markGoalAccomplished(targetS, "find_water")
        setGoal(targetS, "notify_allies")
    elseif not targetS.goalStatus
        or targetS.goalStatus["find_water"] ~= "accomplished" then
        -- They were past find_water somehow (no goal, or on a later
        -- one we don't yet have). Record the accomplishment without
        -- queueing notify_allies on top.
        targetS.goalStatus = targetS.goalStatus or {}
        targetS.goalStatus["find_water"] = "accomplished"
    end
end

local function notifyAlliesUtility(uid, s, params)
    if not isGoalActive(s, "notify_allies") then return -math.huge end
    -- Lock in once a phase is active: a half-done broadcast or walk
    -- shouldn't be yanked by ambient WANDER drift. The lock equals the
    -- goal floor (goal_notify_weight = 4.0): above wander (~0.8) so idle
    -- drift can't interrupt it, but below follow_command (7.0) and
    -- combat/treatment (≥8.0) — notify is a routine, non-critical goal,
    -- so a player order, a fight, or a medic's patient all supersede it
    -- (#306). It resumes once the higher-priority action finishes.
    if s.notifyPhase then return params.goal_notify_weight end
    return params.goal_notify_weight
end

local function notifyAlliesExecute(uid, s, params)
    -- Radio branch ----------------------------------------------------
    if hasRadio(uid) then
        if not s.notifyPhase then
            s.notifyPhase = "broadcasting"
            s.notifyPhaseStartedAt = engine.gameTime()
            unit.stop(uid)
            return
        end
        if s.notifyPhase == "broadcasting" then
            local elapsed = engine.gameTime() - s.notifyPhaseStartedAt
            if elapsed < params.notify_broadcast_seconds then return end
            -- Broadcast: every other radio-bearing acolyte gets the
            -- full source list, find_water flips to accomplished.
            -- Recipients on radio do NOT pick up notify_allies — the
            -- broadcast already saturated the radio-bearer pool. If
            -- any non-radio acolytes exist, they'll need walk-notify;
            -- the broadcaster delegates that responsibility implicitly
            -- because each non-radio recipient (none in v1, since
            -- everyone has a radio) would receive notify_allies via
            -- transferSourcesTo's find_water→notify_allies chain.
            local mySources = s.knownWaterSources or {}
            local ids = unit.getAllIds() or {}
            for _, other in ipairs(ids) do
                if other ~= uid and isAcolyteUid(other) and hasRadio(other) then
                    local otherS = aiState[other]
                    if otherS then
                        for _, src in ipairs(mySources) do
                            addWaterSource(otherS, src.x, src.y)
                        end
                        if isGoalActive(otherS, "find_water") then
                            markGoalAccomplished(otherS, "find_water")
                        else
                            otherS.goalStatus = otherS.goalStatus or {}
                            otherS.goalStatus["find_water"] = "accomplished"
                        end
                    end
                end
            end
            markGoalAccomplished(s, "notify_allies")
            s.notifyPhase = nil
            s.notifyPhaseStartedAt = nil
            return
        end
        return   -- defensive: unknown phase, no-op
    end

    -- Walk branch -----------------------------------------------------
    local walkers    = getWalkNotifiers()
    local uninformed = getUninformedAcolytes(uid)

    if #uninformed == 0 then
        markGoalAccomplished(s, "notify_allies")
        s.notifyPhase = nil
        s.notifyTarget = nil
        s.notifyPhaseStartedAt = nil
        return
    end

    -- Find this unit's rank in the walker pool (stable across all
    -- walkers because both lists are uid-sorted).
    local rank = nil
    for i, w in ipairs(walkers) do
        if w == uid then rank = i; break end
    end
    if not rank then
        -- Goal active but we're not in the walker pool — shouldn't
        -- happen. Self-heal by completing.
        markGoalAccomplished(s, "notify_allies")
        s.notifyPhase = nil
        return
    end

    -- More walkers than uninformed: ranks past #uninformed have no
    -- target. Bow out cleanly.
    if rank > #uninformed then
        markGoalAccomplished(s, "notify_allies")
        s.notifyPhase = nil
        s.notifyTarget = nil
        return
    end

    local targetUid = uninformed[rank]
    s.notifyTarget = targetUid

    local targetInfo = unit.getInfo(targetUid)
    if not targetInfo then
        -- Target despawned mid-walk. Reset phase, re-pick next tick.
        s.notifyPhase = nil
        s.notifyTarget = nil
        return
    end

    local info = unit.getInfo(uid)
    if not info then return end
    local d = distance(info.gridX, info.gridY,
                       targetInfo.gridX, targetInfo.gridY)

    if d > params.notify_arrival_tiles then
        s.notifyPhase = "walking"
        unit.moveTo(uid, targetInfo.gridX, targetInfo.gridY,
                    mv.comfort(uid))  -- routine errand → comfort
        return
    end

    -- Arrived. Stand for notify_transfer_seconds, then hand off.
    if s.notifyPhase ~= "transferring" then
        s.notifyPhase = "transferring"
        s.notifyPhaseStartedAt = engine.gameTime()
        unit.stop(uid)
        return
    end
    local elapsed = engine.gameTime() - s.notifyPhaseStartedAt
    if elapsed < params.notify_transfer_seconds then return end

    transferSourcesTo(s.knownWaterSources or {}, targetUid)
    -- Phase clears; goal STAYS active. Next tick re-enters this
    -- function, recomputes uninformed (now -1), picks a new target
    -- if any remain. This is the parallel-doubling cascade: A informs
    -- B, then both A and B look for further uninformed targets next
    -- round.
    s.notifyPhase = nil
    s.notifyTarget = nil
    s.notifyPhaseStartedAt = nil
end

-----------------------------------------------------------
-- Action: deliver_to_build_site
--
-- Fires when (a) an Appearing building within scan range still has
-- unmet material need and (b) this unit carries at least one of
-- those materials. On first selection, the unit "claims" a delivery
-- plan onto aiState[uid].deliveryClaim — a map {[type] = count} —
-- so other acolytes computing utility see reduced remaining need and
-- only the minimum number of them commit. Lock-in: utility returns a
-- finite 6.0 while a claim is held so the walk-and-deliver sequence
-- isn't pre-empted by routine wander/follow_command, while dire needs
-- (drink/eat, ~10 when the meter is nearly empty) still interrupt;
-- the claim persists through the interruption and delivery resumes.
--
-- Reservations from non-existent units are ignored (stale-claim
-- self-heal) so a dying acolyte's lock doesn't strand materials.
-----------------------------------------------------------

local function inventoryCountOf(uid, matType)
    local inv = unit.getInventory(uid)
    if not inv then return 0 end
    local n = 0
    for _, it in ipairs(inv) do
        if it.defName == matType then n = n + 1 end
    end
    return n
end

-- Item-def weight lookup for the fetch capacity gate. (pickup_ground
-- has its own copy further down — locals are lexically scoped, so
-- this section can't see it.)
local function deliverItemWeight(defName)
    for _, d in ipairs(item.listDefs() or {}) do
        if d.name == defName then return d.weight or 0 end
    end
    return 0
end

-- Nearest technomule, or nil. The colony's construction stock rides
-- on it; deliverers fetch their shortfall from here. No range limit —
-- materials are worth the walk.
local function findTechnomule(fromX, fromY)
    local best, bestD = nil, math.huge
    for _, otherUid in ipairs(unit.getAllIds() or {}) do
        local info = unit.getInfo(otherUid)
        if info and info.defName == "technomule" then
            local d = distance(fromX, fromY, info.gridX, info.gridY)
            if d < bestD then
                best, bestD = { uid = otherUid, gridX = info.gridX,
                                gridY = info.gridY }, d
            end
        end
    end
    return best
end

-- How many units of matType still need a deliverer, considering both
-- engine-side delivered counts AND other live acolytes' active
-- claims. excludeUid lets the caller drop their own claim from the
-- sum so they can re-evaluate without double-counting.
local function remainingUnclaimedNeed(bid, matType, need, delivered, excludeUid)
    local claimed = 0
    for otherUid, st in pairs(aiState) do
        if otherUid ~= excludeUid and st.deliveryClaim
           and st.deliveryClaim.bid == bid then
            if unit.getInfo(otherUid) then    -- skip stale claims
                claimed = claimed + (st.deliveryClaim.materials[matType] or 0)
            end
        end
    end
    return math.max(0, need - (delivered[matType] or 0) - claimed)
end

local function findDeliveryTarget(uid, fromX, fromY, params)
    -- Active-world buildings only — building.list() is global across every
    -- live world page, so off-world buildings used to leak in as delivery
    -- targets (#197). building.getActiveIds() is page-scoped, matching the
    -- active-world unit.getAllIds the actors come from.
    local ids = building.getActiveIds()
    if not ids or #ids == 0 then return nil end
    local best, bestD = nil, params.deliver_scan_range
    for _, bid in ipairs(ids) do
        if bid and building.getActivity(bid) == "appearing"
           and not building.areMaterialsSatisfied(bid) then
            local need      = building.getMaterialNeed(bid) or {}
            local delivered = building.getMaterialDelivered(bid) or {}
            -- My potential contribution per material: own inventory
            -- first, then the technomule's stock for the shortfall.
            -- (Concurrent claimants can both plan on the same mule
            -- stock — the engine transfer fails gracefully for the
            -- loser and the next utility pass re-claims what's still
            -- needed, so the race resolves itself.)
            local mule = findTechnomule(fromX, fromY)
            local claim, fromMule = {}, {}
            local anyClaim, anyFromMule = false, false
            for matType, count in pairs(need) do
                local remaining = remainingUnclaimedNeed(bid, matType,
                                       count, delivered, uid)
                if remaining > 0 then
                    local have = inventoryCountOf(uid, matType)
                    local muleHave = mule
                        and inventoryCountOf(mule.uid, matType) or 0
                    local bring = math.min(have + muleHave, remaining)
                    if bring > 0 then
                        claim[matType] = bring
                        anyClaim = true
                        if bring > have then
                            fromMule[matType] = bring - have
                            anyFromMule = true
                        end
                    end
                end
            end
            if anyClaim then
                local info = building.getInfo(bid)
                if info then
                    local tw = info.tileW or 1
                    local th = info.tileH or 1
                    local cx = info.gridX + tw / 2
                    local cy = info.gridY + th / 2
                    local d  = distance(fromX, fromY, cx, cy)
                    if d <= bestD then
                        best = {
                            bid = bid, gridX = info.gridX, gridY = info.gridY,
                            tileW = tw, tileH = th, distance = d,
                            claim = claim,
                            fromMule = anyFromMule and fromMule or nil,
                        }
                        bestD = d
                    end
                end
            end
        end
    end
    return best
end

local function deliverUtility(uid, s, params)
    -- Locked in once claim is set. The lock survives across ticks so
    -- the walk-and-deliver sequence isn't yanked by ambient utility.
    -- Finite (see header comment) so dire drink/eat still preempt.
    if s.deliveryClaim then return 6.0 end

    local info = unit.getInfo(uid)
    if not info then return -math.huge end

    local target = findDeliveryTarget(uid, info.gridX, info.gridY, params)
    if not target then return -math.huge end

    s.deliveryPendingTarget = target
    return params.deliver_utility
end

-- Fetch phase of a delivery: walk to the technomule and take the
-- claimed shortfall. Returns true while still busy (walking /
-- transferring), false once the fetch list is empty (or abandoned —
-- mule gone, stock gone, or capacity hit; delivery proceeds with
-- whatever is actually in inventory, transferItemToBuilding breaks
-- gracefully on the rest).
local function deliverFetchFromMule(uid, s, info, params)
    local claim = s.deliveryClaim
    if not claim.fromMule or not next(claim.fromMule) then
        return false
    end

    local mule = findTechnomule(info.gridX, info.gridY)
    if not mule then
        claim.fromMule = nil
        return false
    end

    if distance(info.gridX, info.gridY, mule.gridX, mule.gridY)
       > params.mule_fetch_arrival then
        unit.moveTo(uid, mule.gridX, mule.gridY, mv.comfort(uid))  -- hauling → comfort
        return true
    end

    unit.stop(uid)
    local carried = unit.getCarryingWeight(uid) or 0
    local maxW    = unit.getStat(uid, "carrying_capacity") or math.huge
    for matType, count in pairs(claim.fromMule) do
        local taken = 0
        for _ = 1, count do
            local w = deliverItemWeight(matType)
            if carried + w > maxW then
                engine.logWarn("deliver: unit " .. tostring(uid)
                    .. " at capacity (" .. string.format("%.1f", carried + w)
                    .. " > " .. string.format("%.1f", maxW)
                    .. " kg) — leaving rest of " .. matType .. " on mule")
                break
            end
            if not unit.transferItemToUnit(mule.uid, uid, matType) then
                break    -- mule stock ran out (raced another claimant)
            end
            carried = carried + w
            taken = taken + 1
        end
        claim.fromMule[matType] = nil
        -- Whatever we couldn't take, we can't deliver — shrink the
        -- claim so other acolytes' remaining-need math frees up the
        -- difference immediately.
        local shortfall = count - taken
        if shortfall > 0 and claim.materials[matType] then
            local kept = claim.materials[matType] - shortfall
            claim.materials[matType] = kept > 0 and kept or nil
        end
    end
    claim.fromMule = nil
    return false
end

local function deliverExecute(uid, s, params)
    -- Lock in claim on first call so subsequent ticks (and other
    -- acolytes' utility checks) see the reservation.
    if not s.deliveryClaim then
        local target = s.deliveryPendingTarget
        if not target then return end
        s.deliveryClaim = { bid = target.bid, materials = target.claim,
                            fromMule = target.fromMule }
        s.deliveryPendingTarget = nil
    end

    local info = unit.getInfo(uid)
    if not info then return end

    -- Source the shortfall from the technomule before heading to the
    -- build site (own inventory first, then the mule — by design).
    if deliverFetchFromMule(uid, s, info, params) then return end

    -- Fetch may have emptied the claim entirely (mule gone / raced).
    if not next(s.deliveryClaim.materials) then
        s.deliveryClaim = nil
        return
    end

    local binfo = building.getInfo(s.deliveryClaim.bid)
    if not binfo then
        -- Building destroyed between claim and arrival. Release.
        s.deliveryClaim = nil
        return
    end

    local utx  = math.floor(info.gridX)
    local uty  = math.floor(info.gridY)
    local cheb = chebToFootprint(utx, uty, binfo.gridX, binfo.gridY,
                                 binfo.tileW or 1, binfo.tileH or 1)

    if cheb > 1 then
        -- Walk to the nearest border tile. Same approach as build_nearby.
        local bestX, bestY, bestD = nil, nil, math.huge
        local tw = binfo.tileW or 1
        local th = binfo.tileH or 1
        for dx = -1, tw do
            for dy = -1, th do
                if dx == -1 or dx == tw or dy == -1 or dy == th then
                    local nx = binfo.gridX + dx + 0.5
                    local ny = binfo.gridY + dy + 0.5
                    local d  = distance(info.gridX, info.gridY, nx, ny)
                    if d < bestD then
                        bestX, bestY, bestD = nx, ny, d
                    end
                end
            end
        end
        if bestX then
            unit.moveTo(uid, bestX, bestY, mv.comfort(uid))  -- hauling → comfort
        end
        return
    end

    -- Arrived. Hand over each claimed material, one ItemInstance at
    -- a time so per-item state (condition on motors/processors) is
    -- preserved through to the building's biMaterialsDelivered list.
    for matType, count in pairs(s.deliveryClaim.materials) do
        for _ = 1, count do
            if not unit.transferItemToBuilding(uid, s.deliveryClaim.bid,
                                               matType) then
                break    -- inventory ran out or building gone
            end
        end
    end

    s.deliveryClaim = nil
end

-----------------------------------------------------------
-- Action: store_materials
--
-- Auto-deposit excess Materials-category items into the nearest
-- built cargo. Utility scales with how full the unit is — a barely-
-- loaded acolyte holds onto their plates, a 90 %-full one walks
-- them over to the cargo. Only Materials category items go in;
-- food, weapons, and worn gear stay on the unit.
--
-- State on s:
--   storeTarget  = bid (cached during the walk so utility re-scans
--                  cheaply on subsequent ticks)
-----------------------------------------------------------

-- Count items in the inventory whose def category is "Materials".
local function countMaterialsInInventory(uid)
    local inv = unit.getInventory(uid)
    if not inv then return 0 end
    local n = 0
    for _, it in ipairs(inv) do
        if it.category == "Materials" then n = n + 1 end
    end
    return n
end

-- Nearest built building with non-zero storage capacity AND room for
-- at least one more item (current weight < capacity). Returns
-- {bid, gridX, gridY, tileW, tileH, distance} or nil.
local function findStorageTarget(fromX, fromY, maxRange)
    -- Active-world buildings only (#197); see findDeliveryTarget.
    local ids = building.getActiveIds()
    if not ids or #ids == 0 then return nil end
    local best, bestD = nil, maxRange
    for _, bid in ipairs(ids) do
        if bid and building.getActivity(bid) == "built" then
            local cap = building.getStorageCapacity(bid)
            local used = building.getStorageWeight(bid) or 0
            if cap and cap > 0 and used < cap then
                local info = building.getInfo(bid)
                if info then
                    local tw = info.tileW or 1
                    local th = info.tileH or 1
                    local cx = info.gridX + tw / 2
                    local cy = info.gridY + th / 2
                    local d = distance(fromX, fromY, cx, cy)
                    if d <= bestD then
                        best = {
                            bid = bid, gridX = info.gridX, gridY = info.gridY,
                            tileW = tw, tileH = th, distance = d,
                        }
                        bestD = d
                    end
                end
            end
        end
    end
    return best
end

local function storeMaterialsUtility(uid, s, params)
    if countMaterialsInInventory(uid) <= 0 then return -math.huge end

    local info = unit.getInfo(uid)
    if not info then return -math.huge end

    local target = findStorageTarget(info.gridX, info.gridY,
                                     params.store_scan_range)
    if not target then return -math.huge end

    -- Fill fraction. carrying_capacity is a derived stat (body-mass
    -- driven); guard against zero in case eager-stats hasn't fired.
    local carried = unit.getCarryingWeight(uid) or 0
    local maxW = unit.getStat(uid, "carrying_capacity")
    if not maxW or maxW <= 0 then return -math.huge end
    local fill = math.min(1.0, carried / maxW)

    s.storeTarget = target.bid
    return params.store_base_utility * fill * fill * fill
end

local function storeMaterialsExecute(uid, s, params)
    local info = unit.getInfo(uid)
    if not info then return end

    -- Re-resolve target each tick we execute; cheap and self-heals
    -- if the cached cargo got destroyed or filled up.
    local target = findStorageTarget(info.gridX, info.gridY,
                                     params.store_scan_range)
    if not target then return end
    s.storeTarget = target.bid

    local utx  = math.floor(info.gridX)
    local uty  = math.floor(info.gridY)
    local cheb = chebToFootprint(utx, uty, target.gridX, target.gridY,
                                 target.tileW, target.tileH)

    if cheb > 1 then
        local bestX, bestY, bestD = nil, nil, math.huge
        for dx = -1, target.tileW do
            for dy = -1, target.tileH do
                if dx == -1 or dx == target.tileW
                   or dy == -1 or dy == target.tileH then
                    local nx = target.gridX + dx + 0.5
                    local ny = target.gridY + dy + 0.5
                    local d  = distance(info.gridX, info.gridY, nx, ny)
                    if d < bestD then
                        bestX, bestY, bestD = nx, ny, d
                    end
                end
            end
        end
        if bestX then
            unit.moveTo(uid, bestX, bestY, mv.comfort(uid))  -- hauling → comfort
        end
        return
    end

    -- Arrived. Deposit every Materials item in inventory one at a
    -- time. Stop on the first reject (cargo full) — next tick the
    -- utility may pick a different target if one has room.
    local inv = unit.getInventory(uid) or {}
    -- Build a unique list of material defNames (each one might
    -- have multiple instances; the API pops the first matching).
    local seen = {}
    local mats = {}
    for _, it in ipairs(inv) do
        if it.category == "Materials" and not seen[it.defName] then
            seen[it.defName] = true
            mats[#mats + 1] = it.defName
        end
    end
    for _, defName in ipairs(mats) do
        while true do
            local ok = unit.depositToCargo(uid, target.bid, defName)
            if not ok then break end
        end
    end

    s.storeTarget = nil
end

-----------------------------------------------------------
-- Action: build_nearby
--
-- Fires when an under-construction building (Appearing activity,
-- bdBuildWork > 0) is within build_scan_range. Once adjacent
-- (Chebyshev ≤ 1) the unit stands still; the construction tick in
-- scripts/building_spawn.lua counts adjacent workers and applies
-- the rate formula to biBuildProgress per real second.
--
-- Utility excludes the asking unit from the worker count so the
-- value reflects "is it worth ME joining". With saturation_n = 5
-- the 5th would-be joiner sees util = 0 and prefers wander.
-----------------------------------------------------------

-- Returns {bid, gridX, gridY, tileW, tileH, distance} for the
-- nearest Appearing building with bdBuildWork > 0 within maxRange,
-- or nil. Uses building.getActiveIds() / getActivity / getBuildRequired /
-- getInfo — all cheap on a small N of placed buildings.
local function findNearestUnbuilt(fromX, fromY, maxRange)
    -- Active-world buildings only (#197); see findDeliveryTarget.
    local ids = building.getActiveIds()
    if not ids or #ids == 0 then return nil end
    local best, bestD = nil, maxRange
    for _, bid in ipairs(ids) do
        if bid then
            local activity = building.getActivity(bid)
            local required = building.getBuildRequired(bid)
            if activity == "appearing" and required and required > 0 then
                local info = building.getInfo(bid)
                if info and info.gridX and info.gridY then
                    local tw = info.tileW or 1
                    local th = info.tileH or 1
                    -- Building center for distance ranking.
                    local cx = info.gridX + tw / 2
                    local cy = info.gridY + th / 2
                    local d = distance(fromX, fromY, cx, cy)
                    if d <= bestD then
                        best = {
                            bid    = bid,
                            gridX  = info.gridX,
                            gridY  = info.gridY,
                            tileW  = tw,
                            tileH  = th,
                            distance = d,
                        }
                        bestD = d
                    end
                end
            end
        end
    end
    return best
end

-- Acolytes currently in build_nearby targeting this bid, excluding
-- excludeUid (so utility can ask "what'd the count be if I joined").
local function countBuildersAt(bid, excludeUid)
    local count = 0
    local ids = unit.getAllIds() or {}
    for _, uid in ipairs(ids) do
        if uid ~= excludeUid then
            local s = aiState[uid]
            if s and s.currentAction == "build_nearby"
               and s.buildTarget == bid then
                count = count + 1
            end
        end
    end
    return count
end

local function buildNearbyUtility(uid, s, params)
    local info = unit.getInfo(uid)
    if not info then return -math.huge end
    local target = findNearestUnbuilt(info.gridX, info.gridY,
                                      params.build_scan_range)
    if not target then return -math.huge end

    -- Cache for execute. (Execute re-resolves; this just lets
    -- countBuildersAt elsewhere see who's targeting what.)
    s.buildTarget = target.bid

    local workers    = countBuildersAt(target.bid, uid)
    local saturation = params.build_saturation_n
    local fill       = math.max(0, 1.0 - workers / saturation)
    if fill <= 0 then return -math.huge end

    -- Linear distance falloff: 1.0 within arrival range → 0 at
    -- scan range. Anything closer than build_arrival_tiles counts
    -- as adjacent for the purposes of the utility curve.
    local d = target.distance
    local distFactor
    if d <= params.build_arrival_tiles then
        distFactor = 1.0
    else
        local span = params.build_scan_range - params.build_arrival_tiles
        distFactor = math.max(0, 1.0 - (d - params.build_arrival_tiles) / span)
    end

    return params.build_base_utility * fill * distFactor
end

local function buildNearbyExecute(uid, s, params)
    local info = unit.getInfo(uid)
    if not info then return end

    local target = findNearestUnbuilt(info.gridX, info.gridY,
                                      params.build_scan_range)
    if not target then return end
    s.buildTarget = target.bid

    local utx  = math.floor(info.gridX)
    local uty  = math.floor(info.gridY)
    local cheb = chebToFootprint(utx, uty,
                                 target.gridX, target.gridY,
                                 target.tileW, target.tileH)

    if cheb <= 1 then
        -- Adjacent (or standing on) — stop and let the construction
        -- tick count us. No "facing the building" logic yet; can
        -- layer that on later when there's a build animation.
        unit.stop(uid)
        return
    end

    -- Walk toward the nearest border tile of the footprint. For a
    -- 1×1 footprint that's the 8 surrounding tiles; for larger
    -- footprints it's the ring around. Pick the one closest to us.
    local bestX, bestY, bestD = nil, nil, math.huge
    for dx = -1, target.tileW do
        for dy = -1, target.tileH do
            -- Skip interior cells; only border counts as approach.
            if dx == -1 or dx == target.tileW
               or dy == -1 or dy == target.tileH then
                local nx = target.gridX + dx + 0.5
                local ny = target.gridY + dy + 0.5
                local d  = distance(info.gridX, info.gridY, nx, ny)
                if d < bestD then
                    bestX, bestY, bestD = nx, ny, d
                end
            end
        end
    end
    if bestX then
        unit.moveTo(uid, bestX, bestY, mv.comfort(uid))  -- going to build site → comfort
    end
end

-- Bear-specific candidates live in scripts/bear_ai.lua. Future
-- wildlife scripts (panda_ai, polar_bear_ai, …) plug in the same
-- way via unitAi.registerActions / unitAi.setConfig + their own
-- helpers. See `require("scripts.bear_ai")` at the bottom of this
-- file for the load-time registration.

-----------------------------------------------------------
-- Action registry per unit type
-----------------------------------------------------------
-- Per-species action lists. Populated below via registerActions so
-- the universal combat candidates (retreat / engage / attack_target)
-- are prepended uniformly — species lists only declare their ambient
-- actions.
-----------------------------------------------------------
-- Action: dig_designation
--
-- "If it has nothing better to do": claim the nearest designated
-- tile, walk to its nearest corner, pull out the best tool for the
-- material, and dig. Corner progress drains digger-side first
-- world-side (world.digTile), so the tile slopes toward the unit
-- until it drops a z-level and the designation completes.
--
-- Claims live in a module-local table keyed by tile so two acolytes
-- never work the same tile (same in-flight-claim shape as build
-- sites); claims expire on timeout or when the claimant dies.
-----------------------------------------------------------
local digClaims = {}   -- "x,y" → { uid = ..., at = gameTime }

local function digKey(x, y) return x .. "," .. y end

local function digClaimedByOther(key, uid, now, timeout)
    local c = digClaims[key]
    if not c or c.uid == uid then return false end
    if now - c.at > timeout or not unit.exists(c.uid) then
        digClaims[key] = nil
        return false
    end
    return true
end

-- Best carried tool for a material: returns toolName, speed (0 when
-- the unit has no digging tool — no tool, no dig).
local function bestDigTool(uid, params, pickSpeed, shovelSpeed)
    local inv = unit.getInventory(uid)
    if not inv then return nil, 0 end
    local hasShovel, hasPick = false, false
    for _, it in ipairs(inv) do
        if params.dig_tools.shovel.defs[it.defName] then hasShovel = true end
        if params.dig_tools.pick.defs[it.defName]   then hasPick   = true end
    end
    local tool, speed = nil, 0
    if hasShovel and (shovelSpeed or 0) > speed then
        tool, speed = "shovel", shovelSpeed
    end
    if hasPick and (pickSpeed or 0) > speed then
        tool, speed = "pick", pickSpeed
    end
    return tool, speed
end

local function releaseDigJob(s, uid)
    if s.digJob then
        local key = digKey(s.digJob.x, s.digJob.y)
        local c = digClaims[key]
        if c and c.uid == uid then digClaims[key] = nil end
    end
    s.digJob = nil
    s.digPhase = nil
end

-- The designation vanished while we held the job — the tile
-- completed (or was undesignated, rare). BOTH the utility check and
-- the execute loop can be first to notice, depending on tick
-- ordering, so completion lives in one helper: XP if we were
-- actually working it, drop the tool visual, release the claim.
local function digComplete(uid, s, params)
    if s.digPhase == "digging" or s.digPhase == "equipping" then
        unit.addXP(uid, "mining", params.dig_xp_per_tile or 0)
    end
    unit.clearAnimOverride(uid)
    releaseDigJob(s, uid)
end

local function digUtility(uid, s, params)
    local wid = world.getActiveWorldId()
    if not wid then return -math.huge end

    -- Active job: finite lock-in so dire needs (thirst, combat)
    -- still preempt. Released the moment the tile completes — this
    -- check runs BEFORE execute each tick, so completion is usually
    -- detected here (digComplete grants the XP + clears visuals).
    if s.digJob then
        local z = world.getMineDesignationAt(wid, s.digJob.x, s.digJob.y)
        if z then return params.dig_lock_utility end
        digComplete(uid, s, params)
    end

    local info = unit.getInfo(uid)
    if not info then return -math.huge end
    local gx, gy, dist =
        world.nearestMineDesignation(wid, info.gridX, info.gridY)
    if not gx then return -math.huge end
    if dist > params.dig_scan_range then return -math.huge end

    local now = engine.gameTime()
    if digClaimedByOther(digKey(gx, gy), uid, now,
                         params.dig_claim_timeout) then
        return -math.huge
    end

    local _, pickSpeed, shovelSpeed, spoilBlocked =
        world.getDigInfoAt(wid, gx, gy)
    if not pickSpeed then return -math.huge end   -- chunk not loaded
    -- No room for the spoil around this tile (boxed in by water,
    -- cliffs, or other designations): the engine would refuse every
    -- dig tick, so don't take the job at all.
    if spoilBlocked then return -math.huge end
    local tool, speed = bestDigTool(uid, params, pickSpeed, shovelSpeed)
    if not tool or speed <= 0 then return -math.huge end

    -- Stash the scored candidate so execute doesn't re-scan.
    s.digCandidate = { x = gx, y = gy, tool = tool }

    local distFactor = math.max(0, 1 - dist / params.dig_scan_range)
    return params.dig_base_utility * math.min(speed, 1.0) * distFactor
end

-- Corner geometry (grid space, matches mdCorners order NW,NE,SE,SW):
-- corner i of tile (x, y) sits at (x, y) + DIG_CORNERS[i].
local DIG_CORNERS = { {0, 0}, {1, 0}, {1, 1}, {0, 1} }

local function digCornerPos(job, i)
    return job.x + DIG_CORNERS[i][1], job.y + DIG_CORNERS[i][2]
end

-- Stand position for corner i: a quarter tile OUTSIDE the corner,
-- diagonal from the tile center — beside the excavation, never on it.
-- The special corner "top" stands on the tile itself (used when no
-- corner is reachable at the dig level).
local function digStandPos(job, i)
    if i == "top" then
        return job.x + 0.5, job.y + 0.5
    end
    local cx, cy = digCornerPos(job, i)
    local ox, oy = cx - (job.x + 0.5), cy - (job.y + 0.5)
    return cx + ox * 0.5, cy + oy * 0.5
end

-- A corner is reachable when the tile the digger would stand on
-- (just outside the corner) is dry ground at the SAME z as the dig
-- tile. No digging across z levels: a corner whose stand tile is a
-- cliff above (or a hole / water below) is off limits.
local function digCornerReachable(job, i)
    local sx, sy = digStandPos(job, i)
    local tx, ty = math.floor(sx), math.floor(sy)
    if world.getFluidAt(tx, ty) then return false end
    return world.getSurfaceAt(tx, ty) == job.z
end

-- Nearest reachable corner with material left (corners is
-- {c1,c2,c3,c4}). Falls back to "top" — stand on the designated tile
-- itself — when material remains but no corner can be reached at the
-- dig level. nil = nothing left to dig.
local function digNextCorner(job, corners, px, py)
    local best, bi = math.huge, nil
    local anyMaterial = false
    for i = 1, 4 do
        if corners[i] > 0 then
            anyMaterial = true
            if digCornerReachable(job, i) then
                local cx, cy = digCornerPos(job, i)
                local d = (cx - px) ^ 2 + (cy - py) ^ 2
                if d < best then best, bi = d, i end
            end
        end
    end
    if bi then return bi end
    if anyMaterial then return "top" end
    return nil
end

local function digExecute(uid, s, params)
    local wid = world.getActiveWorldId()
    if not wid then return end
    local info = unit.getInfo(uid)
    if not info then return end
    local now = engine.gameTime()

    -- Claim a fresh job and head for its nearest undug corner.
    if not s.digJob then
        local cand = s.digCandidate
        if not cand then return end
        local key = digKey(cand.x, cand.y)
        if digClaimedByOther(key, uid, now, params.dig_claim_timeout) then
            return
        end
        local z, c1, c2, c3, c4 =
            world.getMineDesignationAt(wid, cand.x, cand.y)
        if not z then return end
        cand.z = z
        local ci = digNextCorner(cand, { c1, c2, c3, c4 },
                                 info.gridX, info.gridY)
        if not ci then return end
        digClaims[key] = { uid = uid, at = now }
        s.digCandidate = nil
        s.digJob = cand
        s.digEquipped = false
        s.digPhase = "walking"
        cand.corner = ci
        local sx, sy = digStandPos(cand, ci)
        unit.moveTo(uid, sx, sy, mv.comfort(uid))  -- approaching dig site → comfort
        return
    end

    local job = s.digJob
    -- Keep the claim fresh while we hold the job.
    digClaims[digKey(job.x, job.y)] = { uid = uid, at = now }
    local toolCfg = params.dig_tools[job.tool]

    if s.digPhase == "walking" then
        local sx, sy = digStandPos(job, job.corner)
        local d = distance(info.gridX, info.gridY, sx, sy)
        if d <= params.dig_arrival_tiles then
            unit.stop(uid)
            if not s.digEquipped then
                -- First corner of this job: pull the tool out.
                -- setAnimOverride wins over the engine's state-driven
                -- anim resolution (plain setAnim gets clobbered every
                -- sim tick by publishToRender).
                unit.setAnimOverride(uid, toolCfg.equip_anim)
                s.digPhase = "equipping"
                s.digEquipUntil = now + params.dig_equip_seconds
            else
                unit.setAnimOverride(uid, toolCfg.work_anim)
                s.digPhase = "digging"
                s.lastDigAt = now
            end
        else
            -- Execute only fires when idle, so this re-issue means
            -- the previous walk arrived short or failed.
            unit.moveTo(uid, sx, sy, mv.comfort(uid))  -- approaching dig site → comfort
        end
        return
    end

    if s.digPhase == "equipping" then
        if now >= (s.digEquipUntil or 0) then
            s.digEquipped = true
            unit.setAnimOverride(uid, toolCfg.work_anim)
            s.digPhase = "digging"
            s.lastDigAt = now
        end
        return
    end

    if s.digPhase == "digging" then
        local z, c1, c2, c3, c4 =
            world.getMineDesignationAt(wid, job.x, job.y)
        if not z then
            -- Tile completed (or undesignated out from under us).
            digComplete(uid, s, params)
            return
        end
        local corners = { c1, c2, c3, c4 }
        -- Current corner exhausted → walk around the tile to the
        -- next reachable one (DF-style: the unit digs each corner
        -- from beside it, and the slope follows it around). From
        -- "top" we stay put — everything is in reach from up there.
        if job.corner ~= "top" and corners[job.corner] <= 0 then
            local ci = digNextCorner(job, corners,
                                     info.gridX, info.gridY)
            if ci and ci ~= job.corner then
                job.corner = ci
                unit.clearAnimOverride(uid)
                s.digPhase = "walking"
                local sx, sy = digStandPos(job, ci)
                unit.moveTo(uid, sx, sy, mv.comfort(uid))  -- approaching dig site → comfort
                return
            end
            -- No other corner left: the residue finishes from here.
        end
        -- Material can change as digging exposes new strata; re-read
        -- the speed each swing. spoilBlocked can flip mid-dig too
        -- (other diggers filled the surrounding piles) — abandon the
        -- job rather than swing at a refusing engine forever.
        local _, pickSpeed, shovelSpeed, spoilBlocked =
            world.getDigInfoAt(wid, job.x, job.y)
        local speed = (job.tool == "pick") and (pickSpeed or 0)
                                            or (shovelSpeed or 0)
        if speed <= 0 or spoilBlocked then
            unit.clearAnimOverride(uid)
            releaseDigJob(s, uid)
            return
        end
        -- Idempotent: re-asserts the work anim after preemption.
        unit.setAnimOverride(uid, toolCfg.work_anim)
        local dt = math.min(now - (s.lastDigAt or now), 2.0)
        s.lastDigAt = now
        -- Personal factor: muscle moves material, technique wastes
        -- less of the swing. strength ~1.0 baseline; mining skill 50
        -- = 1.0×, 0 = 0.5×, 100 = 1.5×.
        local strength = unit.getStat(uid, "strength") or 1.0
        local mining   = unit.getSkill(uid, "mining") or 0.0
        local unitFactor = strength * (0.5 + mining / 100.0)
        -- Mining skill rides along: the engine's chunk-yield
        -- accumulator scales by the CURRENT digger's skill, so a
        -- handoff mid-dig switches to the new digger's rate.
        -- Perception scales the gem-find roll when the tile
        -- completes — sharp-eyed miners spot more glints.
        local percep = unit.getStat(uid, "perception") or 1.0
        world.digTile(wid, job.x, job.y, info.gridX, info.gridY,
                      params.dig_rate * speed * unitFactor * dt,
                      mining, percep)
        return
    end
end

-- Called by the dispatch loop when another action preempts an active
-- dig (thirst, combat, player order). Drops the tool VISUAL only —
-- the claim and job survive so the dig resumes afterwards, re-entered
-- through the walking phase.
local function digOnExit(uid, s, params)
    unit.clearAnimOverride(uid)
    if s.digPhase == "digging" or s.digPhase == "equipping" then
        s.digPhase = "walking"
    end
end

-----------------------------------------------------------
-- Action: pickup_ground
--
-- Player-ordered pickup of a ground item (right-click → Pick up;
-- unitAi.commandPickup). Path to the item, then atomically move it
-- into the inventory (item.pickupGround preserves the instance) with
-- the engine pickup animation. If the unit is over carrying capacity
-- when it ARRIVES (it can change en route), refuse and log.
-----------------------------------------------------------
local function pickupGroundEntry(gid)
    for _, g in ipairs(item.listGround() or {}) do
        if g.id == gid then return g end
    end
    return nil
end

local function pickupItemWeight(defName)
    for _, d in ipairs(item.listDefs() or {}) do
        if d.name == defName then return d.weight or 0 end
    end
    return 0
end

local function pickupUtility(uid, s, params)
    local order = s.pickupOrder
    if not order then return -math.huge end
    if not pickupGroundEntry(order.gid) then
        -- Item gone (someone else took it / already collected) — normal,
        -- not a failure.
        s.pickupOrder = nil
        return -math.huge
    end
    if engine.gameTime() - order.issuedAt > (params.pickup_timeout or 30) then
        -- Timed out trying to reach a still-present item: a real failure.
        reportFailure(uid, "Couldn't reach item to pick up")
        s.pickupOrder = nil
        return -math.huge
    end
    return params.pickup_utility
end

local function pickupExecute(uid, s, params)
    local order = s.pickupOrder
    if not order then return end
    local g = pickupGroundEntry(order.gid)
    local info = unit.getInfo(uid)
    if not g or not info then
        s.pickupOrder = nil
        return
    end

    local d = distance(info.gridX, info.gridY, g.x, g.y)
    if d > params.pickup_arrival_tiles then
        unit.moveTo(uid, g.x, g.y, mv.comfort(uid))  -- going to pick up → comfort
        return
    end

    unit.stop(uid)
    -- Capacity check at the moment of truth ("walk, then refuse").
    -- Fill counts: a full canteen on the ground weighs its contents
    -- too (1 L = 1 kg, matching getCarryingWeight).
    local carried = unit.getCarryingWeight(uid) or 0
    local maxW    = unit.getStat(uid, "carrying_capacity") or math.huge
    -- listGround's weight is the INSTANCE weight incl. fill (gems
    -- vary per find); def-mean + fill is the fallback.
    local w       = g.weight or (pickupItemWeight(g.defName) + (g.fill or 0))
    if carried + w > maxW then
        engine.logWarn("pickup_ground: unit " .. tostring(uid)
            .. " over capacity (" .. string.format("%.1f", carried + w)
            .. " > " .. string.format("%.1f", maxW)
            .. " kg) — leaving " .. g.defName)
        s.pickupOrder = nil
        return
    end

    -- Engine pickup animation + the atomic ground→inventory move.
    unit.pickup(uid)
    item.pickupGround(uid, order.gid)
    s.pickupOrder = nil
end

function unitAi.commandPickup(uid, gid)
    local s = ensureState(uid)
    s.pickupOrder = { gid = gid, issuedAt = engine.gameTime() }
    s.nextActionAt = 0
end

-----------------------------------------------------------
-- Action: treat_ally  (Phase D — medic auto-treat)
--
-- A unit that KNOWS bleed-control bandages a bleeding ally. Capability
-- = bleed_control knowledge × intelligence (the same product the treat
-- action and the Knowledge-tab tooltip use). The squad self-organises:
--   * the BEST available medic claims a patient and rushes;
--   * a LESSER medic only steps in when the best is tied up in combat
--     AND nobody else has already claimed that patient.
-- Flow mirrors deliver_to_build_site: claim → fetch the first-aid kit
-- off the technomule → carry it to the patient → unit.treatBleeding
-- (drawing from the kit now in the medic's own inventory), repeating
-- until the patient stops bleeding or the kit runs dry.
--
-- State on s:
--   treatClaim   = { patient = uid }   -- lock-in, visible to others
--   treatPending = patient table       -- utility → execute handoff
--
-- Non-external kinds (concussion / fracture / internal) aren't
-- bandageable, so they don't make a unit a patient.
-----------------------------------------------------------

local TREAT_SKIP_KINDS = {
    concussion = true, fracture = true, internal = true,
}

-- Medic capability: bleed-control knowledge × intelligence. 0 = the
-- unit doesn't know how (or is too dim to apply it).
local function medicCapability(uid)
    local lvl = unit.getKnowledge(uid, "bleed_control")
    if not lvl or lvl <= 0 then return 0 end
    return lvl * (unit.getStat(uid, "intelligence") or 1.0)
end

-- A conscious, living unit can administer aid; a collapsed or dead one
-- can't (and isn't counted as a candidate medic).
local function canActAsMedic(uid)
    local pose = unit.getPose(uid)
    return pose ~= nil and pose ~= "dead" and pose ~= "collapsed"
end

-- Does this unit have a wound worth dressing? — an external bleeder
-- still seeping above the "good enough" threshold AND not already
-- mostly self-clotted (a wound that's clotting on its own doesn't need
-- a bandage wasted on it).
local CLOT_ENOUGH = 0.85
-- An infected wound past this level wants antibiotics (the cure). Applies
-- to ANY wound kind (even the skip-kinds: a closed fracture can still
-- fester), so it's checked outside the bleeder gate.
local INFECT_TREAT_MIN = 0.15
local function needsTreatment(uid, minSeep)
    for _, w in ipairs(unit.getWounds(uid) or {}) do
        if not TREAT_SKIP_KINDS[w.kind] and (w.bandage or 1) > minSeep
           and (w.clot or 0) < CLOT_ENOUGH then
            return true
        end
        if (w.infection or 0) >= INFECT_TREAT_MIN then
            return true   -- needs antibiotics
        end
    end
    return false
end

-- Does the patient have an infected wound worth antibiotics?
local function hasInfection(uid)
    for _, w in ipairs(unit.getWounds(uid) or {}) do
        if (w.infection or 0) >= INFECT_TREAT_MIN then return true end
    end
    return false
end

-- A medic treats its own squad: same faction (a "debug" medic — staged
-- in the debug overlay — also treats player units so test fights can be
-- patched up).
local function isAlly(uid, medicFaction)
    local f = unit.getFaction(uid)
    return f == medicFaction
        or (medicFaction == "debug" and f == "player")
        or (medicFaction == "player" and f == "debug")
end

-- A best-medic who's fighting can't break off — that's what frees a
-- lesser medic to step in.
local function medicBusyInCombat(uid)
    local st = uid and aiState[uid]
    local act = st and st.currentAction
    return act == "retreat" or act == "engage" or act == "attack_target"
end

-- Is `uid` free to take on THIS patient right now? A medic in combat
-- can't break off, and one already committed to a DIFFERENT patient is
-- spoken for — either way it's unavailable, which is what lets a free
-- lesser medic step in. (A medic already claiming THIS patient is still
-- "available" for it — that's the one re-confirming its own claim.)
local function medicAvailable(uid, patientUid)
    if medicBusyInCombat(uid) then return false end
    local st = aiState[uid]
    if st and st.treatClaim and st.treatClaim.patient ~= patientUid then
        return false
    end
    return true
end

-- The best AVAILABLE medic for a patient, scored by capability with a
-- gentle distance discount (a much-nearer competent medic beats a
-- marginally-better distant one, so we don't summon a skilled medic from
-- across the map past a free one standing next to the patient). Excludes
-- the patient itself, the dead/collapsed, NON-allies, medics in combat,
-- and medics already committed to a different patient. Returns the uid,
-- or nil if nobody can help. `params` supplies treat_scan_range.
local function bestMedicFor(patientUid, params)
    local pinfo = unit.getInfo(patientUid)
    local range = (params and params.treat_scan_range) or 60.0
    local bestUid, bestScore = nil, 0
    for _, uid in ipairs(unit.getAllIds() or {}) do
        if uid ~= patientUid and canActAsMedic(uid)
           and isAlly(patientUid, unit.getFaction(uid))
           and medicAvailable(uid, patientUid) then
            local cap = medicCapability(uid)
            if cap > 0 then
                local minfo = unit.getInfo(uid)
                local d = (pinfo and minfo)
                    and distance(pinfo.gridX, pinfo.gridY,
                                 minfo.gridX, minfo.gridY) or 0
                local score = cap * (1 - 0.5 * math.min(1, d / range))
                if score > bestScore then
                    bestUid, bestScore = uid, score
                end
            end
        end
    end
    return bestUid
end

-- Any LIVE, AVAILABLE unit (≠ excludeUid) already claiming this patient?
-- A claimer that's been pulled into combat can't honor its claim while
-- fighting, so it does NOT hold the slot — a free medic must be able to
-- step in (this mirrors medicAvailable/bestMedicFor, which already skip
-- combat-busy medics; without the same skip here the patient would be
-- pinned to the interrupted medic and ignored by everyone else, #306).
-- The claim itself persists (treat_ally is not cleared on preempt, like
-- every other action's locked state) so the fighter resumes this patient
-- once combat ends; if a lesser medic finished it first, treatExecute
-- sees no remaining need and drops the redundant claim.
local function patientClaimed(patientUid, excludeUid)
    for otherUid, st in pairs(aiState) do
        if otherUid ~= excludeUid and st.treatClaim
           and st.treatClaim.patient == patientUid then
            if unit.getInfo(otherUid) and not medicBusyInCombat(otherUid) then
                return true
            end
        end
    end
    return false
end

-- Nearest treatable, currently-unclaimed bleeding ally, or nil.
local function findPatient(uid, info, params)
    local myFaction = unit.getFaction(uid)
    local best, bestD = nil, params.treat_scan_range
    for _, pid in ipairs(unit.getAllIds() or {}) do
        if pid ~= uid and isAlly(pid, myFaction)
           and needsTreatment(pid, params.treat_min_seep)
           and not patientClaimed(pid, uid) then
            local pinfo = unit.getInfo(pid)
            if pinfo and unit.getPose(pid) ~= "dead" then
                local d = distance(info.gridX, info.gridY,
                                   pinfo.gridX, pinfo.gridY)
                if d <= bestD then
                    best = { uid = pid, distance = d }
                    bestD = d
                end
            end
        end
    end
    return best
end

local function treatAllyUtility(uid, s, params)
    -- Locked in once claimed; survives across ticks so the
    -- fetch-and-treat sequence isn't yanked by ambient utility. Finite
    -- so dire survival / combat can still preempt (claim persists).
    if s.treatClaim then return params.treat_lock_utility end

    if medicCapability(uid) <= 0 then return -math.huge end
    if not canActAsMedic(uid) then return -math.huge end
    local info = unit.getInfo(uid)
    if not info then return -math.huge end

    local patient = findPatient(uid, info, params)
    if not patient then return -math.huge end

    -- Squad ranking: only the best AVAILABLE allied medic takes the
    -- patient. bestMedicFor already excludes medics in combat or
    -- committed to another patient (and non-allies), so a free lesser
    -- medic automatically steps in when the best is tied up — and two
    -- bleeding allies get two different medics instead of serialising.
    if bestMedicFor(patient.uid, params) ~= uid then
        return -math.huge
    end

    s.treatPending = patient
    return params.treat_base_utility
end

-- A usable kit the unit already carries (a container holding ≥1
-- bandage): returns its defName, or nil.
local function ownKitDefName(uid)
    for _, it in ipairs(unit.getInventory(uid) or {}) do
        if it.kind == "container" then
            for _, r in ipairs(unit.getItemContents(uid, it.defName) or {}) do
                if r.defName == "bandage" and (r.count or 0) > 0 then
                    return it.defName
                end
            end
        end
    end
    return nil
end

-- Nearest unit carrying a usable kit (the technomule), to fetch from.
local function findKitHolder(fromX, fromY)
    local best, bestD = nil, math.huge
    for _, uid in ipairs(unit.getAllIds() or {}) do
        local kit = ownKitDefName(uid)
        if kit then
            local info = unit.getInfo(uid)
            if info then
                local d = distance(fromX, fromY, info.gridX, info.gridY)
                if d < bestD then
                    best = { uid = uid, gridX = info.gridX,
                             gridY = info.gridY, kit = kit }
                    bestD = d
                end
            end
        end
    end
    return best
end

local function treatExecute(uid, s, params)
    -- Lock in the claim on first call so other medics' utility checks
    -- see the reservation.
    if not s.treatClaim then
        local p = s.treatPending
        if not p then return end
        s.treatClaim   = { patient = p.uid }
        s.treatPending = nil
    end
    local patient = s.treatClaim.patient

    local info = unit.getInfo(uid)
    if not info then s.treatClaim = nil; return end

    -- Patient vanished / died / fully dressed → release.
    if not unit.getInfo(patient) or unit.getPose(patient) == "dead"
       or not needsTreatment(patient, params.treat_min_seep) then
        s.treatClaim = nil
        return
    end

    -- Phase 1: make sure I'm carrying a kit with bandages; if not,
    -- fetch one off the nearest kit-holder (the technomule). (The
    -- no-kit-anywhere fallback — a makeshift tourniquet — is a later
    -- chunk; for now, release so the unit re-evaluates.)
    -- Phase 1: secure supplies. If I'm not carrying a kit, fetch one
    -- off the nearest holder (the technomule). If there's NO kit
    -- anywhere, don't give up — rush to the patient and improvise a
    -- makeshift tourniquet there (the treatBleeding fallback). Better a
    -- crude stopgap than letting them bleed.
    if not ownKitDefName(uid) then
        local holder = findKitHolder(info.gridX, info.gridY)
        if holder then
            if distance(info.gridX, info.gridY, holder.gridX, holder.gridY)
               > params.mule_fetch_arrival then
                unit.moveTo(uid, holder.gridX, holder.gridY, mv.ordered(uid))
                return
            end
            unit.stop(uid)
            unit.transferItemToUnit(holder.uid, uid, holder.kit)
            return   -- re-evaluate next tick now that I hold the kit
        end
        -- no kit reachable → fall through to the patient (tourniquet)
    end

    -- Phase 2: rush to the patient. Target a tile ~1 away (toward me),
    -- not the patient's own tile — a collapsed patient OCCUPIES its
    -- tile, and pathing onto a blocked tile fails outright, leaving the
    -- medic frozen. treat_arrival (1.5) still lets us dress the wound
    -- from the neighbouring tile. (Same "approach the obstacle, don't
    -- stand on it" rule the deliver action uses for build sites.)
    local pinfo = unit.getInfo(patient)
    local d = distance(info.gridX, info.gridY, pinfo.gridX, pinfo.gridY)
    if d > params.treat_arrival then
        local dx, dy = info.gridX - pinfo.gridX, info.gridY - pinfo.gridY
        local len = math.max(0.001, math.sqrt(dx * dx + dy * dy))
        local tx = pinfo.gridX + (dx / len)
        local ty = pinfo.gridY + (dy / len)
        unit.moveTo(uid, tx, ty, mv.sprint(uid))
        return
    end

    -- Phase 3: arrived — dress the worst bleeder. treatBleeding draws
    -- from my own kit (default owner = me). Re-fires on subsequent idle
    -- ticks (lock keeps treat_ally selected) until the patient stops
    -- bleeding or the kit runs dry; a hard failure drops the claim.
    unit.stop(uid)
    local res = unit.treatBleeding(uid, patient)
    if res and not res.ok and res.message ~= "no bleeding wound to treat" then
        -- Surface the failed treatment (red, coalesced per patient). A
        -- patient with only an infected (non-bleeding) wound legitimately
        -- has "no bleeding wound" — that's not a failure, it's the cue to
        -- give antibiotics below, so don't report it.
        reportFailure(patient, "Treatment failed: "
            .. (res.message or "unknown"))
        s.treatClaim = nil
    end
    -- CURE: administer antibiotics to an infected wound (treatBleeding's
    -- antiseptic step only PREVENTS infection on a fresh dressing; an
    -- already-infected wound needs the antibiotics cure). Requires the
    -- INFECTION-CONTROL knowledge; re-fires until the infection is knocked
    -- down or the kit's pills run out.
    if hasInfection(patient) and unit.getKnowledge(uid, "infection_control") then
        local ir = unit.treatInfection(uid, patient)
        if ir and not ir.ok then
            reportFailure(patient, "Infection untreated: "
                .. (ir.message or "unknown"))
        end
    end
end

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
    { name = "retreat",        utility = retreatUtility,
      execute = retreatExecute,
      forceExecute = true },
    { name = "engage",         utility = engageUtility,
      execute = engageExecute },
    { name = "attack_target",  utility = attackTargetUtility,
      execute = attackTargetExecute,
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
unitAi.isGoalActive          = isGoalActive
unitAi.setGoal               = setGoal
unitAi.markGoalAccomplished  = markGoalAccomplished

-- Register acolyte's ambient action list. Combat candidates are
-- prepended by registerActions so the universal-combat invariant
-- holds for acolytes the same way it does for bears.
unitAi.registerActions("acolyte", {
    { name = "idle",           utility = idleUtility,
      execute = idleExecute },
    { name = "wander",         utility = wanderUtility,
      execute = wanderExecute },
    { name = "follow_command", utility = followCommandUtility,
      execute = followCommandExecute },
    { name = "treat_ally", utility = treatAllyUtility,
      execute = treatExecute },
    { name = "drink_from_canteen", utility = drinkUtility,
      execute = drinkExecute },
    { name = "eat_from_inventory", utility = eatUtility,
      execute = eatExecute },
    { name = "refill_canteen", utility = refillUtility,
      execute = refillExecute },
    { name = "search_for_water", utility = searchUtility,
      execute = searchExecute },
    { name = "drink_from_source", utility = drinkFromSourceUtility,
      execute = drinkFromSourceExecute },
    { name = "notify_allies", utility = notifyAlliesUtility,
      execute = notifyAlliesExecute },
    { name = "build_nearby", utility = buildNearbyUtility,
      execute = buildNearbyExecute },
    { name = "deliver_to_build_site", utility = deliverUtility,
      execute = deliverExecute },
    { name = "store_materials", utility = storeMaterialsUtility,
      execute = storeMaterialsExecute },
    { name = "dig_designation", utility = digUtility,
      execute = digExecute, onExit = digOnExit },
    { name = "pickup_ground", utility = pickupUtility,
      execute = pickupExecute },
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
    { name = "idle",           utility = idleUtility,
      execute = idleExecute },
    { name = "wander",         utility = wanderUtility,
      execute = wanderExecute },
    { name = "follow_command", utility = followCommandUtility,
      execute = followCommandExecute },
})

-- Load species satellite scripts. Each one defines its candidates
-- and calls unitAi.registerActions + unitAi.setConfig to plug into
-- the dispatch loop. Done at load time so all defs are wired by
-- the time the first tick runs.
require("scripts.bear_ai")
require("scripts.red_squirrel_ai")

-----------------------------------------------------------
-- FOV-based water memory.
-- Runs every tickOne (10Hz). Always scans regardless of canteen
-- state — without this, a unit with a full canteen who walks past a
-- pond never registers it, then later when thirsty has no memory and
-- can't refill (refill_canteen requires a known source). Cheap to
-- keep running; FOV is small (~40 tiles).
-- Adds every visible water tile to knownWaterSources, dedup'd by
-- WATER_SOURCE_DEDUP_TILES so a long river contributes only a handful
-- of entries instead of dozens. Returns the count of brand-new sources
-- added this tick so callers can react to the discovery moment.
-----------------------------------------------------------
local function scanForWater(uid, s, params)
    local tiles = unit.getVisibleTiles(uid)
    if not tiles then return 0 end
    local added = 0
    for _, t in ipairs(tiles) do
        local fluidType = world.getFluidAt(t.x, t.y)
        if fluidType == "lake" or fluidType == "river" then
            if addWaterSource(s, t.x, t.y) then
                added = added + 1
            end
        end
    end
    return added
end

-----------------------------------------------------------
-- Task arrival / timeout housekeeping
-----------------------------------------------------------
local function maintainTask(uid, s)
    local task = s.commandedTask
    if not task then return end

    local info = unit.getInfo(uid)
    if not info then
        -- Unit gone; drop the task.
        s.commandedTask = nil
        return
    end

    -- Arrival check.
    if distance(info.gridX, info.gridY, task.x, task.y)
       <= TASK_ARRIVAL_TILES then
        s.commandedTask = nil
        return
    end

    -- Timeout check (handles unreachable destinations).
    if engine.gameTime() - task.startedAt > TASK_TIMEOUT_SEC then
        s.commandedTask = nil
    end
end

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

    local s = ensureState(uid)
    seedInitialGoal(s, defName)
    maintainTask(uid, s)

    -- Delirium: a unit whose consciousness has dropped into the delirious band
    -- (heat stroke / hypoxia / salt imbalance — not yet unconscious, which
    -- collapses it) can't act purposefully. It stumbles: aimless slow wander,
    -- no goals/work/combat. Only re-issue when not already moving (no spam).
    if brain.isDelirious(uid) then
        if activity ~= "walking" and activity ~= "running" then
            wanderExecute(uid, s, params)
        end
        s.currentAction = "delirious"
        return
    end

    -- Stuck-walk watchdog. A unit stuck in walking/running with no
    -- position progress never returns to idle, and the execute gate
    -- below (switch-or-idle) then never re-fires its action — it
    -- hangs forever (seen with the water-search spiral walking at an
    -- unpathable waypoint). Force a stop after N seconds without
    -- movement so the AI re-decides from idle. Engine-side root cause
    -- (path stall) tracked separately; this is the safety net.
    do
        local wi = unit.getInfo(uid)
        if wi then
            local moving = (activity == "walking" or activity == "running")
            if moving and s.watchX then
                local moved = (wi.gridX - s.watchX) ^ 2
                            + (wi.gridY - s.watchY) ^ 2
                if moved > 0.01 then
                    s.lastProgressAt = engine.gameTime()
                elseif engine.gameTime() - (s.lastProgressAt or engine.gameTime())
                       > (params.stuck_walk_timeout or 6.0) then
                    engine.logDebug("unitAi: stuck-walk watchdog stopped unit "
                        .. tostring(uid))
                    unit.stop(uid)
                    reportFailure(uid, "Stuck — can't reach destination")
                    s.lastProgressAt = engine.gameTime()
                end
            else
                s.lastProgressAt = engine.gameTime()
            end
            s.watchX, s.watchY = wi.gridX, wi.gridY
        end
    end
    local newSources = scanForWater(uid, s, params)
    -- First-time discovery while pursuing find_water: flip the goal
    -- chain. The next active goal is notify_allies, which fires the
    -- broadcast / walk-notify action defined below. Subsequent finds
    -- (already on notify_allies or past it) just add to the source
    -- list without re-triggering — markGoalAccomplished is idempotent.
    if newSources > 0 and isGoalActive(s, "find_water") then
        markGoalAccomplished(s, "find_water")
        setGoal(s, "notify_allies")
    end

    if engine.gameTime() < s.nextActionAt then return end

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

    scheduleNext(s, params)
end

-----------------------------------------------------------
-- Public: player-issued move command
-----------------------------------------------------------
-- Player code (e.g. right-click handler) calls this instead of
-- `unit.moveTo` so the AI can treat the command as a candidate
-- action and resume it after higher-priority interrupts.
function unitAi.commandMove(uid, tx, ty, speed)
    local s = ensureState(uid)
    s.commandedTask = {
        x         = tx,
        y         = ty,
        speed     = speed,
        startedAt = engine.gameTime(),
    }
    -- Force a fresh decision on the next tick rather than waiting
    -- for the unit's natural cadence — feels more responsive.
    s.nextActionAt = 0
end

-- | Player-issued attack. Sets the unit's active goal to "attack"
--   targeting `targetUid`. The attack_target candidate (combat band,
--   8.0) pathfinds toward the target each tick; when chebyshev distance
--   ≤ unit.getAttackRange, it fires `combat.attack` once and marks
--   the goal accomplished. It out-ranks a pending move order (7.0) so
--   the unit commits to the fight; only dire SELF needs (thirst, hunger
--   scaling past 8) preempt — and they resume once satisfied (#306).
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

-- | "Our" combat strength against `threatUid`. Counts every unit that is
--   EITHER already attacking the threat OR a same-side ally (faction ≠ the
--   threat's) within SWARM_RALLY_RADIUS of it — a POTENTIAL joiner. Used by
--   the retreat candidate so a solo acolyte still sees a bear as futile,
--   while a pack near the threat reads as a winnable swarm and commits
--   together instead of each member fleeing before the swarm forms.
--   (2-faction world for now — player vs wildlife — so "not the threat's
--   faction" = our side; wildlife rallying with wildlife is intended.)
function unitAi.groupEffectivenessVs(threatUid)
    if not unit.exists(threatUid) then return 0 end
    local you = unit.getInfo(threatUid)
    local threatFaction = unit.getFaction(threatUid)
    local total = 0
    for _, uid in ipairs(unit.getAllIds() or {}) do
        local st = aiState[uid]
        local committed = st and st.attackTargetUid == threatUid
        local rallyable = false
        if not committed and you
           and unit.getFaction(uid) ~= threatFaction then
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
    return total
end

-- commandAttack(uid, targetUid [, committed]) — set the attack goal. When
-- `committed` is true (a player or scripted ORDER, vs the AI's own emergent
-- engage), the unit holds far longer before futility breaks it (soft override).
function unitAi.commandAttack(uid, targetUid, committed)
    if not targetUid then return end
    local s = ensureState(uid)
    s.attackTargetUid = targetUid
    s.committed = committed and true or nil
    setGoal(s, "attack")
    s.nextActionAt = 0
end

-----------------------------------------------------------
-- Debug: expose state for the console
-----------------------------------------------------------
function unitAi.getState(uid)
    return aiState[uid]
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

-----------------------------------------------------------
-- Init / Update / Shutdown
-----------------------------------------------------------
function unitAi.init(scriptId)
    engine.logInfo("Unit AI initializing...")
    -- Save hook: persist aiState (knownWaterSources, commandedTask,
    -- currentAction, source-drink phase, search-spiral progress, etc.).
    -- Without this, units load with empty AI state and lose their
    -- water memory + any in-flight player commands.
    local saveLib  = require("scripts.lib.serialize")
    local saveMods = require("scripts.lib.save_modules")
    saveMods.register("unit_ai",
        function()
            -- Serialize only LIVE units' state. aiState is a global
            -- singleton that accumulates entries and never drops them when
            -- a unit is destroyed, so it leaks stale entries for
            -- gone-before-save units. Persisting those is actively unsafe:
            -- on a later cross-session load such an id can collide with a
            -- live off-page entity, and onSaveLoaded then can't tell the
            -- stale loaded-page leftover from legitimate off-page state
            -- (the blob isn't page-keyed) — it would keep + misattribute
            -- it. Dropping dead ids at the source means they never reach
            -- the blob. unit.exists is GLOBAL, so live units on every page
            -- are still saved (#195).
            local live = {}
            for uid, s in pairs(aiState) do
                if unit.exists(uid) then live[uid] = s end
            end
            return saveLib.serialize(live)
        end,
        function(blob)
            -- Snapshot the pre-load singleton BEFORE clobbering. The blob
            -- holds save-time state for ALL pages, but a load should only
            -- touch the loaded page; onSaveLoaded uses this snapshot to
            -- restore still-live OFF-PAGE units' CURRENT state instead of
            -- the blob's stale copy (#195, #191).
            unitAi._preLoadState = {}
            for k, v in pairs(aiState) do unitAi._preLoadState[k] = v end
            local restored = saveLib.deserialize(blob) or {}
            -- Replace in-place so the package.loaded singleton sees it
            for k in pairs(aiState) do aiState[k] = nil end
            for k, v in pairs(restored) do aiState[k] = v end
        end)
end

-- aiState fields on a surviving entry that hold a direct reference to
-- another entity by raw id. After a load these can point at an id that did
-- NOT survive on the loaded page — a missing-def orphan, an entity already
-- gone before the save (its stale ref was still serialized), or an id that
-- now collides with a LIVE off-page entity. The per-tick validators
-- (unit.exists / unit.getInfo / building.getInfo) are GLOBAL raw lookups,
-- so for a collision they'd pass for the wrong off-page entity and the
-- survivor would resume targeting / delivering to it (#195). So any ref
-- whose target isn't in the surviving loaded-page set is scrubbed.
-- NB: any NEW aiState field that stores a unit/building id MUST be listed
-- here, or it silently reintroduces the stale-ref bug.
local AI_UNIT_REF_FIELDS     = { "attackTargetUid", "retreatThreatUid",
                                 "notifyTarget", "lungeTarget" }
local AI_BUILDING_REF_FIELDS = { "buildTarget", "storeTarget" }

-- Clear any ref that doesn't point at a surviving loaded-page entity out
-- of one surviving state entry. Setting a field to nil is exactly the
-- self-heal the AI already runs when a target legitimately vanishes, so
-- the next tick re-decides cleanly. Nested claim tables are dropped
-- wholesale when their target didn't survive (the execute paths treat a
-- nil claim as "release"). Returns #fields cleared.
local function scrubStaleRefs(s, liveUnitSet, liveBuildingSet)
    local cleared = 0
    for _, f in ipairs(AI_UNIT_REF_FIELDS) do
        local v = s[f]
        if v ~= nil and not liveUnitSet[v] then s[f] = nil; cleared = cleared + 1 end
    end
    for _, f in ipairs(AI_BUILDING_REF_FIELDS) do
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
