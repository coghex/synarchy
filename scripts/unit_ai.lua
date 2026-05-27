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

-----------------------------------------------------------
-- Tunables per unit def
-----------------------------------------------------------

-- Wander utility formula:
--   base + stamina_weight * (stamina / max_stamina)
--                         - time_penalty * (now - actionStartedAt)
-- Clamped to -inf if stamina < min_wander_stamina_fraction * max.
--
-- follow_command utility is constant 1.0 when a task is pending;
-- placed deliberately above wander's ~0.8 fresh utility so player
-- commands win in the absence of dire needs.
local config = {
    acolyte = {
        thought_interval = 1.0,    -- seconds between decisions
        thought_jitter   = 0.5,    -- ± fraction of interval
        wander_radius    = 5.0,    -- tiles
        -- Movement speed fractions (multiply max_speed from unit yaml
        -- to get the actual tiles/sec passed to moveTo). Lets one
        -- species run twice as fast as another at the same activity
        -- without restating the ratios per def.
        speed_frac_wander  = 0.5,   -- leisurely
        speed_frac_command = 0.7,   -- combat / following orders
        speed_frac_retreat = 1.0,   -- full sprint
        base_wander_utility          = 0.5,
        wander_stamina_weight        = 0.3,
        wander_time_penalty          = 0.1,    -- per second in session
        wander_min_stamina_fraction  = 0.2,
        command_speed                = 2.0,    -- LEGACY: kept for non-combat consumers
        -- Drinking
        drink_sip_litres        = 0.5,    -- canteen water consumed per sip
        drink_min_thirst        = 0.2,    -- 1 - hydration/max; below this, no drink
        drink_weight            = 10.0,   -- scales utility above wander/command
        drink_canteen_def       = "canteen_steel_2l",
        -- One litre of canteen water restores N L of hydration. A 2 L
        -- canteen at K=11 gives 22 L of hydration ≈ 50% of the
        -- average max_hydration (~43 L), matching the design target
        -- "a full canteen should refill an average acolyte by ~50%".
        drink_hydration_per_litre = 11.0,
        -- Eating. Mirror of drink_from_canteen: only fires when hunger
        -- drops below eat_max_fraction of max_hunger; utility scales
        -- linearly with how empty the meter is, weighted to beat
        -- wander/command. Currently inventory-only — search_for_food
        -- (foraging from flora) is Phase 6.
        eat_max_fraction        = 0.25,
        eat_weight              = 10.0,
        -- Refilling. Utility ramps quadratically above the threshold:
        --   25% empty (threshold): ~0.85 — beats wander, stays below
        --                          follow_command (1.0). Tops off when
        --                          nothing important is going on.
        --   50% empty:             ~0.91 — still below command, so a
        --                          half-empty canteen won't interrupt
        --                          player orders.
        --   75% empty:             ~1.07 — slightly above command;
        --                          will pause an order to top up.
        --   100% empty:            ~1.35 — well above command. Urgent
        --                          — the canteen has run dry.
        canteen_def              = "canteen_steel_2l",
        refill_min_emptiness     = 0.25,
        refill_base_weight       = 0.85,
        refill_urgency_scale     = 0.5,   -- added on top, scaled by (x²)
        refill_arrival_tiles     = 1.5,
        refill_speed             = 1.5,
        -- Source-drinking. Used when the unit has no canteen-with-
        -- water but knows where water is. Speed matches refill.
        source_drink_speed    = 1.5,
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
        search_speed             = 1.5,
        search_max_step          = 32,     -- ~4 rings; then re-anchor origin
        -- Goal-driven utility floor for "find_water". Beats wander
        -- (0.5 + ~0.3) and follow_command (1.0) so a goal-bound
        -- acolyte searches even on a full canteen. Stays well under
        -- a high-thirst drink (~10*thirst), so they still pause to
        -- drink before resuming the spiral.
        goal_search_weight       = 5.0,
        -- Notify-allies (second goal). Radio branch: stand still N
        -- seconds, then push known sources to every other radio-
        -- bearing acolyte. Walk branch: pick an uninformed acolyte
        -- by rank-based split, walk to them, stand N seconds, transfer.
        notify_broadcast_seconds = 1.0,
        notify_transfer_seconds  = 1.0,
        notify_arrival_tiles     = 1.5,
        notify_walk_speed        = 1.8,
        goal_notify_weight       = 5.0,
        -- Construction (build_nearby). Utility shape:
        --   util = base · (1 − workers_present/saturation) · dist_factor
        -- workers_present EXCLUDES the unit asking, so the curve is
        -- 3.0 / 2.4 / 1.8 / 1.2 / 0.6 / 0 as 0..5 others have already
        -- joined. dist_factor is 1.0 at adjacent and linearly falls to
        -- 0 at build_scan_range tiles away.
        build_scan_range       = 30.0,
        build_arrival_tiles    = 1.5,
        build_walk_speed       = 1.5,
        build_base_utility     = 3.0,
        build_saturation_n     = 5,
        -- Material delivery (deliver_to_build_site). Higher than the
        -- build_nearby max so a unit with both materials and an
        -- adjacent build site will deliver first, then transition to
        -- building once empty.
        deliver_scan_range     = 30.0,
        deliver_walk_speed     = 1.6,
        deliver_utility        = 4.0,
        -- Auto-store materials. Utility curve = base · fill³ where
        -- fill = carrying_weight / carrying_capacity. Below ~65 %
        -- full the action sits under wander (0.8); past that it
        -- climbs to base at 100 %. Acolytes only auto-deposit items
        -- in the "Materials" category — supplies, tools, and worn
        -- equipment stay on them.
        store_scan_range       = 30.0,
        store_walk_speed       = 1.6,
        store_base_utility     = 3.0,
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
    local jitter = (math.random() * 2 - 1) * params.thought_jitter
    s.nextActionAt = engine.gameTime() + params.thought_interval * (1 + jitter)
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

    unit.moveTo(uid, tx, ty, params.wander_speed)
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
        if it.food and it.food.nutrition and it.food.nutrition > bestN then
            best, bestN = it, it.food.nutrition
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

    local food = findFoodInInventory(uid)
    if not food then return end

    local hun    = unit.getStat(uid, "hunger")    or 0
    local maxHun = unit.getStat(uid, "max_hunger") or 0
    -- Stomach overflow is wasted (clamp to max_hunger). Item is
    -- consumed regardless — over-feeding loses calories, by design.
    local newHun = math.min(maxHun, hun + food.food.nutrition)
    unit.setStat(uid, "hunger", newHun)
    unit.removeItem(uid, food.defName)
    unit.eat(uid)
end

-----------------------------------------------------------
-- Action: refill_canteen
--
-- Proactive maintenance: when the canteen is at least half-empty and
-- the unit has a remembered water location, walk to it and top up.
-- Utility sits between wander and follow_command per the design spec
-- (more important than idling/wandering, less important than a player
-- command). FOV memory is updated separately at the top of tickOne.
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
    -- and shoots up as it runs dry. At empty (x=1), exceeds the
    -- command priority (1.0) — a dry canteen interrupts orders.
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
    unit.moveTo(uid, tx, ty, params.refill_speed)
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
        if maxHyd > 0 and hyd / maxHyd >= 0.95 then
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
    unit.moveTo(uid, tx, ty, params.source_drink_speed)
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
    -- no longer pushes).
    if isGoalActive(s, "find_water") then
        return params.goal_search_weight
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

    unit.moveTo(uid, wx, wy, params.search_speed)
end

-----------------------------------------------------------
-- Action: follow_command
-----------------------------------------------------------
local function followCommandUtility(uid, s, params)
    if not s.commandedTask then return -math.huge end
    return 1.0
end

local function followCommandExecute(uid, s, params)
    local task = s.commandedTask
    if not task then return end
    unit.moveTo(uid, task.x, task.y, task.speed or params.command_speed)
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
local RETREAT_SAFE_DIST      = 12.0    -- chebyshev tiles
local RETREAT_TARGET_DIST    = 8.0     -- how far to pick a new retreat tile

local function selfWoundedByTarget(uid, s)
    if not s.attackTargetUid then return false end
    local att = unit.getLastAttacker(uid)
    if not att then return false end
    return att.uid == s.attackTargetUid
end

-----------------------------------------------------------
-- Movement speed helpers
--
-- All combat-related movement (attack pursuit, retreat) is paced as
-- a FRACTION of the unit's per-species max_speed (read from YAML
-- via unit.getMaxSpeed). Stat-driven modulation lives in the engine
-- (UnitMoveTo applies an injury/blood multiplier on receipt) so
-- this helper only handles the species-level scaling.
-----------------------------------------------------------

local function speedFor(uid, fraction)
    local maxSpd = unit.getMaxSpeed(uid) or 3.0
    return maxSpd * (fraction or 1.0)
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

-- "Injured" for animation purposes = cumulative wound severity > 1.0
-- (sum across all active wounds). A single bad slash or several
-- moderate ones flip the unit to the limp/struggling combat anim;
-- a couple of light scratches don't. Tunable via INJURED_THRESHOLD.
local INJURED_THRESHOLD = 1.0
local function isInjured(uid)
    local wounds = unit.getWounds(uid)
    if not wounds then return false end
    local total = 0
    for _, w in ipairs(wounds) do
        total = total + (w.severity or 0)
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
    return ratio > RETREAT_FUTILITY_RATIO, ratio
end

local function retreatUtility(uid, s, params)
    -- Carry-through: as long as the unit is in retreat goal, keep
    -- the candidate dominant.
    if isGoalActive(s, "retreat") then return 6.0 end
    if not isGoalActive(s, "attack") then return -math.huge end
    if not selfWoundedByTarget(uid, s) then return -math.huge end
    local futile, ratio = futilityCheck(uid, s)
    if not futile then return -math.huge end
    -- Urgency grows with ratio. ratio=1.5 → 5.0; ratio=3.0 → 8.0.
    return 5.0 + (ratio - RETREAT_FUTILITY_RATIO) * 2.0
end

local function retreatExecute(uid, s, params)
    -- First entry into retreat: transfer the threat ref + switch goal.
    -- Clear any combat-anim override left over from attack_target so
    -- the engine's state-driven walking anim plays during the flight.
    if not isGoalActive(s, "retreat") then
        s.retreatThreatUid = s.attackTargetUid
        s.attackTargetUid  = nil
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
        -- Retreat sprints — full max-speed, per the design spec
        -- ("a unit who retreats should try to run as fast as they can").
        unit.moveTo(uid, tx, ty,
            speedFor(uid, params.speed_frac_retreat or 1.0))
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
-- Score is the natural utility-comparison number: 1.5 for fresh
-- incoming-hit retaliation, above commanded-move (1.0) but below
-- dire-need candidates (thirst / hunger ~10) so a starving bear
-- still drinks before fighting.
-----------------------------------------------------------
local ENGAGE_WINDOW_SEC = 10.0

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
            -- 6.0 preempts non-emergency goal candidates like
            -- search_for_water (5.0). Dire needs (drinking when
            -- empty ~10, eating when starving ~10) still beat us,
            -- which is the intended scale: literally-dying > combat
            -- > general goals > player-issued moves > ambient.
            return { uid = att.uid, score = 6.0 }
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
--
-- Each tick, until the target is dead or gone:
--   * Target missing / dead → clear goal.
--   * In range AND cooldown elapsed → fire combat.attack, stamp
--     attackLastSwingAt.
--   * In range AND on cooldown → stand still and wait.
--   * Out of range → re-pathfind toward target's CURRENT tile so
--     moving targets get tracked. tickOne gates re-issuing moveTo
--     on activity == idle, so we don't wipe usLocalPath while the
--     unit is mid-walk.
-----------------------------------------------------------
local function attackTargetUtility(uid, s, params)
    if not isGoalActive(s, "attack") then return -math.huge end
    if not s.attackTargetUid then return -math.huge end
    return 1.0
end

-- Helper: pop the attack-target's anim override safely. Used when
-- the goal terminates (target dead, gone, mid-fight switch) so we
-- don't leave the unit frozen in a combat-idle stance forever.
local function clearAttackAnim(uid)
    unit.clearAnimOverride(uid)
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

    local me  = unit.getInfo(uid)
    local you = unit.getInfo(target)
    if not me or not you then return end

    local dx = math.abs(me.gridX - you.gridX)
    local dy = math.abs(me.gridY - you.gridY)
    local chebyshev = (dx > dy) and dx or dy
    local range    = unit.getAttackRange(uid) or 1.0
    local cooldown = unit.getAttackCooldown(uid) or 1.5

    if chebyshev <= range then
        -- In range. If we were mid-walk, stop so the next AI tick
        -- sees activity == "idle" and we can settle into the
        -- cooldown loop. unit.stop is idempotent — fine to spam.
        if unit.getActivity(uid) == "walking" then
            unit.stop(uid)
        end
        -- Cooldown gate so we don't fire at 10 Hz (the combat
        -- thread can resolve in microseconds).
        local now  = engine.gameTime()
        local last = s.attackLastSwingAt or 0
        if now - last >= cooldown then
            -- Alternate the two attack motions for visual variety.
            -- Quick = small narrow swing; heavy = wide arc.
            local base = s.attackUseHeavy and "attack_heavy"
                                            or "attack_quick"
            s.attackUseHeavy = not s.attackUseHeavy
            local anim = combatAnimName(uid, base)
            if anim then unit.setAnimOverride(uid, anim) end
            combat.attack(uid, target)
            s.attackLastSwingAt = now
            engine.logDebug("attack: " .. tostring(uid)
                .. " swings at " .. tostring(target)
                .. " (anim=" .. tostring(anim) .. ")")
        else
            -- Mid-cooldown — show the combat-idle stance instead
            -- of falling back to regular idle. setAnimOverride is
            -- cheap to call every tick; engine treats same-anim
            -- writes as a no-op for playback timing.
            local anim = combatAnimName(uid, "combat_idle")
            if anim then unit.setAnimOverride(uid, anim) end
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
            unit.moveTo(uid, you.gridX, you.gridY,
                        speedFor(uid, params.speed_frac_command or 0.7))
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
    -- shouldn't be pre-empted by ambient utility (drink-from-canteen
    -- still wins via the dire-thirst path when the unit is dying,
    -- since that resets notifyPhase implicitly when activity changes
    -- to drinking — see the activity short-circuit at top of tickOne).
    if s.notifyPhase then return math.huge end
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
                    params.notify_walk_speed)
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
-- only the minimum number of them commit. Lock-in: utility returns
-- math.huge while a claim is held so the walk-and-deliver sequence
-- isn't pre-empted by routine wander/follow_command. Dire needs
-- (drink/eat) are -math.huge above any cap, so they still interrupt.
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
    local listStr = building.list()
    if not listStr or listStr == "No buildings placed" then return nil end
    local best, bestD = nil, params.deliver_scan_range
    for id in listStr:gmatch("id=(%d+)") do
        local bid = tonumber(id)
        if bid and building.getActivity(bid) == "appearing"
           and not building.areMaterialsSatisfied(bid) then
            local need      = building.getMaterialNeed(bid) or {}
            local delivered = building.getMaterialDelivered(bid) or {}
            -- My potential contribution per material.
            local claim = {}
            local anyClaim = false
            for matType, count in pairs(need) do
                local remaining = remainingUnclaimedNeed(bid, matType,
                                       count, delivered, uid)
                if remaining > 0 then
                    local have = inventoryCountOf(uid, matType)
                    local bring = math.min(have, remaining)
                    if bring > 0 then
                        claim[matType] = bring
                        anyClaim = true
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
    if s.deliveryClaim then return math.huge end

    local info = unit.getInfo(uid)
    if not info then return -math.huge end

    local target = findDeliveryTarget(uid, info.gridX, info.gridY, params)
    if not target then return -math.huge end

    s.deliveryPendingTarget = target
    return params.deliver_utility
end

local function deliverExecute(uid, s, params)
    -- Lock in claim on first call so subsequent ticks (and other
    -- acolytes' utility checks) see the reservation.
    if not s.deliveryClaim then
        local target = s.deliveryPendingTarget
        if not target then return end
        s.deliveryClaim = { bid = target.bid, materials = target.claim }
        s.deliveryPendingTarget = nil
    end

    local info = unit.getInfo(uid)
    if not info then return end

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
            unit.moveTo(uid, bestX, bestY, params.deliver_walk_speed)
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
    local listStr = building.list()
    if not listStr or listStr == "No buildings placed" then return nil end
    local best, bestD = nil, maxRange
    for id in listStr:gmatch("id=(%d+)") do
        local bid = tonumber(id)
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
            unit.moveTo(uid, bestX, bestY, params.store_walk_speed)
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
-- or nil. Uses building.list() / getActivity / getBuildRequired /
-- getInfo — all cheap on a small N of placed buildings.
local function findNearestUnbuilt(fromX, fromY, maxRange)
    local listStr = building.list()
    if not listStr or listStr == "No buildings placed" then return nil end
    local best, bestD = nil, maxRange
    for id in listStr:gmatch("id=(%d+)") do
        local bid = tonumber(id)
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
        unit.moveTo(uid, bestX, bestY, params.build_walk_speed)
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
local actions = {
    acolyte = {
        { name = "idle",           utility = idleUtility,
          execute = idleExecute },
        { name = "wander",         utility = wanderUtility,
          execute = wanderExecute },
        { name = "follow_command", utility = followCommandUtility,
          execute = followCommandExecute },
        { name = "retreat",        utility = retreatUtility,
          execute = retreatExecute,
          forceExecute = true },
        { name = "engage",         utility = engageUtility,
          execute = engageExecute },
        { name = "attack_target",  utility = attackTargetUtility,
          execute = attackTargetExecute,
          -- See unit_ai.tickOne — the attack candidate runs every
          -- tick so it can stop the unit when entering range,
          -- swing on cooldown, and re-pathfind when target moves.
          forceExecute = true },
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
    },
}

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

-- Load species satellite scripts. Each one defines its candidates
-- and calls unitAi.registerActions + unitAi.setConfig to plug into
-- the dispatch loop. Done at load time so all defs are wired by
-- the time the first tick runs.
require("scripts.bear_ai")

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
--   targeting `targetUid`. The attack_target candidate (score 1.0)
--   pathfinds toward the target each tick; when chebyshev distance
--   ≤ unit.getAttackRange, it fires `combat.attack` once and marks
--   the goal accomplished. Dire-need candidates (thirst, hunger,
--   etc.) outscore 1.0 and preempt — they resume once satisfied.
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

-- | Sum of combat-effectiveness across every unit currently attacking
--   `threatUid`. Used by the retreat candidate to compare "the squad
--   that's currently engaging this guy" vs the threat itself, so a
--   solo acolyte sees a futile fight against a bear while a trio sees
--   it as winnable.
function unitAi.groupEffectivenessVs(threatUid)
    local total = 0
    for _, uid in ipairs(unit.getAllIds() or {}) do
        local st = aiState[uid]
        if st and st.attackTargetUid == threatUid then
            total = total + unitAi.combatEffectiveness(uid)
        end
    end
    return total
end

function unitAi.commandAttack(uid, targetUid)
    if not targetUid then return end
    local s = ensureState(uid)
    s.attackTargetUid = targetUid
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
        function() return saveLib.serialize(aiState) end,
        function(blob)
            local restored = saveLib.deserialize(blob) or {}
            -- Replace in-place so the package.loaded singleton sees it
            for k in pairs(aiState) do aiState[k] = nil end
            for k, v in pairs(restored) do aiState[k] = v end
        end)
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
