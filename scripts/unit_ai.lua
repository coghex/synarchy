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
        wander_speed     = 1.5,    -- tiles/sec for wander moves
        base_wander_utility          = 0.5,
        wander_stamina_weight        = 0.3,
        wander_time_penalty          = 0.1,    -- per second in session
        wander_min_stamina_fraction  = 0.2,
        command_speed                = 2.0,    -- default speed for player commands
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
    },
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
--   knownWaterLocation = { x, y } | nil,   -- single-slot water memory
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
    if not s.knownWaterLocation then return -math.huge end
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
    local loc = s.knownWaterLocation
    if not loc then return end
    local info = unit.getInfo(uid)
    if not info then return end

    -- Don't walk into the water — pick a dry neighbor to stand on.
    -- Picked at execute time only (AI re-executes only on switch or
    -- idle), so the unit doesn't oscillate between neighbor choices
    -- mid-walk.
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
        -- were en route). If gone, forget — next tick will re-scan FOV.
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
            s.knownWaterLocation = nil
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
        s.knownWaterLocation = nil
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
    if not s.knownWaterLocation then return -math.huge end

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
    -- we're done.
    if s.sourcePhase == "ascending" then
        if     pose == "crawling"  then
            unit.transitionTo(uid, "crouching", STRIDE_ASCEND)
        elseif pose == "crouching" then
            unit.transitionTo(uid, "standing", STRIDE_ASCEND)
        elseif pose == "standing"  then
            s.sourcePhase        = nil
            s.knownWaterLocation = nil
        end
        return
    end

    -- No phase active: standing entry. Walk to the bank or kick off
    -- the descent if already adjacent.
    local loc = s.knownWaterLocation
    if not loc then return end
    local info = unit.getInfo(uid)
    if not info then return end

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
            s.knownWaterLocation = nil
        end
        return
    end

    -- Walk to the water edge. Same geometry as refill — push 45%
    -- toward water so the unit stops right at the bank.
    local nx, ny = nearestNonFluidNeighbor(loc.x, loc.y,
                                           info.gridX, info.gridY)
    if not nx then
        s.knownWaterLocation = nil
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
local function searchWaypoint(originX, originY, step, spacing)
    if step <= 0 then return originX, originY end
    local n = #SEARCH_DIRECTIONS
    local ring = math.floor((step - 1) / n) + 1
    local dirIdx = ((step - 1) % n) + 1
    local d = SEARCH_DIRECTIONS[dirIdx]
    return originX + d[1] * ring * spacing,
           originY + d[2] * ring * spacing
end

local function searchUtility(uid, s, params)
    if s.knownWaterLocation then return -math.huge end
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
    -- from a stale origin.
    if not s.searchOrigin or s.searchSession ~= s.actionStartedAt then
        s.searchOrigin  = { x = info.gridX, y = info.gridY }
        s.searchStep    = 1
        s.searchSession = s.actionStartedAt
    end

    local wx, wy = searchWaypoint(s.searchOrigin.x, s.searchOrigin.y,
                                  s.searchStep, params.search_spacing)
    local d = distance(info.gridX, info.gridY, wx, wy)

    if d <= params.search_arrival_tiles then
        s.searchStep = s.searchStep + 1
        if s.searchStep > params.search_max_step then
            -- Spiral exhausted without finding water — re-anchor here
            -- and start over. Avoids an infinite outward walk.
            s.searchOrigin = { x = info.gridX, y = info.gridY }
            s.searchStep   = 1
        end
        wx, wy = searchWaypoint(s.searchOrigin.x, s.searchOrigin.y,
                                s.searchStep, params.search_spacing)
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
    },
}

-----------------------------------------------------------
-- FOV-based water memory.
-- Runs every tickOne (10Hz). Always scans regardless of canteen
-- state — without this, a unit with a full canteen who walks past a
-- pond never registers it, then later when thirsty has no memory and
-- can't refill (refill_canteen requires knownWaterLocation). Cheap
-- to keep running; FOV is small (~40 tiles).
-- Stores the first fresh-water tile encountered; subsequent scans
-- overwrite with whatever's currently visible, so memory always
-- reflects the most recently seen source rather than going stale.
-- When out of LOS, the prior memory persists — that's the point.
-----------------------------------------------------------
local function scanForWater(uid, s, params)
    local tiles = unit.getVisibleTiles(uid)
    if not tiles then return end
    for _, t in ipairs(tiles) do
        local fluidType = world.getFluidAt(t.x, t.y)
        if fluidType == "lake" or fluidType == "river" then
            s.knownWaterLocation = { x = t.x, y = t.y }
            return
        end
    end
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
    maintainTask(uid, s)
    scanForWater(uid, s, params)

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
        if switching or activity == "idle" then
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

-----------------------------------------------------------
-- Debug: expose state for the console
-----------------------------------------------------------
function unitAi.getState(uid)
    return aiState[uid]
end

-----------------------------------------------------------
-- Init / Update / Shutdown
-----------------------------------------------------------
function unitAi.init(scriptId)
    engine.logInfo("Unit AI initializing...")
    -- Save hook: persist aiState (knownWaterLocation, commandedTask,
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
