-- Unit AI water-seeking actions (#538 split from unit_ai.lua).
--
-- Proactive canteen refilling, the no-canteen drink-from-source
-- sequence, the search-for-water rosette walk, and the FOV-based
-- water-memory scan every tick feeds from. Mutually exclusive with
-- drink_from_canteen (unit_ai_needs.lua) via findCanteenWithWater.

local core = require("scripts.unit_ai_core")
local distance             = core.distance
local hasKnownWaterSource  = core.hasKnownWaterSource
local nearestWaterSource   = core.nearestWaterSource
local forgetWaterSource    = core.forgetWaterSource
local addWaterSource       = core.addWaterSource
local isGoalActive         = core.isGoalActive

local mv = require("scripts.movement_speed")
local needs = require("scripts.unit_ai_needs")
local findCanteenWithWater = needs.findCanteenWithWater

local M = {}

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

M.refillUtility          = refillUtility
M.refillExecute          = refillExecute
M.drinkFromSourceUtility = drinkFromSourceUtility
M.drinkFromSourceExecute = drinkFromSourceExecute
M.searchUtility          = searchUtility
M.searchExecute          = searchExecute
M.scanForWater           = scanForWater

return M
