-- Unit AI survival needs (#538 split from unit_ai.lua).
--
-- Personal-maintenance action cluster: idle, wander, drink from a
-- carried canteen, eat from inventory, and forage from the land
-- (#94). Self-contained except for movement_speed and unit_stats.

local mv      = require("scripts.movement_speed")
local ambient = require("scripts.ambient_movement")

local M = {}

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

-- Sample a wander destination around the unit's current position.
-- Shared by both execute variants below so the two can only differ in
-- the hazard policy they ask for, never in where they aim.
local function sampleWanderTarget(uid, params)
    local info = unit.getInfo(uid)
    if not info then return nil end

    local angle = math.random() * 2 * math.pi
    local dist  = math.random() * params.wander_radius
    return info.gridX + math.cos(angle) * dist,
           info.gridY + math.sin(angle) * dist
end

-- The DEFAULT (fall-permitted) wander leg. unit_ai_mental.lua reuses this
-- for panic, lash-out idling, delirium and mental breaks, which must keep
-- behaving exactly as they do today — a panicking unit running off a
-- ledge is the behavior, not a bug (#1217 requirement 3).
local function wanderExecute(uid, s, params)
    local tx, ty = sampleWanderTarget(uid, params)
    if not tx then return end

    -- Aimless wander is a slow meander — well below comfort, so the unit
    -- ambles (and recovers stamina) rather than cruising. Units only move
    -- at comfort/ordered/sprint when they have an actual purpose.
    unit.moveTo(uid, tx, ty, mv.meander(uid))
end

-- The AMBIENT (fall-prohibited) wander leg — what the registered
-- `wander` action uses for acolytes and the technomule (#1217). Same
-- destination sampling, routed through the shared ambient mechanism so
-- the engine refuses a route over a damaging drop and terminates the
-- request when no safe route exists (the AI then resamples on a later
-- thought tick).
local function ambientWanderExecute(uid, s, params)
    local tx, ty = sampleWanderTarget(uid, params)
    if not tx then return end
    ambient.wanderTo(uid, tx, ty)
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
    --
    -- Drain the EXACT instance the sip was computed from (#1220), the
    -- same reason consumable.lua does: unit.modifyItemFill drains the
    -- first item matching defName, so an earlier already-empty canteen
    -- would swallow the drain (clamped to zero) while the hydration
    -- below is still credited — conjuring water and leaving the sipped
    -- canteen full.
    unit.modifyItemFillById(uid, canteen.instanceId, -sip)
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

-- Discrete vs bulk, keyed exactly the way unit.feed keys it
-- (Survival.hs unitFeedFn): a positive calories_per_kg means BULK —
-- consumed by drawing kg of fill — and anything else with food data is
-- DISCRETE, consumed whole.
local function isDiscreteFood(nut)
    return (nut.caloriesPerKg or 0) <= 0
end

-- Pick the most calorific food the unit is carrying.
--
-- `room`/`minRoomFraction` are the stop-before-waste filter (#1219) and
-- are supplied ONLY by the meal loop, and only for its marginal items.
-- With `room` nil this is the plain "is there anything edible?" query
-- the eat and forage gates ask, so entry conditions are untouched.
--
-- The filter SKIPS a too-marginal discrete item rather than ending the
-- search: withholding a ration must not hide a part-full quinoa sack
-- sitting behind it in the largest-available ordering.
local function findFoodInInventory(uid, room, minRoomFraction)
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
        -- Mostly-wasted discrete item: leave it in the pack. Bulk food
        -- wastes nothing (unit.feed draws only the deficit), so it is
        -- never withheld.
        local wasteful = room and nut and isDiscreteFood(nut)
                         and room < minRoomFraction * cal
        if avail > 0 and not wasteful and avail > bestN then
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
    --
    -- Stop before waste (#1219): a discrete item is withheld once the
    -- stomach can no longer hold most of it. Only ever the MARGINAL
    -- item — the threshold arms after the first successful feed of this
    -- meal, so the first item is always opened and a starving unit
    -- carrying nothing but rations still eats. eatExecute's synchronous
    -- loop is the meal boundary.
    local fedOnce = false

    for _ = 1, 10 do
        local hun    = unit.getStat(uid, "hunger")
        local maxHun = unit.getStat(uid, "max_hunger")
        if hun and maxHun and hun >= maxHun * 0.99 then break end

        local room = nil
        if fedOnce and hun and maxHun then room = maxHun - hun end

        local food = findFoodInInventory(uid, room,
                                         params.eat_discrete_min_room_fraction)
        if not food then break end

        -- unit.feed is the authoritative consume-and-credit primitive:
        -- discrete food is removed whole (credit clamped to max_hunger,
        -- overflow wasted by design); bulk food (quinoa sack) sheds just
        -- enough fill to top the stomach up. Plays the eat anim. Returns
        -- the kcal credited, or nil if the item couldn't be consumed.
        if not unit.feed(uid, food.defName) then break end
        fedOnce = true

        -- Food carries salt — the only way to recover from sweat-driven
        -- hyponatremia (the kidneys can't make sodium). See scripts/salts.lua.
        require("scripts.salts").mealSalt(uid)
    end
end

-----------------------------------------------------------
-- Action: forage (#94)
--
-- Acquire food from the LAND when the unit is hungry and carrying
-- none: walk to the nearest harvestable flora tile
-- (world.findHarvestableFlora), harvest it (the yield spawns as
-- ground items), and pick the yield up — eat_from_inventory then
-- takes over. If no harvestable plant is in range but edible ground
-- items are, fetch those instead. The #94 emergency-hunger ladder is
-- inventory → flora → ground food; the first rung is
-- eat_from_inventory itself, which outranks forage whenever the unit
-- actually carries food.
--
-- The "hard interrupt" for a critically hungry unit is the same
-- mechanism every dire need uses here (#306): the utility ramps past
-- follow_command as the calorie store empties, so a starving unit
-- drops its orders and goes foraging. Finite by construction (peak
-- base + scale = 8.4) — never math.huge.
-----------------------------------------------------------
local foodDefCache = {}   -- defName → bool: def has an edible food block

local function isFoodDef(defName)
    local c = foodDefCache[defName]
    if c ~= nil then return c end
    local f = item.getFood and item.getFood(defName)
    local edible = (f ~= nil)
        and ((f.calories or 0) > 0 or (f.caloriesPerKg or 0) > 0)
    foodDefCache[defName] = edible
    return edible
end

-- Blended food need ∈ [0,1]: stomach emptiness matters (it's what
-- "feeling hungry" is) but the calorie STORE emptying is what actually
-- kills — it dominates the ramp so the emergency fires on real
-- starvation, not on a between-meals stomach.
local function forageNeed(uid)
    local hun    = unit.getStat(uid, "hunger")
    local maxHun = unit.getStat(uid, "max_hunger")
    if not hun or not maxHun or maxHun <= 0 then return 0, 1 end
    local hungerFrac = hun / maxHun
    local storeNeed = 0
    local cal    = unit.getStat(uid, "calories")
    local maxCal = unit.getStat(uid, "max_calories")
    if cal and maxCal and maxCal > 0 then
        storeNeed = 1 - math.max(0, math.min(1, cal / maxCal))
    end
    return math.max((1 - hungerFrac) * 0.6, storeNeed), hungerFrac
end

-- #1673: item.listGround is ACTIVE-page scoped while item.pickupGround
-- commits on the CARRIER's page, so a same-numbered gid on another page
-- is a different item entirely. Every listed id is re-resolved on the
-- ACTING unit's own page through item.getGroundForUnit (#1666's
-- owning-page reader) and every predicate below reads the RESOLVED row,
-- so a forager can neither be sent to another world's coordinates nor
-- hand a foreign gid to a pickup that would move something else.
-- Failing closed: an id that does not resolve is not a candidate.
local function findGroundFood(uid, ux, uy, radius)
    local ground = item.listGround()
    if not ground then return nil end
    local best, bestD2 = nil, radius * radius + 1
    for _, g in ipairs(ground) do
        local owned = item.getGroundForUnit(uid, g.id)
        if owned and isFoodDef(owned.defName) then
            local dx, dy = owned.x - ux, owned.y - uy
            local d2 = dx * dx + dy * dy
            if d2 < bestD2 then best, bestD2 = owned, d2 end
        end
    end
    return best
end

local function forageUtility(uid, s, params)
    local need, hungerFrac = forageNeed(uid)
    if hungerFrac >= params.forage_max_fraction then return -math.huge end
    -- Carrying food? Eating it (eat_from_inventory) is the better
    -- move — don't compete with it.
    if findFoodInInventory(uid) then return -math.huge end
    local info = unit.getInfo(uid)
    if not info then return -math.huge end
    local ux = math.floor(info.gridX)
    local uy = math.floor(info.gridY)
    -- Prefer a living plant; fall back to edible ground items.
    local plant = world.findHarvestableFlora
        and world.findHarvestableFlora(ux, uy, params.forage_search_radius)
    if plant then
        s.forageTarget = { kind = "flora", x = plant.gx, y = plant.gy }
    else
        local g = findGroundFood(uid, info.gridX, info.gridY,
                                 params.forage_search_radius)
        if not g then
            s.forageTarget = nil
            return -math.huge
        end
        s.forageTarget = { kind = "ground", gid = g.id,
                           x = math.floor(g.x), y = math.floor(g.y) }
    end
    return params.forage_base_weight
         + params.forage_urgency_scale * need * need
end

local function forageExecute(uid, s, params)
    -- Collecting: pull the harvested yield off the ground, one item
    -- per tick; whatever landed in the inventory is eaten by
    -- eat_from_inventory on the next decisions.
    --
    -- Deliberately UNGATED on carrying capacity (#2293), and alone in
    -- that among the pickup ladders -- fetch, repair and auto-harvest
    -- all weigh the live row immediately before item.pickupGround, and
    -- the player pickup order gates twice. This is #94's emergency
    -- hunger ladder (inventory → flora → ground food) and eating
    -- requires POSSESSION: eat_from_inventory consumes from the
    -- inventory, so food a full unit may not pick up is food it may not
    -- eat. A hard gate here would leave a unit at capacity with
    -- non-food starving beside a meal. The over-capacity state this
    -- admits is priced rather than free -- scripts/movement_speed.lua's
    -- steep OVER_K band slows the unit hard until the load is dealt
    -- with.
    --
    -- A false result is therefore never a capacity refusal:
    -- item.pickupGround cannot produce one (it reads no weight at all,
    -- src/Engine/Scripting/Lua/API/Items/Ground.hs). It means the row
    -- raced away, or this unit has no live page. Ending the collection
    -- is the right answer to both -- the stale gids are retired rather
    -- than retried every tick, and the next decision re-finds whatever
    -- is actually still there.
    if s.foragePhase == "collecting" then
        local loot = s.forageLoot or {}
        local nextGid = table.remove(loot)
        if not nextGid or not item.pickupGround(uid, nextGid) then
            s.foragePhase  = nil
            s.forageLoot   = nil
            s.forageTarget = nil
        end
        return
    end

    local tgt = s.forageTarget
    if not tgt then return end
    local info = unit.getInfo(uid)
    if not info then return end
    -- #1673: forageTarget.gid is a PERSISTED ground reference, picked
    -- off the ACTIVE-page listing, so a save (or a page switch between
    -- utility and execute) can leave it naming another world's row.
    -- Re-resolve it on the carrier's own page BEFORE it steers the walk
    -- below or reaches item.pickupGround, and take the approach
    -- coordinates from the resolved row rather than the listed one.
    if tgt.kind == "ground" then
        local owned = item.getGroundForUnit(uid, tgt.gid)
        if not owned then s.forageTarget = nil; return end
        tgt.x, tgt.y = math.floor(owned.x), math.floor(owned.y)
    end
    local utx = math.floor(info.gridX)
    local uty = math.floor(info.gridY)
    local cheb = math.max(math.abs(utx - tgt.x), math.abs(uty - tgt.y))

    if cheb <= 1 then
        if tgt.kind == "flora" then
            local yields = world.harvestFlora(tgt.x, tgt.y)
            if yields and #yields > 0 then
                unit.pickup(uid)   -- bend-down anim over the plant
                local gids = {}
                for _, yi in ipairs(yields) do gids[#gids + 1] = yi.gid end
                s.forageLoot  = gids
                s.foragePhase = "collecting"
            else
                -- Raced / regrowing / decorative after all: forget the
                -- target; the next decision re-finds.
                s.forageTarget = nil
            end
        else
            unit.pickup(uid)
            -- The direct ground-food rung, and the same reading of a
            -- false result (#2293): a raced or vanished row, never a
            -- capacity refusal, which is why this rung takes the item
            -- whatever the unit is already carrying. Either way the
            -- stale gid is RETIRED -- the target is cleared and
            -- arbitration re-decides next tick rather than walking back
            -- to a row that is not there -- and the loss is logged
            -- instead of discarded, so a picker that keeps losing the
            -- race is visible.
            if not item.pickupGround(uid, tgt.gid) then
                engine.logInfo("forage: unit " .. tostring(uid)
                    .. " lost ground food " .. tostring(tgt.gid)
                    .. " before pickup — retiring the target")
            end
            s.forageTarget = nil
        end
        return
    end

    -- Walk to it. Comfort pace for routine foraging; ordered pace when
    -- genuinely starving (the same urgency that pushed the utility past
    -- follow_command).
    local need = forageNeed(uid)
    local speed = (need > 0.8) and mv.ordered(uid) or mv.comfort(uid)
    unit.moveTo(uid, tgt.x + 0.5, tgt.y + 0.5, speed)
end


M.idleUtility           = idleUtility
M.idleExecute           = idleExecute
M.wanderUtility         = wanderUtility
M.wanderExecute         = wanderExecute
M.ambientWanderExecute  = ambientWanderExecute
M.findCanteenWithWater  = findCanteenWithWater
M.drinkUtility          = drinkUtility
M.drinkExecute          = drinkExecute
M.eatUtility            = eatUtility
M.eatExecute            = eatExecute
M.forageUtility         = forageUtility
M.forageExecute         = forageExecute

return M
