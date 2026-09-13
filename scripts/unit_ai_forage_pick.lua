-- What a food search chose from the land, and the verb that picks
-- exactly that (#2553 split from unit_ai_needs.lua).
--
-- world.findHarvestableFlora's BARE (untagged) call is the food search:
-- it admits only species whose phase-resolved yield contains something
-- EDIBLE (src/Engine/Scripting/Lua/API/Forage/Query.hs's `wanted`), so
-- a starving unit is never sent to fell an oak for inedible logs. The
-- coordinate harvest world.harvestFlora applies no such filter -- it
-- takes the FIRST admitting instance in the tile's stored order
-- (Forage/Harvest.hs's harvestWildFlora) -- and several plants may
-- legitimately stand on one tile. A forager that re-entered by
-- coordinate therefore picked whichever plant happened to be first,
-- handed the yield and the regrowth timer to a wood-producing
-- co-tenant, and walked away hungry.
--
-- Both food-seeking callers (unitAi.harvest's auto_harvest and
-- unit_ai_needs.lua's forage rung) now carry the winner's IDENTITY from
-- the search to the pick and complete through pick() below. Split into
-- its own module because the rule is theirs jointly and neither owns
-- it, and because unit_ai_needs.lua had no room left under
-- tools/lua_module_budget.py's 500-line cap -- the ground-food rung's
-- own "may a forager eat this" lookups move with it, being the same
-- question about the same ladder.
--
-- Nothing here is chop's. Chop has harvested instance-exactly since
-- #1854 (scripts/unit_ai_chop.lua's world.harvestFloraInstance call)
-- and asks for a "wood" tag, which skips the edibility filter entirely.

local M = {}

-- defName -> bool: does this definition carry an edible food block?
--
-- Cached because the ground scan below asks it once per listed row per
-- tick, and an item def's food block cannot change within a session.
local foodDefCache = {}

function M.isFoodDef(defName)
    local c = foodDefCache[defName]
    if c ~= nil then return c end
    local f = item.getFood and item.getFood(defName)
    local edible = (f ~= nil)
        and ((f.calories or 0) > 0 or (f.caloriesPerKg or 0) > 0)
    foodDefCache[defName] = edible
    return edible
end

-- #1673: item.listGround is ACTIVE-page scoped while item.pickupGround
-- commits on the CARRIER's page, so a same-numbered gid on another page
-- is a different item entirely. Every listed id is re-resolved on the
-- ACTING unit's own page through item.getGroundForUnit (#1666's
-- owning-page reader) and every predicate below reads the RESOLVED row,
-- so a forager can neither be sent to another world's coordinates nor
-- hand a foreign gid to a pickup that would move something else.
-- Failing closed: an id that does not resolve is not a candidate.
function M.findGroundFood(uid, ux, uy, radius)
    local ground = item.listGround()
    if not ground then return nil end
    local best, bestD2 = nil, radius * radius + 1
    for _, g in ipairs(ground) do
        local owned = item.getGroundForUnit(uid, g.id)
        if owned and M.isFoodDef(owned.defName) then
            local dx, dy = owned.x - ux, owned.y - uy
            local d2 = dx * dx + dy * dy
            if d2 < bestD2 then best, bestD2 = owned, d2 end
        end
    end
    return best
end

-- Pick the plant the search actually chose.
--
-- A findHarvestableFlora WILD winner reports its plant's stable
-- instanceId (#1854); a CROP PLOT winner reports no instanceId at all,
-- because a plot is tile-keyed by construction and Query.hs never hands
-- out the reserved non-identity value as though it were usable. Both
-- callers therefore store `iid = spot.instanceId` verbatim and this
-- branches on its ABSENCE -- there is no sentinel to compare against.
--
--   * A wild winner is taken by IDENTITY, through
--     world.harvestFloraInstance -- the same exact-instance verb chop
--     has used since #1854. Only that plant's yields spawn and only its
--     own regrowth timer starts, whatever else shares its tile.
--   * A crop plot is taken by COORDINATE, because it has no instance
--     identity to name (requirement 3). This is safe and unambiguous:
--     tilled soil excludes natural flora placement, so a plot never
--     coexists with wild instances (Forage/Harvest.hs), and a bare
--     world.harvestFlora on a plotted tile takes the plot branch or
--     refuses -- it can never fall through to a wild neighbour.
--   * Anything else REFUSES, returning nil. That is the state a load
--     leaves behind: scripts/unit_ai_save_transient.lua strips the
--     stored iid on the chopJob.iid pattern rather than persisting a
--     bare FloraInstanceId the reference graph could not check, so a
--     restored flora target names a tile and nothing else. Falling back
--     to the coordinate verb there would reinstate exactly the defect
--     this module exists to prevent, for the one tick before the next
--     scan overwrites the target. Refusing costs that same tick and
--     cannot mis-pick.
--
-- nil is the callers' existing raced/regrowing signal, so requirement
-- 2's "refuse or reacquire" needs no new path: world.harvestFloraInstance
-- ALREADY returns nil when the tile does not hold the instance, when it
-- is not a harvestable species, when the growth window refuses it, or
-- when its own timer is live (Forage/Harvest.hs), and every caller
-- answers that by forgetting the target so the next decision re-finds.
-- An accepted pick whose authored yield is empty returns an EMPTY table,
-- never nil -- the callers read `#yields > 0`, so it lands on the same
-- branch it always did.
function M.pick(tgt)
    if tgt.iid then
        return world.harvestFloraInstance(tgt.x, tgt.y, tgt.iid)
    end
    if world.getCropPlotAt and world.getCropPlotAt(tgt.x, tgt.y) then
        return world.harvestFlora(tgt.x, tgt.y)
    end
    return nil
end

return M
