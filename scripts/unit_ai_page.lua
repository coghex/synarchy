-- Unit AI page-pairing helpers (#1673).
--
-- The AI's target finders take their own independent snapshot of the
-- ACTIVE world page: unit.getAllIds, building.getActiveIds and
-- craft.getBills each read it separately, and world.show/world.hide
-- only ENQUEUE a selection change the world thread applies later. So
-- nothing stops the active page from moving between two of those calls
-- inside one AI update, and a finder that trusts "we are all on the
-- active page" can hand an actor a candidate from another world.
--
-- The rule these helpers exist to make cheap: every candidate must
-- AFFIRMATIVELY match the acting unit's own page, read off the entity's
-- own projection (unit.getInfo().page / building.getInfo().page, both
-- backed by uiPage / biPage). A missing page, or a comparison against
-- whatever the active page happens to be right now, is not an
-- acceptable substitute -- so `same` is false whenever either side is
-- unknown, and a finder that cannot establish its actor's page selects
-- nothing at all.
--
-- The engine's own commit-time refusal (the four lax verbs in
-- src/Engine/Scripting/Lua/API/Units/Cargo.hs) is the other half: it
-- closes the item-state consequence for every caller. These helpers
-- close the targeting consequence -- an old-page actor measuring and
-- walking toward another page's coordinates -- which no engine-side
-- refusal can undo after the fact.

local M = {}

-- Two page ids name the same world. Unknown on either side is never a
-- match: fail closed.
function M.same(a, b)
    return a ~= nil and b ~= nil and a == b
end

-- The page a live unit stands on, or nil.
function M.ofUnit(uid)
    local info = unit.getInfo(uid)
    return info and info.page or nil
end

-- The page a live building sits on, or nil.
function M.ofBuilding(bid)
    local info = building.getInfo(bid)
    return info and info.page or nil
end

return M
