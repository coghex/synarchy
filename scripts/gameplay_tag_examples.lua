-- Executable examples for the gameplayTags API (#2706).
--
-- docs/gameplay_tags_lua.md is the reference; these are its examples,
-- and the Hspec group "Lua gameplay tags" runs every one of them against
-- the production API, so they cannot drift from what is proven. Nothing
-- in the game loads this module.
--
-- Every gameplayTags verb answers `nil, message` when it refuses, so each
-- example passes a refusal straight back to its caller.

local examples = {}

-- Mark one terrain cell as a meeting point, then find every meeting
-- point on the page. A tile target is the cell's (x, y, z); `pageId` may
-- be nil for the active page. The returned targets always name their
-- page, so they can be handed straight back to any other verb.
function examples.markMeetingPoint(pageId, x, y, z)
  local cell = { kind = "tile", page = pageId, x = x, y = y, z = z }
  local ok, err = gameplayTags.add(cell, "meeting_point")
  if not ok then return nil, err end
  return gameplayTags.query({ tag = "meeting_point" },
                            { categories = "tile", page = pageId })
end

-- Units that are defenders AND ranged, minus the reserve, written with
-- the set operators...
function examples.rangedDefenders(pageId)
  return gameplayTags.query(
    { difference = {
        { intersection = { { tag = "defender" }, { tag = "ranged" } } },
        { tag = "reserve" } } },
    { categories = "unit", page = pageId })
end

-- ...and the same selection with the all/none shorthand.
function examples.rangedDefendersShorthand(pageId)
  return gameplayTags.query({ all = { "defender", "ranged" }, none = { "reserve" } },
                            { categories = "unit", page = pageId })
end

-- A negative query: every unit on the page that is NOT a defender. The
-- complement ranges over every live unit, so untagged units are
-- included.
function examples.nonDefenders(pageId)
  return gameplayTags.query({ complement = { tag = "defender" } },
                            { categories = "unit", page = pageId })
end

-- The scalar modes: how many ranged defenders, and whether any exist,
-- without building a list.
function examples.rangedDefenderCount(pageId)
  local selection = { all = { "defender", "ranged" }, none = { "reserve" } }
  local options = { categories = "unit", page = pageId }
  local n, err = gameplayTags.count(selection, options)
  if n == nil then return nil, err end
  return n, gameplayTags.exists(selection, options)
end

-- Move the meeting point: a script owns relocation. Tags never follow
-- an object anywhere, and a target missing from loaded results may
-- simply be unloaded, not destroyed.
function examples.moveMeetingPoint(from, to)
  local ok, err = gameplayTags.remove(from, "meeting_point")
  if not ok then return nil, err end
  return gameplayTags.add(to, "meeting_point")
end

return examples
