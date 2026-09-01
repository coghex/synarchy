-- Per-species AI action inventory (#1250 review round 1).
--
-- WHICH actions a species can run, answerable from outside the AI.
-- scripts/unit_ai.lua owns the action LISTS themselves and keeps them
-- private; this module records only their NAMES as they are registered,
-- so a player-facing gesture can ask "could this unit actually carry
-- out the behaviour I am about to commit it to?" without reaching into
-- the dispatch loop or hard-coding a species list beside it.
--
-- The bug this exists for: scripts/transfer_session.lua's source rule
-- accepts any player-commandable selected unit, while `escort_transfer`
-- is registered for acolytes and technomules only. A debug-spawned bear
-- or squirrel could therefore be made a Mode A session's source and
-- then never walk anywhere -- a session stuck in `approaching` forever,
-- with no panels and a hold that holds nothing. The rule that decides
-- WHO may be escorted now derives from the same registration that
-- decides who CAN be, so the two cannot drift apart as species are
-- added.
--
-- Recording happens inside `unitAi.registerActions`, which is the ONE
-- place a species' list is built (including the universal combat
-- candidates it prepends), so a satellite script that plugs itself in
-- the documented way is inventoried automatically.

local M = package.loaded["scripts.unit_ai_actions"] or {}
package.loaded["scripts.unit_ai_actions"] = M

-- defName -> { [actionName] = true }
M.byDef = M.byDef or {}

-- The ONE definition of the Mode B transfer order's action name
-- (#2030). It lives here, in the registry, because that is the only
-- module every asker already depends on: the action's own registration
-- (scripts/unit_ai_transfer.lua's `M.action.name`), the command boundary
-- that refuses an actionless carrier, and the two player gestures
-- (scripts/transfer_gestures.lua) all read THIS value, so the string a
-- gate asks about and the string the dispatch loop registers cannot
-- become two strings.
--
-- Here rather than in unit_ai_transfer.lua because a UI module cannot
-- require that one: it reads package.loaded["scripts.unit_ai"] at module
-- scope and faults in a process that never loaded the AI. This module
-- has no dependencies at all, so every consumer can require it directly.
M.TRANSFER_ORDER_ACTION = "transfer_order"

-- Record `list` as `defName`'s action list and return it unchanged, so
-- the caller can keep assigning in one expression.
function M.record(defName, list)
    local set = {}
    for _, a in ipairs(list or {}) do
        if type(a) == "table" and type(a.name) == "string" then
            set[a.name] = true
        end
    end
    M.byDef[defName] = set
    return list
end

-- Has ANY species registered? False in a process that never loaded the
-- unit AI at all -- every headless UI fixture, and the menu screens
-- before a world exists.
function M.registered()
    return next(M.byDef) ~= nil
end

-- Can `defName` run `actionName`?
--
-- An EMPTY registry answers true for everything, and that is the honest
-- answer rather than a convenience: it means no AI is loaded in this
-- process, so there is no species inventory to consult and a UI gesture
-- must not invent a refusal from its absence. Once ANY species has
-- registered, a def with no entry is a def the dispatch loop cannot
-- tick at all (scripts/unit_ai.lua's tickOne returns early on a missing
-- action list), so answering false for it is correct rather than
-- cautious.
function M.has(defName, actionName)
    if not M.registered() then return true end
    local set = defName and M.byDef[defName]
    return (set ~= nil) and (set[actionName] == true)
end

-- The same question about a live unit. A uid whose info has vanished
-- answers false: there is no species to ask about.
function M.unitHas(uid, actionName)
    if not M.registered() then return true end
    local info = uid and unit.getInfo(uid)
    local defName = info and info.defName
    if not defName then return false end
    return M.has(defName, actionName)
end

return M
