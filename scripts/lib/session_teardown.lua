-- Session-teardown boundary (#1610).
--
-- Exit to Menu ends a play session WITHOUT loading anything: it destroys
-- every world and resets the Haskell entity managers, but no engine-to-
-- Lua lifecycle event is emitted, so `saveModules`' load-time machinery
-- (`applyAll`'s reset hooks, the `onSaveLoaded` broadcast) never fires.
-- Every module holding session-scoped state therefore had to be
-- hand-listed in `pauseMenu.onExitToMenu` as its own `pcall` clear --
-- three of them, one per issue (#82 build-tool placement, #102 mine-tool
-- anchor, #1014 transfer session) -- and the two biggest offenders,
-- `unit_ai`'s `aiState` and `building_spawn`'s `state`, were never added
-- at all, so their rows accumulated for the life of the process.
--
-- This is that boundary, declared once: a module opts in by registering
-- its own clear, and Exit to Menu runs the whole set.
--
--   require("scripts.lib.session_teardown").register("build_tool", fn)
--
-- CONTRACT
--
--   * `runAll()` is called from `pauseMenu.onExitToMenu` and NOWHERE
--     else. In particular it is not part of the load transaction: a save
--     load keeps its existing `saveModules.applyAll` reset hooks and
--     `onSaveLoaded` broadcast, and this boundary adds no second clear
--     to it.
--   * It runs BEFORE `world.destroyAll()`, so every callback still sees
--     a live session -- `transfer_session.clear` has to stop the units
--     it is holding, and `mine_tool.cancel` has to reach the engine-side
--     anchor of a world that still exists (docs/engine_contracts.md,
--     "Player transfers").
--   * Callbacks are INDEPENDENT: each runs in its own `pcall`, so one
--     failing clear can neither suppress a later one nor stop the world
--     teardown that follows. That is exactly what the three separate
--     `pcall`s used to buy, kept.
--   * Order is id-ascending, so it depends on the registration set and
--     not on module load order (which varies with lazy `require`s).
--   * A registrant empties its state IN PLACE. Never rebind a table a
--     module published on `package.loaded` -- other modules hold direct
--     references to it.
--
-- THE DRAIN WINDOW. `world.destroyAll()` only ENQUEUES `WorldDestroyAll`
-- (Engine/Scripting/Lua/API/World/Lifecycle.hs); its handler later queues
-- `UnitClearAll` and `BuildingClearAll` onto two further worker queues
-- (World/Thread/Command/Basic.hs). So for some number of ticks after
-- `onExitToMenu` returns, `unit.getAllIds()` and `building.getInfo()`
-- still report the OLD session's entities -- and an ordinary
-- `unitAi.update` / `buildingSpawn.update` in that window would rebuild
-- through `ensureState` exactly the rows the teardown just cleared.
-- `isTornDown()` is the latch that holds those ticks off; it is released
-- by `beginSession()` when the next session activates.
--
-- This registry is deliberately INDEPENDENT of
-- `scripts/lib/save_modules.lua`: its own namespace, its own invocation
-- path, no interaction with `registry` / `resetHooks` / `applyAll` /
-- `onSaveLoaded`. That is what lets the durable `unit_ai` and
-- `building_spawn` COMPONENTS opt in -- `registerResetHook` refuses an
-- id already registered as a save component, and nothing here changes a
-- component's registration, payload version, or load rollback semantics.
local sessionTeardown = package.loaded["scripts.lib.session_teardown"] or {}
package.loaded["scripts.lib.session_teardown"] = sessionTeardown

-- id -> clear function. Kept on the module table so a script reload
-- (engine.loadScript re-executes this chunk) keeps the registrations
-- that earlier-loaded modules already made.
sessionTeardown.callbacks = sessionTeardown.callbacks or {}
if sessionTeardown.tornDown == nil then
    -- Boot state is "a session may run": nothing has been torn down, so
    -- nothing is held off. Only runAll() ever sets this.
    sessionTeardown.tornDown = false
end

local VALID_ID_PATTERN = "^[%a_][%w_]*$"

local function sortedIds()
    local out = {}
    for id in pairs(sessionTeardown.callbacks) do out[#out + 1] = id end
    table.sort(out)
    return out
end

-- Declare `id`'s session-scoped state, cleared by `clearFn` on every
-- Exit to Menu.
--
-- Re-registering an id REPLACES its callback rather than raising: these
-- registrations run at module scope / module init, and engine.loadScript
-- re-executes a chunk on a script reload, which would otherwise turn a
-- reload into a hard error. The id set is small, fixed, and reviewed, so
-- a genuine collision is caught by reading the callers, not at runtime.
function sessionTeardown.register(id, clearFn)
    if type(id) ~= "string" or not id:match(VALID_ID_PATTERN) then
        error("sessionTeardown.register: invalid id " .. tostring(id)
            .. " (must match " .. VALID_ID_PATTERN .. ")")
    end
    if type(clearFn) ~= "function" then
        error("sessionTeardown.register: '" .. id
            .. "' clearFn must be a function")
    end
    sessionTeardown.callbacks[id] = clearFn
end

-- Every registered id, id-ascending. Diagnostic / test oracle.
function sessionTeardown.registeredIds()
    return sortedIds()
end

-- True between Exit to Menu and the next session activation. Update
-- loops that would otherwise rebuild session state from entity queries
-- still draining in that window consult this.
function sessionTeardown.isTornDown()
    return sessionTeardown.tornDown == true
end

-- The next session is live: release the latch. Called from
-- `uiManager.showMenu` for the two gameplay views -- the one transition a
-- new game, a menu-driven load and the test arena all reach -- and from
-- `uiManager.onSaveLoaded`, which every published load reaches whatever
-- triggered it (a debug-console `engine.loadSave` from the menu passes
-- through no menu transition at all). RELEASING only: this runs no
-- callback and clears nothing, so it adds nothing to the load
-- transaction.
function sessionTeardown.beginSession()
    sessionTeardown.tornDown = false
end

-- Run every registered clear. Returns how many raised.
--
-- The latch is set BEFORE the callbacks run, so a callback that itself
-- drives an update cannot repopulate what an earlier one cleared.
function sessionTeardown.runAll()
    sessionTeardown.tornDown = true
    local ids, failed = sortedIds(), 0
    for _, id in ipairs(ids) do
        local ok, err = pcall(sessionTeardown.callbacks[id])
        if not ok then
            failed = failed + 1
            engine.logError("sessionTeardown: '" .. id
                .. "' failed to clear: " .. tostring(err))
        end
    end
    engine.logInfo("Session teardown: ran " .. #ids .. " clear(s), "
        .. failed .. " failed")
    return failed
end

return sessionTeardown
