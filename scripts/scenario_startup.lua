-- Direct scenario startup driver (#2715, SCN-15 of epic #2698).
--
-- `--scenario <path>` boots the arena profile and loads that scenario file
-- as the paused testing session. The engine has already validated and
-- resolved the path (Scenario.Path) and carries it as DATA in its boot
-- configuration: this script never sees it as source text and never
-- builds one from it. Its whole job is the ORDER of the three steps
-- the engine cannot take itself:
--
--   1. DEFINITIONS. The scenario names units, items, buildings and
--      locations, so the arena profile's definition families must be
--      registered first. Headless never runs ui_manager_boot (it gates on
--      a GPU font atlas), so headless drives scripts/startup_loader.lua
--      itself; graphical and offscreen already have ui_manager_boot
--      draining that same queue, and this script only WAITS for it, never
--      draining it a second time.
--   2. SUBMISSION. Exactly one load, through engine.startScenarioStartup:
--      the same service engine.loadScenario uses, marked as the startup
--      load. A refusal is a startup failure, not a retry.
--   3. COMPLETION. The load is followed to its terminal result by its
--      OWN identity. A fatal end in headless/offscreen is the engine's to
--      act on (the startup-failure latch, Engine.Loop.Mode); this script
--      never quits the process. Graphical keeps the paused session and its
--      recovery controls.
--
-- A fallback arena is never "success": the status stays failed whatever
-- stands afterwards, and nothing here resumes the simulation.
--
-- Singleton via package.loaded, the pattern pause.lua and autosave.lua use.
local scenarioStartup = package.loaded["scripts.scenario_startup"] or {}
package.loaded["scripts.scenario_startup"] = scenarioStartup

-- off | definitions | submit | following | done | failed
scenarioStartup.phase = scenarioStartup.phase or "off"
scenarioStartup.request = scenarioStartup.request or nil
scenarioStartup.id = scenarioStartup.id or nil

local function startupLoader()
    return require("scripts.startup_loader")
end

local function fail(message)
    scenarioStartup.phase = "failed"
    scenarioStartup.failure = message
    engine.logError("Scenario startup failed: " .. message)
    engine.failScenarioStartup(message)
end

-- Show the arena the scenario left standing. Graphical and offscreen only:
-- headless has no UI to show (ui_manager_boot never ran). The default
-- arena world was deliberately NOT created at boot (ui_manager_boot's
-- finishArenaBoot), so a ready scenario is the first thing the arena
-- view shows. When no session stands at all (a load that failed before
-- publishing) the ordinary developer arena is the recovery.
local function showArena(ready)
    local request = scenarioStartup.request
    if request.loadsDefinitions then return end
    local ok, uiManager = pcall(require, "scripts.ui_manager")
    if not ok or type(uiManager) ~= "table" or not uiManager.showMenu then return end
    local testArena = require("scripts.test_arena")
    if world.getActiveWorldId() then
        -- a session stands (the scenario, or the flat arena that recovery
        -- published): bind to it without creating another
        testArena.created = true
        uiManager.showMenu("test_arena_view")
    elseif not ready then
        uiManager.showMenu("test_arena")
    end
end

function scenarioStartup.init(scriptId)
    scenarioStartup.scriptId = scriptId
    local request = engine.getScenarioStartup()
    if not request then
        scenarioStartup.phase = "off"
        return
    end
    scenarioStartup.request = request
    scenarioStartup.phase = "definitions"
    engine.logInfo("Scenario startup requested")
end

local function stepDefinitions()
    local loader = startupLoader()
    if scenarioStartup.request.loadsDefinitions then
        if not loader.built then
            loader.build("arena")
            loader.runAll()
        end
    end
    if loader.isFailed() then
        local failure = loader.getFailure()
        fail((failure and failure.message) or "the arena definitions could not be loaded")
    elseif loader.built and loader.isDone() then
        scenarioStartup.phase = "submit"
    end
end

local function stepSubmit()
    local id, rejection = engine.startScenarioStartup()
    if not id then
        -- another operation holds the line (a console client got there
        -- first): the engine gave the claim back, so ask again next tick
        if rejection and rejection.reason == "busy" then return end
        fail("the startup scenario was not accepted: "
            .. tostring(rejection and rejection.message))
        return
    end
    scenarioStartup.id = id
    scenarioStartup.phase = "following"
    engine.logInfo("Scenario startup load #" .. id .. " submitted")
end

local function stepFollow()
    -- the startup load's OWN terminal record, kept by the engine: the latest
    -- scenario operation may already be a capture or another load by now
    local startup = engine.getScenarioStartup()
    local status = startup and startup.result
    if not status or status.id ~= scenarioStartup.id then return end
    scenarioStartup.phase = "done"
    scenarioStartup.outcome = status.outcome
    if status.outcome == "ready" then
        engine.logInfo("Scenario startup load #" .. status.id .. " ready: " .. status.source)
        showArena(true)
    else
        engine.logError("Scenario startup load #" .. status.id .. " failed: "
            .. ((status.failure and status.failure.message) or "unknown"))
        showArena(false)
    end
end

function scenarioStartup.update(dt)
    local phase = scenarioStartup.phase
    if phase == "definitions" then
        stepDefinitions()
    elseif phase == "submit" then
        stepSubmit()
    elseif phase == "following" then
        stepFollow()
    end
end

function scenarioStartup.shutdown()
    scenarioStartup.phase = "off"
end

return scenarioStartup
