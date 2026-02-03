-- UI module - loads and manages UI scripts
local ui = {}

local mainMenuScriptId = nil

function ui.init(scriptId)
    -- Load main menu
    mainMenuScriptId = engine.loadScript("scripts/main_menu.lua", 1.0)
end

function ui.update(dt)
end

function ui.shutdown()
    if mainMenuScriptId then
        engine.killScript(mainMenuScriptId)
    end
end

return ui
