-- Main initialization script
local game = {}

local shellScriptId = nil
local uiScriptId = nil

function game.init(scriptId)
    -- Initialize shell
    shellScriptId = engine.loadScript("scripts/shell.lua", 0.5)
    
    -- Initialize UI (which loads the main menu)
    uiScriptId = engine.loadScript("scripts/main_menu.lua", 1.0)
end

function game.update(dt)
end

function game.onMouseDown(button, x, y)
end

function game.onMouseUp(button, x, y)
end

function game.onKeyDown(key)
end

function game.onKeyUp(key)
end

function game.shutdown()
    if shellScriptId then
        engine.killScript(shellScriptId)
    end
    if uiScriptId then
        engine.killScript(uiScriptId)
    end
end

return game
