-- Main initialization script
local game = {}

local shellScriptId = nil
local uiScriptId = nil
local debugScriptId = nil
local unitManagerScriptId = nil
local unitInfoPanelScriptId = nil

function game.init(scriptId)
    -- Initialize debug
    debugScriptId = engine.loadScript("scripts/debug.lua", 0.1)

    -- Initialize shell
    shellScriptId = engine.loadScript("scripts/shell.lua", 0.5)

    -- Initialize unit manager (loads unit definitions from YAML)
    unitManagerScriptId = engine.loadScript("scripts/unit_manager.lua", 0.1)

    -- Initialize unit info panel (shown when a unit is selected).
    -- Ticks at 0.1s — slow enough to be cheap, fast enough to feel
    -- responsive when selection changes via click.
    unitInfoPanelScriptId = engine.loadScript("scripts/unit_info_panel.lua", 0.1)

    -- Initialize UI (which loads the main menu)
    uiScriptId = engine.loadScript("scripts/ui_manager.lua", 0.1)
end

function game.update(dt)
end

-- Mouse-button constants (match Engine.Scripting.Lua.Thread::LuaMouseDownEvent)
local MOUSE_LEFT  = 1
local MOUSE_RIGHT = 2

function game.onMouseDown(button, x, y)
    -- Only handle clicks that reach us — UI hit-tests run earlier in
    -- the input thread; if a UI element ate the click, this never fires.
    if button == MOUSE_LEFT then
        local id = unit.hitTestAt(x, y)
        if id then
            -- Hit a unit. Select it. The unit_info_panel watcher
            -- will see the change next tick and (a) push unit info
            -- into the shared HUD info panel and (b) clear the
            -- world cursor's tile selection so the tile-cursor
            -- visual goes away.
            unit.select(id)
        else
            -- Click missed all units. Deselect any active unit. We
            -- intentionally don't touch the tile cursor — hud.lua
            -- handles that on its own onMouseDown for the click
            -- that hit terrain instead of a unit.
            unit.deselectAll()
        end
    end
end

function game.onMouseUp(button, x, y)
end

function game.onKeyDown(key)
    -- ESC clears any active unit selection.
    -- Doesn't conflict with shell/UI focus: those modes consume ESC
    -- earlier in the input thread (LuaFocusLost / LuaUIEscape) and
    -- never reach this broadcast.
    if key == "Escape" then
        local selected = unit.getSelected()
        if #selected > 0 then
            unit.deselectAll()
        end
    end
end

function game.onKeyUp(key)
end

function game.shutdown()
    if debugScriptId then
        engine.killScript(debugScriptId)
    end
    if shellScriptId then
        engine.killScript(shellScriptId)
    end
    if unitManagerScriptId then
        engine.killScript(unitManagerScriptId)
    end
    if unitInfoPanelScriptId then
        engine.killScript(unitInfoPanelScriptId)
    end
    if uiScriptId then
        engine.killScript(uiScriptId)
    end
end

return game
