-- UI Manager: text input routing (#544 split from ui_manager.lua).
--
-- Owns keystroke/navigation forwarding to whichever widget family
-- currently holds focus (dropdown filter box > randbox > textbox, in
-- that priority order), UI submit, focus-lost cleanup, and menu-level
-- Escape handling (keybind capture cancel, widget escape, then the
-- per-view fallback: quit/back/pause-toggle). currentMenu lives on the
-- uiManager singleton since the Escape fallback branches on it.
local uiManager = package.loaded["scripts.ui_manager"]

local textbox     = require("scripts.ui.textbox")
local dropdown     = require("scripts.ui.dropdown")
local randbox       = require("scripts.ui.randbox")
local settingsMenu = require("scripts.settings_menu")

-----------------------------------------------------------
-- Input Event Forwarding to Textboxes
-----------------------------------------------------------

function uiManager.onUICharInput(char)
    if dropdown.getFocusedId() then
        return dropdown.onCharInput(char)
    end
    if randbox.getFocusedId() then
        return randbox.onCharInput(char)
    end
    return textbox.onCharInput(char)
end

function uiManager.onUIBackspace()
    if dropdown.getFocusedId() then
        return dropdown.onBackspace()
    end
    if randbox.getFocusedId() then
        return randbox.onBackspace()
    end
    return textbox.onBackspace()
end

function uiManager.onUIDelete()
    if dropdown.getFocusedId() then
        return dropdown.onDelete()
    end
    if randbox.getFocusedId() then
        return randbox.onDelete()
    end
    return textbox.onDelete()
end

function uiManager.onUICursorLeft()
    if dropdown.getFocusedId() then
        return dropdown.onCursorLeft()
    end
    if randbox.getFocusedId() then
        return randbox.onCursorLeft()
    end
    return textbox.onCursorLeft()
end

function uiManager.onUICursorRight()
    if dropdown.getFocusedId() then
        return dropdown.onCursorRight()
    end
    if randbox.getFocusedId() then
        return randbox.onCursorRight()
    end
    return textbox.onCursorRight()
end

function uiManager.onUIHome()
    if dropdown.getFocusedId() then
        return dropdown.onHome()
    end
    if randbox.getFocusedId() then
        return randbox.onHome()
    end
    return textbox.onHome()
end

function uiManager.onUIEnd()
    if dropdown.getFocusedId() then
        return dropdown.onEnd()
    end
    if randbox.getFocusedId() then
        return randbox.onEnd()
    end
    return textbox.onEnd()
end

function uiManager.onUISubmit()
    if dropdown.getFocusedId() then
        return dropdown.onSubmit()
    end
    if randbox.getFocusedId() then
        return randbox.onSubmit()
    end
    local handled, value, id, name = textbox.onSubmit()
    if handled and value then
        engine.logDebug("UI Submit received: " .. tostring(value) .. " from " .. tostring(name))
        settingsMenu.onTextBoxSubmit(name, value)
    end
    return handled
end

function uiManager.onUIEscape()
    -- Keybind editor capture takes escape first: cancel the in-progress
    -- capture (or conflict modal) instead of closing the settings menu.
    if uiManager.currentMenu == "settings"
       and settingsMenu.isCapturingKey and settingsMenu.isCapturingKey() then
        settingsMenu.cancelKeyCapture()
        return true
    end

    -- First, let UI widgets handle escape (close dropdowns, unfocus textboxes)
    if dropdown.getFocusedId() then
        return dropdown.onEscape()
    end
    if randbox.getFocusedId() then
        return randbox.onEscape()
    end
    local handled = textbox.onEscape()
    if handled then return true end
    for id = 1, 100 do
        if dropdown.isOpen(id) then
            dropdown.closeList(id)
            return true
        end
    end

    -- Ghost-focus recovery: no widget claimed this escape, so any
    -- engine-side focus that survived to here is unowned — clear it,
    -- otherwise the keyboard stays captured in UI-text mode.
    UI.clearFocus()

    -- Menu-level escape handling
    local currentMenu = uiManager.currentMenu
    if currentMenu == "main" then
        engine.quit()
    elseif currentMenu == "settings" or currentMenu == "create_world"
        or currentMenu == "save_browser" then
        uiManager.showMenu("back")
    elseif currentMenu == "world_view" then
        uiManager.ensurePauseMenu()
        require("scripts.pause_menu").toggle({ showSave = true })
    elseif currentMenu == "test_arena_view" then
        uiManager.ensurePauseMenu()
        require("scripts.pause_menu").toggle({ showSave = false })
    end
    return true
end

function uiManager.onUIFocusLost()
    dropdown.unfocusAll()
    randbox.unfocusAll()
    textbox.unfocusAll()
end
