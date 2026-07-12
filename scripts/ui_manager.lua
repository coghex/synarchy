-- UI Manager - coordinates all UI pages
--
-- Singleton via package.loaded so a require() (e.g. game.onKeyDown's
-- isGameplayInputActive gate, #182) sees the SAME instance the engine
-- loadScript'd and broadcasts to. engine.loadScript uses dofile, which
-- does not populate package.loaded; without this self-registration a
-- require would re-execute the file as a fresh module (currentMenu reset
-- to "main") with its own disconnected state. Init loadScripts this
-- module last, so the dofile instance is the live one require resolves.
--
-- #544: this file is the entry/coordinator module — the singleton, the
-- cross-module state every split piece below reads or writes
-- (moduleReady/startupBootDone/currentMenu/previousMenu/bootProfile/
-- fbW/fbH/hoveredElement/hoveredCallback), and the small
-- always-relevant gameplay-input-gate + key-forwarding surface. Every
-- other concern — boot/lifecycle, menu transitions, widget/panel click
-- forwarding, scroll/wheel routing, text input routing, HUD/world-
-- event forwarding — lives in scripts/ui_manager_*.lua submodules,
-- required below, which attach their public callback names directly
-- onto this singleton table (same convention as unit_ai.lua's #538
-- split and unit_info_v2.lua's #542 split — see either header for the
-- package.loaded rationale). Click-callback names in particular MUST
-- land on this exact table: UI elements register them as plain
-- strings, and the engine resolves them via broadcastToModules against
-- each loaded script's own returned module (Engine.Scripting.Lua.
-- Thread / Util.broadcastToModules), not a global lookup.
local uiManager = package.loaded["scripts.ui_manager"] or {}
package.loaded["scripts.ui_manager"] = uiManager

-- Shared cross-module state. Written by one submodule, read by
-- others — see each submodule's header for who touches what.
uiManager.startupBootDone = uiManager.startupBootDone or false
uiManager.moduleReady = uiManager.moduleReady or {
    loadingScreen = false,
    tooltipStyle = false,
    mainMenu = false,
    settingsMenu = false,
    createWorldMenu = false,
    worldView = false,
    hud = false,
    saveBrowser = false,
    testArena = false,
    pauseMenu = false,
    popupsAndLogs = false,
}
uiManager.currentMenu  = uiManager.currentMenu or "main"
uiManager.previousMenu = uiManager.previousMenu or nil
uiManager.bootProfile  = uiManager.bootProfile or "normal"
uiManager.fbW = uiManager.fbW or 0
uiManager.fbH = uiManager.fbH or 0
uiManager.hoveredElement  = uiManager.hoveredElement or nil
uiManager.hoveredCallback = uiManager.hoveredCallback or nil

-- Each require below attaches that domain's public functions directly
-- onto the `uiManager` table (they pull it via
-- package.loaded["scripts.ui_manager"], valid since the self-
-- registration above runs first). Load order doesn't matter for
-- correctness — cross-module calls (e.g. input routing calling
-- uiManager.showMenu) resolve at call time, well after every
-- submodule below has finished attaching.
require("scripts.ui_manager_boot")
require("scripts.ui_manager_menu")
require("scripts.ui_manager_widgets")
require("scripts.ui_manager_panels")
require("scripts.ui_manager_scroll")
require("scripts.ui_manager_input")
require("scripts.ui_manager_events")

-----------------------------------------------------------
-- Key Input Forwarding
-----------------------------------------------------------

-- True only when the player is in a gameplay view with no blocking
-- menu/overlay on top. Gameplay key handlers (init.lua game.onKeyDown)
-- gate on this (#182): ordinary menus/overlays don't take UI focus, so
-- the input thread can't divert keys away from gameplay for us — keys
-- still broadcast to every Lua module. currentMenu is the authoritative
-- view, and the pause menu is an overlay shown while currentMenu stays
-- world_view/test_arena_view, so it has to be checked explicitly.
-- Menu-level keys (e.g. Escape closing a menu) are unaffected: they go
-- through uiManager.onUIEscape, a separate broadcast.
--
-- #742: UI.isInputBlocked() folds in EVERY visible input-exclusive
-- modal page (settings/save-browser/create-world/event-combat-injury-
-- unit logs/context menu/... — see UI.Manager.Page's default
-- upInputExclusive classification), not just the pause menu — a modal
-- page makes gameplay input inactive the same way pause always has.
-- The pauseMenu.visible check stays as a redundant belt-and-suspenders
-- (pause's own page is already exclusive) rather than being removed,
-- since it costs nothing and this function predates the page-based
-- mechanism. Click routing (init_mouse.lua) and camera scroll
-- (ui_manager_scroll.lua) share this SAME gate, so all three input
-- kinds go inert behind a modal uniformly. Escape's own dismiss
-- cascade (init_keys.lua) deliberately runs BEFORE this gate — closing
-- the very modal that's blocking gameplay can't itself be blocked.
function uiManager.isGameplayInputActive()
    if uiManager.currentMenu ~= "world_view" and uiManager.currentMenu ~= "test_arena_view" then
        return false
    end
    local pauseMenu = require("scripts.pause_menu")
    if pauseMenu.visible then
        return false
    end
    if UI.isInputBlocked() then
        return false
    end
    return true
end

function uiManager.onKeyDown(key)
    -- Keybind editor capture: while the Input settings tab is waiting for
    -- a key, route the next press to it and swallow it (don't let it reach
    -- gameplay views). The settings menu is a require'd submodule, not a
    -- broadcast target, so this forward is how capture keys reach it.
    local settingsMenu = require("scripts.settings_menu")
    if uiManager.currentMenu == "settings"
       and settingsMenu.isCapturingKey and settingsMenu.isCapturingKey() then
        settingsMenu.onKeyCapture(key)
        return
    end
    if key == "F7" then
        world.openArena()
    end
    if uiManager.currentMenu == "world_view" then
        require("scripts.world_view").onKeyDown(key)
    elseif uiManager.currentMenu == "test_arena" or uiManager.currentMenu == "test_arena_view" then
        require("scripts.test_arena").onKeyDown(key)
    end
end

function uiManager.onKeyUp(key)
    if uiManager.currentMenu == "world_view" then
        local worldView = require("scripts.world_view")
        if worldView.onKeyUp then
            worldView.onKeyUp(key)
        end
    elseif uiManager.currentMenu == "test_arena" or uiManager.currentMenu == "test_arena_view" then
        local testArena = require("scripts.test_arena")
        if testArena.onKeyUp then
            testArena.onKeyUp(key)
        end
    end
end

return uiManager
