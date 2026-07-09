-- UI Manager: menu transitions (#544 split from ui_manager.lua).
--
-- Owns showMenu — the single page-transition switchboard every menu
-- and view routes through — plus the small callback forwards that
-- exist mainly to call it (main-menu/pause-menu item clicks, settings
-- back/save/apply, cancel, create-world, save-browser back, arena
-- ready, open arena) and the save-loaded HUD-toolbar-reset broadcast.
-- currentMenu/previousMenu/bootProfile/fbW/fbH live on the uiManager
-- singleton (scripts/ui_manager.lua) since ui_manager_boot.lua and
-- ui_manager_scroll.lua/ui_manager_input.lua read them too.
local uiManager = package.loaded["scripts.ui_manager"]
local widgets = require("scripts.ui_manager_widgets")

local mainMenu        = require("scripts.main_menu")
local settingsMenu    = require("scripts.settings_menu")
local createWorldMenu = require("scripts.create_world_menu")
local worldView       = require("scripts.world_view")
local saveBrowser     = require("scripts.save_browser")
local hud             = require("scripts.hud")
local loadingScreen   = require("scripts.loading_screen")
local testArena       = require("scripts.test_arena")
local pauseMenu       = require("scripts.pause_menu")

function uiManager.showMenu(menuName, params)
    -- Handle "back" by going to previousMenu (or main if none)
    if menuName == "back" then
        menuName = uiManager.previousMenu or "main"
    end

    local previousMenu = uiManager.currentMenu
    uiManager.previousMenu = previousMenu
    uiManager.currentMenu = menuName

    -- When opening settings from a game view, keep the world visible behind
    local keepWorld = (menuName == "settings")
        and (previousMenu == "world_view" or previousMenu == "test_arena_view")

    if uiManager.moduleReady.mainMenu then mainMenu.hide() end
    if uiManager.moduleReady.settingsMenu then settingsMenu.hide() end
    if uiManager.moduleReady.createWorldMenu then createWorldMenu.hide() end
    if not keepWorld then
        if uiManager.moduleReady.worldView then worldView.hide() end
        if uiManager.moduleReady.hud then hud.hide() end
    end
    -- Sweep the widgets that must die on EVERY menu transition regardless
    -- of keepWorld (#156) — the keepWorld Settings path skips hud.hide()
    -- above, which would otherwise leave the F8 debug overlay visible AND
    -- clickable (uiManager.onMouseDown still gives it first crack via
    -- tryClaimClick, #147) and an in-flight drag-select box committable
    -- (#146) on the Settings screen. One registry entry per widget; see
    -- scripts/ui/view_teardown.lua for the per-widget rationale.
    require("scripts.ui.view_teardown").run("menu")
    if uiManager.moduleReady.saveBrowser then saveBrowser.hide() end
    if uiManager.moduleReady.loadingScreen then loadingScreen.hide() end
    if uiManager.moduleReady.testArena and not keepWorld then testArena.hide() end
    if uiManager.moduleReady.pauseMenu then pauseMenu.hide() end

    if menuName == "main" then
        uiManager.ensureMainMenu()
        mainMenu.show()
    elseif menuName == "settings" then
        uiManager.ensureSettingsMenu()
        settingsMenu.show()
    elseif menuName == "create_world" then
        uiManager.ensureCreateWorldMenu()
        createWorldMenu.show()
    elseif menuName == "world_view" then
        uiManager.ensureWorldView()
        uiManager.ensureGameplayUI()
        worldView.show()
        hud.worldId = "main_world"
        hud.show()
        -- Re-show pause menu if returning from settings (opened via pause menu)
        if previousMenu == "settings" then
            pauseMenu.show()
        end
    elseif menuName == "save_browser" then
        uiManager.ensureMainMenu()
        uiManager.ensureSaveBrowser()
        saveBrowser.show(mainMenu.saves, function(saveName)
            mainMenu.loadAndShowSave(saveName)
        end, function()
            uiManager.showMenu("main")
        end)
    elseif menuName == "loading" then
        uiManager.ensureLoadingScreen()
        params = params or {}
        params.fbW = uiManager.fbW
        params.fbH = uiManager.fbH
        loadingScreen.show(params)
    elseif menuName == "test_arena" then
        uiManager.ensureGameplayUI()
        uiManager.ensureTestArena()
        testArena.show()
    elseif menuName == "test_arena_view" then
        uiManager.ensureGameplayUI()
        uiManager.ensureTestArena()
        -- Arena world already exists and is visible, just show the UI page
        testArena.visible = true
        if not testArena.page then testArena.createUI() end
        UI.showPage(testArena.page)
        world.show(testArena.arenaWorldId)
        hud.worldId = testArena.arenaWorldId
        hud.show()
        -- Arm the tile-editor popup. Done here (not in testArena.show)
        -- because showMenu calls testArena.hide() at the top of every
        -- transition, which would otherwise disarm us.
        require("scripts.tile_editor").setArenaActive(true)
        -- Re-show pause menu if returning from settings (opened via pause menu)
        if previousMenu == "settings" then
            pauseMenu.show()
        end
    end
end

function uiManager.onSaveBrowserBack(elemHandle)
    widgets.handleNonTextBoxClick()
    if saveBrowser.onBackCallback then
        saveBrowser.onBackCallback()
        return true
    end
    uiManager.showMenu("main")
    return true
end

-- Broadcast from the world thread when ANY save finishes loading
-- (main-menu load, debug-console engine.loadSave, etc. — see
-- World/Thread/Helpers.sendSaveLoaded). A load ALWAYS targets the
-- "main_world" page (Engine/Scripting/Lua/API/Save.hs) and resets its
-- engine ToolMode to default (World/Thread/Command/Save.hs); mirror that
-- into the shared HUD toolbar so the visible tool and world.getToolMode()
-- agree, regardless of how the load was triggered.
--
-- hud.markLoadedToolReset applies the reset now if the HUD is already
-- bound to main_world, otherwise defers it until the next main_world bind
-- (consumed in hud.show). This both avoids clobbering the arena's tool
-- state on a main_world load (the toolbar onChange writes
-- world.setToolMode(hud.worldId, ...)) AND still resets the toolbar after
-- an arena→menu→load→world_view round-trip, where hud.worldId is stale
-- ("test_arena") at load time and only rebinds to main_world later. (#103)
function uiManager.onSaveLoaded(survUnitIds, survBuildingIds)
    if hud.markLoadedToolReset then
        hud.markLoadedToolReset()
    end
end

function uiManager.onCreateWorld()
    widgets.handleNonTextBoxClick()
    uiManager.showMenu("create_world")
end

function uiManager.onArenaReady(pageId)
    if pageId == "test_arena" then
        engine.logInfo("Arena ready, switching to test arena view")
        testArena.ready = true
        -- Full menu transition: hides main menu, loading screen, everything
        uiManager.showMenu("test_arena_view")
        if uiManager.bootProfile == "arena" then
            engine.showDebug()
        end
    end
end

function uiManager.onMainMenuItem(elemHandle)
    widgets.handleNonTextBoxClick()
    return mainMenu.handleClick(elemHandle)
end

function uiManager.onPauseMenuItem(elemHandle)
    widgets.handleNonTextBoxClick()
    if uiManager.moduleReady.pauseMenu then
        return pauseMenu.handleClick(elemHandle)
    end
    return false
end

function uiManager.onSettings()
    widgets.handleNonTextBoxClick()
    uiManager.showMenu("settings")
end

function uiManager.onQuit()
    engine.quit()
end

function uiManager.onSettingsBack()
    widgets.handleNonTextBoxClick()
    if uiManager.moduleReady.settingsMenu then settingsMenu.onBack() end
    uiManager.showMenu("back")
end

function uiManager.onSettingsSave()
    widgets.handleNonTextBoxClick()
    if uiManager.moduleReady.settingsMenu then settingsMenu.onSave() end
end

function uiManager.onSettingsApply()
    widgets.handleNonTextBoxClick()
    if uiManager.moduleReady.settingsMenu then settingsMenu.onApply() end
end

function uiManager.onCancel()
    widgets.handleNonTextBoxClick()
    uiManager.showMenu("back")
end

function uiManager.onOpenArena()
    uiManager.showMenu("test_arena")
end
