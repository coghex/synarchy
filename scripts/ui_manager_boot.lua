-- UI Manager: boot/lifecycle (#544 split from ui_manager.lua).
--
-- Owns font/texture readiness, the loading-screen boot sequence (both
-- normal and arena boot profiles), lazy per-module initialization (the
-- moduleReady/ensureX pattern), per-frame polling (update — widget
-- ticks, startup-loader handoff), and the framebuffer-resize
-- broadcast. moduleReady/startupBootDone/currentMenu/bootProfile/
-- fbW/fbH live on the uiManager singleton (scripts/ui_manager.lua)
-- since other split modules read them too; font/texture handles are
-- private to this file.
local uiManager = package.loaded["scripts.ui_manager"]

local boxTextures = require("scripts.ui.box_textures")
local textbox   = require("scripts.ui.textbox")
local checkbox  = require("scripts.ui.checkbox")
local button    = require("scripts.ui.button")
local dropdown  = require("scripts.ui.dropdown")
local scrollbar = require("scripts.ui.scrollbar")
local tabbar    = require("scripts.ui.tabbar")
local slider    = require("scripts.ui.slider")
local randbox   = require("scripts.ui.randbox")
local toggle    = require("scripts.ui.toggle")
local uiList    = require("scripts.ui.list")
local focusIndicator = require("scripts.ui.focus_indicator")

local loadingScreen = require("scripts.loading_screen")
local popup          = require("scripts.popup")
local eventLog       = require("scripts.event_log")
local combatLog      = require("scripts.combat_log")
local injuryLog      = require("scripts.injury_log_panel")
local unitLog        = require("scripts.unit_log")
local contextMenu    = require("scripts.ui.context_menu")

local mainMenu        = require("scripts.main_menu")
local settingsMenu    = require("scripts.settings_menu")
local createWorldMenu = require("scripts.create_world_menu")
local worldManager    = require("scripts.world_manager")
local worldView       = require("scripts.world_view")
local saveBrowser     = require("scripts.save_browser")
local hud             = require("scripts.hud")
local testArena       = require("scripts.test_arena")
local pauseMenu       = require("scripts.pause_menu")
local buildToolRemoteWarning = require("scripts.build_tool_remote_warning")

local boxTexSet = nil
local btnTexSet = nil
local menuFont = nil
local titleFont = nil
local uiscale = 1.0

local fontsLoaded = {
    menu = false,
    title = false
}
local fontsReady = false
local menuFontHandle = nil
local titleFontHandle = nil
local initialized = false
local didInit = false

function uiManager.init(scriptId)
    engine.logDebug("UI Manager initializing...")

    button.init()
    focusIndicator.init()
    scrollbar.init()
    tabbar.init()
    slider.init()
    randbox.init()
    toggle.init()
    uiList.init()
    -- These load their own widget textures. They used to be initialized
    -- only by settingsMenu.init, which the eager boot ran at startup; once
    -- menu init went lazy, a menu that uses them BEFORE Settings is ever
    -- opened (e.g. Create World from an --arena boot) got nil texture sets
    -- and rendered invisible widgets. Init them here with the rest.
    textbox.init()
    checkbox.init()
    dropdown.init()
    uiscale = engine.getUIScale()

    menuFontHandle = engine.loadFont("assets/fonts/arcade.ttf", 24)
    titleFontHandle = engine.loadFont("assets/fonts/gothic.ttf", 96)
    menuFont = menuFontHandle
    titleFont = titleFontHandle

    boxTexSet = boxTextures.load("assets/textures/ui/box", "box")
    btnTexSet = boxTextures.load("assets/textures/ui/button", "button")

    settingsMenu.setShowMenuCallback(function(menuName, params)
        uiManager.showMenu(menuName, params)
    end)

    createWorldMenu.setShowMenuCallback(function(menuName, params)
        uiManager.showMenu(menuName, params)
    end)

    mainMenu.setShowMenuCallback(function(menuName, params)
        uiManager.showMenu(menuName, params)
    end)

    saveBrowser.setShowMenuCallback(function(menuName, params)
        uiManager.showMenu(menuName, params)
    end)

    loadingScreen.setShowMenuCallback(function(menuName, params)
        uiManager.showMenu(menuName, params)
    end)
    testArena.setShowMenuCallback(function(menuName, params)
        uiManager.showMenu(menuName, params)
    end)
    pauseMenu.setShowMenuCallback(function(menuName, params)
        uiManager.showMenu(menuName, params)
    end)

    uiManager.bootProfile = engine.getBootProfile and engine.getBootProfile() or "normal"
    didInit = true
    engine.logDebug("UI Manager waiting for assets...")
end

function uiManager.onAssetLoaded(assetType, handle, path)
    if assetType == "font" then
        if handle == menuFontHandle then
            fontsLoaded.menu = true
        elseif handle == titleFontHandle then
            fontsLoaded.title = true
        end

        if fontsLoaded.menu and fontsLoaded.title then
            fontsReady = true
            uiManager.checkReady()
        end
    end
    if worldView.onAssetLoaded then
        worldView.onAssetLoaded(assetType, handle, path)
    end
    if testArena.onAssetLoaded then
        testArena.onAssetLoaded(assetType, handle, path)
    end
end

function uiManager.checkReady()
    if fontsReady and uiManager.fbW > 0 and uiManager.fbH > 0 then
        if not initialized then
            local startupLoader = require("scripts.startup_loader")
            uiManager.ensureLoadingScreen()
            if uiManager.bootProfile == "arena" then
                startupLoader.build("arena")
                startupLoader.runAll()
                initialized = true
                uiManager.startupBootDone = true
                uiManager.finishArenaBoot()
                return
            end
            -- Boot the loading screen first so the user sees a green
            -- progress bar while startup_loader drains the asset queue.
            -- The rest of the per-module init runs in finishStartupBoot
            -- once startup_loader is done.
            startupLoader.build("normal")
            loadingScreen.show({mode = "startup", fbW = uiManager.fbW, fbH = uiManager.fbH})
            initialized = true
        else
            uiManager.showMenu(uiManager.currentMenu)
        end
    end
end

function uiManager.ensureLoadingScreen()
    if uiManager.moduleReady.loadingScreen then return end
    loadingScreen.init(boxTexSet, menuFont, uiManager.fbW, uiManager.fbH)
    uiManager.moduleReady.loadingScreen = true
end

function uiManager.ensureTooltipStyle()
    if uiManager.moduleReady.tooltipStyle then return end
    local persistedDwell = engine.getTooltipDwellMs()
    local persistedHintDelay = engine.getTooltipHintDelayMs()
    local whitePixelTex = engine.loadTexture(
        "assets/textures/utility/white.png")
    UI.setTooltipStyle({
        font               = menuFont,
        fontSize           = 14,
        textColor          = {1.0, 1.0, 1.0, 1.0},
        bgColor            = {1.0, 1.0, 1.0, 1.0},
        padding            = 24,
        boxTextures        = boxTexSet,
        boxTileSize        = 32,
        mouseOffsetX       = 14,
        mouseOffsetY       = 18,
        dwellMs            = persistedDwell,
        hintDelayMs        = persistedHintDelay,
        spriteGap          = 4,
        hintFontSize       = 11,
        hintColor          = {0.7, 0.7, 0.7, 1.0},
        separatorTexture   = whitePixelTex,
        separatorColor     = {0.7, 0.7, 0.7, 1.0},
        separatorThickness = 2,
    })
    uiManager.moduleReady.tooltipStyle = true
end

function uiManager.ensureMainMenu()
    if uiManager.moduleReady.mainMenu then return end
    mainMenu.init(boxTexSet, btnTexSet, menuFont, titleFont, uiManager.fbW, uiManager.fbH)
    uiManager.moduleReady.mainMenu = true
end

function uiManager.ensureSettingsMenu()
    if uiManager.moduleReady.settingsMenu then return end
    settingsMenu.init(boxTexSet, btnTexSet, menuFont, uiManager.fbW, uiManager.fbH)
    uiManager.moduleReady.settingsMenu = true
end

function uiManager.ensureCreateWorldMenu()
    if uiManager.moduleReady.createWorldMenu then return end
    createWorldMenu.init(boxTexSet, btnTexSet, menuFont, uiManager.fbW, uiManager.fbH)
    uiManager.moduleReady.createWorldMenu = true
end

function uiManager.ensureWorldView()
    if uiManager.moduleReady.worldView then return end
    worldView.init(uiManager.fbW, uiManager.fbH)
    uiManager.moduleReady.worldView = true
end

function uiManager.ensureHUD()
    if uiManager.moduleReady.hud then return end
    hud.init(boxTexSet, menuFont, uiManager.fbW, uiManager.fbH)
    uiManager.moduleReady.hud = true
end

function uiManager.ensureSaveBrowser()
    if uiManager.moduleReady.saveBrowser then return end
    saveBrowser.init(boxTexSet, btnTexSet, menuFont, uiManager.fbW, uiManager.fbH)
    uiManager.moduleReady.saveBrowser = true
end

function uiManager.ensureTestArena()
    if uiManager.moduleReady.testArena then return end
    testArena.init(boxTexSet, menuFont, titleFont, uiManager.fbW, uiManager.fbH)
    uiManager.moduleReady.testArena = true
end

function uiManager.ensurePauseMenu()
    if uiManager.moduleReady.pauseMenu then return end
    pauseMenu.init(boxTexSet, btnTexSet, menuFont, titleFont, uiManager.fbW, uiManager.fbH)
    uiManager.moduleReady.pauseMenu = true
end

function uiManager.ensureBuildToolRemoteWarning()
    if uiManager.moduleReady.buildToolRemoteWarning then return end
    buildToolRemoteWarning.init(boxTexSet, menuFont, titleFont, uiManager.fbW, uiManager.fbH)
    uiManager.moduleReady.buildToolRemoteWarning = true
end

function uiManager.ensurePopupsAndLogs()
    if uiManager.moduleReady.popupsAndLogs then return end
    popup.bootstrap(boxTexSet, btnTexSet, menuFont, uiManager.fbW, uiManager.fbH)
    eventLog.bootstrap(boxTexSet, btnTexSet, menuFont, uiManager.fbW, uiManager.fbH)
    combatLog.bootstrap(boxTexSet, btnTexSet, menuFont, uiManager.fbW, uiManager.fbH)
    injuryLog.bootstrap(boxTexSet, btnTexSet, menuFont, uiManager.fbW, uiManager.fbH)
    unitLog.bootstrap(boxTexSet, btnTexSet, menuFont, uiManager.fbW, uiManager.fbH)
    uiManager.moduleReady.popupsAndLogs = true
end

function uiManager.ensureGameplayUI()
    uiManager.ensureTooltipStyle()
    uiManager.ensureHUD()
    uiManager.ensurePauseMenu()
    uiManager.ensureBuildToolRemoteWarning()
    uiManager.ensurePopupsAndLogs()
end

-- Per-module init pass that used to live inline in checkReady.
-- Called once when startup_loader drains (see uiManager.update).
function uiManager.finishStartupBoot()
    uiManager.ensureTooltipStyle()
    uiManager.ensureMainMenu()
    uiManager.showMenu("main")
end

function uiManager.finishArenaBoot()
    uiManager.ensureTooltipStyle()
    uiManager.ensureGameplayUI()
    uiManager.ensureTestArena()
    uiManager.showMenu("test_arena")
end

function uiManager.onFramebufferResize(width, height)
    uiManager.fbW = width
    uiManager.fbH = height

    if not initialized then
        uiManager.checkReady()
        return
    end

    if uiManager.moduleReady.mainMenu then mainMenu.onFramebufferResize(width, height) end
    if uiManager.moduleReady.settingsMenu then settingsMenu.onFramebufferResize(width, height) end
    if uiManager.moduleReady.createWorldMenu then createWorldMenu.onFramebufferResize(width, height) end
    if uiManager.moduleReady.worldView then worldView.onFramebufferResize(width, height) end
    if uiManager.moduleReady.hud then hud.onFramebufferResize(width, height) end
    if uiManager.moduleReady.saveBrowser then saveBrowser.onFramebufferResize(width, height) end
    if uiManager.moduleReady.loadingScreen then loadingScreen.onFramebufferResize(width, height) end
    if uiManager.moduleReady.popupsAndLogs then
        popup.onFramebufferResize(width, height)
        eventLog.onFramebufferResize(width, height)
        combatLog.onFramebufferResize(width, height)
        injuryLog.onFramebufferResize(width, height)
        unitLog.onFramebufferResize(width, height)
    end
    -- #747: an open context menu/submenu is a singleton popup with no
    -- moduleReady gate of its own (lazily inits via ensureReady in
    -- cm.show) — a no-op when nothing is open, so it's always safe to
    -- forward.
    contextMenu.onFramebufferResize(width, height)
    if uiManager.moduleReady.pauseMenu then pauseMenu.onFramebufferResize(width, height) end
    if uiManager.moduleReady.buildToolRemoteWarning then
        buildToolRemoteWarning.onFramebufferResize(width, height)
    end

    local currentMenu = uiManager.currentMenu
    if currentMenu == "main" then
        if mainMenu.page then UI.showPage(mainMenu.page) end
    elseif currentMenu == "settings" then
        if settingsMenu.page then UI.showPage(settingsMenu.page) end
    elseif currentMenu == "create_world" then
        if createWorldMenu.page then UI.showPage(createWorldMenu.page) end
    elseif currentMenu == "world_view" then
        if worldView.page then UI.showPage(worldView.page) end
        if hud.page then UI.showPage(hud.page) end
    elseif currentMenu == "save_browser" then
        if saveBrowser.page then UI.showPage(saveBrowser.page) end
    elseif currentMenu == "test_arena_view" then
        if testArena.page then UI.showPage(testArena.page) end
    end
end

function uiManager.update(dt)
    textbox.update(dt)
    dropdown.update(dt)
    worldManager.update(dt)

    if uiManager.moduleReady.worldView then
        worldView.update(dt)
    end

    -- Create world menu update (generation polling)
    if uiManager.moduleReady.createWorldMenu then
        createWorldMenu.update(dt)
    end

    if uiManager.moduleReady.hud then
        hud.update(dt)
    end

    if uiManager.moduleReady.loadingScreen then
        loadingScreen.update(dt)
    end

    -- Startup-loader transition: when the asset queue drains, run the
    -- deferred per-module init pass and pop the main menu. The
    -- finishStartupBoot guard prevents re-entry.
    if loadingScreen.mode == "startup"
       and loadingScreen.phase == "done"
       and not uiManager.startupBootDone then
        uiManager.startupBootDone = true
        uiManager.finishStartupBoot()
    end

    if uiManager.moduleReady.testArena then
        testArena.update(dt)
    end

    -- slider drag detection
    if slider.getDraggingId() then
        local mx, my = engine.getMousePosition()
        if mx and my then
            local ww, wh = engine.getWindowSize()
            if ww and wh and ww > 0 and wh > 0 then
                mx = mx * (uiManager.fbW / ww)
                my = my * (uiManager.fbH / wh)
            end
            slider.onDragMove(mx, my)
        end
    end

    -- Scrollbar tab drag — same pattern as the slider: poll the
    -- module's draggingId each frame and feed it the cursor in
    -- framebuffer coords. uiManager.onMouseUp (scripts/ui_manager_widgets.lua)
    -- clears the state.
    if scrollbar.getDraggingId() then
        local mx, my = engine.getMousePosition()
        if mx and my then
            local ww, wh = engine.getWindowSize()
            if ww and wh and ww > 0 and wh > 0 then
                mx = mx * (uiManager.fbW / ww)
                my = my * (uiManager.fbH / wh)
            end
            scrollbar.onDragMove(mx, my)
        end
    end

    -- Hover detection with coordinate scaling
    local mx, my = engine.getMousePosition()
    if mx and my then
        local ww, wh = engine.getWindowSize()
        if ww and wh and ww > 0 and wh > 0 then
            mx = mx * (uiManager.fbW / ww)
            my = my * (uiManager.fbH / wh)
        end

        local elem, cb = UI.findHoverTarget(mx, my)

        if elem ~= uiManager.hoveredElement then
            if uiManager.hoveredElement and uiManager.hoveredCallback then
                uiManager.onHoverLeave(uiManager.hoveredElement, uiManager.hoveredCallback)
            end

            uiManager.hoveredElement = elem
            uiManager.hoveredCallback = cb
            if elem and cb then
                uiManager.onHoverEnter(elem, cb)
            end
        end
    end
end

function uiManager.shutdown()
    if not didInit then return end
    mainMenu.shutdown()
    settingsMenu.shutdown()
    createWorldMenu.shutdown()
    worldView.shutdown()
    hud.shutdown()
    saveBrowser.shutdown()
    loadingScreen.shutdown()
    testArena.shutdown()
    pauseMenu.shutdown()
    buildToolRemoteWarning.shutdown()
    popup.shutdown()
    eventLog.shutdown()
    combatLog.shutdown()
    injuryLog.shutdown()
    unitLog.shutdown()
end
