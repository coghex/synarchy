-- UI Manager - coordinates all UI pages
local uiManager = {}

local boxTextures = require("scripts.ui.box_textures")
local boxTexSet = nil
local btnTexSet = nil
local menuFont = nil
local titleFont = nil
local fbW, fbH = 0, 0
local uiscale = 1.0

local fontsLoaded = {
    menu = false,
    title = false
}
local fontsReady = false
local menuFontHandle = nil
local titleFontHandle = nil
local initialized = false

local mainMenu = nil
local settingsMenu = nil
local createWorldMenu = nil
local worldView = nil
local saveBrowser = nil
local worldManager = nil
local textbox = nil
local checkbox = nil
local button = nil
local dropdown = nil
local scrollbar = nil
local tabbar = nil
local slider = nil
local randbox = nil
local toggle = nil
local uiList = nil
local loadingScreen = nil
local contextMenu = require("scripts.ui.context_menu")
local testArena = nil
local pauseMenu = nil
local popup = nil
local eventLog = nil
local combatLog = nil
local injuryLog = nil
local unitLog = nil

local hoveredElement = nil
local hoveredCallback = nil

local hud = nil

local currentMenu = "main"
local previousMenu = nil

local function handleNonTextBoxClick()
    if textbox then
        textbox.unfocusAll()
    end
    if randbox then
        randbox.unfocusAll()
    end
    -- Dropdowns keep their own focused flag; missing them here left
    -- dd.focused == true after the engine-side focus moved on, and the
    -- input dispatch below routes by dd.focused first — so keystrokes
    -- meant for a later-focused textbox landed in the stale dropdown.
    if dropdown then
        dropdown.unfocusAll()
    end
end

function uiManager.init(scriptId)
    engine.logDebug("UI Manager initializing...")
    
    textbox = require("scripts.ui.textbox")
    checkbox = require("scripts.ui.checkbox")
    button = require("scripts.ui.button")
    dropdown = require("scripts.ui.dropdown")
    scrollbar = require("scripts.ui.scrollbar")
    tabbar = require("scripts.ui.tabbar")
    slider = require("scripts.ui.slider")
    randbox = require("scripts.ui.randbox")
    toggle = require("scripts.ui.toggle")
    uiList = require("scripts.ui.list")
    loadingScreen = require("scripts.loading_screen")
    popup = require("scripts.popup")
    eventLog = require("scripts.event_log")
    combatLog = require("scripts.combat_log")
    injuryLog = require("scripts.injury_log_panel")
    unitLog = require("scripts.unit_log")

    button.init()
    scrollbar.init()
    tabbar.init()
    slider.init()
    randbox.init()
    toggle.init()
    uiList.init()
    uiscale = engine.getUIScale()
    
    menuFontHandle = engine.loadFont("assets/fonts/arcade.ttf", 24)
    titleFontHandle = engine.loadFont("assets/fonts/gothic.ttf", 96)
    menuFont = menuFontHandle
    titleFont = titleFontHandle
    
    boxTexSet = boxTextures.load("assets/textures/box", "box")
    btnTexSet = boxTextures.load("assets/textures/ui/button", "button")
    
    mainMenu = require("scripts.main_menu")
    settingsMenu = require("scripts.settings_menu")
    createWorldMenu = require("scripts.create_world_menu")
    worldManager = require("scripts.world_manager")
    worldView = require("scripts.world_view")
    saveBrowser = require("scripts.save_browser")
    hud = require("scripts.hud")
    testArena = require("scripts.test_arena")
    pauseMenu = require("scripts.pause_menu")
    
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
    if worldView and worldView.onAssetLoaded then
        worldView.onAssetLoaded(assetType, handle, path)
    end
    if testArena and testArena.onAssetLoaded then
        testArena.onAssetLoaded(assetType, handle, path)
    end
end

function uiManager.checkReady()
    if fontsReady and fbW > 0 and fbH > 0 then
        if not initialized then
            -- Boot the loading screen first so the user sees a green
            -- progress bar while startup_loader drains the asset queue.
            -- The rest of the per-module init runs in finishStartupBoot
            -- once startup_loader is done.
            loadingScreen.init(boxTexSet, menuFont, fbW, fbH)
            local startupLoader = require("scripts.startup_loader")
            startupLoader.build()
            loadingScreen.show({mode = "startup", fbW = fbW, fbH = fbH})
            initialized = true
        else
            uiManager.showMenu(currentMenu)
        end
    end
end

-- Per-module init pass that used to live inline in checkReady.
-- Called once when startup_loader drains (see uiManager.update).
function uiManager.finishStartupBoot()
    mainMenu.init(boxTexSet, btnTexSet, menuFont, titleFont, fbW, fbH)
    settingsMenu.init(boxTexSet, btnTexSet, menuFont, fbW, fbH)
    createWorldMenu.init(boxTexSet, btnTexSet, menuFont, fbW, fbH)
    worldView.init(fbW, fbH)
    hud.init(boxTexSet, menuFont, fbW, fbH)
    saveBrowser.init(boxTexSet, btnTexSet, menuFont, fbW, fbH)
    testArena.init(boxTexSet, menuFont, titleFont, fbW, fbH)
    pauseMenu.init(boxTexSet, btnTexSet, menuFont, titleFont, fbW, fbH)
    popup.bootstrap(boxTexSet, btnTexSet, menuFont, fbW, fbH)
    eventLog.bootstrap(boxTexSet, btnTexSet, menuFont, fbW, fbH)
    combatLog.bootstrap(boxTexSet, btnTexSet, menuFont, fbW, fbH)
    injuryLog.bootstrap(boxTexSet, btnTexSet, menuFont, fbW, fbH)
    unitLog.bootstrap(boxTexSet, btnTexSet, menuFont, fbW, fbH)
    local persistedDwell = engine.getTooltipDwellMs()
    local persistedHintDelay = engine.getTooltipHintDelayMs()
    -- engine.loadTexture caches by path, so this returns the
    -- same handle that drag_select etc. already use for the
    -- 1×1 white pixel — letting the separator render in the
    -- exact colour we tint it, instead of inheriting the box
    -- centre tile.
    local whitePixelTex = engine.loadTexture(
        "assets/textures/hud/utility/white.png")
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
    uiManager.showMenu("main")
end

function uiManager.onFramebufferResize(width, height)
    fbW = width
    fbH = height
    
    if not initialized then
        uiManager.checkReady()
        return
    end
    
    if mainMenu then mainMenu.onFramebufferResize(width, height) end
    if settingsMenu then settingsMenu.onFramebufferResize(width, height) end
    if createWorldMenu then createWorldMenu.onFramebufferResize(width, height) end
    if worldView then worldView.onFramebufferResize(width, height) end
    if hud then hud.onFramebufferResize(width, height) end
    if saveBrowser then saveBrowser.onFramebufferResize(width, height) end
    if loadingScreen then loadingScreen.onFramebufferResize(width, height) end
    if popup then popup.onFramebufferResize(width, height) end
    if eventLog then eventLog.onFramebufferResize(width, height) end
    if combatLog then combatLog.onFramebufferResize(width, height) end
    if injuryLog then injuryLog.onFramebufferResize(width, height) end
    if unitLog then unitLog.onFramebufferResize(width, height) end
    if pauseMenu then pauseMenu.onFramebufferResize(width, height) end
    
    if currentMenu == "main" then
        if mainMenu and mainMenu.page then UI.showPage(mainMenu.page) end
    elseif currentMenu == "settings" then
        if settingsMenu and settingsMenu.page then UI.showPage(settingsMenu.page) end
    elseif currentMenu == "create_world" then
        if createWorldMenu and createWorldMenu.page then UI.showPage(createWorldMenu.page) end
    elseif currentMenu == "world_view" then
        if worldView and worldView.page then UI.showPage(worldView.page) end
        if hud and hud.page then UI.showPage(hud.page) end
    elseif currentMenu == "save_browser" then
        if saveBrowser and saveBrowser.page then UI.showPage(saveBrowser.page) end
    elseif currentMenu == "test_arena_view" then
        if testArena and testArena.page then UI.showPage(testArena.page) end
    end 
end

function uiManager.showMenu(menuName, params)
    -- Handle "back" by going to previousMenu (or main if none)
    if menuName == "back" then
        menuName = previousMenu or "main"
    end

    previousMenu = currentMenu
    currentMenu = menuName

    -- When opening settings from a game view, keep the world visible behind
    local keepWorld = (menuName == "settings")
        and (previousMenu == "world_view" or previousMenu == "test_arena_view")

    mainMenu.hide()
    settingsMenu.hide()
    createWorldMenu.hide()
    if not keepWorld then
        worldView.hide()
        hud.hide()
    end
    if saveBrowser then saveBrowser.hide() end
    if loadingScreen then loadingScreen.hide() end
    if testArena and not keepWorld then testArena.hide() end
    if pauseMenu then pauseMenu.hide() end

    if menuName == "main" then
        mainMenu.show()
    elseif menuName == "settings" then
        settingsMenu.show()
    elseif menuName == "create_world" then
        createWorldMenu.show()
    elseif menuName == "world_view" then
        worldView.show()
        hud.worldId = "main_world"
        hud.show()
        -- Re-show pause menu if returning from settings (opened via pause menu)
        if previousMenu == "settings" then
            if pauseMenu then pauseMenu.show() end
        end
    elseif menuName == "save_browser" then
        saveBrowser.show(mainMenu.saves, function(saveName)
            mainMenu.loadAndShowSave(saveName)
        end, function()
            uiManager.showMenu("main")
        end)
    elseif menuName == "loading" then
        params = params or {}
        params.fbW = fbW
        params.fbH = fbH
        loadingScreen.show(params)
    elseif menuName == "test_arena" then
        testArena.show()    -- this queues creation + shows loading screen
    elseif menuName == "test_arena_view" then
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
            if pauseMenu then pauseMenu.show() end
        end
    end
end

function uiManager.onListItemClick(elemHandle)
    handleNonTextBoxClick()
    if uiList then
        return uiList.handleCallback("onListItemClick", elemHandle)
    end
    return false
end

function uiManager.onSaveBrowserBack(elemHandle)
    handleNonTextBoxClick()
    if saveBrowser and saveBrowser.onBackCallback then
        saveBrowser.onBackCallback()
        return true
    end
    uiManager.showMenu("main")
    return true
end

function uiManager.onCreateWorld()
    handleNonTextBoxClick()
    uiManager.showMenu("create_world")
end

function uiManager.onArenaReady(pageId)
    if pageId == "test_arena" and testArena then
        engine.logInfo("Arena ready, switching to test arena view")
        testArena.ready = true
        -- Full menu transition: hides main menu, loading screen, everything
        uiManager.showMenu("test_arena_view")
    end
end

function uiManager.onMainMenuItem(elemHandle)
    handleNonTextBoxClick()
    if mainMenu then
        return mainMenu.handleClick(elemHandle)
    end
    return false
end

function uiManager.onPauseMenuItem(elemHandle)
    handleNonTextBoxClick()
    if pauseMenu then
        return pauseMenu.handleClick(elemHandle)
    end
    return false
end

function uiManager.onSettings()
    handleNonTextBoxClick()
    uiManager.showMenu("settings")
end

function uiManager.onQuit()
    engine.quit()
end

function uiManager.onSettingsBack()
    handleNonTextBoxClick()
    if settingsMenu then settingsMenu.onBack() end
    uiManager.showMenu("back")
end

function uiManager.onSettingsSave()
    handleNonTextBoxClick()
    if settingsMenu then settingsMenu.onSave() end
end

function uiManager.onSettingsApply()
    handleNonTextBoxClick()
    if settingsMenu then settingsMenu.onApply() end
end

function uiManager.onCancel()
    handleNonTextBoxClick()
    uiManager.showMenu("back")
end

function uiManager.update(dt)
    if textbox then
        textbox.update(dt)
    end
    if dropdown then
        dropdown.update(dt)
    end
    
    if worldManager then
        worldManager.update(dt)
    end
    
    if worldView then
        worldView.update(dt)
    end

    -- Create world menu update (generation polling)
    if createWorldMenu then
        createWorldMenu.update(dt)
    end

    if hud then
        hud.update(dt)
    end

    if loadingScreen then
        loadingScreen.update(dt)
    end

    -- Startup-loader transition: when the asset queue drains, run the
    -- deferred per-module init pass and pop the main menu. The
    -- finishStartupBoot guard prevents re-entry.
    if loadingScreen and loadingScreen.mode == "startup"
       and loadingScreen.phase == "done"
       and not uiManager.startupBootDone then
        uiManager.startupBootDone = true
        uiManager.finishStartupBoot()
    end

    if testArena then
        testArena.update(dt)
    end

    -- slider drag detection
    if slider and slider.getDraggingId() then
        local mx, my = engine.getMousePosition()
        if mx and my then
            local ww, wh = engine.getWindowSize()
            if ww and wh and ww > 0 and wh > 0 then
                mx = mx * (fbW / ww)
                my = my * (fbH / wh)
            end
            slider.onDragMove(mx, my)
        end
    end

    -- Scrollbar tab drag — same pattern as the slider: poll the
    -- module's draggingId each frame and feed it the cursor in
    -- framebuffer coords. onMouseUp (down below) clears the state.
    if scrollbar and scrollbar.getDraggingId() then
        local mx, my = engine.getMousePosition()
        if mx and my then
            local ww, wh = engine.getWindowSize()
            if ww and wh and ww > 0 and wh > 0 then
                mx = mx * (fbW / ww)
                my = my * (fbH / wh)
            end
            scrollbar.onDragMove(mx, my)
        end
    end
    
    -- Hover detection with coordinate scaling
    local mx, my = engine.getMousePosition()
    if mx and my then
        local ww, wh = engine.getWindowSize()
        if ww and wh and ww > 0 and wh > 0 then
            mx = mx * (fbW / ww)
            my = my * (fbH / wh)
        end
        
        local elem, cb = UI.findHoverTarget(mx, my)
        
        if elem ~= hoveredElement then
            if hoveredElement and hoveredCallback then
                uiManager.onHoverLeave(hoveredElement, hoveredCallback)
            end
            
            hoveredElement = elem
            hoveredCallback = cb
            if elem and cb then
                uiManager.onHoverEnter(elem, cb)
            end
        end
    end
end

function uiManager.onMouseDown(button_num, x, y)
    -- Defer to the debug overlay's parallel hit-test first. It runs
    -- independently of the UI manager and is idempotent across all
    -- onMouseDown subscribers, so re-calling here is safe.
    local debugOverlay = require("scripts.debug")
    if debugOverlay.tryClaimClick(button_num, x, y) then
        return
    end

    local ww, wh = engine.getWindowSize()
    local sx, sy = x, y
    if ww and wh and ww > 0 and wh > 0 then
        sx = x * (fbW / ww)
        sy = y * (fbH / wh)
    end
    if dropdown then
        dropdown.onClickOutside(sx, sy)
    end
    if randbox then
        randbox.onClickOutside(sx, sy)
    end
    if hud then
        hud.onMouseDown(button_num, sx, sy)
    end
end

function uiManager.onHoverEnter(elemHandle, callbackName)
    if button and callbackName == "onButtonClick" then
        button.onHoverEnter(elemHandle)
    elseif checkbox and callbackName == "onCheckboxClick" then
        checkbox.onHoverEnter(elemHandle)
    elseif textbox and textbox.isTextBoxCallback(callbackName) then
        textbox.onHoverEnter(elemHandle)
    elseif dropdown and dropdown.isDropdownCallback(callbackName) then
        dropdown.onHoverEnter(elemHandle)
    elseif slider and slider.isSliderCallback(callbackName) then
        slider.onHoverEnter(elemHandle)
    elseif toggle and toggle.isToggleCallback(callbackName) then
        toggle.onHoverEnter(elemHandle)
    elseif randbox and randbox.isRandBoxCallback(callbackName) then
        randbox.onHoverEnter(elemHandle)
    elseif uiList and uiList.isListCallback(callbackName) then
        uiList.onHoverEnter(elemHandle)
    elseif contextMenu and contextMenu.isContextMenuCallback(callbackName) then
        contextMenu.onHoverEnter(elemHandle)
    end
end

function uiManager.onHoverLeave(elemHandle, callbackName)
    if button and callbackName == "onButtonClick" then
        button.onHoverLeave(elemHandle)
    elseif checkbox and callbackName == "onCheckboxClick" then
        checkbox.onHoverLeave(elemHandle)
    elseif textbox and textbox.isTextBoxCallback(callbackName) then
        textbox.onHoverLeave(elemHandle)
    elseif dropdown and dropdown.isDropdownCallback(callbackName) then
        dropdown.onHoverLeave(elemHandle)
    elseif slider and slider.isSliderCallback(callbackName) then
        slider.onHoverLeave(elemHandle)
    elseif toggle and toggle.isToggleCallback(callbackName) then
        toggle.onHoverLeave(elemHandle)
    elseif randbox and randbox.isRandBoxCallback(callbackName) then
        randbox.onHoverLeave(elemHandle)
    elseif uiList and uiList.isListCallback(callbackName) then
        uiList.onHoverLeave(elemHandle)
    elseif contextMenu and contextMenu.isContextMenuCallback(callbackName) then
        contextMenu.onHoverLeave(elemHandle)
    end
end

function uiManager.shutdown()
    if mainMenu then mainMenu.shutdown() end
    if settingsMenu then settingsMenu.shutdown() end
    if createWorldMenu then createWorldMenu.shutdown() end
    if worldView then worldView.shutdown() end
    if hud then hud.shutdown() end
    if saveBrowser then saveBrowser.shutdown() end
    if loadingScreen then loadingScreen.shutdown() end
    if testArena then testArena.shutdown() end
    if pauseMenu then pauseMenu.shutdown() end
    if popup then popup.shutdown() end
    if eventLog then eventLog.shutdown() end
    if combatLog then combatLog.shutdown() end
    if injuryLog then injuryLog.shutdown() end
    if unitLog then unitLog.shutdown() end
end

function uiManager.onTextBoxClick(elemHandle)
    -- Clear the other widget families (textbox.focus handles its own
    -- family) so their focused flags can't go stale and steal keys.
    if dropdown then dropdown.unfocusAll() end
    if randbox then randbox.unfocusAll() end
    if textbox then
        textbox.handleClickByElement(elemHandle)
    end
end

function uiManager.onCheckboxClick(elemHandle)
    handleNonTextBoxClick()
    if checkbox then
        return checkbox.handleClickByElement(elemHandle)
    end
    return false
end

function uiManager.onButtonClick(elemHandle)
    handleNonTextBoxClick()
    if button then
        return button.handleClickByElement(elemHandle)
    end
    return false
end

-- Dispatched when a row in the event-log panel is clicked. Routes
-- to event_log.onRowClick which re-pops the popup for that entry.
function uiManager.onEventLogRowClick(elemHandle)
    if eventLog and eventLog.onRowClick then
        return eventLog.onRowClick(elemHandle)
    end
    return false
end

-- Combat-log panel click routes. Tab clicks (including the pinned
-- "All" tab) re-render content for the selected battle. The scroll
-- buttons shift the battle-tab strip one cell at a time.
function uiManager.onCombatLogTabClick(elemHandle)
    if combatLog and combatLog.onTabClick then
        return combatLog.onTabClick(elemHandle)
    end
    return false
end

function uiManager.onCombatLogScrollPrev(elemHandle)
    if combatLog and combatLog.onScrollPrev then
        return combatLog.onScrollPrev()
    end
    return false
end

function uiManager.onCombatLogScrollNext(elemHandle)
    if combatLog and combatLog.onScrollNext then
        return combatLog.onScrollNext()
    end
    return false
end

-- Injury-log panel click routes (mirror the combat-log ones; tabs are
-- per injured unit).
function uiManager.onInjuryLogTabClick(elemHandle)
    if injuryLog and injuryLog.onTabClick then
        return injuryLog.onTabClick(elemHandle)
    end
    return false
end

function uiManager.onInjuryLogScrollPrev(elemHandle)
    if injuryLog and injuryLog.onScrollPrev then
        return injuryLog.onScrollPrev()
    end
    return false
end

function uiManager.onInjuryLogScrollNext(elemHandle)
    if injuryLog and injuryLog.onScrollNext then
        return injuryLog.onScrollNext()
    end
    return false
end

-- Per-unit log panel: tab clicks switch the All/Event/Combat/Injury view.
function uiManager.onUnitLogTabClick(elemHandle)
    if unitLog and unitLog.onTabClick then
        return unitLog.onTabClick(elemHandle)
    end
    return false
end

-- Dispatched when a popup line is clicked. Routes to popup.onLineClick
-- which cycles the camera through the line's stored coords.
function uiManager.onPopupLineClick(elemHandle)
    if popup and popup.onLineClick then
        return popup.onLineClick(elemHandle)
    end
    return false
end

-- Dispatched when a popup's title-bar mute-toggle icon is clicked.
-- Routes to popup.onMuteToggleClick, which toggles the category's
-- `popup` notification setting via setNotificationOverrides.
function uiManager.onPopupMuteToggleClick(elemHandle)
    if popup and popup.onMuteToggleClick then
        return popup.onMuteToggleClick(elemHandle)
    end
    return false
end

function uiManager.onTabClick(elemHandle)
    if tabbar then
        return tabbar.handleCallback("onTabClick", elemHandle)
    end
    return false
end

function uiManager.onDropdownClick(elemHandle)
    handleNonTextBoxClick()
    if dropdown then
        return dropdown.handleCallback("onDropdownClick", elemHandle)
    end
    return false
end

function uiManager.onDropdownOptionClick(elemHandle)
    handleNonTextBoxClick()
    if dropdown then
        return dropdown.handleCallback("onDropdownOptionClick", elemHandle)
    end
    return false
end

function uiManager.onRandBoxClick(elemHandle)
    handleNonTextBoxClick()
    if randbox then
        return randbox.handleCallback("onRandBoxClick", elemHandle)
    end
    return false
end

function uiManager.onRandomizeClick(elemHandle)
    handleNonTextBoxClick()
    if randbox then
        return randbox.handleCallback("onRandomizeClick", elemHandle)
    end
    return false
end

function uiManager.onToggleClick(elemHandle)
    if toggle then
        return toggle.handleClickByElement(elemHandle)
    end
    return false
end

-- Unit-info v2 sprite-tab strip. Each tab's box has this callback
-- registered via UI.setOnClick; clicks make that unit the active one.
function uiManager.onUnitInfoTabClick(elemHandle)
    local mod = package.loaded["scripts.unit_info_v2"]
    if mod and mod.handleTabClick then
        return mod.handleTabClick(elemHandle)
    end
    return false
end

function uiManager.onUnitInfoScrollLeft(elemHandle)
    local mod = package.loaded["scripts.unit_info_v2"]
    if mod and mod.handleScrollLeft then
        return mod.handleScrollLeft(elemHandle)
    end
    return false
end

function uiManager.onUnitInfoScrollRight(elemHandle)
    local mod = package.loaded["scripts.unit_info_v2"]
    if mod and mod.handleScrollRight then
        return mod.handleScrollRight(elemHandle)
    end
    return false
end

function uiManager.onUnitInfoSubTabClick(elemHandle)
    local mod = package.loaded["scripts.unit_info_v2"]
    if mod and mod.handleSubTabClick then
        return mod.handleSubTabClick(elemHandle)
    end
    return false
end

function uiManager.onUnitInfoLogClick(elemHandle)
    local mod = package.loaded["scripts.unit_info_v2"]
    if mod and mod.handleLogClick then
        return mod.handleLogClick(elemHandle)
    end
    return false
end

function uiManager.onInventoryTabClick(elemHandle)
    local mod = package.loaded["scripts.unit_info_v2"]
    if mod and mod.handleInvTabClick then
        return mod.handleInvTabClick(elemHandle)
    end
    return false
end

-- Build-menu tab strip + icon clicks. See scripts/build_tool.lua's
-- handleTabClick / handleIconClick for dispatch.
function uiManager.onBuildMenuTabClick(elemHandle)
    local mod = package.loaded["scripts.build_tool"]
    if mod and mod.handleTabClick then
        return mod.handleTabClick(elemHandle)
    end
    return false
end

function uiManager.onBuildMenuIconClick(elemHandle)
    local mod = package.loaded["scripts.build_tool"]
    if mod and mod.handleIconClick then
        return mod.handleIconClick(elemHandle)
    end
    return false
end

-- Cargo inventory popup tab strip + per-row right-click. The popup
-- is triggered from init.lua's right-click handler (Contents menu).
function uiManager.onCargoInventoryTabClick(elemHandle)
    local mod = package.loaded["scripts.cargo_inventory_panel"]
    if mod and mod.handleTabClick then
        return mod.handleTabClick(elemHandle)
    end
    return false
end

function uiManager.onCargoInventoryItemRightClick(elemHandle)
    local mod = package.loaded["scripts.cargo_inventory_panel"]
    if mod and mod.handleItemRightClick then
        return mod.handleItemRightClick(elemHandle)
    end
    return false
end

function uiManager.onInventoryItemRightClick(elemHandle)
    local mod = package.loaded["scripts.unit_info_v2"]
    if mod and mod.handleInvItemRightClick then
        return mod.handleInvItemRightClick(elemHandle)
    end
    return false
end

function uiManager.onEquipSlotRightClick(elemHandle)
    local mod = package.loaded["scripts.unit_info_v2"]
    if mod and mod.handleEquipSlotRightClick then
        return mod.handleEquipSlotRightClick(elemHandle)
    end
    return false
end

function uiManager.onAccessoryRowRightClick(elemHandle)
    local mod = package.loaded["scripts.unit_info_v2"]
    if mod and mod.handleAccessoryRowRightClick then
        return mod.handleAccessoryRowRightClick(elemHandle)
    end
    return false
end

-- Context menu click routes. Both fire as broadcast callbacks named in
-- scripts.ui.context_menu — items run the row's callback, backdrop
-- closes the menu without doing anything else.
function uiManager.onContextMenuItemClick(elemHandle)
    if contextMenu then
        return contextMenu.handleItemClick(elemHandle)
    end
    return false
end

function uiManager.onContextMenuBackdrop(elemHandle)
    if contextMenu then
        return contextMenu.handleBackdropClick(elemHandle)
    end
    return false
end
-- Note: "onEventLogRightClick" is broadcast to every loaded script —
-- hud.onEventLogRightClick handles it directly. No uiManager delegate
-- needed (and adding one would cause the menu to open twice).

function uiManager.onToggleRightClick(elemHandle)
    if toggle then
        return toggle.handleRightClickByElement(elemHandle)
    end
    return false
end

function uiManager.onToggleOptionClick(elemHandle)
    if toggle then
        return toggle.handleClickByElement(elemHandle)
    end
    return false
end

function uiManager.onMouseUp(button_num, x, y)
    if slider then
        slider.onMouseUp()
    end
    if scrollbar then
        scrollbar.onMouseUp()
    end
    if button then
        button.onMouseUp()
    end
end

function uiManager.onTabFrameScroll(elemHandle)
    return false
end

-----------------------------------------------------------
-- Scroll Button Clicks (scrollbar up/down arrows)
-----------------------------------------------------------

function uiManager.onScrollUp(elemHandle)
    if dropdown then
        local handled = dropdown.handleCallback("onScrollUp", elemHandle)
        if handled then return true end
    end
    if settingsMenu and currentMenu == "settings" then
        if settingsMenu.handleScrollCallback then
            if settingsMenu.handleScrollCallback("onScrollUp", elemHandle) then
                return true
            end
        end
    end
    if createWorldMenu and currentMenu == "create_world" then
        if createWorldMenu.handleScrollCallback then
            if createWorldMenu.handleScrollCallback("onScrollUp", elemHandle) then
                return true
            end
        end
    end
    if saveBrowser and currentMenu == "save_browser" then
        if saveBrowser.handleScrollCallback then
            if saveBrowser.handleScrollCallback("onScrollUp", elemHandle) then
                return true
            end
        end
    end
    if combatLog and combatLog.isVisible and combatLog.isVisible()
       and combatLog.handleScrollCallback then
        if combatLog.handleScrollCallback("onScrollUp", elemHandle) then
            return true
        end
    end
    if injuryLog and injuryLog.isVisible and injuryLog.isVisible()
       and injuryLog.handleScrollCallback then
        if injuryLog.handleScrollCallback("onScrollUp", elemHandle) then
            return true
        end
    end
    if unitLog and unitLog.isVisible and unitLog.isVisible()
       and unitLog.handleScrollCallback then
        if unitLog.handleScrollCallback("onScrollUp", elemHandle) then
            return true
        end
    end
    -- Unit info v2 has its own stats-panel scrollbar.
    local uimod = package.loaded["scripts.unit_info_v2"]
    if uimod and uimod.handleScrollCallback then
        if uimod.handleScrollCallback("onScrollUp", elemHandle) then
            return true
        end
    end
    return false
end

function uiManager.onScrollDown(elemHandle)
    if dropdown then
        local handled = dropdown.handleCallback("onScrollDown", elemHandle)
        if handled then return true end
    end
    if settingsMenu and currentMenu == "settings" then
        if settingsMenu.handleScrollCallback then
            if settingsMenu.handleScrollCallback("onScrollDown", elemHandle) then
                return true
            end
        end
    end
    if createWorldMenu and currentMenu == "create_world" then
        if createWorldMenu.handleScrollCallback then
            if createWorldMenu.handleScrollCallback("onScrollDown", elemHandle) then
                return true
            end
        end
    end
    if saveBrowser and currentMenu == "save_browser" then
        if saveBrowser.handleScrollCallback then
            if saveBrowser.handleScrollCallback("onScrollDown", elemHandle) then
                return true
            end
        end
    end
    if combatLog and combatLog.isVisible and combatLog.isVisible()
       and combatLog.handleScrollCallback then
        if combatLog.handleScrollCallback("onScrollDown", elemHandle) then
            return true
        end
    end
    if injuryLog and injuryLog.isVisible and injuryLog.isVisible()
       and injuryLog.handleScrollCallback then
        if injuryLog.handleScrollCallback("onScrollDown", elemHandle) then
            return true
        end
    end
    if unitLog and unitLog.isVisible and unitLog.isVisible()
       and unitLog.handleScrollCallback then
        if unitLog.handleScrollCallback("onScrollDown", elemHandle) then
            return true
        end
    end
    local uimod = package.loaded["scripts.unit_info_v2"]
    if uimod and uimod.handleScrollCallback then
        if uimod.handleScrollCallback("onScrollDown", elemHandle) then
            return true
        end
    end
    return false
end

function uiManager.onLogPanelScroll(elemHandle)
    -- This callback exists so the right panel is clickable and receives
    -- scroll events. The actual scrolling is handled by onUIScroll.
    return false
end

-------------------------------------------------------------
--- Slider Clicks
-------------------------------------------------------------

function uiManager.onSliderTrackClick(elemHandle)
    handleNonTextBoxClick()
    if slider then
        return slider.handleCallback("onSliderTrackClick", elemHandle)
    end
    return false
end

function uiManager.onSliderKnobClick(elemHandle)
    handleNonTextBoxClick()
    if slider then
        return slider.handleCallback("onSliderKnobClick", elemHandle)
    end
    return false
end

-- Click on a scrollbar tab → start dragging. The per-frame poll in
-- update() takes it from here. onMouseUp clears the drag state.
function uiManager.onScrollTabGrab(elemHandle)
    handleNonTextBoxClick()
    if scrollbar then
        return scrollbar.onTabGrab(elemHandle)
    end
    return false
end

-- Click on the unit-info-v2 stats-panel transparent background. Pure
-- no-op; we register the click solely to keep the element in the
-- engine's clickable set so wheel events over the panel body route
-- through onUIScroll instead of being misread as world zoom.
function uiManager.onStatsPanelBgClick(elemHandle)
    local mod = package.loaded["scripts.unit_info_v2"]
    if mod and mod.onStatsPanelBgClick then
        return mod.onStatsPanelBgClick(elemHandle)
    end
    return false
end

-----------------------------------------------------------
-- Mouse Wheel Scroll (UI elements)
-----------------------------------------------------------

function uiManager.onUIScroll(elemHandle, dx, dy)
    if dropdown then
        local handled = dropdown.onScroll(elemHandle, dx, dy)
        if handled then return end
    end
    if settingsMenu and currentMenu == "settings" then
        if settingsMenu.onScroll(elemHandle, dx, dy) then
            return
        end
    end
    if createWorldMenu and currentMenu == "create_world" then
        if createWorldMenu.onScroll(elemHandle, dx, dy) then
            return
        end
    end
    if saveBrowser and currentMenu == "save_browser" then
        if saveBrowser.onScroll(elemHandle, dx, dy) then
            return
        end
    end
    if combatLog and combatLog.isVisible and combatLog.isVisible()
       and combatLog.onScroll then
        if combatLog.onScroll(elemHandle, dx, dy) then
            return
        end
    end
    if injuryLog and injuryLog.isVisible and injuryLog.isVisible()
       and injuryLog.onScroll then
        if injuryLog.onScroll(elemHandle, dx, dy) then
            return
        end
    end
    if unitLog and unitLog.isVisible and unitLog.isVisible()
       and unitLog.onScroll then
        if unitLog.onScroll(elemHandle, dx, dy) then
            return
        end
    end
    -- Unit info v2 stats-panel scrollbar.
    local uimod = package.loaded["scripts.unit_info_v2"]
    if uimod and uimod.onScroll then
        if uimod.onScroll(elemHandle, dx, dy) then
            return
        end
    end
end

-----------------------------------------------------------
-- Game Scroll (no UI element under cursor, no shift)
-----------------------------------------------------------

function uiManager.onScroll(dx, dy)
    if currentMenu == "world_view" and worldView then
        worldView.onScroll(dx, dy)
        hud.onScroll(dx, dy)
    elseif (currentMenu == "test_arena" or currentMenu == "test_arena_view") and testArena then
        testArena.onScroll(dx, dy)
        hud.onScroll(dx, dy)
    end
end

-----------------------------------------------------------
-- Z-Slice Scroll (shift+scroll)
-----------------------------------------------------------

function uiManager.onZSliceScroll(dx, dy)
    if currentMenu == "world_view" and worldView then
        worldView.onZSliceScroll(dx, dy)
    elseif (currentMenu == "test_arena" or currentMenu == "test_arena_view") and testArena then
        testArena.onZSliceScroll(dx, dy)
    end
end

-----------------------------------------------------------
-- Input Event Forwarding to Textboxes
-----------------------------------------------------------

function uiManager.onDropdownDisplayClick(elemHandle)
    -- Clear the OTHER widget families only — the dropdown module
    -- manages focus within its own family, and a blanket
    -- dropdown.unfocusAll() here would reset the typed filter text
    -- when clicking the display box of an already-focused dropdown.
    if textbox then textbox.unfocusAll() end
    if randbox then randbox.unfocusAll() end
    if dropdown then
        return dropdown.handleCallback("onDropdownDisplayClick", elemHandle)
    end
    return false
end

function uiManager.onUICharInput(char)
    if dropdown and dropdown.getFocusedId() then
        return dropdown.onCharInput(char)
    end
    if randbox and randbox.getFocusedId() then
        return randbox.onCharInput(char)
    end
    if textbox then
        return textbox.onCharInput(char)
    end
    return false
end

function uiManager.onUIBackspace()
    if dropdown and dropdown.getFocusedId() then
        return dropdown.onBackspace()
    end
    if randbox and randbox.getFocusedId() then
        return randbox.onBackspace()
    end
    if textbox then
        return textbox.onBackspace()
    end
    return false
end

function uiManager.onUIDelete()
    if dropdown and dropdown.getFocusedId() then
        return dropdown.onDelete()
    end
    if randbox and randbox.getFocusedId() then
        return randbox.onDelete()
    end
    if textbox then
        return textbox.onDelete()
    end
    return false
end

function uiManager.onUICursorLeft()
    if dropdown and dropdown.getFocusedId() then
        return dropdown.onCursorLeft()
    end
    if randbox and randbox.getFocusedId() then
        return randbox.onCursorLeft()
    end
    if textbox then
        return textbox.onCursorLeft()
    end
    return false
end

function uiManager.onUICursorRight()
    if dropdown and dropdown.getFocusedId() then
        return dropdown.onCursorRight()
    end
    if randbox and randbox.getFocusedId() then
        return randbox.onCursorRight()
    end
    if textbox then
        return textbox.onCursorRight()
    end
    return false
end

function uiManager.onUIHome()
    if dropdown and dropdown.getFocusedId() then
        return dropdown.onHome()
    end
    if randbox and randbox.getFocusedId() then
        return randbox.onHome()
    end
    if textbox then
        return textbox.onHome()
    end
    return false
end

function uiManager.onUIEnd()
    if dropdown and dropdown.getFocusedId() then
        return dropdown.onEnd()
    end
    if randbox and randbox.getFocusedId() then
        return randbox.onEnd()
    end
    if textbox then
        return textbox.onEnd()
    end
    return false
end

function uiManager.onUISubmit()
    if dropdown and dropdown.getFocusedId() then
        return dropdown.onSubmit()
    end
    if randbox and randbox.getFocusedId() then
        return randbox.onSubmit()
    end
    if textbox then
        local handled, value, id, name = textbox.onSubmit()
        if handled and value then
            engine.logDebug("UI Submit received: " .. tostring(value) .. " from " .. tostring(name))
            if settingsMenu then settingsMenu.onTextBoxSubmit(name, value) end
        end
        return handled
    end
    return false
end

function uiManager.onUIEscape()
    -- First, let UI widgets handle escape (close dropdowns, unfocus textboxes)
    if dropdown and dropdown.getFocusedId() then
        return dropdown.onEscape()
    end
    if randbox and randbox.getFocusedId() then
        return randbox.onEscape()
    end
    if textbox then
        local handled = textbox.onEscape()
        if handled then return true end
    end
    if dropdown then
        for id = 1, 100 do
            if dropdown.isOpen(id) then
                dropdown.closeList(id)
                return true
            end
        end
    end

    -- Ghost-focus recovery: no widget claimed this escape, so any
    -- engine-side focus that survived to here is unowned — clear it,
    -- otherwise the keyboard stays captured in UI-text mode.
    UI.clearFocus()

    -- Menu-level escape handling
    if currentMenu == "main" then
        engine.quit()
    elseif currentMenu == "settings" or currentMenu == "create_world"
        or currentMenu == "save_browser" then
        uiManager.showMenu("back")
    elseif currentMenu == "world_view" then
        if pauseMenu then
            pauseMenu.toggle({ showSave = true })
        end
    elseif currentMenu == "test_arena_view" then
        if pauseMenu then
            pauseMenu.toggle({ showSave = false })
        end
    end
    return true
end

function uiManager.onUIFocusLost()
    if dropdown then
        dropdown.unfocusAll()
    end
    if randbox then
        randbox.unfocusAll()
    end
    if textbox then
        textbox.unfocusAll()
    end
end

-----------------------------------------------------------
-- World Generation Log (forwarded from world thread)
-----------------------------------------------------------

function uiManager.onWorldGenLog(text)
    if createWorldMenu then
        createWorldMenu.onWorldGenLog(text)
    end
end

function uiManager.onWorldPreviewReady(textureHandle)
    engine.logDebug("World preview texture ready: " .. tostring(textureHandle))
    if createWorldMenu and createWorldMenu.onWorldPreviewReady then
        createWorldMenu.onWorldPreviewReady(textureHandle)
    end
end

-----------------------------------------------------------
-- Tile/Chunk Info Panel (forwarded to HUD)
-----------------------------------------------------------

function uiManager.onSetInfoBasic(text)
    if hud then hud.setInfoBasic(text) end
end

function uiManager.onSetInfoAdvanced(text)
    if hud then hud.setInfoAdvanced(text) end
end

function uiManager.onSetInfoText(basicText, advancedText)
    if hud then hud.setInfoText(basicText, advancedText) end
end

function uiManager.onSetWeatherInfo(text)
    if hud then hud.setWeatherInfo(text) end
end

function uiManager.onSetResourcesInfo(text)
    if hud then hud.setResourcesInfo(text) end
end

function uiManager.onClearInfo()
    if hud then hud.clearInfo() end
end

function uiManager.onOpenArena()
    uiManager.showMenu("test_arena")
end

-----------------------------------------------------------
-- Key Input Forwarding
-----------------------------------------------------------

function uiManager.onKeyDown(key)
    if key == "F7" then
        world.openArena()
    end
    if currentMenu == "world_view" and worldView then
        worldView.onKeyDown(key)
    elseif (currentMenu == "test_arena" or currentMenu == "test_arena_view") and testArena then
        testArena.onKeyDown(key)
    end
end

function uiManager.onKeyUp(key)
    if currentMenu == "world_view" and worldView then
        if worldView.onKeyUp then
            worldView.onKeyUp(key)
        end
    elseif (currentMenu == "test_arena" or currentMenu == "test_arena_view") and testArena then
        if testArena.onKeyUp then
            testArena.onKeyUp(key)
        end
    end
end

return uiManager
