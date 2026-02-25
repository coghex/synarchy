-- Main Menu Module (pure module, no global callbacks)
local scale = require("scripts.ui.scale")
local panel = require("scripts.ui.panel")
local button = require("scripts.ui.button")
local label = require("scripts.ui.label")
local mainMenu = {}

mainMenu.page = nil
mainMenu.panelId = nil
mainMenu.boxTexSet = nil
mainMenu.buttonTexSet = nil
mainMenu.menuFont = nil
mainMenu.titleFont = nil
mainMenu.fbW = 0
mainMenu.fbH = 0
mainMenu.uiCreated = false

mainMenu.titleLabelId = nil

-- Owned element IDs for scoped cleanup
mainMenu.ownedLabels  = {}
mainMenu.ownedButtons = {}
mainMenu.ownedPanels  = {}

-- Save state
mainMenu.saves = {}        -- cached list from engine.listSaves()
mainMenu.latestSave = nil  -- name of most recent save

-- Callback to switch menus (set by ui_manager)
mainMenu.showMenuCallback = nil

-- Base sizes (unscaled)
mainMenu.baseSizes = {
    titleFontSize = 96,
    fontSize = 32,
    buttonHeight = 80,
    buttonSpacing = 24,
    buttonTileSize = 64,
    buttonPaddingX = 160,
    buttonOverflow = 16,
    menuPaddingX = 100,
    menuPaddingY = 100,
    menuTileSize = 64,
    menuOverflow = 0,
    titleOffset = 80,
}

mainMenu.buttons = {}

-- Build menu items dynamically based on available saves
function mainMenu.buildMenuItems()
    local items = {}
    
    -- Check for saves
    mainMenu.saves = engine.listSaves() or {}
    
    if #mainMenu.saves > 0 then
        -- Sort by timestamp descending to find latest
        table.sort(mainMenu.saves, function(a, b)
            return a.timestamp > b.timestamp
        end)
        mainMenu.latestSave = mainMenu.saves[1].name
        
        -- Add "Continue" button (loads most recent save)
        table.insert(items, {
            name = "continue",
            label = "Continue",
            onClick = function() mainMenu.onContinue() end,
        })
        
        -- If multiple saves, add "Load Game" button
        if #mainMenu.saves > 1 then
            table.insert(items, {
                name = "load_game",
                label = "Load Game",
                onClick = function() mainMenu.onLoadGame() end,
            })
        end
    end
    
    -- Always show these
    table.insert(items, {
        name = "create_world",
        label = "Create World",
        onClick = function() mainMenu.onCreateWorld() end,
    })
    table.insert(items, {
        name = "settings",
        label = "Settings",
        onClick = function() mainMenu.onSettings() end,
    })
    table.insert(items, {
        name = "quit",
        label = "Quit",
        onClick = function() mainMenu.onQuit() end,
    })
    
    return items
end

-----------------------------------------------------------
-- Scoped cleanup
-----------------------------------------------------------

function mainMenu.destroyOwned()
    for _, id in ipairs(mainMenu.ownedLabels)  do label.destroy(id) end
    for _, id in ipairs(mainMenu.ownedButtons) do button.destroy(id) end
    for _, id in ipairs(mainMenu.ownedPanels)  do panel.destroy(id) end
    mainMenu.ownedLabels  = {}
    mainMenu.ownedButtons = {}
    mainMenu.ownedPanels  = {}
end

function mainMenu.setShowMenuCallback(callback)
    mainMenu.showMenuCallback = callback
end

function mainMenu.init(boxTex, btnTex, font, tFont, width, height)
    mainMenu.boxTexSet = boxTex
    mainMenu.buttonTexSet = btnTex
    mainMenu.menuFont = font
    mainMenu.titleFont = tFont
    mainMenu.fbW = width
    mainMenu.fbH = height
    
    mainMenu.createUI()
end

function mainMenu.createUI()
    if mainMenu.uiCreated then
        if mainMenu.page then
            UI.deletePage(mainMenu.page)
            mainMenu.destroyOwned()
            mainMenu.buttons = {}
            mainMenu.titleLabelId = nil
        end
    end
    
    -- Build menu items dynamically based on saves
    local menuItems = mainMenu.buildMenuItems()
    
    local uiscale = scale.get()
    local s = scale.applyAll(mainMenu.baseSizes)
    
    engine.logDebug("Creating main menu with framebuffer size: "
        .. mainMenu.fbW .. " x " .. mainMenu.fbH
        .. ", scale: " .. uiscale
        .. ", saves found: " .. #mainMenu.saves)
    
    mainMenu.page = UI.newPage("main_menu", "menu")

    -- Calculate max text width for button sizing
    local maxLabelWidth = 0
    for i, item in ipairs(menuItems) do
        local labelWidth = engine.getTextWidth(mainMenu.menuFont, item.label, s.fontSize)
        if labelWidth > maxLabelWidth then
            maxLabelWidth = labelWidth
        end
    end
    
    local buttonWidth = maxLabelWidth + s.buttonPaddingX
    
    -- Calculate menu dimensions
    local menuHeight = #menuItems * (s.buttonHeight + s.buttonSpacing)
        + s.buttonSpacing + s.menuPaddingY
    local menuWidth = buttonWidth + s.menuPaddingX
    
    -- Center the menu panel
    local menuX = (mainMenu.fbW - menuWidth) / 2
    local menuY = (mainMenu.fbH - menuHeight) / 2
    
    -- Create menu panel
    mainMenu.panelId = panel.new({
        name = "menu_panel",
        page = mainMenu.page,
        x = menuX,
        y = menuY,
        width = menuWidth,
        height = menuHeight,
        textureSet = mainMenu.boxTexSet,
        color = {1.0, 1.0, 1.0, 1.0},
        tileSize = s.menuTileSize,
        overflow = s.menuOverflow,
        zIndex = 1,
        padding = {
            top = s.menuPaddingY / 2,
            bottom = s.menuPaddingY / 2,
            left = s.menuPaddingX / 2,
            right = s.menuPaddingX / 2,
        },
        uiscale = 1.0,
    })
    table.insert(mainMenu.ownedPanels, mainMenu.panelId)
    
    local baseZ = panel.getZIndex(mainMenu.panelId)
    
    -- Create title above menu panel
    mainMenu.titleLabelId = label.new({
        name = "title",
        text = "Ecce Homo",
        font = mainMenu.titleFont,
        fontSize = mainMenu.baseSizes.titleFontSize,
        color = {1.0, 1.0, 1.0, 1.0},
        page = mainMenu.page,
        uiscale = uiscale,
    })
    table.insert(mainMenu.ownedLabels, mainMenu.titleLabelId)
    
    local titleW, titleH = label.getSize(mainMenu.titleLabelId)
    local titleX = (mainMenu.fbW - titleW) / 2
    local titleY = menuY - s.titleOffset
    UI.addToPage(mainMenu.page,
        label.getElementHandle(mainMenu.titleLabelId), titleX, titleY)
    
    -- Create menu buttons
    local buttonElements = {}
    local buttonSizes = {}
    
    for i, item in ipairs(menuItems) do
        local labelWidth = engine.getTextWidth(
            mainMenu.menuFont, item.label, s.fontSize)
        local thisButtonWidth = labelWidth + s.buttonPaddingX
        
        local btnId = button.new({
            name = item.name,
            text = item.label,
            width = thisButtonWidth / uiscale,
            height = mainMenu.baseSizes.buttonHeight,
            fontSize = mainMenu.baseSizes.fontSize,
            tileSize = mainMenu.baseSizes.buttonTileSize,
            overflow = mainMenu.baseSizes.buttonOverflow,
            uiscale = uiscale,
            page = mainMenu.page,
            font = mainMenu.menuFont,
            textureSet = mainMenu.boxTexSet,
            bgColor = {1.0, 1.0, 1.0, 1.0},
            textColor = {1.0, 1.0, 1.0, 1.0},
            onClick = item.onClick,
            -- No callbackName: uses default "onButtonClick"
        })
        table.insert(mainMenu.ownedButtons, btnId)
        
        local btnW, btnH = button.getSize(btnId)
        
        table.insert(buttonElements, button.getElementHandle(btnId))
        table.insert(buttonSizes, { width = btnW, height = btnH })
        
        UI.setZIndex(button.getElementHandle(btnId), baseZ + 1)
        
        mainMenu.buttons[item.name] = btnId
    end
    
    -- Place buttons in a centered column
    panel.placeColumn(
        mainMenu.panelId,
        buttonElements,
        buttonSizes,
        {
            x = "50%",
            y = "50%",
            origin = "center",
            spacing = s.buttonSpacing,
        }
    )
    
    mainMenu.uiCreated = true
    engine.logDebug("Main menu created with " .. #menuItems .. " buttons")
end

-----------------------------------------------------------
-- Callbacks
-----------------------------------------------------------

function mainMenu.onContinue()
    if mainMenu.latestSave then
        engine.logInfo("Continuing from save: " .. mainMenu.latestSave)
        mainMenu.loadAndShowSave(mainMenu.latestSave)
    end
end

function mainMenu.onLoadGame()
    engine.logInfo("Opening load game screen")
    -- TODO: Show a save browser. For now, load the latest.
    if mainMenu.latestSave then
        mainMenu.loadAndShowSave(mainMenu.latestSave)
    end
end

function mainMenu.onCreateWorld()
    if mainMenu.showMenuCallback then
        mainMenu.showMenuCallback("create_world")
    end
end

function mainMenu.onSettings()
    if mainMenu.showMenuCallback then
        mainMenu.showMenuCallback("settings")
    end
end

function mainMenu.onQuit()
    engine.quit()
end

-----------------------------------------------------------
-- Load a save and transition to world view
-----------------------------------------------------------

function mainMenu.loadAndShowSave(saveName)
    local worldView = require("scripts.world_view")
    local worldManager = require("scripts.world_manager")
    
    -- 1. Deserialize save file and queue WorldLoadSave on the world thread
    local ok = engine.loadSave(saveName)
    if not ok then
        engine.logError("Failed to load save: " .. saveName)
        return
    end
    
    -- 2. Tell worldManager about the loaded world so Lua state is consistent
    worldManager.currentWorld = "main_world"
    worldManager.active = true
    
    -- 3. Send all textures to the world
    --    (The Haskell WorldLoadSave handler restores WorldState but textures
    --     are GPU handles that must be re-sent from Lua)
    worldView.sendTexturesToWorld("main_world")
    
    -- 4. Show the world on the Haskell side
    world.show("main_world")
    
    -- 5. Transition UI to world view
    if mainMenu.showMenuCallback then
        mainMenu.showMenuCallback("world_view")
    end
end

function mainMenu.show()
    mainMenu.createUI()  -- Rebuild to re-check saves
    if mainMenu.page then
        UI.showPage(mainMenu.page)
        engine.logDebug("Main menu shown")
    end
end

function mainMenu.hide()
    if mainMenu.page then
        UI.hidePage(mainMenu.page)
        engine.logDebug("Main menu hidden")
    end
end

function mainMenu.onFramebufferResize(width, height)
    engine.logDebug("Main menu onFramebufferResize: "
        .. width .. " x " .. height)
    mainMenu.fbW = width
    mainMenu.fbH = height
    
    if mainMenu.uiCreated then
        mainMenu.createUI()
    end
end

function mainMenu.shutdown()
    mainMenu.destroyOwned()
    if mainMenu.page then
        UI.deletePage(mainMenu.page)
    end
end

return mainMenu
