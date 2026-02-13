-- Create World Menu Module
local scale          = require("scripts.ui.scale")
local panel          = require("scripts.ui.panel")
local label          = require("scripts.ui.label")
local button         = require("scripts.ui.button")
local tabbar         = require("scripts.ui.tabbar")
local sprite         = require("scripts.ui.sprite")
local randbox        = require("scripts.ui.randbox")
local dropdown       = require("scripts.ui.dropdown")
local textbox        = require("scripts.ui.textbox")

local createWorldMenu = {}

-----------------------------------------------------------
-- Z-Index Plan
-----------------------------------------------------------
local Z_PANEL       = 1
local Z_TITLE       = 2
local Z_LEFT_PANEL  = 3
local Z_RIGHT_PANEL = 3
local Z_TAB_FRAME   = 4
local Z_TAB_BUTTONS = 5
local Z_CONTENT     = 6
local Z_WIDGETS     = 7
local Z_PREVIEW     = 7
local Z_BUTTONS     = 8

-----------------------------------------------------------
-- Base sizes (unscaled)
-----------------------------------------------------------
createWorldMenu.baseSizes = {
    fontSize       = 24,
    btnWidth       = 200,
    btnHeight      = 52,
    generateBtnWidth = 360,
    btnSpacing     = 16,
    tabHeight      = 32,
    tabFontSize    = 20,
    rowSpacing     = 50,
    nameBoxWidth   = 400,
    randboxWidth   = 200,
    randboxHeight  = 40,
    dropdownHeight = 40,
    textboxWidth   = 100,
    textboxHeight  = 40,
}

-----------------------------------------------------------
-- World Size Options
-----------------------------------------------------------
local worldSizeOptions = {
    { text = "Tiny (32)",       value = "32" },
    { text = "Small (64)",      value = "64" },
    { text = "Medium (128)",    value = "128" },
    { text = "Large (256)",     value = "256" },
    { text = "Huge (512)",      value = "512" },
    { text = "Massive (1024)",  value = "1024" },
}

-----------------------------------------------------------
-- Module state
-----------------------------------------------------------
createWorldMenu.page         = nil
createWorldMenu.panelId      = nil
createWorldMenu.leftPanelId  = nil
createWorldMenu.rightPanelId = nil
createWorldMenu.panelTexSet  = nil
createWorldMenu.buttonTexSet = nil
createWorldMenu.menuFont     = nil
createWorldMenu.fbW          = 0
createWorldMenu.fbH          = 0
createWorldMenu.uiCreated    = false
createWorldMenu.tabBarId     = nil
createWorldMenu.activeTab    = "settings"
createWorldMenu.showMenuCallback = nil
createWorldMenu.worldPreviewTexture = nil

-- Pending world parameters
createWorldMenu.pending = {
    worldName  = "",
    seed       = "",
    worldSize  = "128",
    plateCount = "7",
}

-- Button IDs
createWorldMenu.backButtonId     = nil
createWorldMenu.defaultsButtonId = nil
createWorldMenu.generateButtonId = nil

-- Settings tab widget IDs
createWorldMenu.nameRandBoxId    = nil
createWorldMenu.seedRandBoxId    = nil
createWorldMenu.sizeDropdownId   = nil

-- Advanced tab widget IDs
createWorldMenu.plateCountTextBoxId = nil

-- Per-tab element handles for show/hide
createWorldMenu.tabElements = {}

-- Owned element IDs for cleanup
createWorldMenu.ownedLabels     = {}
createWorldMenu.ownedButtons    = {}
createWorldMenu.ownedPanels     = {}
createWorldMenu.ownedTabbars    = {}
createWorldMenu.ownedSprites    = {}
createWorldMenu.ownedRandBoxes  = {}
createWorldMenu.ownedDropdowns  = {}
createWorldMenu.ownedTextBoxes  = {}

-----------------------------------------------------------
-- Tab registry
-----------------------------------------------------------
local tabDefs = {
    { key = "settings", name = "Settings" },
    { key = "advanced", name = "Advanced" },
}

-----------------------------------------------------------
-- Cleanup
-----------------------------------------------------------

function createWorldMenu.destroyOwned()
    for _, id in ipairs(createWorldMenu.ownedLabels)    do label.destroy(id) end
    for _, id in ipairs(createWorldMenu.ownedButtons)   do button.destroy(id) end
    for _, id in ipairs(createWorldMenu.ownedPanels)    do panel.destroy(id) end
    for _, id in ipairs(createWorldMenu.ownedTabbars)   do tabbar.destroy(id) end
    for _, id in ipairs(createWorldMenu.ownedSprites)   do sprite.destroy(id) end
    for _, id in ipairs(createWorldMenu.ownedRandBoxes) do randbox.destroy(id) end
    for _, id in ipairs(createWorldMenu.ownedDropdowns) do dropdown.destroy(id) end
    for _, id in ipairs(createWorldMenu.ownedTextBoxes) do textbox.destroy(id) end

    createWorldMenu.ownedLabels     = {}
    createWorldMenu.ownedButtons    = {}
    createWorldMenu.ownedPanels     = {}
    createWorldMenu.ownedTabbars    = {}
    createWorldMenu.ownedSprites    = {}
    createWorldMenu.ownedRandBoxes  = {}
    createWorldMenu.ownedDropdowns  = {}
    createWorldMenu.ownedTextBoxes  = {}
end

-- Tracking helpers
function createWorldMenu.trackLabel(id)
    table.insert(createWorldMenu.ownedLabels, id)
    return id
end
function createWorldMenu.trackButton(id)
    table.insert(createWorldMenu.ownedButtons, id)
    return id
end
function createWorldMenu.trackPanel(id)
    table.insert(createWorldMenu.ownedPanels, id)
    return id
end
function createWorldMenu.trackTabbar(id)
    table.insert(createWorldMenu.ownedTabbars, id)
    return id
end
function createWorldMenu.trackSprite(id)
    table.insert(createWorldMenu.ownedSprites, id)
    return id
end
function createWorldMenu.trackRandBox(id)
    table.insert(createWorldMenu.ownedRandBoxes, id)
    return id
end
function createWorldMenu.trackDropdown(id)
    table.insert(createWorldMenu.ownedDropdowns, id)
    return id
end
function createWorldMenu.trackTextBox(id)
    table.insert(createWorldMenu.ownedTextBoxes, id)
    return id
end

-----------------------------------------------------------
-- World Size Validators
-----------------------------------------------------------

function createWorldMenu.sizeValidator(char)
    return char:match("^%d$") ~= nil
end

function createWorldMenu.sizeMatcher(inputText, options)
    if not inputText or inputText == "" then return nil end
    for i, opt in ipairs(options) do
        if opt.value == inputText then
            return i
        end
    end
    local lower = inputText:lower()
    for i, opt in ipairs(options) do
        if opt.text:lower():sub(1, #lower) == lower then
            return i
        end
    end
    return nil
end

-----------------------------------------------------------
-- Callbacks
-----------------------------------------------------------

function createWorldMenu.setShowMenuCallback(callback)
    createWorldMenu.showMenuCallback = callback
end

-----------------------------------------------------------
-- Init
-----------------------------------------------------------

function createWorldMenu.init(panelTex, btnTex, font, width, height)
    createWorldMenu.panelTexSet  = panelTex
    createWorldMenu.buttonTexSet = btnTex
    createWorldMenu.menuFont     = font
    createWorldMenu.fbW          = width
    createWorldMenu.fbH          = height

    createWorldMenu.worldPreviewTexture = engine.loadTexture("assets/textures/world/notexture.png")

    createWorldMenu.createUI()
end

-----------------------------------------------------------
-- Full UI rebuild
-----------------------------------------------------------

function createWorldMenu.createUI()
    createWorldMenu.destroyOwned()

    createWorldMenu.backButtonId        = nil
    createWorldMenu.defaultsButtonId    = nil
    createWorldMenu.generateButtonId    = nil
    createWorldMenu.panelId             = nil
    createWorldMenu.leftPanelId         = nil
    createWorldMenu.rightPanelId        = nil
    createWorldMenu.tabBarId            = nil
    createWorldMenu.nameRandBoxId       = nil
    createWorldMenu.seedRandBoxId       = nil
    createWorldMenu.sizeDropdownId      = nil
    createWorldMenu.plateCountTextBoxId = nil
    createWorldMenu.tabElements         = {}

    if createWorldMenu.uiCreated and createWorldMenu.page then
        UI.deletePage(createWorldMenu.page)
    end

    local uiscale = scale.get()
    local s = scale.applyAllWith(createWorldMenu.baseSizes, uiscale)

    createWorldMenu.page = UI.newPage("create_world_menu", "modal")

    -- Main panel
    local panelWidth  = math.floor(createWorldMenu.fbW * 0.85)
    local panelHeight = math.floor(createWorldMenu.fbH * 0.85)
    local panelX = (createWorldMenu.fbW - panelWidth) / 2
    local panelY = (createWorldMenu.fbH - panelHeight) / 2

    createWorldMenu.panelId = createWorldMenu.trackPanel(panel.new({
        name       = "create_world_panel",
        page       = createWorldMenu.page,
        x = panelX, y = panelY,
        width      = panelWidth,
        height     = panelHeight,
        textureSet = createWorldMenu.panelTexSet,
        color      = {1.0, 1.0, 1.0, 1.0},
        tileSize   = 64,
        zIndex     = Z_PANEL,
        padding    = { top = 60, bottom = 100, left = 50, right = 50 },
        uiscale    = uiscale,
    }))
    local bounds = panel.getContentBounds(createWorldMenu.panelId)

    -- Title
    createWorldMenu.createTitle(panelX, panelY, bounds, s, uiscale)

    -- Calculate split dimensions
    local contentStartY = panelY + bounds.y + s.fontSize + math.floor(30 * uiscale)
    local contentHeight = panelHeight - (contentStartY - panelY) - bounds.y - s.btnHeight - math.floor(60 * uiscale)
    local leftWidth = math.floor(bounds.width * 0.4)
    local rightWidth = bounds.width - leftWidth - math.floor(20 * uiscale)

    -- Left panel (tabbed)
    createWorldMenu.createLeftPanel(panelX, panelY, bounds, contentStartY, 
                                     leftWidth, contentHeight, s, uiscale)

    -- Right panel (world preview)
    createWorldMenu.createRightPanel(panelX, panelY, bounds, contentStartY,
                                      leftWidth, rightWidth, contentHeight, s, uiscale)

    -- Bottom buttons
    createWorldMenu.createButtons(panelX, panelY, panelWidth, panelHeight,
                                   bounds, s, uiscale)

    createWorldMenu.uiCreated = true
end

-----------------------------------------------------------
-- Title
-----------------------------------------------------------

function createWorldMenu.createTitle(panelX, panelY, bounds, s, uiscale)
    local titleLabelId = createWorldMenu.trackLabel(label.new({
        name     = "create_world_title",
        text     = "Create World",
        font     = createWorldMenu.menuFont,
        fontSize = createWorldMenu.baseSizes.fontSize,
        color    = {1.0, 1.0, 1.0, 1.0},
        page     = createWorldMenu.page,
        uiscale  = uiscale,
    }))
    local titleW, _ = label.getSize(titleLabelId)
    local titleHandle = label.getElementHandle(titleLabelId)
    local titleX = panelX + bounds.x + (bounds.width - titleW) / 2
    local titleY = panelY + bounds.y + s.fontSize
    UI.addToPage(createWorldMenu.page, titleHandle, titleX, titleY)
    UI.setZIndex(titleHandle, Z_TITLE)
end

-----------------------------------------------------------
-- Left Panel (Tabbed Settings)
-----------------------------------------------------------

function createWorldMenu.createLeftPanel(panelX, panelY, bounds, contentStartY,
                                          leftWidth, contentHeight, s, uiscale)
    local leftX = panelX + bounds.x
    
    createWorldMenu.leftPanelId = createWorldMenu.trackPanel(panel.new({
        name       = "left_panel",
        page       = createWorldMenu.page,
        x          = leftX,
        y          = contentStartY,
        width      = leftWidth,
        height     = contentHeight,
        textureSet = createWorldMenu.panelTexSet,
        color      = {0.9, 0.9, 0.9, 1.0},
        tileSize   = 64,
        zIndex     = Z_LEFT_PANEL,
        padding    = { top = 10, bottom = 10, left = 10, right = 10 },
        uiscale    = uiscale,
    }))
    
    local leftBounds = panel.getContentBounds(createWorldMenu.leftPanelId)
    
    -- Tab bar
    local tabList = {}
    for _, def in ipairs(tabDefs) do
        table.insert(tabList, { name = def.name, key = def.key })
    end
    
    createWorldMenu.tabBarId = createWorldMenu.trackTabbar(tabbar.new({
        name              = "create_world_tabs",
        page              = createWorldMenu.page,
        x                 = leftX + leftBounds.x,
        y                 = contentStartY + leftBounds.y,
        width             = leftBounds.width,
        font              = createWorldMenu.menuFont,
        fontSize          = createWorldMenu.baseSizes.tabFontSize,
        tabHeight         = createWorldMenu.baseSizes.tabHeight,
        frameHeight       = leftBounds.height - s.tabHeight - math.floor(20 * uiscale),
        uiscale           = uiscale,
        zIndex            = Z_TAB_FRAME,
        textColor         = {0.0, 0.0, 0.0, 1.0},
        selectedTextColor = {1.0, 1.0, 1.0, 1.0},
        tabs              = tabList,
        onChange = function(key, index, tbId)
            createWorldMenu.activeTab = key
            createWorldMenu.showTab(key)
        end,
    }))
    
    tabbar.selectByKey(createWorldMenu.tabBarId, createWorldMenu.activeTab)
    
    -- Create ALL tab contents up front
    local frameX, frameY, frameW, frameH = tabbar.getFrameBounds(createWorldMenu.tabBarId)
    
    createWorldMenu.createSettingsContent(frameX, frameY, frameW, frameH, s, uiscale)
    createWorldMenu.createAdvancedContent(frameX, frameY, frameW, frameH, s, uiscale)

    -- Show only the active tab
    createWorldMenu.showTab(createWorldMenu.activeTab)
end

-----------------------------------------------------------
-- Tab Switching (show/hide, no rebuild)
-----------------------------------------------------------

function createWorldMenu.showTab(key)
    for tabKey, elements in pairs(createWorldMenu.tabElements) do
        local visible = (tabKey == key)
        for _, elem in ipairs(elements) do
            if elem.type == "label" then
                UI.setVisible(elem.handle, visible)
            elseif elem.type == "randbox" then
                randbox.setVisible(elem.id, visible)
            elseif elem.type == "dropdown" then
                dropdown.setVisible(elem.id, visible)
            elseif elem.type == "textbox" then
                textbox.setVisible(elem.id, visible)
            end
        end
    end
end

-----------------------------------------------------------
-- Settings Tab Content
-----------------------------------------------------------

function createWorldMenu.createSettingsContent(frameX, frameY, frameW, frameH, s, uiscale)
    local cx = frameX + math.floor(20 * uiscale)
    local cy = frameY + math.floor(20 * uiscale)
    local cw = frameW - math.floor(40 * uiscale)
    local rowIndex = 0
    local elements = {}

    local function rowY(n)
        return cy + s.rowSpacing * n
    end

    ---------------------------------------------------------
    -- Row 1: World Name (randbox - wide)
    ---------------------------------------------------------
    local nameLabelId = createWorldMenu.trackLabel(label.new({
        name     = "world_name_label",
        text     = "Name",
        font     = createWorldMenu.menuFont,
        fontSize = createWorldMenu.baseSizes.fontSize,
        color    = {1.0, 1.0, 1.0, 1.0},
        page     = createWorldMenu.page,
        uiscale  = uiscale,
    }))
    local nameLabelHandle = label.getElementHandle(nameLabelId)
    UI.addToPage(createWorldMenu.page, nameLabelHandle,
                 cx, rowY(rowIndex) + s.fontSize)
    UI.setZIndex(nameLabelHandle, Z_CONTENT)
    table.insert(elements, { type = "label", handle = nameLabelHandle })

    local nameW = math.floor(createWorldMenu.baseSizes.nameBoxWidth * uiscale)
    local nameBtnSize = math.floor(createWorldMenu.baseSizes.randboxHeight * uiscale)
    local nameTotalW = nameW + nameBtnSize

    createWorldMenu.nameRandBoxId = createWorldMenu.trackRandBox(randbox.new({
        name     = "world_name",
        width    = createWorldMenu.baseSizes.nameBoxWidth,
        height   = createWorldMenu.baseSizes.randboxHeight,
        page     = createWorldMenu.page,
        font     = createWorldMenu.menuFont,
        fontSize = 24,
        uiscale  = uiscale,
        zIndex   = Z_WIDGETS,
        randType = randbox.Type.NAME,
        default  = createWorldMenu.pending.worldName ~= ""
                       and createWorldMenu.pending.worldName or nil,
        onChange  = function(value, id, name)
            createWorldMenu.pending.worldName = value
        end,
    }))

    randbox.setPosition(createWorldMenu.nameRandBoxId,
                        cx + cw - nameTotalW, rowY(rowIndex))
    table.insert(elements, { type = "randbox", id = createWorldMenu.nameRandBoxId })

    rowIndex = rowIndex + 1

    ---------------------------------------------------------
    -- Row 2: Seed (randbox)
    ---------------------------------------------------------
    local seedLabelId = createWorldMenu.trackLabel(label.new({
        name     = "world_seed_label",
        text     = "Seed",
        font     = createWorldMenu.menuFont,
        fontSize = createWorldMenu.baseSizes.fontSize,
        color    = {1.0, 1.0, 1.0, 1.0},
        page     = createWorldMenu.page,
        uiscale  = uiscale,
    }))
    local seedLabelHandle = label.getElementHandle(seedLabelId)
    UI.addToPage(createWorldMenu.page, seedLabelHandle,
                 cx, rowY(rowIndex) + s.fontSize)
    UI.setZIndex(seedLabelHandle, Z_CONTENT)
    table.insert(elements, { type = "label", handle = seedLabelHandle })

    local rbW = math.floor(createWorldMenu.baseSizes.randboxWidth * uiscale)
    local rbBtnSize = math.floor(createWorldMenu.baseSizes.randboxHeight * uiscale)
    local rbTotalW = rbW + rbBtnSize

    createWorldMenu.seedRandBoxId = createWorldMenu.trackRandBox(randbox.new({
        name     = "world_seed",
        width    = createWorldMenu.baseSizes.randboxWidth,
        height   = createWorldMenu.baseSizes.randboxHeight,
        page     = createWorldMenu.page,
        font     = createWorldMenu.menuFont,
        fontSize = 24,
        uiscale  = uiscale,
        zIndex   = Z_WIDGETS,
        randType = randbox.Type.HEX_SEED,
        default  = createWorldMenu.pending.seed ~= ""
                       and createWorldMenu.pending.seed or nil,
        onChange  = function(value, id, name)
            createWorldMenu.pending.seed = value
        end,
    }))

    randbox.setPosition(createWorldMenu.seedRandBoxId,
                        cx + cw - rbTotalW, rowY(rowIndex))
    table.insert(elements, { type = "randbox", id = createWorldMenu.seedRandBoxId })

    rowIndex = rowIndex + 1

    ---------------------------------------------------------
    -- Row 3: World Size (dropdown)
    ---------------------------------------------------------
    local sizeLabelId = createWorldMenu.trackLabel(label.new({
        name     = "world_size_label",
        text     = "Size",
        font     = createWorldMenu.menuFont,
        fontSize = createWorldMenu.baseSizes.fontSize,
        color    = {1.0, 1.0, 1.0, 1.0},
        page     = createWorldMenu.page,
        uiscale  = uiscale,
    }))
    local sizeLabelHandle = label.getElementHandle(sizeLabelId)
    UI.addToPage(createWorldMenu.page, sizeLabelHandle,
                 cx, rowY(rowIndex) + s.fontSize)
    UI.setZIndex(sizeLabelHandle, Z_CONTENT)
    table.insert(elements, { type = "label", handle = sizeLabelHandle })

    createWorldMenu.sizeDropdownId = createWorldMenu.trackDropdown(dropdown.new({
        name              = "world_size",
        options           = worldSizeOptions,
        default           = createWorldMenu.pending.worldSize,
        font              = createWorldMenu.menuFont,
        fontSize          = 24,
        height            = createWorldMenu.baseSizes.dropdownHeight,
        page              = createWorldMenu.page,
        x = 0, y = 0,
        uiscale           = uiscale,
        zIndex            = Z_WIDGETS,
        validateChar      = createWorldMenu.sizeValidator,
        matchFn           = createWorldMenu.sizeMatcher,
        maxVisibleOptions = 6,
        onChange = function(value, text, id, name)
            createWorldMenu.pending.worldSize = value
            engine.logInfo("World size pending: " .. text)
        end,
    }))

    local ddSizeId = createWorldMenu.sizeDropdownId
    local ddSizeW, _ = dropdown.getSize(ddSizeId)
    dropdown.setPosition(ddSizeId, cx + cw - ddSizeW, rowY(rowIndex))
    table.insert(elements, { type = "dropdown", id = ddSizeId })

    createWorldMenu.tabElements["settings"] = elements
end

-----------------------------------------------------------
-- Advanced Tab Content
-----------------------------------------------------------

function createWorldMenu.createAdvancedContent(frameX, frameY, frameW, frameH, s, uiscale)
    local cx = frameX + math.floor(20 * uiscale)
    local cy = frameY + math.floor(20 * uiscale)
    local cw = frameW - math.floor(40 * uiscale)
    local rowIndex = 0
    local elements = {}

    local function rowY(n)
        return cy + s.rowSpacing * n
    end

    ---------------------------------------------------------
    -- Row 1: Plate Count (textbox)
    ---------------------------------------------------------
    local plateLabelId = createWorldMenu.trackLabel(label.new({
        name     = "plate_count_label",
        text     = "Plate Count",
        font     = createWorldMenu.menuFont,
        fontSize = createWorldMenu.baseSizes.fontSize,
        color    = {1.0, 1.0, 1.0, 1.0},
        page     = createWorldMenu.page,
        uiscale  = uiscale,
    }))
    local plateLabelHandle = label.getElementHandle(plateLabelId)
    UI.addToPage(createWorldMenu.page, plateLabelHandle,
                 cx, rowY(rowIndex) + s.fontSize)
    UI.setZIndex(plateLabelHandle, Z_CONTENT)
    table.insert(elements, { type = "label", handle = plateLabelHandle })

    local tbW = math.floor(createWorldMenu.baseSizes.textboxWidth * uiscale)
    createWorldMenu.plateCountTextBoxId = createWorldMenu.trackTextBox(textbox.new({
        name     = "plate_count_input",
        width    = createWorldMenu.baseSizes.textboxWidth,
        height   = createWorldMenu.baseSizes.textboxHeight,
        page     = createWorldMenu.page,
        x        = cx + cw - tbW,
        y        = rowY(rowIndex),
        uiscale  = uiscale,
        font     = createWorldMenu.menuFont,
        fontSize = 24,
        default  = createWorldMenu.pending.plateCount,
        textType = textbox.Type.NUMBER,
        zIndex   = Z_WIDGETS,
    }))
    table.insert(elements, { type = "textbox", id = createWorldMenu.plateCountTextBoxId })

    createWorldMenu.tabElements["advanced"] = elements
end

-----------------------------------------------------------
-- Right Panel (World Preview)
-----------------------------------------------------------

function createWorldMenu.createRightPanel(panelX, panelY, bounds, contentStartY,
                                           leftWidth, rightWidth, contentHeight, s, uiscale)
    local rightX = panelX + bounds.x + leftWidth + math.floor(20 * uiscale)
    
    createWorldMenu.rightPanelId = createWorldMenu.trackPanel(panel.new({
        name       = "right_panel",
        page       = createWorldMenu.page,
        x          = rightX,
        y          = contentStartY,
        width      = rightWidth,
        height     = contentHeight,
        textureSet = createWorldMenu.panelTexSet,
        color      = {0.2, 0.2, 0.2, 1.0},
        tileSize   = 64,
        zIndex     = Z_RIGHT_PANEL,
        padding    = { top = 10, bottom = 10, left = 10, right = 10 },
        uiscale    = uiscale,
    }))
    
    local rightBounds = panel.getContentBounds(createWorldMenu.rightPanelId)
    
    local previewSize = math.min(rightBounds.width, rightBounds.height) * 0.7
    local previewX = rightX + rightBounds.x + (rightBounds.width - previewSize) / 2
    local previewY = contentStartY + rightBounds.y + (rightBounds.height - previewSize) / 2
    
    if createWorldMenu.worldPreviewTexture then
        createWorldMenu.trackSprite(sprite.new({
            name    = "world_preview",
            page    = createWorldMenu.page,
            x       = previewX,
            y       = previewY,
            width   = previewSize,
            height  = previewSize,
            texture = createWorldMenu.worldPreviewTexture,
            color   = {1.0, 1.0, 1.0, 1.0},
            zIndex  = Z_PREVIEW,
            uiscale = uiscale,
        }))
    end
end

-----------------------------------------------------------
-- Bottom buttons
-----------------------------------------------------------

function createWorldMenu.createButtons(panelX, panelY, panelWidth, panelHeight,
                                        bounds, s, uiscale)
    createWorldMenu.backButtonId = createWorldMenu.trackButton(button.new({
        name       = "back_btn",
        text       = "Back",
        width      = createWorldMenu.baseSizes.btnWidth,
        height     = createWorldMenu.baseSizes.btnHeight,
        fontSize   = createWorldMenu.baseSizes.fontSize,
        uiscale    = uiscale,
        page       = createWorldMenu.page,
        font       = createWorldMenu.menuFont,
        textureSet = createWorldMenu.buttonTexSet,
        bgColor    = {1.0, 1.0, 1.0, 1.0},
        textColor  = {0.0, 0.0, 0.0, 1.0},
        zIndex     = Z_BUTTONS,
        onClick = function(id, name)
            if createWorldMenu.showMenuCallback then
                createWorldMenu.showMenuCallback("main")
            end
        end,
    }))

    createWorldMenu.defaultsButtonId = createWorldMenu.trackButton(button.new({
        name       = "defaults_btn",
        text       = "Defaults",
        width      = createWorldMenu.baseSizes.btnWidth,
        height     = createWorldMenu.baseSizes.btnHeight,
        fontSize   = createWorldMenu.baseSizes.fontSize,
        uiscale    = uiscale,
        page       = createWorldMenu.page,
        font       = createWorldMenu.menuFont,
        textureSet = createWorldMenu.buttonTexSet,
        bgColor    = {1.0, 1.0, 1.0, 1.0},
        textColor  = {0.0, 0.0, 0.0, 1.0},
        zIndex     = Z_BUTTONS,
        onClick = function(id, name)
            createWorldMenu.onDefaults()
        end,
    }))

    createWorldMenu.generateButtonId = createWorldMenu.trackButton(button.new({
        name       = "generate_btn",
        text       = "Generate World",
        width      = createWorldMenu.baseSizes.generateBtnWidth,
        height     = createWorldMenu.baseSizes.btnHeight,
        fontSize   = createWorldMenu.baseSizes.fontSize,
        uiscale    = uiscale,
        page       = createWorldMenu.page,
        font       = createWorldMenu.menuFont,
        textureSet = createWorldMenu.buttonTexSet,
        bgColor    = {0.2, 0.8, 0.2, 1.0},
        textColor  = {1.0, 1.0, 1.0, 1.0},
        zIndex     = Z_BUTTONS,
        onClick = function(id, name)
            createWorldMenu.onGenerateWorld()
        end,
    }))

    local backW, backH = button.getSize(createWorldMenu.backButtonId)
    local defaultsW, _ = button.getSize(createWorldMenu.defaultsButtonId)
    local generateW, _ = button.getSize(createWorldMenu.generateButtonId)
    
    local bottomPad = math.floor(100 * uiscale)
    local btnY = panelY + panelHeight - bottomPad + (bottomPad - backH) / 2
    
    local backX = panelX + bounds.x
    local defaultsX = backX + backW + s.btnSpacing
    local generateX = panelX + bounds.x + bounds.width - generateW

    local backH_     = button.getElementHandle(createWorldMenu.backButtonId)
    local defaultsH_ = button.getElementHandle(createWorldMenu.defaultsButtonId)
    local generateH_ = button.getElementHandle(createWorldMenu.generateButtonId)

    UI.setPosition(backH_,     backX, btnY)
    UI.setPosition(defaultsH_, defaultsX, btnY)
    UI.setPosition(generateH_, generateX, btnY)
    
    UI.setZIndex(backH_,     Z_BUTTONS)
    UI.setZIndex(defaultsH_, Z_BUTTONS)
    UI.setZIndex(generateH_, Z_BUTTONS)
end

-----------------------------------------------------------
-- Button handlers
-----------------------------------------------------------

function createWorldMenu.onDefaults()
    engine.logInfo("Loading create world defaults...")
    createWorldMenu.pending = {
        worldName  = "",
        seed       = "",
        worldSize  = "128",
        plateCount = "7",
    }
    createWorldMenu.createUI()
    if createWorldMenu.page then UI.showPage(createWorldMenu.page) end
end

function createWorldMenu.onGenerateWorld()
    -- Read plate count from textbox before generating
    if createWorldMenu.plateCountTextBoxId then
        createWorldMenu.pending.plateCount = textbox.getValue(createWorldMenu.plateCountTextBoxId)
    end

    local p = createWorldMenu.pending

    local seedNum = tonumber(p.seed, 16) or 0
    local sizeNum = tonumber(p.worldSize) or 128
    local plateNum = tonumber(p.plateCount) or 7

    if plateNum < 2 then plateNum = 2 end
    if plateNum > 20 then plateNum = 20 end

    engine.logInfo("Generating world: name=" .. p.worldName
        .. " seed=0x" .. p.seed
        .. " size=" .. tostring(sizeNum)
        .. " plates=" .. tostring(plateNum))

    -- Store params on worldView so it uses them when creating the world
    local worldView = require("scripts.world_view")
    worldView.worldParams = {
        seed = seedNum,
        worldSize = sizeNum,
        plateCount = plateNum,
        worldName = p.worldName,
    }

    if createWorldMenu.showMenuCallback then
        createWorldMenu.showMenuCallback("world_view")
    end
end

-----------------------------------------------------------
-- Show / Hide / Resize
-----------------------------------------------------------

function createWorldMenu.show()
    createWorldMenu.createUI()
    if createWorldMenu.page then UI.showPage(createWorldMenu.page) end
end

function createWorldMenu.hide()
    if createWorldMenu.page then UI.hidePage(createWorldMenu.page) end
end

function createWorldMenu.onFramebufferResize(width, height)
    createWorldMenu.fbW = width
    createWorldMenu.fbH = height
    if createWorldMenu.uiCreated then createWorldMenu.createUI() end
end

-----------------------------------------------------------
-- Shutdown
-----------------------------------------------------------

function createWorldMenu.shutdown()
    createWorldMenu.destroyOwned()
    if createWorldMenu.page then UI.deletePage(createWorldMenu.page) end
end

return createWorldMenu
