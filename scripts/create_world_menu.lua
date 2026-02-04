-- Create World Module (pure module)
local createWorldMenu = {}

local page = nil
local boxTexSet = nil
local menuFont = nil
local fbW, fbH = 0, 0
local uiCreated = false

local worldSettings = {
    name = "New World",
    seed = "",
    size = "medium"
}

local sizeOptions = {"small", "medium", "large", "huge"}
local currentSizeIndex = 2

local elements = {}

function createWorldMenu.init(boxTex, font, width, height)
    boxTexSet = boxTex
    menuFont = font
    fbW = width
    fbH = height
    
    worldSettings.seed = tostring(math.random(1000000, 9999999))
    
    createWorldMenu.createUI()
end

function createWorldMenu.createUI()
    if uiCreated and page then
        UI.deletePage(page)
        elements = {}
    end
    
    page = UI.newPage("create_world_menu", "modal")
    
    -- Larger panel - 60% width, 70% height
    local panelWidth = math.floor(fbW * 0.6)
    local panelHeight = math.floor(fbH * 0.7)
    local panelX = (fbW - panelWidth) / 2
    local panelY = (fbH - panelHeight) / 2
    
    local panel = UI.newBox("create_world_panel", panelWidth, panelHeight, boxTexSet, 64, 0.1, 0.1, 0.15, 0.95, page)
    UI.addToPage(page, panel, panelX, panelY)
    
    local titleText = UI.newText("cw_title", "Create New World", menuFont, 1.0, 1.0, 1.0, 1.0, page)
    local titleWidth = engine.getTextWidth(menuFont, "Create New World")
    UI.addChild(panel, titleText, (panelWidth - titleWidth) / 2, 50)
    
    local yOffset = 150
    local lineHeight = 120  -- Increased spacing
    local leftMargin = 80
    
    -- World name
    local nameLabel = UI.newText("name_label", "World Name:", menuFont, 0.9, 0.9, 0.9, 1.0, page)
    UI.addChild(panel, nameLabel, leftMargin, yOffset)
    
    local nameDisplayWidth = panelWidth - (leftMargin * 2)
    local nameDisplay = UI.newBox("name_display", nameDisplayWidth, 60, boxTexSet, 32, 0.2, 0.2, 0.25, 1.0, page)
    UI.addChild(panel, nameDisplay, leftMargin, yOffset + 40)
    
    elements.nameText = UI.newText("name_text", worldSettings.name, menuFont, 1.0, 1.0, 1.0, 1.0, page)
    UI.addChild(nameDisplay, elements.nameText, 20, 18)
    
    yOffset = yOffset + lineHeight
    
    -- Seed
    local seedLabel = UI.newText("seed_label", "Seed:", menuFont, 0.9, 0.9, 0.9, 1.0, page)
    UI.addChild(panel, seedLabel, leftMargin, yOffset)
    
    elements.seedText = UI.newText("seed_text", worldSettings.seed, menuFont, 0.8, 0.8, 0.8, 1.0, page)
    UI.addChild(panel, elements.seedText, leftMargin + 150, yOffset)
    
    -- Random seed button
    local randomBtn = UI.newBox("random_seed_btn", 150, 50, boxTexSet, 32, 0.3, 0.4, 0.5, 1.0, page)
    UI.addChild(panel, randomBtn, panelWidth - leftMargin - 150, yOffset - 10)
    UI.setClickable(randomBtn, true)
    UI.setOnClick(randomBtn, "onRandomSeed")
    local randomLabel = UI.newText("random_label", "Random", menuFont, 1.0, 1.0, 1.0, 1.0, page)
    local randomLabelWidth = engine.getTextWidth(menuFont, "Random")
    UI.addChild(randomBtn, randomLabel, (150 - randomLabelWidth) / 2, 15)
    
    yOffset = yOffset + lineHeight
    
    -- World size
    createWorldMenu.createSizeControl(panel, panelWidth, yOffset, leftMargin)
    
    yOffset = yOffset + lineHeight + 40
    
    -- Larger buttons
    local btnWidth = 200
    local btnHeight = 70
    local btnSpacing = 30
    local totalBtnWidth = btnWidth * 2 + btnSpacing
    local btnStartX = (panelWidth - totalBtnWidth) / 2
    
    local createBtn = UI.newBox("create_btn", btnWidth, btnHeight, boxTexSet, 32, 0.2, 0.7, 0.3, 1.0, page)
    UI.addChild(panel, createBtn, btnStartX, yOffset)
    UI.setClickable(createBtn, true)
    UI.setOnClick(createBtn, "onCreate")
    local createLabel = UI.newText("create_label", "Create", menuFont, 1.0, 1.0, 1.0, 1.0, page)
    local createLabelWidth = engine.getTextWidth(menuFont, "Create")
    UI.addChild(createBtn, createLabel, (btnWidth - createLabelWidth) / 2, 25)
    
    local cancelBtn = UI.newBox("cancel_btn", btnWidth, btnHeight, boxTexSet, 32, 0.5, 0.3, 0.3, 1.0, page)
    UI.addChild(panel, cancelBtn, btnStartX + btnWidth + btnSpacing, yOffset)
    UI.setClickable(cancelBtn, true)
    UI.setOnClick(cancelBtn, "onCancel")
    local cancelLabel = UI.newText("cancel_label", "Cancel", menuFont, 1.0, 1.0, 1.0, 1.0, page)
    local cancelLabelWidth = engine.getTextWidth(menuFont, "Cancel")
    UI.addChild(cancelBtn, cancelLabel, (btnWidth - cancelLabelWidth) / 2, 25)
    
    uiCreated = true
    engine.logInfo("Create World menu created")
end

function createWorldMenu.createSizeControl(parent, parentWidth, yPos, leftMargin)
    local labelText = UI.newText("size_label", "World Size:", menuFont, 1.0, 1.0, 1.0, 1.0, page)
    UI.addChild(parent, labelText, leftMargin, yPos)
    
    -- Larger arrows
    local arrowSize = 60
    local arrowStartX = leftMargin + 250
    
    local arrowBtn = UI.newBox("size_left", arrowSize, arrowSize, boxTexSet, 32, 0.3, 0.3, 0.4, 1.0, page)
    UI.addChild(parent, arrowBtn, arrowStartX, yPos - 10)
    UI.setClickable(arrowBtn, true)
    UI.setOnClick(arrowBtn, "onSizePrev")
    local leftArrow = UI.newText("size_left_text", "<", menuFont, 1.0, 1.0, 1.0, 1.0, page)
    UI.addChild(arrowBtn, leftArrow, 20, 15)
    
    elements.sizeText = UI.newText("size_display", worldSettings.size, menuFont, 1.0, 1.0, 1.0, 1.0, page)
    UI.addChild(parent, elements.sizeText, arrowStartX + arrowSize + 30, yPos)
    
    local arrowBtnR = UI.newBox("size_right", arrowSize, arrowSize, boxTexSet, 32, 0.3, 0.3, 0.4, 1.0, page)
    UI.addChild(parent, arrowBtnR, arrowStartX + arrowSize + 30 + 150, yPos - 10)
    UI.setClickable(arrowBtnR, true)
    UI.setOnClick(arrowBtnR, "onSizeNext")
    local rightArrow = UI.newText("size_right_text", ">", menuFont, 1.0, 1.0, 1.0, 1.0, page)
    UI.addChild(arrowBtnR, rightArrow, 20, 15)
end

function createWorldMenu.onRandomSeed()
    worldSettings.seed = tostring(math.random(1000000, 9999999))
    UI.setText(elements.seedText, worldSettings.seed)
end

function createWorldMenu.onSizePrev()
    currentSizeIndex = currentSizeIndex - 1
    if currentSizeIndex < 1 then currentSizeIndex = #sizeOptions end
    worldSettings.size = sizeOptions[currentSizeIndex]
    UI.setText(elements.sizeText, worldSettings.size)
end

function createWorldMenu.onSizeNext()
    currentSizeIndex = currentSizeIndex + 1
    if currentSizeIndex > #sizeOptions then currentSizeIndex = 1 end
    worldSettings.size = sizeOptions[currentSizeIndex]
    UI.setText(elements.sizeText, worldSettings.size)
end

function createWorldMenu.getSettings()
    return worldSettings
end

function createWorldMenu.show()
    if page then
        UI.showPage(page)
        engine.logInfo("Create World menu shown")
    end
end

function createWorldMenu.hide()
    if page then
        UI.hidePage(page)
        engine.logInfo("Create World menu hidden")
    end
end

function createWorldMenu.onFramebufferResize(width, height)
    fbW = width
    fbH = height
    if uiCreated then
        createWorldMenu.createUI()
    end
end

function createWorldMenu.shutdown()
    if page then
        UI.deletePage(page)
    end
end

return createWorldMenu
