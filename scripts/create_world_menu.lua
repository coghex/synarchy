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
    
    local panelWidth = 500
    local panelHeight = 400
    local panelX = (fbW - panelWidth) / 2
    local panelY = (fbH - panelHeight) / 2
    
    local panel = UI.newBox("create_world_panel", panelWidth, panelHeight, boxTexSet, 64, 0.1, 0.1, 0.15, 0.95, page)
    UI.addToPage(page, panel, panelX, panelY)
    
    local titleText = UI.newText("cw_title", "Create New World", menuFont, 1.0, 1.0, 1.0, 1.0, page)
    local titleWidth = engine.getTextWidth(menuFont, "Create New World")
    UI.addChild(panel, titleText, (panelWidth - titleWidth) / 2, 30)
    
    local yOffset = 100
    local lineHeight = 70
    
    local nameLabel = UI.newText("name_label", "World Name:", menuFont, 0.9, 0.9, 0.9, 1.0, page)
    UI.addChild(panel, nameLabel, 40, yOffset)
    
    local nameDisplay = UI.newBox("name_display", panelWidth - 80, 40, boxTexSet, 32, 0.2, 0.2, 0.25, 1.0, page)
    UI.addChild(panel, nameDisplay, 40, yOffset + 30)
    
    elements.nameText = UI.newText("name_text", worldSettings.name, menuFont, 1.0, 1.0, 1.0, 1.0, page)
    UI.addChild(nameDisplay, elements.nameText, 10, 8)
    
    yOffset = yOffset + lineHeight
    
    local seedLabel = UI.newText("seed_label", "Seed:", menuFont, 0.9, 0.9, 0.9, 1.0, page)
    UI.addChild(panel, seedLabel, 40, yOffset)
    
    elements.seedText = UI.newText("seed_text", worldSettings.seed, menuFont, 0.8, 0.8, 0.8, 1.0, page)
    UI.addChild(panel, elements.seedText, 150, yOffset)
    
    local randomBtn = UI.newBox("random_seed_btn", 100, 35, boxTexSet, 32, 0.3, 0.4, 0.5, 1.0, page)
    UI.addChild(panel, randomBtn, panelWidth - 150, yOffset - 5)
    UI.setClickable(randomBtn, true)
    UI.setOnClick(randomBtn, "onRandomSeed")
    local randomLabel = UI.newText("random_label", "Random", menuFont, 1.0, 1.0, 1.0, 1.0, page)
    UI.addChild(randomBtn, randomLabel, 15, 6)
    
    yOffset = yOffset + lineHeight
    
    createWorldMenu.createSizeControl(panel, panelWidth, yOffset)
    
    yOffset = yOffset + lineHeight + 20
    
    local btnWidth = 150
    local btnHeight = 50
    local btnSpacing = 20
    local totalBtnWidth = btnWidth * 2 + btnSpacing
    local btnStartX = (panelWidth - totalBtnWidth) / 2
    
    local createBtn = UI.newBox("create_btn", btnWidth, btnHeight, boxTexSet, 32, 0.2, 0.7, 0.3, 1.0, page)
    UI.addChild(panel, createBtn, btnStartX, yOffset)
    UI.setClickable(createBtn, true)
    UI.setOnClick(createBtn, "onCreate")
    local createLabel = UI.newText("create_label", "Create", menuFont, 1.0, 1.0, 1.0, 1.0, page)
    local createLabelWidth = engine.getTextWidth(menuFont, "Create")
    UI.addChild(createBtn, createLabel, (btnWidth - createLabelWidth) / 2, 18)
    
    local cancelBtn = UI.newBox("cancel_btn", btnWidth, btnHeight, boxTexSet, 32, 0.5, 0.3, 0.3, 1.0, page)
    UI.addChild(panel, cancelBtn, btnStartX + btnWidth + btnSpacing, yOffset)
    UI.setClickable(cancelBtn, true)
    UI.setOnClick(cancelBtn, "onCancel")
    local cancelLabel = UI.newText("cancel_label", "Cancel", menuFont, 1.0, 1.0, 1.0, 1.0, page)
    local cancelLabelWidth = engine.getTextWidth(menuFont, "Cancel")
    UI.addChild(cancelBtn, cancelLabel, (btnWidth - cancelLabelWidth) / 2, 18)
    
    uiCreated = true
    engine.logInfo("Create World menu created")
end

function createWorldMenu.createSizeControl(parent, parentWidth, yPos)
    local labelText = UI.newText("size_label", "World Size:", menuFont, 1.0, 1.0, 1.0, 1.0, page)
    UI.addChild(parent, labelText, 40, yPos)
    
    local arrowBtn = UI.newBox("size_left", 40, 40, boxTexSet, 32, 0.3, 0.3, 0.4, 1.0, page)
    UI.addChild(parent, arrowBtn, 200, yPos)
    UI.setClickable(arrowBtn, true)
    UI.setOnClick(arrowBtn, "onSizePrev")
    local leftArrow = UI.newText("size_left_text", "<", menuFont, 1.0, 1.0, 1.0, 1.0, page)
    UI.addChild(arrowBtn, leftArrow, 12, 8)
    
    elements.sizeText = UI.newText("size_display", worldSettings.size, menuFont, 1.0, 1.0, 1.0, 1.0, page)
    UI.addChild(parent, elements.sizeText, 260, yPos + 8)
    
    local arrowBtnR = UI.newBox("size_right", 40, 40, boxTexSet, 32, 0.3, 0.3, 0.4, 1.0, page)
    UI.addChild(parent, arrowBtnR, 370, yPos)
    UI.setClickable(arrowBtnR, true)
    UI.setOnClick(arrowBtnR, "onSizeNext")
    local rightArrow = UI.newText("size_right_text", ">", menuFont, 1.0, 1.0, 1.0, 1.0, page)
    UI.addChild(arrowBtnR, rightArrow, 12, 8)
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
